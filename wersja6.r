# Funkcja pobierająca wartość krytyczną dla testu złożonego
critic_value_hellwig_complex <- function(n) {
  critic_values_frame <- read.csv("tabela_dla_zlozonych.csv", stringsAsFactors = FALSE)
  values_col_name <- "α.0.05"
  res <- critic_values_frame[critic_values_frame$n == n, values_col_name]
  if(length(res) == 0) stop(paste("Brak wartości krytycznej dla n =", n))
  return(res)
}

# Funkcja pobierająca wartość krytyczną dla testu prostego
critic_value_hellwig_simple <- function(n) {
  critic_values_frame <- read.csv("tabela_dla_prostych.csv", stringsAsFactors = FALSE)
  values_col_name <- "α.0.05"
  res <- critic_values_frame[critic_values_frame$n == n, values_col_name]
  if(length(res) == 0) stop(paste("Brak wartości krytycznej dla n =", n))
  return(res)
}

hellwig_test <- function(x, hypothesis_type = c("simple", "complex"), x_mean = NULL, x_sigma = NULL, m = NULL, test_number = NA) {
  hypothesis_type <- match.arg(hypothesis_type) # rodzaj hipotezy
  n <- length(x)

    if (is.null(m)) {   # liczba cel
        m <- n
    }
  
  if (hypothesis_type == "simple") {
    if (is.null(x_mean) || is.null(x_sigma)) {
      stop("Dla hipotezy prostej należy podać znane wartości mu i sigma.")
    }
    
    u <- (x - x_mean) / x_sigma
    critical_value <- critic_value_hellwig_simple(n)

  } else if (hypothesis_type == "complex") {
    x_mean <- mean(x)
    x_sigma <- sd(x)
    u <- (x - x_mean) / x_sigma
    critical_value <- critic_value_hellwig_complex(n)
  }
  
  Fi <- pnorm(u)    # dystrybuanta rozkładu normalnego
  cell_indices <- floor(Fi * m) + 1
  cell_indices[cell_indices > m] <- m
  
  cell_counts <- table(factor(cell_indices, levels = 1:m))
  K_n <- sum(cell_counts == 0)
  decision <- ifelse(K_n > critical_value, "TAK", "NIE")
  
  return(data.frame(
    numer_testu = test_number,
    rodzaj = hypothesis_type,
    K_puste = K_n,
    wartosc_krytyczna = critical_value,
    odrzucono = decision
  ))
}


# Główna funkcja wykonująca testy i zapisująca wyniki
run_hellwig_tests <- function(n = 25, number_of_tests = 10, mean = 0, sigma = 1, output_dir = "wyniki") {
  dir.create(output_dir, showWarnings = FALSE)

  distributions <- list(
    norm = function() rnorm(n, mean = mean, sd = sigma),
    t_student1 = function() rt(n, df = 1),
    t_student2 = function() rt(n, df = 2),
    t_student4 = function() rt(n, df = 4),
    chisq1 = function() rchisq(n, df = 1),
    chisq2 = function() rchisq(n, df = 2),
    chisq4 = function() rchisq(n, df = 4),
    uniform = function() runif(n)
  )

  for (dist_name in names(distributions)) {
    cat("\n▶ Testujemy rozkład:", dist_name, "\n")
    dist_fun <- distributions[[dist_name]]

    result_rows <- data.frame()

    for (i in 1:number_of_tests) {
      x_sample <- dist_fun()
      
        result_complex <- hellwig_test(x_sample, hypothesis_type = "complex", test_number = i)
        result_simple  <- hellwig_test(x_sample, hypothesis_type = "simple", x_mean = mean, x_sigma = sigma, test_number = i)

      result_rows <- rbind(result_rows, result_complex, result_simple)
    }

    write.csv(result_rows,
              file = file.path(output_dir, paste0(dist_name, ".csv")),
              row.names = FALSE)
    cat("✅ Zapisano:", paste0(dist_name, ".csv"), "\n")
  }

  cat("\n✅ Wszystkie testy zakończone. Wyniki zapisano w katalogu:", output_dir, "\n")
}
