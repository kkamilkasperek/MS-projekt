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

# Test Davida-Hellwiga – złożony
david_hellwig_test_complex <- function(x, test_number = NA) {
  n <- length(x)
  m <- n
  critical_value <- critic_value_hellwig_complex(n)

  x_bar <- mean(x)
  s <- sd(x)
  z <- (x - x_bar) / s
  u <- pnorm(z)

  cell_indices <- floor(u * m) + 1
  cell_indices[cell_indices > m] <- m

  cell_counts <- table(factor(cell_indices, levels = 1:m))
  K_n <- sum(cell_counts == 0)
  decision <- ifelse(K_n > critical_value, "TAK", "NIE")

  return(data.frame(
    numer_testu = test_number,
    rodzaj = "complex",
    K_puste = K_n,
    wartosc_krytyczna = critical_value,
    odrzucono = decision
  ))
}

# Test Davida-Hellwiga – prosty
david_hellwig_test_simple <- function(x, test_number = NA) {
  mu <- 0
  sigma <- 1
  n <- length(x)
  m <- n

  critical_value <- critic_value_hellwig_simple(n)

  z <- (x - mu) / sigma
  u <- pnorm(z)

  cell_indices <- floor(u * m) + 1
  cell_indices[cell_indices > m] <- m

  cell_counts <- table(factor(cell_indices, levels = 1:m))
  K_n <- sum(cell_counts == 0)
  decision <- ifelse(K_n > critical_value, "TAK", "NIE")

  return(data.frame(
    numer_testu = test_number,
    rodzaj = "simple",
    K_puste = K_n,
    wartosc_krytyczna = critical_value,
    odrzucono = decision
  ))
}

# Główna funkcja wykonująca testy i zapisująca wyniki
run_hellwig_tests <- function(n = 25, B = 10, output_dir = "wyniki") {
  dir.create(output_dir, showWarnings = FALSE)

  distributions <- list(
    norm = function() rnorm(n),
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

    for (i in 1:B) {
      x_sample <- dist_fun()

      result_complex <- david_hellwig_test_complex(x_sample, test_number = i)
      result_simple  <- david_hellwig_test_simple(x_sample, test_number = i)

      result_rows <- rbind(result_rows, result_complex, result_simple)
    }

    write.csv(result_rows,
              file = file.path(output_dir, paste0(dist_name, ".csv")),
              row.names = FALSE)
    cat("✅ Zapisano:", paste0(dist_name, ".csv"), "\n")
  }

  cat("\n✅ Wszystkie testy zakończone. Wyniki zapisano w katalogu:", output_dir, "\n")
}
