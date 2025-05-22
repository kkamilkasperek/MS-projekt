# Funkcje do wczytywania wartości krytycznych (poprawione)
critic_value_hellwig <- function(n) {
    critic_values_frame <- read.csv(file = "wartosci-krytyczne.csv", header = TRUE, 
                                  sep = ",", stringsAsFactors = FALSE)
    
    values_col_name <- "α.0.05"
    n_column <- critic_values_frame[["n"]]
    result <- critic_values_frame[n_column == n, values_col_name]
    
    if (length(result) == 0) {
        stop("Nie znaleziono wartości krytycznej dla podanego n = ", n)
    }
    return(result)
}

right_quantile_of_cell_dist <- function(n) {
    data_frame <- read.csv(file = "prawe-kwantyle-interpolowane-rozkladu-pustych-cel.csv", 
                          header = TRUE, sep = ",", stringsAsFactors = FALSE)
    
    values_col_name <- "α.0.05"
    n_column <- data_frame[["n"]]
    result <- data_frame[n_column == n, values_col_name]
    
    if (length(result) == 0) {
        stop("Nie znaleziono kwantyla dla podanego n = ", n)
    }
    return(result)
}

# Funkcja pobierająca wartość krytyczną (teraz zdefiniowana)
pobierz_wartosc_krytyczna <- function(n, hipoteza = "zlozona") {
    if (hipoteza == "prosta") {
        return(critic_value_hellwig(n))
    } else {
        return(right_quantile_of_cell_dist(n))
    }
}

# Główna funkcja testu (ulepszona)
test_davida_hellwiga <- function(x, m, alpha = 0.05, hipoteza = "zlozona") {
    # Walidacja wejścia
    if (!is.numeric(x)) stop("x musi być numeryczny")
    if (length(x) < 3) stop("Próba musi mieć co najmniej 3 obserwacje")
    if (m < 2) stop("Liczba celi (m) musi być większa niż 1")
    if (!hipoteza %in% c("zlozona", "prosta")) stop("Nieprawidłowy typ hipotezy")
    
    n <- length(x)
    x_sorted <- sort(x)
    
    if (hipoteza == "zlozona") {
        x_mean <- mean(x)
        x_sd <- sd(x)
        if (x_sd < .Machine$double.eps) stop("Odchylenie standardowe nie może być zerowe")
        u <- (x_sorted - x_mean) / x_sd
    } else {
        u <- x_sorted
    }
    
    phi_u <- pnorm(u)
    l <- floor(m * phi_u) + 1
    l <- pmin(l, m)
    
    m_j <- table(factor(l, levels = 1:m))
    K_m <- sum(m_j == 0)
    
    tryCatch({
        K_alpha <- pobierz_wartosc_krytyczna(n, hipoteza)
    }, error = function(e) {
        stop("Problem z pobraniem wartości krytycznej: ", e$message)
    })
    
    decyzja <- ifelse(K_m <= K_alpha, 
                     "Odrzuc H0: rozkład nie jest normalny", 
                     "Nie ma podstaw do odrzucenia H0: rozkład może być normalny")
    
    # Bardziej szczegółowe zwracanie wyników
    list(
        statystyka_testu = K_m,
        wartosc_krytyczna = K_alpha,
        decyzja = decyzja,
        liczba_celi = m,
        rozklad_obserwacji_w_celach = as.data.frame(m_j),
        parametry = list(n = n, alpha = alpha, hipoteza = hipoteza),
        czy_odrzucono_H0 = K_m <= K_alpha
    )
}