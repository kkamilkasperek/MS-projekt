david_hellwig_test <- function(x, m = 10, K_critical = NULL, alpha = 0.05, plot = FALSE) {
  # Sprawdzenie poprawności danych
  if (!is.numeric(x)) stop("Dane muszą być wektorem liczbowym.")
  if (length(x) < 5) stop("Zbyt mała liczba obserwacji (minimum 5).")
  
  n <- length(x)
  x_sorted <- sort(x)
  x_mean <- mean(x)
  x_sd <- sd(x)
  
  # Standaryzacja i wartości dystrybuanty
  u <- (x_sorted - x_mean) / x_sd
  phi <- pnorm(u)
  
  # Przypisanie do celi
  l <- floor(m * phi) + 1
  l[l > m] <- m  # Korekta dla wartości równych 1
  
  # Zliczanie elementów w celach
  counts <- table(factor(l, levels = 1:m))
  K_star <- sum(counts == 0)
  
  cat("Statystyka testowa K* =", K_star, "\n")
  
  # Jeśli podano wartość krytyczną – sprawdź wynik
  if (!is.null(K_critical)) {
    if (K_star > K_critical) {
      cat("Wniosek: Odrzucamy H0 – dane nie pochodzą z rozkładu normalnego.\n")
    } else {
      cat("Wniosek: Brak podstaw do odrzucenia H0 – dane mogą pochodzić z rozkładu normalnego.\n")
    }
  } else {
    cat("Uwaga: Nie podano wartości krytycznej. Wynik testu: K* =", K_star, "\n")
  }
  
  # Opcjonalne rysowanie
  if (plot) {
    barplot(counts,
            main = "Liczba elementów w celach (David-Hellwig)",
            xlab = "Cele",
            ylab = "Liczba obserwacji",
            col = "lightblue")
    abline(h = 0, col = "red")
  }
  
  invisible(K_star)
}
