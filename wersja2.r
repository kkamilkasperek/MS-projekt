critic_value_hellwig = function(n) {
    critic_values_frame = read.csv(file = "wartosci-krytyczne.csv", header = TRUE,  sep = ",", row.names = NULL,  stringsAsFactors = FALSE)
    
    values_col_name = "α.0.05"
    n_column = critic_values_frame[["n"]]
    result = critic_values_frame[n_column == n, values_col_name]
    return(result)
}

david_hellwig_test <- function(x, m = length(x), critical_value) {
  # Sortowanie danych (do zobrazowania)
  x_sorted <- sort(x)
  
  # Krok 1: estymacja parametrów rozkładu normalnego
  n <- length(x)
  x_bar <- mean(x_sorted)
  s <- sd(x_sorted)
  
  # Krok 2: standaryzacja
  z <- (x_sorted - x_bar) / s
  
  # Krok 3: wartości dystrybuanty normalnej
  u <- pnorm(z)
  
  # Krok 4: przypisanie do przedziałów (komórek)
  cell_indices <- floor(u * m) + 1
  # Korekta jeśli trafia w 1.000
  cell_indices[cell_indices > m] <- m
  
  # Krok 5: zliczenie liczby obserwacji w każdej komórce
  cell_counts <- table(factor(cell_indices, levels = 1:m))
  
  # Krok 6: liczba pustych komórek
  K_n <- sum(cell_counts == 0)
  
  # Wyświetlenie przypisań (dla lepszego zrozumienia)
  cat("Przypisania do komórek:\n")
  assign_df <- data.frame(
    x = round(x_sorted, 4),
    z = round(z, 4),
    u = round(u, 4),
    komorka = cell_indices
  )
  print(assign_df)
  
  # Wypisanie liczby pustych komórek i porównanie z wartością krytyczną
  cat("\nLiczba pustych komórek (K_n):", K_n, "\n")
  cat("Wartość krytyczna:", critical_value, "\n")
  
  # Decyzja
  if (K_n > critical_value) {
    cat("\n❌ Odrzucamy hipotezę H0: dane nie pochodzą z rozkładu normalnego\n")
    return(FALSE)
  } else {
    cat("\n✅ Brak podstaw do odrzucenia H0: dane mogą pochodzić z rozkładu normalnego\n")
    return(TRUE)
  }
}