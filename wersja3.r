critic_value_hellwig = function(n) {
    critic_values_frame = read.csv(file = "tabela_dla_prostych.csv", header = TRUE,  sep = ",", row.names = NULL,  stringsAsFactors = FALSE)
    
    values_col_name = "α.0.05"
    n_column = critic_values_frame[["n"]]
    result = critic_values_frame[n_column == n, values_col_name]
    return(result)
}

david_hellwig_test <- function(x) {
  n <- length(x)
  m <- n
  critical_value <- critic_value_hellwig(n)

  x_bar <- mean(x)
  s <- sd(x)
  z <- (x - x_bar) / s
  u <- pnorm(z)

  cell_indices <- floor(u * m) + 1
  cell_indices[cell_indices > m] <- m

  cell_counts <- table(factor(cell_indices, levels = 1:m))
  K_n <- sum(cell_counts == 0)

  cat("Przypisania do komórek:\n")
  assign_df <- data.frame(
    x = round(x, 4),
    z = round(z, 4),
    u = round(u, 4),
    komorka = cell_indices
  )
  print(assign_df)

  cat("\nLiczba pustych komórek (K*):", K_n, "\n")
  cat("Wartość krytyczna:", critical_value, "\n")

  if (K_n > critical_value) {
    cat("\n❌ Odrzucamy hipotezę H₀: dane nie pochodzą z rozkładu normalnego\n")
    return(FALSE)
  } else {
    cat("\n✅ Brak podstaw do odrzucenia H₀: dane mogą pochodzić z rozkładu normalnego\n")
    return(TRUE)
  }
}
