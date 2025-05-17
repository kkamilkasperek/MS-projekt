# R komentarz

critic_value_hellwig = function(n) {
    critic_values_frame = read.csv(file = "wartosci-krytyczne.csv", header = TRUE,  sep = ",", row.names = NULL,  stringsAsFactors = FALSE)
    
    values_col_name = "α.0.05"
    n_column = critic_values_frame[["n"]]
    result = critic_values_frame[n_column == n, values_col_name]
    return(result)
}

right_quantile_of_cell_dist = function(n) {
    data_frame = read.csv(file = "prawe-kwantyle-interpolowane-rozkladu-pustych-cel.csv", header = TRUE,  sep = ",", row.names = NULL,  stringsAsFactors = FALSE)
    
    values_col_name = "α.0.05"
    n_column = data_frame[["n"]]
    result = data_frame[n_column == n, values_col_name]
    return(result)
}