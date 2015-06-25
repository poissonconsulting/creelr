plural <- function(x, s = FALSE, end = "") {
  paste0(x, ifelse(s, "s", ""), end)
}

punctuate_strings <- function(x, qualifier = "or") {
  if (length(x) == 1)
    return(x)
  n <- length(x)
  paste(paste(x[-n], collapse = ", "), qualifier, x[n])
}
