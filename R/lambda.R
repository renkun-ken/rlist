lambda_symbols <- c(".", ".i", ".name")

lambda <- function(expr) {
  if (is.formula(expr)) {
    if (length(expr) == 2L) 
      return(Recall(expr[[2L]]))
    lhs <- expr[[2L]]
    expr <- expr[[3L]]
    lhs_symbols <- as.character(if (is.symbol(lhs)) lhs else lhs[-1L])
    lambda_symbols[which(nzchar(lhs_symbols))] <- lhs_symbols
  }
  list(expr = expr, symbols = lambda_symbols)
} 
