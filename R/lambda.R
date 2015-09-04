lambda_symbols <- c(".", ".i", ".name")
lambda_class <- "lambda_expression"

lambda <- function(expr) {
  if (inherits(expr, lambda_class)) return(expr)
  if (is.formula(expr)) {
    if (length(expr) == 2L) return(Recall(expr[[2L]]))
    lhs <- expr[[2L]]
    expr <- expr[[3L]]
    lhs_symbols <- as.character(if (is.symbol(lhs)) lhs else lhs[-1L])
    lambda_symbols[which(nzchar(lhs_symbols))] <- lhs_symbols
  }
  structure(list(expr = expr, symbols = lambda_symbols), class = lambda_class)
}
