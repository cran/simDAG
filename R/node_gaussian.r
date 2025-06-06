
## a node modeled using linear regression
#' @export
node_gaussian <- function(data, parents, formula=NULL, betas,
                          intercept, error, var_corr=NULL) {

  # if formula includes random effects, use node_lmer() instead
  if (!is.null(formula) & !is.null(var_corr)) {
    out <- node_lmer(data=data, formula=formula, betas=betas,
                     intercept=intercept, var_corr=var_corr, error=error,
                     family="gaussian")
    return(out)
  }

  if (!data.table::is.data.table(data)) {
    data.table::setDT(data)
  }

  if (!is.null(formula)) {
    data <- stats::model.matrix(object=formula, data=data)
    data <- as.data.frame(data[, -1])
  } else {
    data <- as.data.frame(data[, parents, with=FALSE])
  }

  out <- intercept +
    rowSums(mapply("*", data, betas)) +
    stats::rnorm(n=nrow(data), mean=0, sd=error)
  return(out)
}
