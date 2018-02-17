#' @export
#' @import data.table

qbucket = function(x, n) {
  rankvec = rank(x)
  outvec = ceiling(rankvec / length(x) * n)
  return(outvec)
}

# x = c(1,3,2,4,5)
# qbucket(x, 3)
# x = c(6,3,2,4,5)
# qbucket(x, 3)
# x = rnorm(1000)
# asdf = qbucket(x, 5)
# x[asdf==1]
# x[asdf==5]
# sum(asdf==5)
