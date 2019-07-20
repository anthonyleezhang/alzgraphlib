#' @export
#' @import data.table

wmean = function(x, weights) {
  return(weighted.mean(x,weights))
}

# x = c(1,2)
# weights = c(1,2)
# wmean(x, weights)
