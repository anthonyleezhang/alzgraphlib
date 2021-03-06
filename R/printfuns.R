#' @export

# Does exactly the same thing as print(paste())

d = function(...) {
  print(paste(..., sep = ""))
}

# d(1,2)

#' @export

# Fast print data table with no limit

pr = function(data, topn = 10000) {
  print(data, topn = topn)
}

#' @export

p = function(...) {
  return(paste(..., sep = ""))
}