#' @export
#' @import data.table

makeranks = function(data, y, weights = 1) {
  
  # Later, make this do ranks by group
  
  weightname = deparse(substitute(weights))
  data[, temp_weightvar := eval(parse(text = weightname))]
  
  yname = deparse(substitute(y))
  asdf = data[, .(weight = sum(temp_weightvar)), by = .(yvar = get(yname))]
  asdf[order(-weight), temprank := 1:.N]
  
  data = merge(data, asdf[, .(yvar, temprank)], by.x = yname, by.y = "yvar")
  
  rankname = paste(yname, "_rank", sep = "")
  setnames(data, "temprank", rankname)
  
  data[, temp_weightvar := NULL]
  
  return(data)
}



