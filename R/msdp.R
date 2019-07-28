#' @export
#' @import data.table

msdp = function(plotdata, y1, y2, y3 = NULL, y4 = NULL, ymax = Inf) {
  
  y1name = deparse(substitute(y1))
  y2name = deparse(substitute(y2))
  y3name = deparse(substitute(y3))
  y4name = deparse(substitute(y4))
  
  data = rbindlist(list(
    plotdata[, .(y = eval(parse(text = y1name)), type = y1name)],
    plotdata[, .(y = eval(parse(text = y2name)), type = y2name)]
  ))
  
  if(y3name != "NULL") {
    y3name = deparse(substitute(y3))
    data = rbindlist(list(
      data, 
      plotdata[, .(y = eval(parse(text = y3name)), type = y3name)]
    ))
  }
  
  if(y4name != "NULL") {
    y4name = deparse(substitute(y4))
    data = rbindlist(list(
      data,
      plotdata[, .(y = eval(parse(text = y4name)), type = y4name)]
    ))
  }
  
  return(sdp(data[abs(y) < ymax], y, type))
}




