#' @export
#' @import data.table

skp = function(plotdata, x, y, color = 1, label = 1, size = 1, yzero = 0, jitter = NA) {
  
  xname = deparse(substitute(x))
  plotdata[, temp_x := eval(parse(text = xname))]
  
  yname = deparse(substitute(y))
  plotdata[, temp_y1 := eval(parse(text = yname))]
  plotdata[, temp_y := as.numeric(temp_y1)]
  
  colname = deparse(substitute(color))
  plotdata[, temp_colvar := eval(parse(text = colname))]
  
  sizename = deparse(substitute(size))
  plotdata[, temp_size := eval(parse(text = sizename))]
  
  labelname = deparse(substitute(label))
  plotdata[, temp_label := as.character(eval(parse(text = labelname)))]
  
  if(dim(plotdata)[1] > 10000) {small_plotdata = plotdata[sample(1:.N, 10000)]}
  else {small_plotdata = plotdata}
  
  if(yzero == 1) {yscale = scale_y_continuous(name = yname, limits = c(0, plotdata[, max(y)]))}
  else {yscale = scale_y_continuous(name = yname)}
  
  if(colname == '1') {
    plot = ggplot(small_plotdata, aes(x = temp_x, y = temp_y, size = temp_size, label = temp_label))
  } else {
    plot = ggplot(small_plotdata, aes(x = temp_x, y = temp_y, group = temp_colvar, color = temp_colvar, size = temp_size, label = temp_label))
  }
  
  if(colname == '1') {
  } else if(class(plotdata$temp_colvar) %in% c("numeric", "integer")) {
    plot = plot + scale_color_gradient(name = colname, low = "blue1", high = "darkorange1")
  } else {
    plot = plot + scale_color_gdocs(name = colname)
  }
  
  if(labelname == 1) {
    if(is.na(jitter)) {plot = plot + geom_point(alpha = 0.7)}
    else {plot = plot + geom_jitter(width = jitter[1], height = jitter[2], alpha = 0.7)}
  } else {
    plot = plot + geom_text()
  }
  
  plot = plot + scale_size_continuous(name = sizename, range = c(2,6))
  
  plot = plot + geom_smooth(size = 2, data = plotdata) + 
    xlab(xname) +
    yscale + 
    theme(text = element_text(size = 40))
  
  return(plot)
}

# data = data.table(x = 1:10/2, y = (1:10)^2)
# skp(data, x, y)
# datevec = as.Date("2010-01-01") + 1:10
# data = data.table(x = datevec, y = (1:10)^2)
# skp(data, x, y)


