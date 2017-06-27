skp = function(plotdata, x, y, size = 1, yzero = 0, jitter = NA) {
  
  xname = deparse(substitute(x))
  plotdata[, temp_x := eval(parse(text = xname))]
  
  yname = deparse(substitute(y))
  plotdata[, temp_y := eval(parse(text = yname))]
  
  sizename = deparse(substitute(size))
  plotdata[, temp_size := eval(parse(text = sizename))]
  
  if(dim(plotdata)[1] > 10000) {small_plotdata = plotdata[sample(1:.N, 10000)]}
  else {small_plotdata = plotdata}
  
  if(yzero == 1) {yscale = scale_y_continuous(name = yname, limits = c(0, plotdata[, max(y)]))}
  else {yscale = scale_y_continuous(name = yname)}
  
  plot = ggplot(small_plotdata, aes(x = temp_x, y = temp_y, size = temp_size))
  
  if(is.na(jitter)) {plot = plot + geom_point()}
  else {plot = plot + geom_jitter(width = jitter[1], height = jitter[2])}
  
  plot = plot + geom_smooth(size = 1.3, data = plotdata) + 
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


