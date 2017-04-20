skp = function(plotdata, x, y, yzero = 0) {
  
  xname = deparse(substitute(x))
  plotdata[, temp_x := eval(parse(text = xname))]
  
  yname = deparse(substitute(y))
  plotdata[, temp_y := eval(parse(text = yname))]
  
  if(yzero == 1) {yscale = scale_y_continuous(name = yname, limits = c(0, plotdata[, max(y)]))}
  else {yscale = scale_y_continuous(name = yname)}
  
  plot = ggplot(plotdata, aes(x = temp_x, y = temp_y)) + 
    geom_point() +
    geom_smooth(size = 1.3) + 
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


