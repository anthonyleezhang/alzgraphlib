skp = function(plotdata, x, y, yzero = 0) {
  
  xname = deparse(substitute(x))
  plotdata[, temp_x := eval(parse(text = xname))]
  
  yname = deparse(substitute(y))
  plotdata[, temp_y := eval(parse(text = yname))]
  
  plot = ggplot(plotdata, aes(x = temp_x, y = temp_y)) + 
    geom_point() +
    geom_smooth(size = 1.3) + 
    scale_x_continuous(name = xname) +
    theme(text = element_text(size = 40))
  
  if(yzero == 1) {
    plot = plot + scale_y_continuous(name = yname, limits = c(0, plotdata[, max(y)]))
  } else {
    plot = plot + scale_y_continuous(name = yname)
  }
  
  return(plot)
}