skp = function(plotdata, xname = "x", yname = "y", yzero = 0) {
  plot = ggplot(plotdata, aes(x = get(xname), y = get(yname))) + 
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