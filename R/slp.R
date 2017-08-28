#' @export
#' @import data.table

slp = function(plotdata, x, y, color = 1, lty = 1, yzero = 0) {
  
  xname = deparse(substitute(x))
  yname = deparse(substitute(y))
  
  plotdata[, temp_x := eval(parse(text = xname))]
  plotdata[, temp_y := eval(parse(text = yname))]
  
  colname = deparse(substitute(color))
  plotdata[, temp_colvar := eval(parse(text = colname))]
  
  ltyname = deparse(substitute(lty))
  plotdata[, temp_ltyvar := eval(parse(text = ltyname))]
  
  plot = ggplot(plotdata, aes(x = temp_x, y = temp_y, group = paste(temp_colvar, temp_ltyvar), 
                              color = temp_colvar, lty = factor(temp_ltyvar))) + 
    geom_line(size = 1.3, alpha = 0.7) + 
    xlab(xname) + 
    theme(text = element_text(size = 40)) + 
    scale_linetype_discrete(name = ltyname)
  
  if(class(plotdata$temp_colvar) %in% c("numeric", "integer")) {plot = plot + scale_color_gradient(name = colname, low = "blue1", high = "darkorange1")
  } else {
    plot = plot + scale_color_discrete(name = colname)
  }
  
  if(yzero == 1) {
    plot = plot + scale_y_continuous(name = yname, limits = c(0, plotdata[, max(get(yname))]))
  } else {
    plot = plot + scale_y_continuous(name = yname)
  }
  
  return(plot)
}




