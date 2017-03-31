slp = function(plotdata, xname = "x", yname = "y", colname = "", ltyname = "", yzero = 0, factor_color = TRUE) {
  
  if(colname == "") {plotdata[, temp_colvar := 1]}
  if(colname != "") {plotdata[, temp_colvar := get(colname)]}
  
  if(ltyname == "") {plotdata[, temp_ltyvar := 1]}
  if(ltyname != "") {plotdata[, temp_ltyvar := get(ltyname)]}
  
  if(factor_color == TRUE) {
    plot = ggplot(plotdata, aes(x = get(xname), y = get(yname), group = factor(paste(temp_colvar, temp_ltyvar)), 
                                color = factor(temp_colvar), lty = factor(temp_ltyvar))) + 
      geom_line(size = 1.3, alpha = 0.7) + scale_color_discrete(name = colname)
  }
  
  if(factor_color == FALSE) {
    plot = ggplot(plotdata, aes(x = get(xname), y = get(yname), group = factor(paste(temp_colvar, temp_ltyvar)), 
                                color = as.numeric(temp_colvar), lty = factor(temp_ltyvar))) + 
      geom_line(size = 1.3, alpha = 0.7) + scale_color_continuous(name = colname)
  }
  
  plot = plot + 
    geom_line(size = 1.3, alpha = 0.7) + 
    xlab(xname) + 
    theme(text = element_text(size = 40)) + 
    scale_linetype_discrete(name = ltyname)
  
  if(yzero == 1) {
    plot = plot + scale_y_continuous(name = yname, limits = c(0, plotdata[, max(get(yname))]))
  } else {
    plot = plot + scale_y_continuous(name = yname)
  }
  
  return(plot)
}