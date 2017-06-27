# Stacked bar chart

ssbp = function(plotdata, x, y, color = 1) {
  
  xname = deparse(substitute(x))
  plotdata[, temp_x := eval(parse(text = xname))]
  
  yname = deparse(substitute(y))
  plotdata[, temp_y := eval(parse(text = yname))]
  
  colname = deparse(substitute(color))
  plotdata[, temp_colvar := eval(parse(text = colname))]
  
  plot = ggplot(plotdata, aes(x = temp_x, y = temp_y, group = temp_colvar, fill = temp_colvar)) + 
    geom_bar(stat = "identity", position = "dodge") + 
    scale_x_continuous(name = xname) +
    scale_y_continuous(name = yname) +
    scale_color_discrete(name = colname) +
    theme(text = element_text(size = 40))
  
  return(plot)
}
