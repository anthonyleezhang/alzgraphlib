#' @export
#' @import data.table

# Proportions plot

spp = function(data, x, color) {
  
  xname = deparse(substitute(x))
  data[, temp_x := eval(parse(text = xname))]
  
  colname = deparse(substitute(color))
  data[, temp_colvar := eval(parse(text = colname))]
  
  plotdata = data[, .N, by = .(temp_x, temp_colvar)]
  
  plot = ggplot(plotdata, aes(x = temp_x, y = N, group = temp_colvar, fill = temp_colvar)) + 
    geom_bar(stat = "identity", position = "fill") + 
    scale_x_continuous(name = xname) +
    scale_y_continuous(name = "Fraction") +
    scale_color_discrete(name = colname) +
    theme(text = element_text(size = 40))
  
  return(plot)
}
