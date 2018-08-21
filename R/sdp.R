#' @export
#' @import data.table

sdp = function(plotdata, x, color = 1, lty = 1, weights = 1, topn = 10, maxsize = 200000) {
  
  if(plotdata[, .N] > maxsize) {plotdata = plotdata[sample(1:.N, maxsize)]}
  
  xname = deparse(substitute(x))
  plotdata[, temp_x := eval(parse(text = xname))]
  
  colname = deparse(substitute(color))
  plotdata[, temp_colvar := eval(parse(text = colname))]
  
  ltyname = deparse(substitute(lty))
  plotdata[, temp_ltyvar := eval(parse(text = ltyname))]
  
  weightname = deparse(substitute(weights))
  plotdata[, temp_weights := eval(parse(text = weightname))]
  
  weightdata = plotdata[, .(weight = sum(temp_weights)), by = .(temp_colvar, temp_ltyvar)]
  weightdata[order(weight), rank := 1:.N]
  
  plotdata = merge(plotdata, weightdata[rank <= topn, .(temp_colvar, temp_ltyvar)], 
                   by = c("temp_colvar", "temp_ltyvar"))
  
  plotdata[, norm_weights := temp_weights / sum(temp_weights), by = .(temp_colvar, temp_ltyvar)]
  
  
  plot = ggplot(plotdata, aes(x = temp_x, group = factor(paste(temp_colvar, temp_ltyvar)), 
                              color = temp_colvar, lty = factor(temp_ltyvar), weights = norm_weights))
  
  if(class(plotdata$temp_colvar) %in% c('integer', 'numeric') {
    plot = plot + scale_color_gradient(name = colname, low = "blue1", high = "darkorange1")
  } else {
    plot = plot + scale_color_gdocs(name = colname)
  }
  
  plot = plot + 
    geom_line(stat = "density", size = 1.3, alpha = 0.7) + 
    scale_x_continuous(name = xname) +
    scale_y_continuous(name = "Density") +
    scale_linetype_discrete(name = ltyname) +
    theme(text = element_text(size = 40))
  
  return(plot)
}


