sdp = function(plotdata, xname = "x", colname = "", ltyname = "", wname = "", topn = 10, factor_color = TRUE) {
  
  if(colname == "") {plotdata[, temp_colvar := 1]}
  if(colname != "") {plotdata[, temp_colvar := get(colname)]}
  
  if(ltyname == "") {plotdata[, temp_ltyvar := 1]}
  if(ltyname != "") {plotdata[, temp_ltyvar := get(ltyname)]}
  
  if(wname == "") {plotdata[, temp_weights := 1]}
  if(wname != "") {plotdata[, temp_weights := get(wname)]}
  
  weightdata = plotdata[, .(weight = sum(temp_weights)), by = .(temp_colvar, temp_ltyvar)]
  weightdata[order(weight), rank := 1:.N]
  
  plotdata = merge(plotdata, weightdata[rank <= topn, .(temp_colvar, temp_ltyvar)], 
                   by = c("temp_colvar", "temp_ltyvar"))
  
  plotdata[, norm_weights := temp_weights / sum(temp_weights), by = .(temp_colvar, temp_ltyvar)]
  
  if(factor_color == TRUE) {
    plot = ggplot(plotdata, aes(x = get(xname), group = factor(paste(temp_colvar, temp_ltyvar)), 
                                color = factor(temp_colvar), lty = factor(temp_ltyvar), weights = norm_weights))
    
    plot = plot + scale_color_discrete(name = colname)
  }
  
  if(factor_color == FALSE) {
    plot = ggplot(plotdata, aes(x = get(xname), group = factor(paste(temp_colvar, temp_ltyvar)), 
                                color = temp_colvar, lty = factor(temp_ltyvar)))
    plot = plot + scale_color_gradient(low = "blue1", high = "darkorange1")
  }
  
  plot = plot + 
    geom_line(stat = "density", size = 1.3, alpha = 0.7) + 
    scale_x_continuous(name = xname) +
    scale_y_continuous(name = "Density") +
    scale_linetype_discrete(name = ltyname) +
    theme(text = element_text(size = 40))
  
  return(plot)
}