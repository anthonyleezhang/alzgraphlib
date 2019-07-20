#' @export
#' @import data.table

fslp = function(plotdata, x, y1, y2, y3 = NULL, y4 = NULL, color = 1, ymax = Inf, cols = TRUE) {
  
  xname = deparse(substitute(x))
  y1name = deparse(substitute(y1))
  y2name = deparse(substitute(y2))
  y3name = deparse(substitute(y3))
  y4name = deparse(substitute(y4))
  colname = deparse(substitute(color))
  
  data = rbindlist(list(
    plotdata[, .(x = eval(parse(text = xname)), y = eval(parse(text = y1name)), color = eval(parse(text = colname)), type = y1name)],
    plotdata[, .(x = eval(parse(text = xname)), y = eval(parse(text = y2name)), color = eval(parse(text = colname)), type = y2name)]
  ))
  
  if(y3name != "NULL") {
    y3name = deparse(substitute(y3))
    data = rbindlist(list(
      data, 
      plotdata[, .(x = eval(parse(text = xname)), y = eval(parse(text = y3name)), color = eval(parse(text = colname)), type = y3name)]
    ))
  }
  
  if(y4name != "NULL") {
    y4name = deparse(substitute(y4))
    data = rbindlist(list(
      data,
      plotdata[, .(x = eval(parse(text = xname)), y = eval(parse(text = y4name)), color = eval(parse(text = colname)), type = y4name)]
    ))
  }
  
  data[, y := y / y[1], by = type]
  
  plot = ggplot(data, aes(x = x, y = y, group = color, 
                              color = color)) + 
    geom_line(size = 2, alpha = 0.7) + 
    xlab(xname) + 
    theme(text = element_text(size = 40))
  
  if(colname == '1') {
    plot = plot + scale_color_continuous(guide = FALSE)
  } else if(class(plotdata$temp_colvar) %in% c("numeric", "integer")) {
    plot = plot + scale_color_gradient(name = colname, low = "blue1", high = "darkorange1")
  } else {
    plot = plot + scale_color_gdocs(name = colname)
  }
  
  if(cols == TRUE) {
    plot = plot + facet_grid(. ~ type)
  } else {
    plot = plot + facet_grid(type ~ .)
  }
  
  return(plot)
}




