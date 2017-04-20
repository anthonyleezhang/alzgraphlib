# Quantiles kernel plot

qqp = function(plotdata, x, y) {
  
  xname = deparse(substitute(x))
  plotdata[, temp_x := eval(parse(text = xname))]
  plotdata[, xquantile := rank(temp_x) / length(temp_x)]
  
  yname = deparse(substitute(y))
  plotdata[, temp_y := eval(parse(text = yname))]
  plotdata[, yquantile := rank(temp_y) / length(temp_y)]
  
  plot = skp(plotdata, xquantile, yquantile)
  
  plot = plot + 
    xlab(paste(xname, "quantile")) + 
    ylab(paste(yname, "quantile"))
  
  return(plot)
}

# data = data.table(my_x = 10 * rnorm(1000))
# data[, my_y := my_x + rnorm(1000)]
# 
# qqp(data, my_x, my_y)
# qqp(data, my_x+5, my_y)
