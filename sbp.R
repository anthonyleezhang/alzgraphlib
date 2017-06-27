sbp = function(plotdata, x, color = 1, weights = 1) {
  
  xname = deparse(substitute(x))
  plotdata[, temp_x := eval(parse(text = xname))]
  
  colname = deparse(substitute(color))
  plotdata[, temp_colvar := eval(parse(text = colname))]
  
  weightname = deparse(substitute(weights))
  plotdata[, temp_weights := eval(parse(text = weightname))]
  
  if(weightname == "1") {
    rawdata[, temp_weights := 1 / .N, by = temp_colvar]
  } else {
    rawdata[!is.na(temp_x), temp_weights := get(weightname) / sum(get(weightname), na.rm = TRUE), by = temp_colvar]
  }
  
  # See algebra file for derivation
  
  rawdata[, temp_weights := get(weightname)]
  
  rawdata[, E_w := mean(temp_weights), by = temp_colvar]
  rawdata[, tot_w := sum(temp_weights), by = temp_colvar]
  rawdata[, tot_N := .N, by = temp_colvar]
  
  rawdata[, q := sum(temp_weights) / tot_w[1], by = .(temp_colvar, temp_x)]
  
  plotdata = rawdata[, .(
    frac = q[1],
    frac_var = q[1] * (var(temp_weights) / E_w[1]^2) / tot_N[1] + 
      ((mean(temp_weights))^2 / E_w[1]^2) * q[1] * (1-q[1]) / tot_N[1]),
    by = .(temp_colvar, temp_x)]
  
  plotdata[, frac_se := sqrt(frac_var)]
  
  #######
  
  plot = ggplot(plotdata, aes(x = temp_x, y = frac, ymin = frac - 2 * frac_se, ymax = frac + 2 * frac_se,
                              group = factor(temp_colvar), fill = factor(temp_colvar))) + 
    geom_bar(stat = "identity", position = "dodge") + 
    geom_errorbar(position = "dodge", size = 1.3, alpha = 0.5) + 
    scale_x_discrete(name = xname) +
    scale_y_continuous(name = "Mass") +
    scale_color_discrete(name = colname) +
    theme(text = element_text(size = 40))
  
  return(plot)
}

