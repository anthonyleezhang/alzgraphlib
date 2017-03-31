sbp = function(input_data, xname = "x", sname = "Series", wname = "") {
  rawdata = input_data
  
  if(wname == "") {
    rawdata[, temp_weights := 1 / .N, by = get(sname)]
  }
  
  if(wname != "") {
    rawdata[!is.na(get(xname)), temp_weights := get(wname) / sum(get(wname), na.rm = TRUE), by = get(sname)]
  }
  
  # See algebra file for derivation
  
  rawdata[, temp_weights := get(wname)]
  
  rawdata[, E_w := mean(temp_weights), by = get(sname)]
  rawdata[, tot_w := sum(temp_weights), by = get(sname)]
  rawdata[, tot_N := .N, by = get(sname)]
  
  rawdata[, q := sum(temp_weights) / tot_w[1], by = .(get(sname), get(xname))]
  
  plotdata = rawdata[, .(
    frac = q[1],
    frac_var = q[1] * (var(temp_weights) / E_w[1]^2) / tot_N[1] + 
      ((mean(temp_weights))^2 / E_w[1]^2) * q[1] * (1-q[1]) / tot_N[1]),
    by = .(get(sname), get(xname))]
  
  plotdata[, frac_se := sqrt(frac_var)]
  
  #######
  
  names(plotdata)[1:2] = c(sname, xname)
  
  plot = ggplot(plotdata, aes(x = get(xname), y = frac, ymin = frac - 2 * frac_se, ymax = frac + 2 * frac_se,
                              group = factor(get(sname)), fill = factor(get(sname)))) + 
    geom_bar(stat = "identity", position = "dodge") + 
    geom_errorbar(position = "dodge", size = 1.3, alpha = 0.5) + 
    scale_x_discrete(name = xname) +
    scale_y_continuous(name = "Mass") +
    scale_color_discrete(name = sname) +
    theme(text = element_text(size = 40))
  
  return(plot)
}