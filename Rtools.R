slp = function(plotdata, xname = "x", yname = "y", sname = "Series", yzero = 0) {
  plot = ggplot(plotdata, aes(x = get(xname), y = get(yname), group = get(sname), color = get(sname))) + 
    geom_line(size = 1.3, alpha = 0.7) + 
    scale_x_continuous(name = xname) +
    scale_color_discrete(name = sname) +
    theme(text = element_text(size = 40))
  
  if(yzero == 1) {
    plot = plot + scale_y_continuous(name = yname, limits = c(0, plotdata[, max(y)]))
  } else {
    plot = plot + scale_y_continuous(name = yname)
  }
  
  return(plot)
}

# plotdata = data.table(x = 1:10, y = 21:30, Series = factor(floor(1:10/5)))
# slp(plotdata, yzero = 1)

sdp = function(plotdata, xname = "x", sname = "Series", wname = "") {
  
  if(wname == "") {
    plot = ggplot(plotdata, aes(x = get(xname), group = factor(get(sname)), color = factor(get(sname))))
  }
  
  if(wname != "") {
    plotdata[!is.na(get(xname)), temp_weights := get(wname) / sum(get(wname), na.rm = TRUE), by = get(sname)]
    plot = ggplot(plotdata, aes(x = get(xname), group = factor(get(sname)), color = factor(get(sname)), weights = temp_weights))
  }
  
  plot = plot + 
    geom_line(stat = "density", size = 1.3, alpha = 0.7) + 
    scale_x_continuous(name = xname) +
    scale_y_continuous(name = "Density") +
    scale_color_discrete(name = sname) +
    theme(text = element_text(size = 40))
  
  return(plot)
}

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

# plotdata = rbindlist(list(
#   data.table(x = rnorm(10000, mean = 0), Series = 0),
#   data.table(x = rnorm(10000, mean = 1), Series = 1)
# ))
# sdp(plotdata)

datatopn = function(data, yvar, byvar = "", meanvar = "", weightvar = "", sumvar = "", topn = 5) {
  
  if(sumvar != "") {data[, temp_sumvar := get(sumvar)]}
  if(sumvar == "") {data[, temp_sumvar := 1]}
  
  if(weightvar != "") {data[, temp_weightvar := get(weightvar)]}
  if(weightvar == "") {data[, temp_weightvar := 1]}
  
  if(meanvar != "") {data[, temp_meanvar := get(meanvar)]}
  if(meanvar == "") {data[, temp_meanvar := 1]}
  
  if(byvar != "") {data[, temp_byvar := get(byvar)]}
  if(byvar == "") {data[, temp_byvar := 1]}
  
  temp = data[, .(weight = sum(temp_sumvar), 
                  temp_meanvar = sum(temp_weightvar * temp_meanvar, na.rm = TRUE) / 
                    sum(temp_weightvar, na.rm = TRUE)), 
              by = .(temp_byvar, get(yvar))]
  temp[order(temp_byvar, weight, decreasing = TRUE), rank := 1:.N, by = temp_byvar]
  temp[, groupweight := sum(weight), by = temp_byvar]
  
  temp2 = temp[rank <= topn]
  temp2[, frac := percent(weight / groupweight)]
  temp2[, totfrac := percent(sum(weight) / groupweight), by = temp_byvar]
  
  temp2 = temp2[order(temp_byvar, rank)]
  temp2[, rank := NULL]
  
  setcolorder(temp2, c(1,2,6,7,4,3,5))
  
  setnames(temp2, "temp_meanvar", meanvar)
  setnames(temp2, "temp_byvar", byvar)
  setnames(temp2, "get", yvar)
  
  print(temp2)
  return(temp2)
}

# setwd("C:/Users/Anthony Lee Zhang/Dropbox/projects/cps/cepr")
# rm(list = ls())
# 
# load("cepr_march_2015.RData")
# byvar = "race"
# yvar = "educ"
# sumvar = "weight"
# weightvar = "weight"
# meanvar = "income"
# topn = 5







