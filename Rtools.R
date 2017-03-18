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

skp = function(plotdata, xname = "x", yname = "y", yzero = 0) {
  plot = ggplot(plotdata, aes(x = get(xname), y = get(yname))) + 
    geom_point() +
    geom_smooth(size = 1.3) + 
    scale_x_continuous(name = xname) +
    theme(text = element_text(size = 40))
  
  if(yzero == 1) {
    plot = plot + scale_y_continuous(name = yname, limits = c(0, plotdata[, max(y)]))
  } else {
    plot = plot + scale_y_continuous(name = yname)
  }
  
  return(plot)
}

# plotdata = data.table(x = 1:1000/100)
# plotdata[, y := x + rnorm(.N)]
# skp(plotdata, yzero = 0)

sdp = function(plotdata, xname = "x", colname = "", ltyname = "", wname = "", factor_color = TRUE) {
  
  if(colname == "") {plotdata[, temp_colvar := 1]}
  if(colname != "") {plotdata[, temp_colvar := get(colname)]}
  
  if(ltyname == "") {plotdata[, temp_ltyvar := 1]}
  if(ltyname != "") {plotdata[, temp_ltyvar := get(ltyname)]}
  
  if(wname == "") {plotdata[, temp_weights := 1/.N, by = .(temp_colvar, temp_ltyvar)]}
  if(wname != "") {plotdata[, temp_weights := get(wname) / sum(get(wname), na.rm = TRUE), 
                            by = .(temp_colvar, temp_ltyvar)]}
  
  if(factor_color == TRUE) {
    plot = ggplot(plotdata, aes(x = get(xname), group = factor(paste(temp_colvar, temp_ltyvar)), 
                                color = factor(temp_colvar), lty = factor(temp_ltyvar)))
    
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

dbg = function(data, yvar, byvar = "", meanvars = character(0), weightvar = "", topn = 5) {
  
  data[, temp_yvar := get(yvar)]
  
  if(weightvar != "") {data[, temp_weightvar := get(weightvar)]}
  if(weightvar == "") {data[, temp_weightvar := 1]}
  
  if(byvar != "") {data[, temp_byvar := get(byvar)]}
  if(byvar == "") {data[, temp_byvar := 1]}
  
  data[, totweight := sum(temp_weightvar)]
  
  # This is a ugly eval(parse()) loop but I can't think of a better way to do it...
  
  cmdstring = "temp = data[, .(weight = sum(temp_weightvar), totweight = totweight[1]"
  
  for(my_meanvar in meanvars) {
    temp_cmd = paste(", mean_", my_meanvar, " = sum(temp_weightvar * ", my_meanvar, ", na.rm = TRUE) / sum(temp_weightvar, na.rm = TRUE)", sep = "")
    cmdstring = paste(cmdstring, temp_cmd, sep = "")
  }
  
  cmdstring = paste(cmdstring, "), by = .(temp_byvar, temp_yvar)]", sep = "")
  
  eval(parse(text = cmdstring))
  
  ##########
  
  temp[order(temp_byvar, weight, decreasing = TRUE), rank := 1:.N, by = temp_byvar]
  temp[, groupweight := sum(weight), by = temp_byvar]
  
  temp2 = temp[rank <= topn]
  temp2 = temp2[order(temp_byvar, rank)]
  temp2[, frac := percent(weight / groupweight)]
  temp2[, totfrac := percent(weight / totweight), by = temp_byvar]
  temp2[, cum_groupfrac := percent(cumsum(weight) / groupweight), by = temp_byvar]
  temp2[, group_totfrac := percent(groupweight / totweight), by = temp_byvar]
  
  temp2[, rank := NULL]
  
  meannames = names(temp2)[which(grepl("mean_", names(temp2)))]
  
  setcolorder(temp2, c("temp_byvar", "temp_yvar", "frac", "cum_groupfrac",
                       "totfrac", "group_totfrac", meannames, "weight", "groupweight", "totweight"))
  
  setnames(temp2, "temp_byvar", byvar)
  setnames(temp2, "temp_yvar", yvar)
  
  print(temp2)
  return(temp2)
}

# setwd("E:/acs_2010")
# rm(list = ls())
# source("C:/Users/Anthony Lee Zhang/Dropbox/operational/alzRtools/Rtools.R")
# 
# load("data_small_clean.RData")
# 
# out = datatopn(data, yvar = "race", byvar = "sex", meanvar = c("income", "numage"), weightvar = "perwt")







