dbg = function(data, yname, byname = "", meannames = character(0), sumnames = character(0), uniquenames = character(0), wname = "", topn = 50) {
  
  data[, temp_yvar := get(yname)]
  
  if(wname != "") {data[, temp_weightvar := get(wname)]}
  if(wname == "") {data[, temp_weightvar := 1]}
  
  if(byname != "") {data[, temp_byvar := get(byname)]}
  if(byname == "") {data[, temp_byvar := 1]}
  
  data[, totweight := sum(temp_weightvar)]
  
  # This is a ugly eval(parse()) loop but I can't think of a better way to do it...
  
  cmdstring = "temp = data[, .(count = .N, weight = sum(temp_weightvar), totweight = totweight[1]"
  
  for(my_meanvar in meannames) {
    temp_cmd = paste(", mean_", my_meanvar, " = sum(temp_weightvar * ", my_meanvar, ", na.rm = TRUE) / sum(temp_weightvar, na.rm = TRUE)", sep = "")
    cmdstring = paste(cmdstring, temp_cmd, sep = "")
  }
  
  for(my_sumvar in sumnames) {
    temp_cmd = paste(", sum_", my_sumvar, " = sum(", my_sumvar, ", na.rm = TRUE)", sep = "")
    cmdstring = paste(cmdstring, temp_cmd, sep = "")
  }
  
  for(my_uniquevar in uniquenames) {
    temp_cmd = paste(", unique_", my_uniquevar, " = length(unique(", my_uniquevar, "))", sep = "")
    cmdstring = paste(cmdstring, temp_cmd, sep = "")
  }
  
  cmdstring = paste(cmdstring, "), by = .(temp_byvar, temp_yvar)]", sep = "")
  
  eval(parse(text = cmdstring))
  
  data[, temp_yvar := NULL]
  data[, temp_weightvar := NULL]
  data[, temp_byvar := NULL]
  
  ##########
  
  temp[order(temp_byvar, weight, decreasing = TRUE), rank := 1:.N, by = temp_byvar]
  temp[, groupweight := sum(weight), by = temp_byvar]
  
  temp2 = temp[rank <= topn]
  temp2 = temp2[order(temp_byvar, rank)]
  temp2[, frac := weight / groupweight]
  temp2[, pctfrac := percent(frac)]
  temp2[, totfrac := percent(weight / totweight), by = temp_byvar]
  temp2[, cum_groupfrac := cumsum(weight) / groupweight, by = temp_byvar]
  temp2[, pct_cum_groupfrac := percent(cum_groupfrac), by = temp_byvar]
  temp2[, group_totfrac := percent(groupweight / totweight), by = temp_byvar]
  
  temp2[, rank := NULL]
  
  meannames = names(temp2)[which(grepl("mean_", names(temp2)))]
  meannames = names(temp2)[which(grepl("sum_", names(temp2)))]
  meannames = names(temp2)[which(grepl("unique_", names(temp2)))]
  
  setcolorder(temp2, c("temp_byvar", "temp_yvar", "pctfrac", "pct_cum_groupfrac", 
                       "totfrac", "group_totfrac", meannames, sumnames, uniquenames, "count", "weight", "groupweight", 
                       "totweight", "frac", "cum_groupfrac"))
  
  setnames(temp2, "temp_byvar", byname)
  setnames(temp2, "temp_yvar", yname)
  
  print(temp2)
  return(temp2)
}