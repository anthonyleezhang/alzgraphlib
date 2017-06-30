yearquarter = function(date) {
  # Return midpoint of quarter of date
  
  fakemonth = floor((month(date)-1) / 3) * 3 + 2
  return(ymd(paste(year(date), fakemonth, "15")))
}

