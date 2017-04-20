# Does exactly the same thing as print(paste())

d = function(...) {
  print(paste(..., sep = ""))
}

# d(1,2)
