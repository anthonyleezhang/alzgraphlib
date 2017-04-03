data = data.table(x = 1:10, y = (1:10)^2, color = c(1,1,1,1,1,2,2,2,2,2))

skp(data, x, y)
skp(data, x, sqrt(y))

slp(data, x, y)
slp(data, x, sqrt(y))
slp(data, x, y, color)
slp(data, x, y, factor(color))


data = data.table(x = c(rnorm(100, 0), rnorm(100, 1)), color = c(rep(1,100), rep(2,100)), 
                  color2 = c(rep(1,50), rep(2,50), rep(3,50), rep(4,50)), weight = runif(200))

sdp(data, x, color)
sdp(data, x, color, weights = weight)

dbg(data, color)
dbg(data, color, color2)
dbg(data, y = color, by = color2)
rm(list = ls())


