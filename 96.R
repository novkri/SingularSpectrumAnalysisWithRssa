load("C:/Users/chris/Desktop/savw/data_new_167_96.RData") #data frame

load("C:/Users/chris/Desktop/savw/ts_96.RData") # ts

library(dplyr)
library(Rssa)

plot(ts_96, main = "Средняя температура, градусы Цельсия", sub = "C 2018-09-28 до 2019-12-26, шаг дискретизации - 5 точек в месяц", xlab = "Время", ylab = "Температура")


# SSA ---------
# Decompose
s_96 <- ssa(ts_96)
s_96_s <- ssa(ts_96, L = 11)
summary(s_96_s)
# plot(s_full)
plot(s_96_s, type = "vectors", idx=1:10)
plot(s_96_s, type = "paired", idx=1:10)
plot(s_96, type = "series", groups = as.list(1:10)) 



# W-correlation
plot(wcor(s_96_s)) 
plot(wcor(s_96_s, groups = 1:15))


# Reconstruction -------
# ?reconstruct
r_for_all_pairs <- reconstruct(s_96, groups = list(OneTwo = c(1,2), TwoThree = c(2,3), ThreeFour = c(3,4), FourFive = c(4,5), FiveSix = c(5,6), SixSeven = c(6,7)))
plot(r_for_all_pairs)

r_for_all <- reconstruct(s_96, groups = list(1, 2, 3, 4, 5, 6, 7, 8))
plot(r_for_all, add.residuals = FALSE, add.original = FALSE)

r.fort <- reconstruct(s_96, groups = list(Trend = 1:4, Seasonality = 9:11)) 
plot(r.fort, add.residuals = FALSE, add.original = TRUE, plot.method = "xyplot", superpose = TRUE, auto.key = list(columns = 2))




#res1 <- reconstruct(s_full, groups = list(1:3)) 
#trend <- res1$F1 
#plot(res1, add.residuals = FALSE, plot.type = "single", col = c("black", "red"), lwd = c(1, 2))
# Calculate the residuals
#res <- residuals(r_for_all)
#plot(res, main = "residuals")

# гетероскедастичность ? остатков
# s.env <- ssa(res^2, L=10000) 
# rsd <- sqrt(reconstruct(s.env, groups=list(1,2))$F1) 
# plot(res, type='l'); lines(rsd, type='l'); lines(-rsd, type='l')




# Forecasting ------
# Recurrent forecast, the forecasted points added to the base series 
for1a <- rforecast(s_96, groups = list(1:4, 1:6, 1:9, 1:25), len = 30,
                   only.new = F)
plot(cbind(ts_96, for1a$F4), plot.type='single', col=c('black','red'))


# Vector forecast
for2 <- vforecast(s_96, groups = list(1:3, 1:6, 1:25), len = 50, only.new = FALSE)
plot(cbind(ts_96, for2$F3), plot.type='single', col=c('black','red')) 



for1a <- rforecast(s_96, groups = list(1:12), len = 30)
for2 <- vforecast(s_96, groups = list(1:16), len = 30)
plot(cbind(ts_96, for1a,for2), plot.type = "single", col=c("black","red", "blue"))

