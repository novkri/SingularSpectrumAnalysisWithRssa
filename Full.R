load("D:/R/savw/data_new_167.RData") #data frame
V5_full <- c(data_new_167_2$V5)
ts_full <- ts(V5_full)
load("D:/R/savw/ts_full.RData") # ts

library(dplyr)
library(Rssa)



# Графики начальных данных
plot(ts_full, main = "Средняя температура, градусы Цельсия", sub = "C 2018-10-01 до 2019-10-15, шаг дискретизации - 1 мин", xlab = "Время", ylab = "Температура")

# SSA ---------
# Decompose
s_full <- ssa(ts_full)
s_full_n <- ssa(ts_full)
s_full_b <- ssa(ts_full, L = 100000)
summary(s_full)
plot(s_full, type = "vectors", idx=1:10)
plot(s_full, type = "paired", idx=1:10)
plot(s_full, type = "series", groups = as.list(1:10)) 



# Estimate the peroids from 2nd, 3rd, 5th and 6th eigenvectors using ESPRIT
pe <- parestimate(s, groups = list(1:8))
print(pe)
plot(pe, col = c("green", "red", "blue"))
#save(s_full, file = "C:/Users/chris/Desktop/savw/s_full.RData")
#load("D:/R/savw/s_full.RData")



# W-correlation
plot(wcor(s_full)) 
plot(wcor(s_full, groups = 1:10))


# Reconstruction -------
# ?reconstruct
r_for_all_pairs <- reconstruct(s_full, groups = list(OneTwo = c(1,2), TwoThree = c(2,3), ThreeFour = c(3,4), FourFive = c(4,5), FiveSix = c(5,6), SixSeven = c(6,7)))
plot(r_for_all_pairs)

r_for_all <- reconstruct(s_full, groups = list(1, 2, 3, 4, 5, 6, 7, 8))
plot(r_for_all, add.residuals = FALSE, add.original = FALSE)

r.fort <- reconstruct(s_full, groups = list(Trend = 1:2, Seasonality = 3:4)) 
plot(r.fort, add.residuals = FALSE, add.original = TRUE, plot.method = "xyplot", superpose = TRUE, auto.key = list(columns = 2))




#res1 <- reconstruct(s_full, groups = list(1:3)) 
#trend <- res1$F1 
#plot(res1, add.residuals = FALSE, plot.type = "single", col = c("black", "red"), lwd = c(1, 2))
# Calculate the residuals
res <- residuals(r_for_all_pairs)
plot(res, main = "residuals")

# гетероскедастичность ? остатков
# s.env <- ssa(res^2, L=10000) 
# rsd <- sqrt(reconstruct(s.env, groups=list(1,2))$F1) 
# plot(res, type='l'); lines(rsd, type='l'); lines(-rsd, type='l')




# CANT!!!!!!!!!!????????????
# Forecasting ------


# Recurrent forecast, the forecasted points added to the base series 
for1a <- rforecast(s_full, groups = list(1:13, 1:28), len = 10000, only.new = FALSE)
plot(cbind(ts_full, for1a$F1), plot.type='single', col=c('black','red'))


# Vector forecast
for2 <- vforecast(s_full, groups = list(1:13, 1:28), len = 10000, only.new = FALSE)
plot(cbind(ts_full, for2$F1), plot.type='single', col=c('black','red')) 


for1a <- rforecast(s_full, groups = list(1:28), len = 10000, only.new = FALSE)
for2 <- vforecast(s_full, groups = list(1:28), len = 10000, only.new = FALSE)
plot(cbind(ts_full, for1a,for2), plot.type = "single", col=c("black","red", "blue"))



# Use of predict() for prediction
p <- predict(s_full, groups = list(1:28), len = 10000)
plot(p, ylab = "Forecasteed Values")
