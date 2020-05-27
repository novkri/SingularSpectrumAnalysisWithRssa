load("D:/R/savw/data_new_167_260.RData") #data frame

load("D:/R/savw/ts_200.RData") # ts

library(dplyr)
library(?Rssa)




# Графики начальных данных
plot(ts_200, main = "Средняя температура, градусы Цельсия", sub = "C 2018-09-28 до 2019-12-26, шаг дискретизации - раз в 2 дня", xlab = "Время", ylab = "Температура")

# SSA ---------
# Decompose
s_200 <- ssa(ts_200)
summary(s_200)
s_200_s <- ssa(ts_200, L = 60, svd.method = "propack")
summary(s_200_s)
# plot(s_full)
plot(s_200_s, type = "vectors", idx=1:10)
plot(s_200_s, type = "paired", idx=1:10)
plot(s_200, type = "series", groups = as.list(1:10)) 




g <- grouping.auto(s_200, freq.bins = list(0.1, 0.2, 0.3, 0.4, 0.5, 0.05), threshold = 0.95)
plot(reconstruct(s_200, groups = g))
plot(g)








# W-correlation
plot(wcor(s_200)) 
plot(wcor(s_200, groups = 1:14))


# Reconstruction -------
# ?reconstruct
r_for_all_pairs <- reconstruct(s_200, groups = list(OneTwo = c(1,2), TwoThree = c(2,3), ThreeFour = c(3,4), FourFive = c(4,5), FiveSix = c(5,6), SixSeven = c(6,7)))
plot(r_for_all_pairs)

r_for_all <- reconstruct(s_200, groups = list(1, 2, 3, 4, 5, 6, 7, 8))
plot(r_for_all, add.residuals = FALSE, add.original = FALSE)

r.fort <- reconstruct(s_200, groups = list(Trend = 1:3, Seasonality = 7:8)) 
plot(r.fort, add.residuals = FALSE, add.original = TRUE, plot.method = "xyplot", superpose = TRUE)




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
for1a <- rforecast(s_200, groups = list(1:3, 1:6, 1:7, 1:11), len = 100,
                   only.new = F)
plot(cbind(ts_200, for1a$F2), plot.type='single', col=c('black','red'))


# Vector forecast
for2 <- vforecast(s_200, groups = list(1:3, 1:6, 1:9), len = 100, only.new = FALSE)
plot(cbind(ts_200, for2$F2), plot.type='single', col=c('black','red')) 



for1a <- rforecast(s_200, groups = list(1:6), len = 100, only.new = FALSE)
for2 <- vforecast(s_200, groups = list(1:6), len = 100, only.new = FALSE)
plot(cbind(ts_200, for1a,for2), plot.type = "single", col=c("black","red", "blue"))





for3 <- forecast(s_200, groups = list(1:6), bootstrap = T, len = 100)
plot(for3, include = 100, type = "l")

# Use of predict() for prediction
p <- predict(s_200, groups = list(1:6), method = "recurrent", len = 100)
plot(p, ylab = "Forecasteed Values")
