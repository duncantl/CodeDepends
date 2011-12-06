###################################################
### chunk number 1: loadPackages
###################################################
library(NMMAPSlite)
initDB("NMMAPS")

library(splines)
library(xtable)
library(tsModel)
library(lattice)
library(mda)
library(gam)


###################################################
### chunk number 2: showSignals
###################################################
chic <- readCity("chic", collapseAge = TRUE)
fit <- glm(death ~ l1pm10tmean + ns(date, 4 * 14) + ns(tmpd, 6) + dow, 
           data = chic, na.action = na.exclude, family = poisson)
pr <- predict(fit, type = "terms")


###################################################
### chunk number 3: plotShowSignals
###################################################
dates <- seq(as.Date("1987-01-01"), as.Date("2000-12-31"), "day")
plot(dates, pr[, 2], type = "l", lty = 2, ylim = range(pr, na.rm = TRUE), ylab = "")
lines(dates, pr[, 3], col = gray(0.6))
lines(dates, pr[, 1])


###################################################
### chunk number 4: loadChicagoData
###################################################
data <- readCity("chic")


###################################################
### chunk number 5: SingleLagModels
###################################################
f0 <- glm(death ~ pm10tmean + tmpd + agecat, data = data, family = poisson)
f1 <- glm(death ~ Lag(pm10tmean, 1, agecat) + tmpd + agecat, data = data, family = poisson)
f2 <- glm(death ~ Lag(pm10tmean, 2, agecat) + tmpd + agecat, data = data, family = poisson)
f3 <- glm(death ~ Lag(pm10tmean, 3, agecat) + tmpd + agecat, data = data, family = poisson)
f4 <- glm(death ~ Lag(pm10tmean, 4, agecat) + tmpd + agecat, data = data, family = poisson)
ss <- list(summary(f0), summary(f1), summary(f2), summary(f3), summary(f4))
models <- lapply(ss, function(x) x$coefficients[2, c("Estimate", "Std. Error")])


###################################################
### chunk number 6: SingleLagResults
###################################################
results <- data.frame(do.call("rbind", models))
row.names(results) <- paste("Lag", 0:4, "PM10")
names(results) <- c("Estimate", "Std. Error")
xtab <- xtable(results, 
               caption = "Results from single lag models, Chicago, IL, 1987--2000", 
               label = "SingleLagResults", digits = rep(6, 3))
print(xtab)


###################################################
### chunk number 7: DLagModels
###################################################
data <- readCity("chic")
fit <- glm(death ~ Lag(pm10tmean, 0:4, agecat) + tmpd + agecat, data = data, 
           family = poisson)
summ <- summary(fit)


###################################################
### chunk number 8: DLResults
###################################################
print(xtable(summ, 
             caption = "Results from distributed lag model, Chicago, IL, 1987--2000",
             label = "DLagResults", digits = c(6, 6, 6, 2, 2)))


###################################################
### chunk number 9: ComputeTotalEffect
###################################################
rn <- rownames(summ$coefficients)
i <- grep("pm10tmean", rn, fixed = TRUE)
coefs <- summ$coefficients[i, "Estimate"]
total <- sum(coefs)


###################################################
### chunk number 10: loadNYCdata
###################################################
data.c <- readCity("ny", collapseAge = TRUE)


###################################################
### chunk number 11: temperatureLags
###################################################
maxlag <- 0:13
models <- sapply(maxlag, function(mlag) {
    fit <- glm(death ~ l1pm10tmean + Lag(tmpd, seq(0, mlag)),
               data = data.c, family = poisson)
    summ <- summary(fit)
    c(coef(fit)["l1pm10tmean"], summ$coefficients["l1pm10tmean", 2])
})


###################################################
### chunk number 12: fakePlotTempLags eval=FALSE
###################################################
## rng <- range(models[1,] - 1.96 * models[2,], models[1,] + 1.96 * models[2,], 0)
## par(mar = c(4, 5, 1, 1))
## plot(maxlag, models[1,], type = "b", pch = 20, ylim = rng, 
##      xlab = "Maximum temperature lag", 
##      ylab = expression(hat(beta) * " for " * PM[10] * " at lag 1"))
## lines(maxlag, models[1,] + 1.96 * models[2,], lty = 2)
## lines(maxlag, models[1,] - 1.96 * models[2,], lty = 2)
## abline(h = 0, lty = 3)


###################################################
### chunk number 13: plotTempLags
###################################################
rng <- range(models[1,] - 1.96 * models[2,], models[1,] + 1.96 * models[2,], 0)
par(mar = c(4, 5, 1, 1))
plot(maxlag, models[1,], type = "b", pch = 20, ylim = rng, 
     xlab = "Maximum temperature lag", 
     ylab = expression(hat(beta) * " for " * PM[10] * " at lag 1"))
lines(maxlag, models[1,] + 1.96 * models[2,], lty = 2)
lines(maxlag, models[1,] - 1.96 * models[2,], lty = 2)
abline(h = 0, lty = 3)


###################################################
### chunk number 14: temperatureBySeasonWarm
###################################################
data <- readCity("ny", collapseAge = TRUE)

maxlag <- 0:13
data.warm <- subset(data, quarters(date) %in% c("Q2", "Q3"))
    
models.warm <- sapply(maxlag, function(mlag) {
    fit <- glm(death ~ l1pm10tmean + Lag(tmpd, seq(0, mlag)),
               data = data.warm, family = poisson)
    summ <- summary(fit)
    c(coef(fit)["l1pm10tmean"], summ$coefficients["l1pm10tmean", 2])
})    


###################################################
### chunk number 15: temperatureBySeasonCold
###################################################
data.cold <- subset(data, quarters(date) %in% c("Q1", "Q4"))
    
models.cold <- sapply(maxlag, function(mlag) {
    fit <- glm(death ~ l1pm10tmean + Lag(tmpd, seq(0, mlag)),
               data = data.cold, family = poisson)
    summ <- summary(fit)
    c(coef(fit)["l1pm10tmean"], summ$coefficients["l1pm10tmean", 2])
})


###################################################
### chunk number 16: fakePlotTempBySeason eval=FALSE
###################################################
## y <- c(models.warm[1,], models.cold[1,])
## xpts <- rep(maxlag, 2)
## f <- gl(2, length(maxlag), labels = c("Warm season", "Cold season"))
## std <- c(models.warm[2,], models.cold[2,])
## rng <- range(y - 1.96 * std, y + 1.96 * std, 0)
## rng <- rng + c(-1 , 1) * 0.05 * diff(rng)
## 
## p <- xyplot(y ~ xpts | f, as.table = TRUE, ylim = rng, subscripts = TRUE,
##             panel = function(x, y, subscripts, ...) {
##                 panel.xyplot(x, y, ...)
##                 llines(x, y - 1.96 * std[subscripts], lty = 2)
##                 llines(x, y + 1.96 * std[subscripts], lty = 2)
##                 panel.abline(h = 0, lty = 3)
##             }, xlab = "Maximum temperature lag", layout = c(1, 2), type = "b",
##             ylab = expression(hat(beta) * " for " * PM[10] * " at lag 1"),
##             pch = 20)
## print(p)


###################################################
### chunk number 17: plotTempBySeason
###################################################
y <- c(models.warm[1,], models.cold[1,])
xpts <- rep(maxlag, 2)
f <- gl(2, length(maxlag), labels = c("Warm season", "Cold season"))
std <- c(models.warm[2,], models.cold[2,])
rng <- range(y - 1.96 * std, y + 1.96 * std, 0)
rng <- rng + c(-1 , 1) * 0.05 * diff(rng)

p <- xyplot(y ~ xpts | f, as.table = TRUE, ylim = rng, subscripts = TRUE,
            panel = function(x, y, subscripts, ...) {
                panel.xyplot(x, y, ...)
                llines(x, y - 1.96 * std[subscripts], lty = 2)
                llines(x, y + 1.96 * std[subscripts], lty = 2)
                panel.abline(h = 0, lty = 3)
            }, xlab = "Maximum temperature lag", layout = c(1, 2), type = "b",
            ylab = expression(hat(beta) * " for " * PM[10] * " at lag 1"),
            pch = 20)
print(p)


###################################################
### chunk number 18: temperatureLagsWithSeason
###################################################
data.c <- readCity("ny", collapseAge = TRUE)
maxlag <- 0:13
models <- sapply(maxlag, function(mlag) {
    fit <- glm(death ~ l1pm10tmean + ns(date, 4 * 14)  
               + Lag(tmpd, seq(0, mlag)),
               data = data.c, family = poisson)
    summ <- summary(fit)
    c(coef(fit)["l1pm10tmean"], summ$coefficients["l1pm10tmean", 2])
})    


###################################################
### chunk number 19: plotTempLagsWithSeason
###################################################
rng <- range(models[1,] - 1.96 * models[2,], models[1,] + 1.96 * models[2,], 0)
par(mar = c(4, 5, 1, 1))
plot(maxlag, models[1,], type = "b", pch = 20, ylim = rng, 
     xlab = "Maximum temperature lag", 
     ylab = expression(hat(beta) * " for " * PM[10] * " at lag 1"))
lines(maxlag, models[1,] + 1.96 * models[2,], lty = 2)
lines(maxlag, models[1,] - 1.96 * models[2,], lty = 2)
abline(h = 0, lty = 3)


###################################################
### chunk number 20: loadDenvData
###################################################
data.raw <- readCity("denv", collapseAge = TRUE)


###################################################
### chunk number 21: fitGAMmortality
###################################################
xpts <- seq(as.Date("1987-01-01"), as.Date("2000-12-31"), "day")
fit2 <- gam(death ~ s(date, 2 * 14), family = poisson, data = data.raw)
p2 <- predict(fit2, data.frame(date = xpts), type = "response")

fit12 <- gam(death ~ s(date, 12 * 14), family = poisson, data = data.raw)
p12 <- predict(fit12, data.frame(date = xpts), type = "response")


###################################################
### chunk number 22: plotGAMmortality
###################################################
par(mar = c(2, 4, 2, 2))
with(data.raw, plot(date, death, pch = ".", ylab = "Mortality count"))
lines(xpts, p12, col = gray(.6), lwd = 2)
lines(xpts, p2, lwd = 2)


###################################################
### chunk number 23: fitGAMPM10
###################################################
xpts <- seq(as.Date("1987-01-01"), as.Date("2000-12-31"), "day")
fit2pm10 <- gam(pm10tmean ~ s(date, 2 * 14), data = data.raw)
p2pm10 <- predict(fit2pm10, data.frame(date = xpts))

fit12pm10 <- gam(pm10tmean ~ s(date, 12 * 14), data = data.raw)
p12pm10 <- predict(fit12pm10, data.frame(date = xpts))


###################################################
### chunk number 24: fitGAMO3
###################################################
xpts <- seq(as.Date("1987-01-01"), as.Date("2000-12-31"), "day")
fit2o3 <- gam(o3tmean ~ s(date, 2 * 14), data = data.raw)
p2o3 <- predict(fit2o3, data.frame(date = xpts))

fit12o3 <- gam(o3tmean ~ s(date, 12 * 14), data = data.raw)
p12o3 <- predict(fit12o3, data.frame(date = xpts))


###################################################
### chunk number 25: plotGAMPoll
###################################################
par(mar = c(2, 4, 2, 2), mfrow = c(2, 1))
with(data.raw, {
    plot(date, pm10tmean, pch = ".", 
         ylab = expression(PM[10] * " level (detrended)"))
})
lines(xpts, p12pm10, col = gray(.7), lwd = 1.5)
lines(xpts, p2pm10, lwd = 2)
with(data.raw, {
    plot(date, o3tmean, pch = ".", 
         ylab = expression(O[3] * " level (detrended)"))
})
lines(xpts, p12o3, col = gray(.7), lwd = 1.5)
lines(xpts, p2o3, lwd = 2)


###################################################
### chunk number 26: fitModelsDF
###################################################
dfValues <- c(2, 4, 6, 8, 10, 12, 14)
modelsGAM <- sapply(dfValues, function(dfVal) {
    total.df <- dfVal * 14
    fit <- gam(death ~ l1pm10tmean + tmpd + s(date, total.df), 
               data = data.raw, family = poisson, 
               control = gam.control(epsilon = 1e-8, bf.epsilon = 1e-8))
    gamex <- gam.exact(fit)
    c(coef(fit)["l1pm10tmean"], gamex$coefficients["l1pm10tmean", "A-exact SE"])
})


###################################################
### chunk number 27: fitModelsDFglm
###################################################
dfValues <- c(2, 4, 6, 8, 10, 12, 14)
modelsGLM <- sapply(dfValues, function(dfVal) {
    total.df <- dfVal * 14
    fit <- glm(death ~ l1pm10tmean + tmpd + ns(date, total.df), 
               data = data.raw, family = poisson)
    summ <- summary(fit)
    c(coef(fit)["l1pm10tmean"], summ$coefficients["l1pm10tmean", 2])
})


###################################################
### chunk number 28: plotModelsDF
###################################################
par(mar = c(4, 5, 2, 2), mfrow = c(2, 1))
rng <- range(modelsGAM[1,]-1.96*modelsGAM[2,], modelsGAM[1,]+1.96*modelsGAM[2,], 
             modelsGLM[1,]-1.96*modelsGLM[2,], modelsGLM[1,]+1.96*modelsGLM[2,],
             0)
plot(dfValues, modelsGAM[1, ], type = "b", pch = 20, xlab = "df per year", 
     ylab = expression(hat(beta)), ylim = rng, main = "(a)")
lines(dfValues, modelsGAM[1,] + 1.96 * modelsGAM[2,], lty = 2)
lines(dfValues, modelsGAM[1,] - 1.96 * modelsGAM[2,], lty = 2)
abline(h = 0, lty = 3)
plot(dfValues, modelsGLM[1, ], type = "b", pch = 20, xlab = "df per year", 
     ylab = expression(hat(beta)), ylim = rng, main = "(b)")
lines(dfValues, modelsGLM[1,] + 1.96 * modelsGLM[2,], lty = 2)
lines(dfValues, modelsGLM[1,] - 1.96 * modelsGLM[2,], lty = 2)
abline(h = 0, lty = 3)


###################################################
### chunk number 29: getDetroitData
###################################################
data <- readCity("det", collapseAge = TRUE)


###################################################
### chunk number 30: getGCVdf
###################################################
library(mda)
pm10 <- data$l1pm10tmean
x <- unclass(data$date)
use <- complete.cases(pm10, x)
br.fit <- bruto(x[use], pm10[use])
optimal.df <- br.fit$df


###################################################
### chunk number 31: fitModel
###################################################
library(gam)
fit <- gam(death ~ l1pm10tmean + s(date, optimal.df), 
           data = data, family = quasipoisson)
v <- gam.exact(fit)


###################################################
### chunk number 32: logRR
###################################################
print(v$coefficients["l1pm10tmean", "Estimate"])


###################################################
### chunk number 33: AExactStdError
###################################################
print(v$coefficients["l1pm10tmean", "A-exact SE"])


###################################################
### chunk number 34: overdispersion
###################################################
summary(fit)$dispersion


###################################################
### chunk number 35: predictPM10
###################################################
pm10 <- data$l1pm10tmean
x <- unclass(data$date)
use <- complete.cases(pm10, x)
br.fit <- bruto(x[use], pm10[use])
df.pm10 <- br.fit$df


###################################################
### chunk number 36: predictMortality
###################################################
death <- data$death
use <- complete.cases(death , x)
br.fit <- bruto(x[use], death[use])
df.death <- br.fit$df


###################################################
### chunk number 37: FitGAMOptimal
###################################################
fit1 <- gam(death ~ l1pm10tmean + s(date, df.pm10), 
            data = data, family = quasipoisson)
fit2 <- gam(death ~ l1pm10tmean + s(date, df.death), 
            data = data, family = quasipoisson)
v1 <- gam.exact(fit1)
v2 <- gam.exact(fit2)


###################################################
### chunk number 38: assembleComparison
###################################################
r <- rbind(v1$coefficients["l1pm10tmean", c("Estimate", "A-exact SE")],
           v2$coefficients["l1pm10tmean", c("Estimate", "A-exact SE")])
r <- r * 1000
r <- cbind(r, c(df.pm10, df.death) / 14)
rownames(r) <- c("Predict \\PMTen", "Predict Mortality")
colnames(r) <- c("Estimate", "A-exact SE", "df/year")


###################################################
### chunk number 39: makeTableComparison
###################################################
print(xtable(r, label = "tableComparison", caption = "Comparison of methods for choosing the degrees of freedom in the smooth function of time"),
      sanitize.rownames.function = function(x) x)


###################################################
### chunk number 40: getLargest20Cities
###################################################
meta <- getMetaData("citycensus")
ord <- order(meta[, "pop100"], decreasing = TRUE)
sites <- as.character(meta[ord, "city"][1:20])  ## Take top 20


###################################################
### chunk number 41: MultiSiteAnalysis eval=FALSE
###################################################
 r <- lapply(sites, function(site) {
         sitedata <- readCity(site)
         fit <- glm(death ~ l1pm10tmean + agecat + tmpd 
                    + ns(date, 8 * 14), data = sitedata, 
                    family = poisson)
         summ <- summary(fit)
         summ$coefficients["l1pm10tmean", c("Estimate", "Std. Error")]
 })
 results <- do.call("rbind", r)


###################################################
### chunk number 42: LoadMultiSiteResults
###################################################
##load("results-MultiSiteAnalysis.rda")  ## 'results'


###################################################
### chunk number 43: histBetas
###################################################
par(cex.axis = 0.7)
hist(results[,1], main = expression("Histogram of " * hat(beta)),
     xlab = expression(hat(beta)))
rug(results[,1])


