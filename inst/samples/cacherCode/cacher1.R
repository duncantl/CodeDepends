## Analysis of the largest 20 cities in the NMMAPS database

tmp = 1:10

## Read in the data
cities <- readLines("citylist.txt")
classes <- readLines("colClasses.txt")

## Only keep variables we need
vars <- c("date", "dow", "death", "tmpd", "rmtmpd", "dptp", "rmdptp",
          "l1pm10tmean")

data <- lapply(cities, function(city) {
        filename <- file.path("data", paste(city, "csv", sep = "."))
        d0 <- read.csv(filename, colClasses = classes, nrow = 5200)

        d0[, vars]
})

names(data) <- letters[seq(along = cities)]

## Analysis of PM10
estimates <- sapply(data, function(city) {
        fit <- glm(death ~ dow + ns(date, 7*14) + ns(tmpd, 6) + ns(rmtmpd, 6)
                   + ns(dptp, 3) + ns(rmdptp, 3) + l1pm10tmean,
                   data = city, family = quasipoisson)
        summ <- summary(fit)
        summ$coefficients["l1pm10tmean", 1:2]
})

effect <- weighted.mean(estimates[1, ], 1 / estimates[2, ]^2)
stderr <- sqrt(1 / sum(1 / estimates[2, ]^2))
