################################################################################
## Set outcome

outcome <- commandArgs(trailingOnly = TRUE)[1]

if(length(outcome) == 0 || is.na(outcome)) {
        stop("need to specify 'outcome'")
}

######################################################################
## Fit county-specific models with simulatenous inclusion of all
## components [multiple pollutant]

## Get data
specdata <- .readRDS("SpecData-combined.rds")

library(splines)
library(tsModel)

source("spec-tools.R")

################################################################################
## Exposure terms

pollutants <- c("SULFATE", "NITRATE", "silicon", "Elemental_Carbon",
                "OC_K14", "Sodium_Ion")
spec.terms <- list(lag0 = pollutants,
                   lag1 = paste("Lag(", pollutants, ", 1)"),
                   lag2 = paste("Lag(", pollutants, ", 2)"))

################################################################################
## Fit models

## Get county identifiers
fipsList <- readLines("speciation-fips.txt")
ranges <- dget("data-ranges.R")

stopifnot(all(fipsList %in% rownames(ranges)))

results <- lapply(spec.terms, function(sterms) {
        sterms <- sapply(parse(text = sterms), deparse, width.cutoff = 100)
        cat(sterms, "\n")

        cycleCounties(fipsList, specdata, sterms, outcome,
                      type = "multiple", ranges = ranges)
})

pollutants <- spec.terms[[1]]

results <- lapply(results, function(lag) {
        colnames(lag$beta) <- pollutants
        lag
})

## Get IQRs for each pollutant

iqr <- dget("component-IQR.R")



################################################################################
## Combine results

source("pooling.R")

seeds <- if(outcome == "CVD") {
        c(3211, 4646, 61321)
} else {
        c(870, 4646, 61321)
}

pooled <- lapply(seq_along(results), function(i) {
        message("lag ", i - 1)
        set.seed(seeds[i])
        p <- with(results[[i]], mvpool(beta, var, maxiter = 5000, burn = 500))
        rownames(p$gamma) <- pollutants
        print(p$gamma * iqr[pollutants] * 100)
        cat("\n")
        p
})
names(pooled) <- paste("lag", 0:(length(spec.terms) - 1), sep = "")

################################################################################
## Print results to a text file

sink(paste("results/multi-", outcome, ".txt", sep = ""))

for(i in seq_along(pooled)) {
        cat("Lag ", i - 1, "\n", sep = "")
        x <- pooled[[i]]$gamma * iqr[pollutants] * 100
        y <- cbind(x, lo = x[,1] - 1.96 * x[,2],
                   hi = x[,1] + 1.96 * x[,2])
        print(y)
        cat("\n")
}
sink()


################################################################################

rm(specdata)
save.image(file = paste("results/multi-", outcome, ".RData", sep = ""))
