## Estimate variogram

mdata <- .readRDS("monitor-data/monitor-subset.rds")
mdata$LATITUDE <- NULL
mdata$LONGITUDE <- NULL
spmdata <- split(mdata, mdata$SITEPOC)
index.names <- names(spmdata)

all.dates <- seq(as.Date("2000-01-01"), as.Date("2006-12-31"), "day")
dateDF <- data.frame(DATE = all.dates)

pollutants <- c("SULFATE", "NITRATE", "silicon", "Elemental_Carbon",
                "OC_K14", "Sodium_Ion", "AMMONIUM")

ad <- lapply(spmdata, function(x) {
        m <- merge(dateDF, x, by = "DATE", all.x = TRUE)
        data.matrix(m[, pollutants])
})

## Dims:  day, pollutant, site
adata <- array(dim = c(dim(ad[[1]]), length(ad)))

for(i in seq_along(ad)) {
        adata[, , i] <- ad[[i]]
}

dimnames(adata) <- list(as.character(all.dates), pollutants, index.names)

sitedata <- .readRDS("monitor-data/sitedata.rds")
row.names(sitedata) <- sitedata$SITEPOC

sitepairs <- as.matrix(expand.grid(sitedata$SITEPOC, sitedata$SITEPOC))
colnames(sitepairs) <- c("site1", "site2")
repeats <- sitepairs[,1] == sitepairs[,2]
sitepairs <- sitepairs[!repeats, ]
key <- apply(sitepairs, 1, function(x) paste(sort(x), collapse = ""))

sitepairs <- sitepairs[!duplicated(key), ]
sitepairs <- as.data.frame(sitepairs, stringsAsFactors = FALSE)

dist <- apply(sitepairs, 1, function(x) {
        s1 <- sitedata[x["site1"], ]
        s2 <- sitedata[x["site2"], ]

        ## Convert distance to miles (approximately)
        yd <- (s1$LATITUDE - s2$LATITUDE) * 69.2
        xd <- (s1$LONGITUDE - s2$LONGITUDE) * 69.2 * 0.78
        sqrt(xd^2 + yd^2)
})
sitepairs$distance <- dist

getsites <- function(maxdist, mindist = 0) {
        with(sitepairs, distance <= maxdist & distance >= mindist)
}

plotsites <- function(maxdist, mindist = 0, add = FALSE, ...) {
        p <- getsites(maxdist, mindist)
        p <- unique(unlist(p))

        if(!add)
                with(sitedata[p, ], plot(LONGITUDE, LATITUDE, ...))
        else
                with(sitedata[p, ], points(LONGITUDE, LATITUDE, ...))
}


## Compute variances

vars <- t( mapply(function(site1, site2) {
        d <- adata[, , site1] - adata[, , site2]
        diag(var(d, use = "pairwise"))
}, sitepairs[, 1], sitepairs[, 2]))

## Covariances

covs <- t( mapply(function(site1, site2) {
        cc <- cov(adata[, , site1], adata[, , site2], use = "pairwise")
        diag(cc)
}, sitepairs[, 1], sitepairs[, 2]))
colnames(covs) <- paste(colnames(covs), "c", sep = ".")

## Correlations (for plotting)
cors <- t( mapply(function(site1, site2) {
        cc <- cor(adata[, , site1], adata[, , site2], use = "pairwise")
        diag(cc)
}, sitepairs[, 1], sitepairs[, 2]))
colnames(cors) <- paste(colnames(cors), "r", sep = ".")

means <- t( mapply(function(site1, site2) {
        s <- (adata[, , site1] + adata[, , site2]) / 2
        colMeans(s, na.rm = TRUE)
}, sitepairs[, 1], sitepairs[, 2]))
colnames(means) <- paste(colnames(means), "m", sep = ".")

nobs <- mapply(function(site1, site2) {
        d <- adata[, , site1] - adata[, , site2]
        sum(complete.cases(d))
}, sitepairs[, 1], sitepairs[, 2], USE.NAMES = FALSE)

rownames(vars) <- NULL
rownames(means) <- NULL
rownames(covs) <- NULL
rownames(cors) <- NULL
sitepairs <- data.frame(sitepairs, vars, means, covs, cors)
sitepairs$nobs <- nobs

use <- complete.cases(sitepairs)
sitepairs <- sitepairs[use, ]

keep <- sitepairs$nobs > 0
sitepairs <- sitepairs[keep, ]

.saveRDS(sitepairs, file = "sitepairs.rds")

