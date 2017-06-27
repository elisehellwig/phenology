drivepath <- '/Users/echellwig/Drive/Phenology'
library(rethinking)
library(phenoclim)
library(plyr)


pm <- readRDS(file.path(drivepath,'Results/walnutpaper/TTTplantmodel.RDS'))
vars <- names(pm)
##############################################

mmw <- ldply(1:length(pm), function(i) {
    pdf <- phenology(pm[[i]])
    data.frame(cultivar=vars[i],
               year=pdf$year,
               stagelength=pdf$length1,
               daylinear=pdf$daylinear1,
               lmfit=pdf$fitdaylinear1)
})

#attributes 182, 42



mmw$stagelengthc <- mmw$stagelength - 182
mmw$daylinearc <- mmw$daylinear - 42


bayeswalnut1 <- map2stan(
    alist(
        stagelengthc ~ dnorm(mu, sigma),
        mu <- a_cultivar[cultivar] + b_cultivar[cultivar],
        a_cultivar[cultivar] ~ dnorm(0, 5),
        b_cultivar[cultivar] ~ dnorm(0, 5),
        sigma ~ dunif(0,50)),
    data=mmw, iter=6000, warmup=2000, chains=4)




bayeswalnut2 <- map2stan(
    alist(
        stagelengthc ~ dnorm(mu, sigma),
        mu <- a_cultivar[cultivar] + b_cultivar[cultivar]*daylinearc,
        c(a_cultivar, b_cultivar)[cultivar] ~ dmvnorm2(c(a,b),sigma_cultivar,Rho),
        a ~ dnorm(0,10),
        b ~ dnorm(0,10),
        sigma_cultivar ~ dcauchy(0,2),
        sigma ~ dcauchy(0,2),
        Rho ~ dlkjcorr(2)
    ),
    data=mmw, iter=8000, warmup=3000, chains=4)


mmw$bayesfit <- apply(link(bayeswalnut2), 2, mean)+182


rmse$lm <- sapply(1:length(vars), function(i) {
    d <- mmw[mmw$cultivar==vars[i],]
    rmsd(d$lmfit, d$stagelength)
})

rmse$bayes <- sapply(1:length(vars), function(i) {
    d <- mmw[mmw$cultivar==vars[i],]
    rmsd(d$bayesfit, d$stagelength)
})


