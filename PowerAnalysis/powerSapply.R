#!/usr/local/bioinfo/R/3.2.2/bin/Rscript

#qsub -q normal.q -b Y -V -cwd -N CI_simulations "./powerSapply.R"


setwd("~/jobs/outcrossing/PowerAnalysis")

library(lme4)
library(Matrix)
library(MASS)
library(MuMIn)
library(lattice)
library(boot)
# Get the sim.glmm function from github
library(RCurl)
options(RCurlOptions=list(cainfo=system.file("CurlSSL","cacert.pem",package="RCurl")))
eval(expr=parse(text=getURL("https://raw.githubusercontent.com/pcdjohnson/sim.glmm/master/sim.glmm.R")))

load("modelViability.RData")
design <- read.table("ViabilityPowerAnalysis.csv", header = TRUE, sep = ";", dec = ".")
design <- na.omit(design)

sigma_geno <- seq(from = 0, to = 1.1, by = 0.05)


# Function to simulate viability data
sim.viabilityData <- function(infile, varGeno) {
  sim.data <- sim.glmm(design.data = infile,
                       fixed.eff = list(intercept = as.numeric(fixef(bestlogit))),
                       rand.V = c(IDBox = as.data.frame(VarCorr(bestlogit))[1,4], 
                                  IDPod = as.data.frame(VarCorr(bestlogit))[2,4], 
                                  Plant = as.data.frame(VarCorr(bestlogit))[3,4], 
                                  UniqueGeno = varGeno),
                       distribution = "binomial")
  return(sim.data)
}

## Function to compute the standard error of the parameters
mySumm <- function(.) {
  c(beta=fixef(.), sigma=as.data.frame(VarCorr(.))[,4])
}

# Function to get the confidence intervals of the genotypic variance using bootstrap
sim.total <- function(NbSimul, infile, varGeno, NbBoot) {
  
  # Model estimation
  mod.fit <- glmer(response/n ~ (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueGeno),
                   data = sim.viabilityData(infile, varGeno),
                   family = "binomial"(link = "logit"),
                   weights = n, na.action = na.omit)
  
  # Get the estimate of the genotypic variance
  est.varBox <- as.numeric(as.data.frame(VarCorr(mod.fit))[1,4])
  est.varPod <- as.numeric(as.data.frame(VarCorr(mod.fit))[2,4])
  est.varGeno <- as.numeric(as.data.frame(VarCorr(mod.fit))[4,4])
  est.varPlant <- as.numeric(as.data.frame(VarCorr(mod.fit))[3,4])
  
  # Bootstrap to get confidence intervals of genotypic variance
  require(boot)
  bootstrap <- bootMer(mod.fit, FUN = mySumm, nsim = NbBoot)
  bCI.intercept <- boot.ci(bootstrap, conf = 0.95, index = 1, type = "norm")
  bCI.box <- boot.ci(bootstrap, conf = 0.95, index = 2, type = "norm")
  bCI.pod <- boot.ci(bootstrap, conf = 0.95, index = 3, type = "norm")
  bCI.plant <- boot.ci(bootstrap, conf = 0.95, index = 4, type = "norm")
  bCI.geno <- boot.ci(bootstrap, conf = 0.95, index=5, type="norm")
  CI.intercept.tmp <- as.data.frame(bCI.intercept[4])
  CI.box.tmp <- as.data.frame(bCI.box[4])
  CI.pod.tmp <- as.data.frame(bCI.pod[4])
  CI.plant.tmp <- as.data.frame(bCI.plant[4])
  CI.geno.tmp <- as.data.frame(bCI.geno[4])
  vector <- c(CI.intercept.tmp[1,1], CI.intercept.tmp[1,2], CI.intercept.tmp[1,3], CI.geno.tmp[1,1], CI.geno.tmp[1,2], CI.geno.tmp[1,3], varGeno, est.varGeno)
  return(vector)
}

NbSimul <- 100
NbBoot <- 100

CI <- sapply(1:NbSimul, function(...) sim.total(NbSimul = NbSimul, infile = design, varGeno = 0.6, NbBoot = NbBoot))
CI <- t(CI)
save(CI, file = "CI_06.RData")
