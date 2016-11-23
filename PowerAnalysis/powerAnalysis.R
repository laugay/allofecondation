#!/usr/local/bioinfo/R/3.2.2/bin/Rscript

#qsub -q normal.q -b Y -V -cwd -N CI_simulations "./powerAnalysis.R"


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

sim.total <- function(NbSimul, infile, varGeno, NbBoot) {
  CI_geno <- NULL
  for (i in 1:NbSimul) {
    mod.fit <- glmer(response/n ~ (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueGeno),
                     data = sim.viabilityData(infile, varGeno),
                     family = "binomial"(link = "logit"),
                     weights = n, na.action = na.omit)
    require(boot)
    bootstrap <- bootMer(mod.fit, mySumm, nsim = NbBoot)
    bCI.2 <- boot.ci(bootstrap, conf = 0.95, index=5, type="norm")
    CI.geno.tmp <- as.data.frame(bCI.2[4])
    CI.geno.tmp[,4] <- varGeno
    colnames(CI.geno.tmp) <- c("normal.perc", "normal.inf", "normal.sup", "varGeno")
    
    CI_geno <- rbind(CI_geno, CI.geno.tmp)
  }
  return(CI_geno)
}

NbSimul = 100
NbBoot = 100

CI_simul <- NULL
for (geno in sigma_geno) {
  set.seed(123)
  print(geno)
  CI_simul_tmp <- sim.total(NbSimul = NbSimul, infile = design, varGeno = geno, NbBoot = NbBoot)
  CI_simul <- rbind(CI_simul, CI_simul_tmp)
}

save(CI_simul, file = "CI_simul.RData")


