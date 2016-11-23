
#################################################################################################
##### Tutorial: Power analysis for generalized linear mixed models in ecology and evolution #####
#################################################################################################

library(lme4)
library(Matrix)
library(MASS)

## Power analysis for a two-sample t-test

set.seed(123456) # set seed for random number generator to give repeatable results

# Simulate n = 30 normally distributed observations in each group, with means u_young=2 in the young group and u_old=u_young+d=2.2 in 
# the treatment group, and s=0.5
y.young <- rnorm(n = 30, mean = 2, sd = 0.5)
y.old <- rnorm(n = 30, mean = 2 + 0.2, sd = 0.5)

boxplot(list(Young = y.young, Old = y.old), ylab = "Speed (mm/s)", main = "Boxplot of larval swimming speed\nby age group")

# H0: there is no difference between the two means
# H1: there is a difference

t.test(x = y.young, y = y.old, alternative = "two.sided", paired = FALSE, var.equal = TRUE)$p.value
# 0.2500745 > 0.05, we do not reject the null hypothesis

# Create a function to repeat the simulation process
sim.t.test <- function(n, delta, sigma) {
  y.young <- rnorm(n = n, mean = 0, sd = sigma)
  y.old <- rnorm(n = n, mean = delta, sd = sigma)
  t.test(x = y.young, y = y.old, alternative = "two.sided", paired = FALSE, var.equal = TRUE)$p.value
}

# Run the simulation function 1000 times
nsim = 1000
sim.p <- sapply(1:nsim, function(...) sim.t.test(n = 30, delta = 0.2, sigma = 0.5))

hist(sim.p, nclass = 20)
abline(v = 0.05, col = "red")

# Power is estimated as the number of significant test results divided by the total number of tests
sum(sim.p < 0.05)/nsim
# 0.311
# (could also be done with mean(sim.p < 0.05))

# The power estimate is too low (31%): there is only a 1 in 3 chance of detecting the specified treatment effect.
# We repeat the simulations with higher n until we achieve 80% power

n.vec <- seq(30, 120, by = 5)

sim.power.over.n <- sapply(n.vec, function(n.new) {
  sim.p <- sapply(1:nsim, function(...) sim.t.test(n = n.new, delta = 0.2, sigma = 0.5))
  mean(sim.p < 0.05)
})

names(sim.power.over.n) <- paste("n = ", n.vec)
sim.power.over.n

# Plot a power curve
plot(n.vec, sim.power.over.n, ylab = "Power", xlab = "n", ylim = 0:1, main = "Plot of power against n per group", type = "l")
abline(h = 0.8, lty = 2)


## Power analysis for a two-sample t-test using simulations with sim.glmm
# Get the sim.glmm function from github
library(RCurl)
options(RCurlOptions=list(cainfo=system.file("CurlSSL","cacert.pem",package="RCurl")))
eval(expr=parse(text=getURL("https://raw.githubusercontent.com/pcdjohnson/sim.glmm/master/sim.glmm.R")))

snaildata <- data.frame(group = factor(rep(c("Young", "Old"), each = 100), levels = c("Young", "Old")))
snaildata

# Simulate the data
snaildata <- sim.glmm(design.data = snaildata, fixed.eff = list(intercept = 2, group = c(Young = 0, Old = 0.2)), 
                      distribution = "gaussian", SD = 0.5)
snaildata

boxplot(response ~ group, data = snaildata, ylab = "Speed (mm/s)", main = "Boxplot of larval swimming speed\nby age group")

t.test(response ~ group, data = snaildata, var.equal = TRUE)$p.value
# 0.0002219909 < 0.05 so we reject the null hypothesis

# We run the simulation 1000 times
sim.p <- sapply(1:nsim, function(...) {
  snaildata <- sim.glmm(design.data = snaildata, fixed.eff = list(intercept = 2, group = c(Young = 0, Old = 0.2)), 
                        distribution = "gaussian", SD = 0.5)
  t.test(response ~ group, data = snaildata, var.equal = TRUE)$p.value
})
mean(sim.p < 0.05)

###################################################################################################################

### Predicting the power of a trial of insecticidal mosquito nets using a binomial GLMM

latsq <- rbind(c("C", "E1", "E2", "E3", "E4", "E5"),
               c("E5", "C", "E1", "E2", "E3", "E4"),
               c("E4", "E5", "C", "E1", "E2", "E3"),
               c("E3", "E4", "E5", "C", "E1", "E2"),
               c("E2", "E3", "E4", "E5", "C", "E1"),
               c("E1", "E2", "E3", "E4", "E5", "C"))
colnames(latsq) <- paste("hut", 1:nrow(latsq), sep = "")
rownames(latsq) <- paste("week", 1:ncol(latsq), sep = "")
latsq

# Make a template data set with 6 huts x 6 weeks x 6 nights per week = 216 rows. Each row will record the mosquito mortality data for
# one hut on one night
mosdata <- expand.grid(hut = factor(1:ncol(latsq)), week = factor(1:nrow(latsq)), night = factor(1:6))
mosdata <- mosdata[order(mosdata$hut, mosdata$week, mosdata$night),]
# Add an observation (row ID) number which will be used to simulate overdispersion
mosdata$observation <- factor(formatC(1:nrow(mosdata), flag = "0", width = 3))
# Assign treatments to each row
mosdata$net <- factor(diag(latsq[mosdata$week, mosdata$hut]))
# Specify the number of mosquitoes entering each hut each night
mosdata$n <- 25
head(mosdata)

set.seed(321123)
mosdata <- sim.glmm(design.data = mosdata, 
                    fixed.eff = list(intercept = qlogis(0.7), net = log(c(C = 1, E1 = 1.7, E2 = 1.7, E3 = 1.7, E4 = 1.7, E5 = 1.7))), 
                    rand.V = c(hut = 0.5, week = 0.5, observation = 1), 
                    distribution = "binomial")







