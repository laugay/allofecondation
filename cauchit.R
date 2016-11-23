box_intrapop <- subset(box, Plan == "Intrapop", select = c(propViabilityNA, Block, Zone, UniqueGeno, UniqueQuadrat, Plate, IDPod, Plant, IDBox, NbSeedminusNA))
m0 <- glmer(propViabilityNA ~ Zone + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (1|UniqueGeno) + (1|Plate) + (1|Plate), data = box_intrapop, family = "binomial"(link = 'cauchit'), weights = NbSeedminusNA, na.action = na.omit)

m1 <- glmer(propViabilityNA ~ (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (1|UniqueGeno) + (1|Plate) + (1|Plate), data = box_intrapop, family = "binomial"(link = 'cauchit'), weights = NbSeedminusNA, na.action = na.omit)

anova(m0, m1)
model.sel(m0, m1)

m0 <- glmer(propViabilityNA ~ Block + Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (0 + Plan|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
summary(m0)
fixef(m0)

m01 <- glmer(propViabilityNA ~ Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (0 + Plan|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m0, m01)

m02 <- glmer(propViabilityNA ~ Block + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (0 + Plan|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m0, m02)

m03 <- glmer(propViabilityNA ~ Block + Plan + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (0 + Plan|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m0, m03)

m04 <- glmer(propViabilityNA ~ Block + Plan + Zone0 + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (0 + Plan|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m0, m04)

m05 <- glmer(propViabilityNA ~ Block + Plan + Zone0 + (1|IDBox) + (1|Plant) + (1|UniqueQuadrat) + (0 + Plan|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m0, m05)

m06 <- glmer(propViabilityNA ~ Block + Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|UniqueQuadrat) + (0 + Plan|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m0, m06)

m07 <- glmer(propViabilityNA ~ Block + Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (0 + Plan|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m0, m07)

m08 <- glmer(propViabilityNA ~ Block + Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (1|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m0, m08)

m09 <- glmer(propViabilityNA ~ Block + Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (0 + Plan|UniqueGeno) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m0, m09)

m010 <- glmer(propViabilityNA ~ Block + Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (0 + Plan|UniqueGeno) + (1|Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m0, m010)

AICc(m0, m01, m02, m03, m04, m05, m06, m07, m08, m09, m010)
model.sel(m0, m01, m02, m03, m04, m05, m06, m07, m08, m09, m010)

# No interaction Plan - Genotype

m1 <- glmer(propViabilityNA ~ Block + Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (1|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)

m11 <- glmer(propViabilityNA ~ Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (1|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m1, m11)

m12 <- glmer(propViabilityNA ~ Block + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (1|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m1, m12)

m13 <- glmer(propViabilityNA ~ Block + Plan + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (1|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m1, m13)

m14 <- glmer(propViabilityNA ~ Block + Plan + Zone0 + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (1|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m1, m14)

m15 <- glmer(propViabilityNA ~ Block + Plan + Zone0 + (1|IDBox) + (1|Plant) + (1|UniqueQuadrat) + (1|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m1, m15)

m16 <- glmer(propViabilityNA ~ Block + Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|UniqueQuadrat) + (1|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m1, m16)

m17 <- glmer(propViabilityNA ~ Block + Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m1, m17)

m18 <- glmer(propViabilityNA ~ Block + Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m1, m18)

m19 <- glmer(propViabilityNA ~ Block + Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (1|UniqueGeno) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m1, m19)

m110 <- glmer(propViabilityNA ~ Block + Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (1|UniqueGeno) + (1|Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m1, m110)

AICc(m1, m11, m12, m13, m14, m15, m16, m17, m18, m19, m110)
model.sel(m1, m11, m12, m13, m14, m15, m16, m17, m18, m19, m110)

# No Block effect

m2 <- m11

m21 <- glmer(propViabilityNA ~ Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (1|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m2, m21)

m22 <- glmer(propViabilityNA ~ Plan + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (1|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m2, m22)

m23 <- glmer(propViabilityNA ~ Plan + Zone0 + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (1|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m2, m23)

m24 <- glmer(propViabilityNA ~ Plan + Zone0 + (1|IDBox) + (1|Plant) + (1|UniqueQuadrat) + (1|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m2, m24)

m25 <- glmer(propViabilityNA ~ Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|UniqueQuadrat) + (1|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m2, m25)

m26 <- glmer(propViabilityNA ~ Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m2, m26)

m27 <- glmer(propViabilityNA ~ Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m2, m27)

m28 <- glmer(propViabilityNA ~ Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (1|UniqueGeno) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m2, m28)

m29 <- glmer(propViabilityNA ~ Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueQuadrat) + (1|UniqueGeno) + (1|Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m2, m29)

AICc(m2, m21, m22, m23, m24, m25, m26, m27, m28, m29)
model.sel(m2, m21, m22, m23, m24, m25, m26, m27, m28, m29)

# No Quadrat effect

m3 <- m26

m31 <- glmer(propViabilityNA ~ Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m3, m31)

m32 <- glmer(propViabilityNA ~ Plan + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m3, m32)

m33 <- glmer(propViabilityNA ~ Plan + Zone0 + (1|IDPod) + (1|Plant) + (1|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m3, m33)

m34 <- glmer(propViabilityNA ~ Plan + Zone0 + (1|IDBox) + (1|Plant) + (1|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m3, m34)

m35 <- glmer(propViabilityNA ~ Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|UniqueGeno) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m3, m35)

m36 <- glmer(propViabilityNA ~ Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m3, m36)

m37 <- glmer(propViabilityNA ~ Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueGeno) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m3, m37)

m38 <- glmer(propViabilityNA ~ Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|UniqueGeno) + (1|Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m3, m38)

AICc(m3, m31, m32, m33, m34, m35, m36, m37, m38)
model.sel(m3, m31, m32, m33, m34, m35, m36, m37, m38)

# No Genotype effect

m4 <- m36

m41 <- glmer(propViabilityNA ~ Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m4, m41)

m42 <- glmer(propViabilityNA ~ Plan + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m4, m42)

m43 <- glmer(propViabilityNA ~ Plan + Zone0 + (1|IDPod) + (1|Plant) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m4, m43)

m44 <- glmer(propViabilityNA ~ Plan + Zone0 + (1|IDBox) + (1|Plant) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m4, m44)

m45 <- glmer(propViabilityNA ~ Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m4, m45)

m46 <- glmer(propViabilityNA ~ Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m4, m46)

m47 <- glmer(propViabilityNA ~ Plan + Zone0 + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m4, m47)

AICc(m4, m41, m42, m43, m44, m45, m46, m47)
model.sel(m4, m41, m42, m43, m44, m45, m46, m47)

# No Zone0 effect

m5 <- m42

m51 <- glmer(propViabilityNA ~ (1|IDBox) + (1|IDPod) + (1|Plant) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m5, m51)

m52 <- glmer(propViabilityNA ~ Plan + (1|IDPod) + (1|Plant) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m5, m52)

m53 <- glmer(propViabilityNA ~ Plan + (1|IDBox) + (1|Plant) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m5, m53)

m54 <- glmer(propViabilityNA ~ Plan + (1|IDBox) + (1|IDPod) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m5, m54)

m55 <- glmer(propViabilityNA ~ Plan + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m5, m55)

m56 <- glmer(propViabilityNA ~ Plan + (1|IDBox) + (1|IDPod) + (1|Plant) + (1|Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m5, m56)

AICc(m5, m51, m52, m53, m54, m55, m56)
model.sel(m5, m51, m52, m53, m54, m55, m56)

# No Plan effect

m6 <- m51

m61 <- glmer(propViabilityNA ~ (1|IDPod) + (1|Plant) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m6, m61)

m62 <- glmer(propViabilityNA ~ (1|IDBox) + (1|Plant) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m6, m62)

m63 <- glmer(propViabilityNA ~ (1|IDBox) + (1|IDPod) + (1|Plate) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m6, m63)

m64 <- glmer(propViabilityNA ~ (1|IDBox) + (1|IDPod) + (1|Plant) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m6, m64)

m65 <- glmer(propViabilityNA ~ (1|IDBox) + (1|IDPod) + (1|Plant) + (1|Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m6, m65)

AICc(m6, m61, m62, m63, m64, m65)
model.sel(m6, m61, m62, m63, m64, m65)

# No Plate effect

m7 <- m64

m71 <- glmer(propViabilityNA ~ (1|IDPod) + (1|Plant) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m7, m71)

m72 <- glmer(propViabilityNA ~ (1|IDBox) + (1|Plant) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m7, m72)

m73 <- glmer(propViabilityNA ~ (1|IDBox) + (1|IDPod) + (1|Block:Plate), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m7, m73)

m74 <- glmer(propViabilityNA ~ (1|IDBox) + (1|IDPod) + (1|Plant), data = box, family = "binomial"(link = 'logit'), weights = NbSeedminusNA, na.action = na.omit)
anova(m7, m74)

summary(m7)


mod1 <- glmer(Vradicule ~ Block + Plan + (0 + Plan|UniqueGeno) + (1|Plate) + (1|UniqueQuadrat) + (1|Plant) + (1|IDPod) + (1|IDBox) + (1|IDSeed), data = data, family="poisson")

mySumm <- function(.) {
  c(beta=fixef(.),sigma=as.data.frame(VarCorr(.))[,4])
}

bootstrap <- bootMer(mod1, mySumm, nsim = 100)
bootstrap
# IDBox and IDSeed variances = 0 so they are not significative

CI_IDBox <- boot.ci(bootstrap, conf = 0.95, type = "norm", index = 5)
CI_IDPod <- boot.ci(bootstrap, conf = 0.95, type = "norm", index = 6)
CI_plant <- boot.ci(bootstrap, conf = 0.95, type = "norm", index = 7)
CI_GenoInter <- boot.ci(bootstrap, conf = 0.95, type = "norm", index = 8)
CI_GenoIntra <- boot.ci(bootstrap, conf = 0.95, type = "norm", index = 9)
CI_Geno <- boot.ci(bootstrap, conf = 0.95, type = "norm", index = 10)
CI_Quadrat <- boot.ci(bootstrap, conf = 0.95, type = "norm", index = 11)
CI_Plate <- boot.ci(bootstrap, conf = 0.95, type = "norm", index = 12)
CI_Seed <- boot.ci(bootstrap, conf = 0.95, type = "norm", index = 13)
intercept <- boot.ci(bootstrap, conf = 0.95, type = "norm", index = 1)
block2 <- boot.ci(bootstrap, conf = 0.95, type = "norm", index = 2)
block3 <- boot.ci(bootstrap, conf = 0.95, type = "norm", index = 3)
CI_planIntra <- boot.ci(bootstrap, conf = 0.95, type = "norm", index = 4)


mod2 <- glmer(Vradicule ~ Plan + (0 + Plan|UniqueGeno) + (1|Plate) + (1|UniqueQuadrat) + (1|Plant) + (1|IDPod) + (1|IDBox), data = data, family="poisson")
anova(mod1, mod2)
mod3 <- glmer(Vradicule ~ Block + (0 + Plan|UniqueGeno) + (1|Plate) + (1|UniqueQuadrat) + (1|Plant) + (1|IDPod) + (1|IDBox), data = data, family="poisson")
anova(mod1, mod3)
mod4 <- glmer(Vradicule ~ (0 + Plan|UniqueGeno) + (1|Plate) + (1|UniqueQuadrat) + (1|Plant) + (1|IDPod) + (1|IDBox), data = data, family="poisson")
anova(mod3, mod4)
mod5 <- glmer(Vradicule ~ Block + (1|UniqueGeno) + (1|Plate) + (1|UniqueQuadrat) + (1|Plant) + (1|IDPod) + (1|IDBox), data = data, family = "poisson")
anova(mod3, mod5)
mod6 <- glmer(Vradicule ~ Block + (0 + Plan|UniqueGeno) + (1|UniqueQuadrat) + (1|Plant) + (1|IDPod) + (1|IDBox), data = data, family = "poisson")
anova(mod3, mod6)
mod7 <- glmer(Vradicule ~ Block + (0 + Plan|UniqueGeno) + (1|Plate) + (1|Plant) + (1|IDPod) + (1|IDBox), data = data, family="poisson")
anova(mod3, mod7)
mod8 <- glmer(Vradicule ~ Block + (0 + Plan|UniqueGeno) + (1|Plate) + (1|UniqueQuadrat) + (1|IDPod) + (1|IDBox), data = data, family="poisson")
anova(mod3, mod8)
mod9 <- glmer(Vradicule ~ Block + (0 + Plan|UniqueGeno) + (1|Plate) + (1|UniqueQuadrat) + (1|Plant) + (1|IDBox), data = data, family="poisson")
anova(mod3, mod9)
mod10 <- glmer(Vradicule ~ Block + (0 + Plan|UniqueGeno) + (1|Plate) + (1|UniqueQuadrat) + (1|Plant) + (1|IDPod), data = data, family="poisson")
anova(mod3, mod10)
model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10)

m1 <- glmer(Vradicule ~ Block + (1|UniqueGeno) + (1|Plate) + (1|UniqueQuadrat) + (1|Plant) + (1|IDPod) + (1|IDBox), data = test, family = "poisson")
m2 <- glmer(Vradicule ~ (1|UniqueGeno) + (1|Plate) + (1|UniqueQuadrat) + (1|Plant) + (1|IDPod) + (1|IDBox), data = test, family = "poisson")
anova(m1, m2)
m3 <- glmer(Vradicule ~ Block + (1|Plate) + (1|UniqueQuadrat) + (1|Plant) + (1|IDPod) + (1|IDBox), data = test, family = "poisson")
anova(m1, m3)
m4 <- glmer(Vradicule ~ Block + (1|UniqueGeno) + (1|UniqueQuadrat) + (1|Plant) + (1|IDPod) + (1|IDBox), data = test, family = "poisson")
anova(m1, m4)
m5 <- glmer(Vradicule ~ Block + (1|UniqueGeno) + (1|Plate) + (1|Plant) + (1|IDPod) + (1|IDBox), data = test, family = "poisson")
anova(m1, m5)
m6 <- glmer(Vradicule ~ Block + (1|UniqueGeno) + (1|Plate) + (1|UniqueQuadrat) + (1|IDPod) + (1|IDBox), data = test, family = "poisson")
anova(m1, m6)
m7 <- glmer(Vradicule ~ Block + (1|UniqueGeno) + (1|Plate) + (1|UniqueQuadrat) + (1|Plant) + (1|IDBox), data = test, family = "poisson")
anova(m1, m7)
m8 <- glmer(Vradicule ~ Block + (1|UniqueGeno) + (1|Plate) + (1|UniqueQuadrat) + (1|Plant) + (1|IDPod), data = test, family = "poisson")
anova(m1, m8)
model.sel(m1, m2, m3, m4, m5, m6, m7, m8)

m1 <- glmer(Vradicule ~ Block + (1|Plate) + (1|UniqueQuadrat) + (1|Plant), data = test, family = "poisson")
m2 <- glmer(Vradicule ~ (1|Plate) + (1|UniqueQuadrat) + (1|Plant), data = test, family = "poisson")
anova(m1, m2)
m3 <- glmer(Vradicule ~ Block + (1|UniqueQuadrat) + (1|Plant), data = test, family = "poisson")
anova(m1, m3)
m4 <- glmer(Vradicule ~ Block + (1|Plate) + (1|Plant), data = test, family = "poisson")
anova(m1, m4)
m5 <- glmer(Vradicule ~ Block + (1|Plate) + (1|UniqueQuadrat), data = test, family = "poisson")
anova(m1, m5)
model.sel(m1, m2, m3, m4, m5)


