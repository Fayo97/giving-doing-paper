# The original file used to produce the chapter 1 in my dissertation is: "W1.Models_pt2_March2017" in the folder "Chap1_Giving_Doing" in "CLEAN_DATA" in PNPM folder on dropbox.
#
# This present file is different 
#
ftab <- function(x){
  a <- table(x, useNA = "always")
  return (a)
}

#  get rid of scientific notation
options(scipen = 999)


#READING IN the wave 1 DATA
library(foreign)
W1.data <- read.dta("GW1-NOcentrd-0217.dta")
#  this is the data set used to produce chapter 1 (Beard replic)... even though I cleaned the data for wave I further and added new variables. this file above should not be changed in order to ensure reproducibility. OK
#
dim(W1.data)
# 22294   71
#

summary(W1.data)
#RECODING RELIGION DIFFERENTLY
#-------------------------------
W1.data$RELIG2 <- "Muslim"
W1.data$RELIG2[W1.data$ReligionsRaw==2 | W1.data$ReligionsRaw==3] <- "Christian"
W1.data$RELIG2[W1.data$ReligionsRaw==4 | W1.data$ReligionsRaw==5]<- "Buddhist/Hindu"
# W1.data$RELIG2[W1.data$ReligionsRaw==5] <- "Hindu"
W1.data$RELIG2[W1.data$ReligionsRaw > 5]<- "Other"
#
ftab(W1.data$RELIG2)
#
W1.data$RELIG2 <- as.factor(W1.data$RELIG2)
W1.data$RELIG2 = factor(W1.data$RELIG2, levels=c("Muslim", "Christian", "Other", "Buddhist/Hindu"))


# centering with 'scale()' function inside a wrapper function
center_scale <- function(x) {
  scale(x, scale = FALSE)
}
#
W1.data$Assets_ctrd  <- center_scale(W1.data$TotAssets)
W1.data$KnowLeaders_ctrd  <- center_scale(W1.data$KnowLeaders)
W1.data$PctIslam_ctrd  <- center_scale(W1.data$PctIslam)


## create a new binary var ==> MajorMuslim (1/0)
W1.data$MajorMuslim <- 0
W1.data$MajorMuslim[W1.data$PctIslam > 50] <- 1
ftab(W1.data$MajorMuslim)


#  recoding "Major ethnic" as Ethnic minority ===> for easier interpretation in the models
#
# new var
W1.data$EthnicMinor <- NA
W1.data$EthnicMinor[W1.data$MajorEth==0] <- 1
W1.data$EthnicMinor[W1.data$MajorEth!=0] <- 0
ftab(W1.data$EthnicMinor)
ftab(W1.data$MajorEth)
#  ok great!

# ================== TOBIT simplified ==================
library(censReg)
cens.Doing <- censReg(TotTime ~
                        # @@@@@ Resources @@@@@
                      + PrimEduc
                      + SeniorHigh
                      + Univ
                      + factor(percapita)
                      + TotAssets
                      + Hdemployed
                      # @@@@@ NEED @@@@@
                      + HDNoEduc
                      + DirectCash
                      #
                      # @@@@@@@@@@ RELIGION @@@@@@@@@@
                      + nonMuslim
                      + nonMuslim:TotAssets
                      # @@@@@ Community Characteristics @@@@@
                      + MajorMuslim
                      + log(VilPop)
                      + homog
                      # @@@@@ Control variables @@@@@@@
                      + femhead
                      + HDMigrated
                      + MajorEth,
                  left=0, right=Inf, data = W1.data)
#
library(texreg)
screenreg(cens.Doing, digits=3, single.row = TRUE, custom.model.names = c("Doing"))
# save it:
saveRDS(cens.Doing, "doing-model.rds")

#
cens.Giving <- censReg( # @@@@@ Household SES @@@@@
  TotMoney ~ # @@@@@ Resources @@@@@
    + PrimEduc
  + SeniorHigh
  + Univ
  + factor(percapita)
  + TotAssets
  + Hdemployed
  # @@@@@ NEED @@@@@
  + HDNoEduc
  + DirectCash
  #
  # @@@@@@@@@@ RELIGION @@@@@@@@@@
  + nonMuslim
  + nonMuslim:TotAssets
  # @@@@@ Community Characteristics @@@@@
  + MajorMuslim
  + log(VilPop)
  + homog
  # @@@@@ Control variables @@@@@@@
  + femhead
  + HDMigrated
  + MajorEth,
  left=0, right=Inf, data = W1.data)
#
screenreg(cens.Giving, digits=3, single.row = TRUE, custom.model.names = c("Giving"))

# save it:
saveRDS(cens.Giving, "giving-model.rds")

# This is a treasure!---v
par(mfrow=c(1,1), family="serif") #  using Times New Roman

# require(coefplot2)
# coefplot2(cens.Doing, intercept=F, pch.pts=20, lwd.1=1, lwd.2=0.6)
# coefplot2(cens.Giving,intercept=F, pch.pts=17)
# #

# Extracting coefs and Std Errors
#
# 1) for DOING
#=========================
coefs.T <- coef(summary(cens.Doing))[, 1]
names(coefs.T)
se.T <- coef(summary(cens.Doing))[, 2]
names(se.T)
#  drop logsigma
se.T <- se.T[-c(1,19)]
names(se.T)
coefs.T <- coefs.T[-c(1,19)]
names(coefs.T)
#  ok

#  NOW FOR MOney model
coefs.M <- coef(summary(cens.Giving))[, 1]
names(coefs.M)
se.M <- coef(summary(cens.Giving))[, 2]
names(se.M)
#  drop logsigma
se.M <- se.M[-c(1,19)]
names(se.M)
coefs.M <- coefs.M[-c(1,19)]
names(coefs.M)

#=============== the plots ==================
#
longnames=c("Primary school", "Senior high", "University degree",
                      "Low percapita expenditures", "High percapita expenditures",
                      "Household Assets", "Employed head",
                      "No Education",   "Direct cash",
                      "Non-Muslim","Household Assets * Non-Muslim",
                      "Majority Muslim", "Ethnic homogeneity", "Population size (logged)",
                      "Female head","Recently migrated","Belongs to ethnic majority")

#
coefplot2::coefplot2(cens.Doing,
                     # coefs.T, se.T,
                     intercept=FALSE,
                     xlim=c(-1, 0.8),
                     v.axis=TRUE,top.axis=T,
                     #  h.axis=T,
                     cex.var=0.7, pch.pts=23, lwd.1=1, lwd.2=0.6,
                     # varnames=longnames,
                     # varnames=c("Primary school", "Senior high", "University degree",
                     #            "Low percapita expenditures", "High percapita expenditures",
                     #            "Household Assets", "Employed head",
                     #            "No Education",   "Direct cash",
                     #            "Non-Muslim","Household Assets * Non-Muslim",
                     #            "Majority Muslim", "Ethnic homogeneity", "Population size (logged)",
                     #            "Female head","Recently migrated","Belongs to ethnic majority"),
                     cex.axis=0.2,
                     mar=c(1.5, 11, 5.7,1), cex.main=0.7,
                     main="

                     Tobit regression (DOING)\n



                     ")
# the spacing and empty line are important to keep the graph elegant!
coefplot2::coefplot2(cens.Giving,
                     # coefs.M, se.M,
                     intercept=FALSE,
                     xlim=c(-2.5, 2.8),
                     v.axis=TRUE,top.axis=T,
                     #  h.axis=T,
                     cex.var=0.7, pch.pts=22, lwd.1=1, lwd.2=0.6,
                     # varnames=longnames,
                     # varnames=c("Primary school", "Senior high", "University degree",
                     #            "Low percapita expenditures", "High percapita expenditures",
                     #            "Household Assets", "Employed head",
                     #            "No Education",   "Direct cash",
                     #            "Non-Muslim","Household Assets * Non-Muslim",
                     #            "Majority Muslim", "Ethnic homogeneity", "Population size (logged)",
                     #            "Female head","Recently migrated","Belongs to ethnic majority"),
                     cex.axis=0.2,
                     mar=c(1.5, 11, 5.7,1), cex.main=0.7,
                     main="

                     Tobit regression (GIVING)\n



                     ")


# BETTER PLOTS FOR WORLD DEVPT ARTICLE:
#
  ## TAKE A SUBSET OF THE COEFS/VARS

names(coefs.T)
#
coefs.T_sub <- coefs.T[c("HDNoEduc","factor(percapita)1", "DirectCash", "TotAssets", "Hdemployed",
                         "Univ","factor(percapita)3")]
names(coefs.T_sub)
#
se.T_sub <- se.T[c("HDNoEduc","factor(percapita)1", "DirectCash", "TotAssets", "Hdemployed",
                   "Univ","factor(percapita)3")]
names(se.T_sub)

#

names(coefs.M)
#
coefs.M_sub <- coefs.M[c("HDNoEduc","factor(percapita)1", "DirectCash", "TotAssets", "Hdemployed",
                           "Univ","factor(percapita)3")]
names(coefs.M_sub)
#
se.M_sub <- se.M[c("HDNoEduc","factor(percapita)1", "DirectCash", "TotAssets", "Hdemployed",
                     "Univ","factor(percapita)3")]
names(se.M_sub)
#

par(mfrow=c(1,1), family="serif", col.main = "black") #  using Times New Roman

#
#
# DOING
# png(filename = "Chap1_coefplots.png", width=1600, height=700) # this was for the plot in baclk and white. where the models differed by the shape for coeffs.
#
png(filename = "Ch1_coefplots-col.png", width=1600, height=700)
# colored coeff plots.
par(mfrow=c(1,2),  family="serif", col.main = "black")
#
gr.T <- coefplot2::coefplot2(coefs.T_sub, se.T_sub, xlim=c(-2, 2.2),
                             # xlim=c(-0.7, 0.8),
                     v.axis=TRUE,top.axis=T,
                     # h.axis=T,
                     cex.var=2.4, lwd.1=1.8, lwd.2=1.1,
                     pch.pts=20, cex.pts=5.1, col="#98777b",
                     varnames=c("No education",
                                "Low per capita expenditures",
                                "Direct cash assistance",
                                #
                                "Household assets",
                                "Employed, head",
                                "University degree",
                                "High percapita expenditures"),
                     cex.axis=18,
                     mar=c(2.5, 27, 7.8,1),
                     cex.main=2.5,
                     # legend=T, legend.arg=legend("bottomright", "Giving: Red\n Doing: Blue", col=c("tomato", "blue")),
                     main="Doing (Y = Time)\n")
# (controlling for social capital, ethnic homogeneity, gender, \nmigration, ethnicity, vilage population size, and household size)

#
#
# GIVING
gr.M <- coefplot2::coefplot2(coefs.M_sub, se.M_sub, xlim=c(-2, 2.2),
                     v.axis=TRUE,top.axis=T,
                     # h.axis=T,
                     cex.var=2.4, lwd.1=1.8, lwd.2=1.1,
                      # pch.pts=11,
                     pch.pts=20,
                     cex.pts=3.5, col="blue",
                     varnames=c("No education",
                                "Low per capita expenditures",
                                "Direct cash assistance",
                                #
                                "Household assets",
                                "Employed, head",
                                "University degree",
                                "High percapita expenditures"),
                     cex.axis=16,
                     mar=c(2.5, 28, 7.8,1),
                     cex.main=2.5,
                     # legend=T, legend.arg=legend("bottomright", "Giving: Red\n Doing: Blue", col=c("tomato", "blue")),
                     main="Giving (Y = Money)\n")
# (controlling for social capital, ethnic homogeneity, gender, \nmigration, ethnicity, vilage population size, and household size)
#
dev.off()

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#
#
#
#
#
names(W1.data)
# ===== NOW: some simple descriptives for a table ==========
summary(W1.data[c("TotMoney", "TotTime", "PrimEduc", "SeniorHigh", "Univ" , "percapita", "TotAssets", "Hdemployed",
                  "HDNoEduc", "DirectCash", "nonMuslim", "MajorMuslim","VilPop",
                  "homog", "femhead", "HDMigrated", "MajorEth")])

sd(W1.data$homog)



table(W1.data$HDMigrated, W1.data$Univ)
ftab(W1.data$Univ)

table(W1.data$SeniorHigh, W1.data$HDMigrated)
table(W1.data$JuniorHigh, W1.data$HDMigrated)
table(W1.data$HDNoEduc, W1.data$HDMigrated)

# ========= HURDLE TECHNIQUES FOR THE Participation MODELS =============================
# ========= DECOMPOSING THE DATA INTO TWO PARTS: ================
#$$$$$$$$  A ZERO-CLUMPED PORTION AND A CONTINUOUS PORTION    $$$$$$$$$$$$$$$$$$$$$$$$
library(rcompanion) # to find the Pseudo R-square
library(broom)
library(pscl)
library(MASS) # to do a stepwise search using `stepAIC()`

# MODELING `TIME` AND `MONEY`  LIKE A HURLDE PROCESS FOR A "SEMI-CONTINUOUS" DEPENDENT VARIABLE
# Data transformations (Y)
#  time
W1.data$nonZeroTIME <- ifelse(W1.data$TotTime > 0, 1, 0)
ftab(W1.data$nonZeroTIME)
head(W1.data$nonZeroTIME, 100)
#
# money
W1.data$nonZeroMONEY <- ifelse(W1.data$TotMoney > 0, 1, 0)
ftab(W1.data$nonZeroMONEY)
head(W1.data$nonZeroMONEY, 100)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 1) Models for TIME @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#  the binary portion for TIME
plot(table(W1.data$TotTime), col="black", bty="l")
plot(table(W1.data$TotMoney), col="black", bty="l")
#
#
# chisq.test(W1.data$percapita, W1.data$nonMuslim)
#
DO.Bin <- glm (nonZeroTIME ~
                 # @@@@@ Household SES @@@@@
                 HDNoEduc
               + PrimEduc
               + SeniorHigh
               + Univ
               + factor(percapita)
               + TotAssets
               + DirectCash
               # @@@@@@@@@@ social networks @@@@@@@@@@
               + femhead
               + MajorEth
               # @@@@@ Community Characteristics @@@@@
               + MajorMuslim
               + log(VilPop)
               + homog
               # @@@@@ Control variables @@@@@@@
               + AGE
               + nonMuslim
               + HHsize
               # @@@@@ Interaction terms @@@@@
               + nonMuslim:TotAssets,
               data=W1.data, family = binomial(link = logit))
#
summary.glm(DO.Bin, cor=F)

#
#  the gamma portion now for TIME:
DO.Gam <- glm (TotTime ~
                 # @@@@@ Household SES @@@@@
                 HDNoEduc
               + PrimEduc
               + SeniorHigh
               + Univ
               + factor(percapita)
               + TotAssets
               + DirectCash
               # @@@@@@@@@@ social networks @@@@@@@@@@
               + femhead
               + MajorEth
               # @@@@@ Community Characteristics @@@@@
               + MajorMuslim
               + log(VilPop)
               + homog
               # @@@@@ Control variables @@@@@@@
               + AGE
               + nonMuslim
               + HHsize
               # @@@@@ Interaction terms @@@@@
               + nonMuslim:TotAssets,
               data = subset(W1.data, nonZeroTIME == 1),
               family = Gamma(link = log))
#
#
summary.glm(DO.Gam, cor=F)
# stepAIC(DO.Gam)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 2) Models for MONEY @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Give.Bin <- glm (nonZeroMONEY ~
                   # @@@@@ Household SES @@@@@
                   HDNoEduc
                 + PrimEduc
                 + SeniorHigh
                 + Univ
                 + factor(percapita)
                 + TotAssets
                 + Hdemployed
                 + DirectCash
                 # @@@@@@@@@@ social networks @@@@@@@@@@
                 + femhead
                 + EthnicMinor
                 # @@@@@ Community Characteristics @@@@@
                 + MajorMuslim
                 + log(VilPop)
                 + homog
                 # @@@@@ Control variables @@@@@@@
                 + AGE
                 + nonMuslim
                 + HHsize
                 # @@@@@ Interaction terms @@@@@
                 + nonMuslim:TotAssets,
                 data=W1.data, family = binomial(link = logit))
#
summary.glm(Give.Bin, cor=F)
#
Give.Gam <- glm (TotMoney ~
                   # @@@@@ Household SES @@@@@
                   HDNoEduc
                 + PrimEduc
                 + SeniorHigh
                 + Univ
                 + factor(percapita)
                 + TotAssets
                 + DirectCash
                 # @@@@@@@@@@ social networks @@@@@@@@@@
                 + femhead
                 + EthnicMinor
                 # @@@@@ Community Characteristics @@@@@
                 + MajorMuslim
                 + log(VilPop)
                 + homog
                 # @@@@@ Control variables @@@@@@@
                 + AGE
                 + nonMuslim
                 + HHsize
                 # @@@@@ Interaction terms @@@@@
                 + nonMuslim:TotAssets,
                 data = subset(W1.data, nonZeroMONEY == 1),
                 family = Gamma(link = log))
#
summary.glm(Give.Gam, cor=F)

library(texreg)
screenreg(l=list(DO.Bin,DO.Gam), digits = 3, single.row = TRUE,
          custom.model.names = c("Doing (binary)", "Doing (Gamma)"))
#
screenreg(l=list(Give.Bin, Give.Gam), digits = 3, single.row = TRUE,
          custom.model.names = c("Giving (binary)", "Giving (Gamma)"))

plot(DO.Gam)
plot(Give.Gam)























