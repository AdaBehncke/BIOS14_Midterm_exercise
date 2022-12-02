# Model fit to estimate the effect of the after-ripening duration on the germination rate for each population of the plant species, and how this effect is . 

dat <- read.csv("dormancy.csv")

library(glmmTMB)

#---------------------------------------------------------------------------

# The model is fitted separately for each population (CC, LM, PM, T).
# First, data subsets are created for each population.

CCdat <- dat[dat$pop=="CC",]
LMdat <- dat[dat$pop=="LM",]
PMdat <- dat[dat$pop=="PM",]
Tdat <- dat[dat$pop=="T",]

save(CCdat, LMdat, PMdat, Tdat, file = "data_subsets.RData")

#---------------------------------------------------------------------------

# Then, the model is fitted for each population.

mCC <- glmmTMB(germ2 ~ timetosowing + MCseed + (1|mother) + (1|blocktray),
               family = binomial(link="logit"), weights = nseed, data = CCdat)

mLM <- glmmTMB(germ2 ~ timetosowing + MCseed + (1|mother) + (1|blocktray),
               family = binomial(link="logit"), weights = nseed, data = LMdat)

mPM <- glmmTMB(germ2 ~ timetosowing + MCseed + (1|mother) + (1|blocktray),
               family = binomial(link="logit"), weights = nseed, data = PMdat)

mT <- glmmTMB(germ2 ~ timetosowing + MCseed + (1|mother) + (1|blocktray),
               family = binomial(link="logit"), weights = nseed, data = Tdat)

#-----------------------------------------------------------------------------------

# We calculate the goodness of the models (r^2) using the MuMIN package
library(MuMIn)
r2 <- data.frame(r.squaredGLMM(mCC), r.squaredGLMM(mLM), r.squaredGLMM(mPM), r.squaredGLMM(mT))

#------------------------------------------------------------------------------------

# The model fits and the predicted germination rate probability as a function of the after-ripening time, for the mean seed size of the population and the mean +/- sd seed size.

coefsCC = summary(mCC)$coef   #store the model coefficients for each population for further operation.
coefsLM = summary(mLM)$coef
coefsPM = summary(mPM)$coef
coefsT = summary(mT)$coef

xvals_CC <- seq(min(CCdat$timetosowing, na.rm = T),    #create x values for the model function
             max (CCdat$timetosowing, na.rm = T))
y_hat_CC <- coefsCC$cond[1,1] + coefsCC$cond[2,1]*xvals_CC
y_hat_CC_big <- coefsCC$cond[1,1] + coefsCC$cond[2,1]*xvals_CC + coefsCC$cond[3,1]*sd(CCdat$MCseed)
y_hat_CC_small <- coefsCC$cond[1,1] + coefsCC$cond[2,1]*xvals_CC - coefsCC$cond[3,1]*sd(CCdat$MCseed)

xvals_LM <- seq(min(LMdat$timetosowing, na.rm = T),    
             max (LMdat$timetosowing, na.rm = T))
y_hat_LM <- coefsLM$cond[1,1] + coefsLM$cond[2,1]*xvals_LM
y_hat_LM_big <- coefsLM$cond[1,1] + coefsLM$cond[2,1]*xvals_LM + coefsLM$cond[3,1]*sd(LMdat$MCseed)
y_hat_LM_small <- coefsLM$cond[1,1] + coefsLM$cond[2,1]*xvals_LM - coefsLM$cond[3,1]*sd(LMdat$MCseed)

xvals_PM <- seq(min(PMdat$timetosowing, na.rm = T),    
             max (PMdat$timetosowing, na.rm = T))
y_hat_PM <- coefsPM$cond[1,1] + coefsPM$cond[2,1]*xvals_PM
y_hat_PM_big <- coefsPM$cond[1,1] + coefsPM$cond[2,1]*xvals_PM + coefsPM$cond[3,1]*sd(PMdat$MCseed)
y_hat_PM_small <- coefsPM$cond[1,1] + coefsPM$cond[2,1]*xvals_PM - coefsPM$cond[3,1]*sd(PMdat$MCseed)

xvals_T <- seq(min(Tdat$timetosowing, na.rm = T),    
             max (Tdat$timetosowing, na.rm = T))
y_hat_T <- coefsT$cond[1,1] + coefsT$cond[2,1]*xvals_T
y_hat_T_big <- coefsT$cond[1,1] + coefsT$cond[2,1]*xvals_T + coefsT$cond[3,1]*sd(Tdat$MCseed)
y_hat_T_small <- coefsT$cond[1,1] + coefsT$cond[2,1]*xvals_T - coefsT$cond[3,1]*sd(Tdat$MCseed)


# The predicted values are saved into a data frame for posterior use.

predvals_CC <- data.frame(xvals_CC, y_hat_CC, y_hat_CC_big, y_hat_CC_small)

predvals_LM <- data.frame(xvals_LM, y_hat_LM, y_hat_LM_big, y_hat_LM_small)

predvals_PM <- data.frame(xvals_PM, y_hat_PM, y_hat_PM_big, y_hat_PM_small)

predvals_T <- data.frame(xvals_T, y_hat_T, y_hat_T_big, y_hat_T_small)

#-------------------------------------------------------------------------------------------------
  
# Calculation of the after-ripening time needed for 50% germination rate (from now on T50). 
# T50 is calculated for mean seed size and for seed size = mean +/- sd).
# T50 calculations are made for each population.

coefsCC = summary(mCC)$coef
T50_CC = -coefsCC$cond[1,1]/coefsCC$cond[2,1]
T50_CC_big =  - (coefsCC$cond[1,1] + coefsCC$cond[3,1]*sd(CCdat$MCseed))/coefsCC$cond[2,1]
T50_CC_small =  - (coefsCC$cond[1,1] - coefsCC$cond[3,1]*sd(CCdat$MCseed))/coefsCC$cond[2,1]

coefsLM = summary(mLM)$coef
T50_LM = -coefsLM$cond[1,1]/coefsLM$cond[2,1]
T50_LM_big =  - (coefsLM$cond[1,1] + coefsLM$cond[3,1]*sd(LMdat$MCseed))/coefsLM$cond[2,1]
T50_LM_small =  - (coefsLM$cond[1,1] - coefsLM$cond[3,1]*sd(LMdat$MCseed))/coefsLM$cond[2,1]

coefsPM = summary(mPM)$coef
T50_PM = -coefsPM$cond[1,1]/coefsPM$cond[2,1]
T50_PM_big =  - (coefsPM$cond[1,1] + coefsPM$cond[3,1]*sd(PMdat$MCseed))/coefsPM$cond[2,1]
T50_PM_small =  - (coefsPM$cond[1,1] - coefsPM$cond[3,1]*sd(PMdat$MCseed))/coefsPM$cond[2,1]

coefsT = summary(mT)$coef
T50_T = -coefsT$cond[1,1]/coefsT$cond[2,1]
T50_T_big =  - (coefsT$cond[1,1] + coefsT$cond[3,1]*sd(Tdat$MCseed))/coefsT$cond[2,1]
T50_T_small =  - (coefsT$cond[1,1] - coefsT$cond[3,1]*sd(Tdat$MCseed))/coefsT$cond[2,1]


# The T50 values are saved into a data frame for posterior use

T50vals <- setNames(data.frame(c("CC", "LM", "PM", "T" ),
                               c(T50_CC_small, T50_LM_small, T50_PM_small, T50_T_small), 
                               c(T50_CC, T50_LM, T50_PM, T50_T),
                               c(T50_CC_big, T50_LM_big, T50_PM_big, T50_T_big)),
                               c("Population","T50 for seed size = mean - sd", "T50 for mean seed size", "T50 for seed size = mean + sd"))

#-----------------------------------------------------------------------------------------

# The results of the analysis are saved as an RData file

save(mCC, mLM, mPM, mT, predvals_CC, predvals_LM, predvals_PM, predvals_T, T50vals, file = "models.RData")

