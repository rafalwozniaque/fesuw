
# Usage examples

# The datasets crime.csv, Oscar.csv, randdata.csv comes from:
# Hill, R.C., Griffiths, W.E., Lim, G.C., (2008). Principles of Econometrics, 3rd Ed., John Wiley & Sons

# -------------------------------------------------------------------------------------------------
# function calculating three R2 statistics of the panel model with FE or RE

source("static_wide_panels_R2.R")

crime = read.csv(file="crime.csv", header=TRUE, sep=",")
crime = pdata.frame(crime, index=c("county", "year"))

# fixed-effects model
model = plm(lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+ltaxpc+lwcon+lwfir,
            data=crime, model="within")
summary(model)

static_wide_panels_R2(model)

# -------------------------------------------------------------------------------------------------
# function calculating marginal effects of the logit and the probit models

source("marginaleffects.R")

oscar = read.csv(file="Oscar.csv", header=TRUE, sep=";")
oscar = na.omit(oscar)

model <- glm(winner~nominations+gglobes, data = oscar, family = binomial(link = "logit"))
summary(model)

x = c(1,7.43,1.31) #convention: (intercept, x1, x2, ...)
marginaleffects(model, x)


# -------------------------------------------------------------------------------------------------
# function performing linktest of the logit and the probit models

source("linktest.R")

oscar = read.csv(file="Oscar.csv", header=TRUE, sep=";")
oscar = na.omit(oscar)

model <- glm(winner~nominations+gglobes, data = oscar, family = binomial(link = "logit"))
summary(model)
linktest_result = linktest(model)
summary(linktest_result)


# -------------------------------------------------------------------------------------------------
# function calculating marginal effects of the ordered choice models
# only ordered logit and ordered probit after glm function

source("ome.R")

library("sandwich")
library("zoo")
library("lmtest")
library("MASS")

randdata = read.csv(file="randdata.csv", header=TRUE, sep=",")
randdata$health = 0
randdata$health[randdata$hlthp==1]=1
randdata$health[randdata$hlthf==1]=2
randdata$health[randdata$hlthg==1]=3
indeksy = which(randdata$health==0)
randdata = randdata[-indeksy,]

model = polr(as.factor(health)~income+female+num, data=randdata, method="logistic")
model = polr(as.factor(health)~income+female+num, data=randdata, method="probit")
summary(model)

x = c(mean(randdata$income), mean(randdata$female), mean(randdata$num))
ome(model, x)

x = c(7000, 0, 4)
ome(model, x)

# -------------------------------------------------------------------------------------------------
# function calculating marginal effects of the tobit model
# only after censReg function

library("MASS")
library("sandwich")
library("zoo")
library("car")
library("lmtest")
library("Formula")
library("plm")
library("stargazer")
library("maxLik")

library("bdsmatrix")
library("glmmML")
library("miscTools")


library("censReg")
library( "truncreg")
library("AER")
library("DescTools")
data( "Affairs", package = "AER")

setwd("C:/Users/U138619/Documents/97_Prywatne/06_Zajêcia/Advanced_econometrics/Pakiet_wneuw")
# write.csv(x = Affairs,file = "Affairs.csv")
Affairs = read.csv(file="Affairs.csv", header=TRUE, sep=",")

# Example 1
model <- censReg(affairs~age+yearsmarried+religiousness+occupation+rating,
                 left=0, right=Inf, data=Affairs)

x = rbind(1, mean(Affairs$age), mean(Affairs$yearsmarried), mean(Affairs$religiousness), 
          mean(Affairs$occupation), mean(Affairs$rating))
dummies_indices = c()
me = tobit_marginal_effects(model, x, dummies_indices=c())

# Example 2
model <- censReg(affairs~age+yearsmarried+religiousness+children,
                 left=0, right=Inf, data=Affairs)
summary(model)

x = rbind(1, mean(Affairs$age), mean(Affairs$yearsmarried), mean(Affairs$religiousness), 0)
dummies_indices = c(5)

me = tobit_marginal_effects(model, x, dummies_indices=c(5))
