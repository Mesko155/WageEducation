library(tidyverse)
library(flextable)
library(ggthemes)
library(lubridate)
library(psych)
library(stargazer)
library(ivreg)
library(modelsummary)
library(car)
#library(broom)
#library(AER)


#https://cran.r-project.org/web/packages/ivreg/vignettes/ivreg.html

options(scipen = 999)
options(scipen = 0)

# ENV & DATA -------------------------------------------------------------------
rm(list=ls())
main <- read.csv("data_weekly_earnings.csv")


# FUNCTIONS & DATASET ADDITIONS ------------------------------------------------
agefunc <- 
  function(dob, age.day = today(), units = "years", floor = TRUE) {
    calc.age =
      lubridate::interval(dob, age.day) /
      lubridate::duration(num = 1, units = units)
    if (floor)
      return(as.integer(floor(calc.age)))
    return(calc.age)
  }

main$age <- agefunc(main$born)
main$wage_mean <- main$wage - mean(main$wage)
main$logwage <- log(main$wage)
main$exp <- main$age - main$educ - 6 #jer do seste godine nisi u skoli
main$firstq <- ifelse(quarter(main$born) == 1, 1, 0)
main$expsq <- main$exp^2


#distance VERY CLOSE/CLOSE/FAR/VERY FAR

# REGRESSIONS ------------------------------------------------------------------
cormatrix <- 
  cor(
    main[,
         c(
           "wage",
           "logwage",
           "educ",
           "educ_parent",
           "iq",
           "dist_to_unitown",
           "exp",
           "expsq",
           "age"
            )
         ],
    method = "pearson",
    use = "complete.obs"
    ) %>%
  round(4)

# The exogenous variables he refers to are what you call instruments/exclusion restrictions.
# Correlation among instruments is not a problem per se. However, the more correlated they are,
# the less powerful they become as the extra information provided by the second instrument decreases. 


basemodel <- 
  main %>%
    lm(log(wage) ~ educ, data = .)

# educ_parent as IV

main %>%
  lm(educ ~ educ_parent, data = .) %>%
  summary()

base_IVep <- 
  main %>%
    ivreg(log(wage) ~ educ | educ_parent, data = .)

# IQ as control
base_iq <- 
  main %>%
    ivreg(log(wage) ~ educ + iq, data = .)

base_iq_IVep <-   
  main %>%
    ivreg(log(wage) ~ iq | educ | educ_parent, data = .)


stargazer(basemodel, base_iq, base_IVep, base_iq_IVep, type = "text") #model names = false




#-------------------------------------------------------------------------------

#distance dodan
base_iq_IVep_IVdi <-   
  main %>%
    ivreg(log(wage) ~ iq | educ | educ_parent + dist_to_unitown, data = .)


stargazer(base_iq_IVep, base_iq_IVep_IVdi, type = "text")




#-------------------------------------------------------------------------------

base_iq_exp2_IVep_IVdi <-   
  main %>%
  ivreg(log(wage) ~ iq + I(exp^2) | educ | educ_parent + dist_to_unitown, data = .)

summary(base_iq_exp_IVep_IVdi)

finalModel <-   
  main %>%
  ivreg(log(wage) ~ iq | educ + I(exp^2) | age + educ_parent + dist_to_unitown, data = .)

summary(finalModel)


stargazer(base_iq_IVep_IVdi, finalModel, type = "text")



#-------------------------------------------------------------------------------


#probati sa mincer review quadratic educ # NISTA OVO

base2_iq_exp2_IVep_IVdi_IVage <-   
  main %>%
  ivreg(log(wage) ~ iq | I(educ^2) + I(exp^2) | age + educ_parent + dist_to_unitown, data = .)

summary(base2_iq_exp2_IVep_IVdi_IVage)

stargazer(base_iq_exp2_IVep_IVdi_IVage, base2_iq_exp2_IVep_IVdi_IVage, type = "text")


#-------------------------------------------------------------------------------

#added nonquadratic NISTA OVO JER VJEROVATNO AUTOMATSKI doda vertex
base2_iq_exp_exp2_IVep_IVdi_IVage <-   
  main %>%
  ivreg(log(wage) ~ iq | educ + I(exp^2) | age + educ_parent + dist_to_unitown, data = .)

summary(base2_iq_exp_exp2_IVep_IVdi_IVage)
# ako dodas age squared nista se ne promejni

test <-   
  main %>%
  ivreg(log(wage) ~ iq | educ + exp + I(exp^2) | age + educ_parent + dist_to_unitown, data = .)

summary(test) #IMA SMISLA ZA VERTEX EXP = 35 a negativan znaci OPENS DOWN

test2 <-   
  main %>%
  ivreg(log(wage) ~ exp + I(exp^2) | age + I(age^2), data = .)

summary(test2)

a <-   
  main %>%
  ivreg(log(wage) ~ iq | educ + I(exp^2) | age + educ_parent + dist_to_unitown, data = .)

#-------------------------------------------------------------------------------

#provjera za poly

nopoly <-   
  main %>%
  ivreg(log(wage) ~ iq | educ + I(exp^2) | age + educ_parent + dist_to_unitown, data = .)

poly <-   
  main %>%
  ivreg(log(wage) ~ iq | educ + poly(exp, 2, raw = TRUE) | poly(age, 2, raw = TRUE) + educ_parent + dist_to_unitown, data = .)
# https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faqhow-do-i-interpret-the-sign-of-the-quadratic-term-in-a-polynomial-regression/
# interpret quadratics


polyage <-   
  main %>%
  ivreg(log(wage) ~ iq | educ + poly(exp, 4, raw = TRUE) | poly(age, 4, raw = TRUE) + educ_parent + dist_to_unitown, data = .)
# https://uoepsy.github.io/msmr/2021/msmrlabs/03_nonlin.html#:~:text=%E2%80%9CRaw%E2%80%9D%20(or%20%E2%80%9CNatural,2%2C%203%20and%20so%20on.
# interpret higher order poly


polyeduc <-   
  main %>%
  ivreg(log(wage) ~ iq | poly(educ, 2, raw=TRUE) + poly(exp, 2, raw = TRUE) | poly(age, 2, raw = TRUE) + poly(educ_parent, 2, raw=TRUE) + dist_to_unitown, data = .)


nestzaiq <-   
  main %>%
  ivreg(log(wage) ~  educ + poly(exp, 2, raw = TRUE) | iq + poly(age, 2, raw = TRUE) + educ_parent + dist_to_unitown, data = .)

summary(nestzaiq)

orthogonal <-   
  main %>%
  ivreg(log(wage) ~  iq | educ + poly(exp, 2) | poly(age, 2) + educ_parent + dist_to_unitown, data = .)

summary(orthogonal)

# sign kad nije kvadrirano
expage <-   
  main %>%
  ivreg(log(wage) ~ iq | educ + exp | age + educ_parent + dist_to_unitown, data = .)

summary(expage)

stargazer(expage, poly, type = "text")

summary(nopoly)
summary(poly) #nije isto bgmi, SA RAW JESTE ISTO
summary(polyage)
summary(polyeduc)

# POLY JE SAD MOZDA NAJBOLJI MODEL AL NIJE ZA EXP SIGNIFICANT



# da probam bez edu parents
bezpar <-   
  main %>%
  ivreg(log(wage) ~ iq | educ + poly(exp, 2, raw = TRUE) | poly(age, 2, raw = TRUE) + dist_to_unitown, data = .)

bezexp2ep <-   
  main %>%
  ivreg(log(wage) ~ iq | educ + exp | educ_parent + age + dist_to_unitown, data = .)

bezexp2 <-   
  main %>%
  ivreg(log(wage) ~ iq | educ + exp | age + dist_to_unitown, data = .)

summary(bezexp2ep)
summary(bezexp2)
stargazer(bezexp2ep, bezexp2, type = "text")



summary(bezpar)
stargazer(poly, bezpar, type = "text")


# poly(age,2, raw = TRUE)

stargazer(nopoly, poly, type = "text")

#-------------------------------------------------------------------------------

b <-   
  main %>%
  ivreg(log(wage) ~ exp + I(exp^2), data = .)

summary(b)

c <-   
  main %>%
  ivreg(log(wage) ~ exp + poly(exp, 2), data = .)

summary(c)

main$exp2 <- main$exp^2

p <- predict(finalModel, main$educ)

main %>%
  ggplot(aes(educ, logwage)) +
  geom_point() +


#-------------------------------------------------------------------------------

#first quarter introduced
base_iq_exp2_IVep_IVdi_IVage_IVfq <-   
  main %>%
  ivreg(log(wage) ~ iq | educ + I(exp^2) | firstq + age + educ_parent + dist_to_unitown, data = .)

summary(base_iq_exp2_IVep_IVdi_IVage_IVfq)


stargazer(
  base_iq_exp2_IVep_IVdi_IVage_IVfq,
  base_iq_exp2_IVep_IVdi_IVage,
  type = "text")

#vjerovatno ga necu koristiti, objasniti i u teoriji

#-------------------------------------------------------------------------------

# porediti sa i bez educ parent ko IV

base_iq_exp2_IVep_IVdi_IVage


base_iq_exp2_IVdi_IVage <-   
  main %>%
  ivreg(log(wage) ~ iq | educ + I(exp^2) | age + dist_to_unitown, data = .)

stargazer(
  base_iq_exp2_IVep_IVdi_IVage,
  base_iq_exp2_IVdi_IVage,
  type = "text"
)

summary(base_iq_exp2_IVep_IVdi_IVage)
summary(base_iq_exp2_IVdi_IVage)

#-------------------------------------------------------------------------------

# SA TUTORIALA PRATIM INSTRUMENT VALIDITY
modgoal <-   
  main %>%
  ivreg(log(wage) ~ iq | educ | educ_parent + dist_to_unitown, data = .)


mod1 <-   
  main %>%
  ivreg(log(wage) ~ iq | educ | educ_parent, data = .)

mod2 <-   
  main %>%
  ivreg(log(wage) ~ iq | educ | dist_to_unitown, data = .)

mod3 <-   
  main %>%
  ivreg(log(wage) ~ iq | educ | educ_parent + dist_to_unitown, data = .)

stargazer(
  mod1,
  mod2,
  mod3,
  type = "text"
)

# kojem estimatoru vjerovati?
summary(mod1)
summary(mod2)
summary(mod3)
# ovde bi trebo robust estimates onda uzeti, al to preskacemo
# coeftest(cig_ivreg_diff3, vcov = vcovHC, type = "HC1")



# first stage 2SLS, tj regressamo na endo
mod_rel1 <- 
  main %>%
    ivreg(educ ~ iq + educ_parent, data = .)

mod_rel2 <- 
  main %>%
    ivreg(educ ~ iq + dist_to_unitown, data = .)

mod_rel3 <- 
  main %>%
    ivreg(educ ~ iq + educ_parent + dist_to_unitown, data = .)

summary(mod_rel1)
summary(mod_rel2)
summary(mod_rel3)

#linearHypothesis(
#  mod_rel1, 
#  "educ_parent = 0", 
#  vcov = vcovHC,
#  type = "HC1"
#  )

linearHypothesis(
  mod_rel1, 
  "educ_parent = 0"
  )

linearHypothesis(
  mod_rel2, 
  "dist_to_unitown = 0"
  )

linearHypothesis(
  mod_rel3, 
  c("educ_parent = 0", "dist_to_unitown = 0")
  )

summary(mod1)
summary(mod2)
summary(mod3) #ovo je sa ivregom
# znaci ne moram manualno ove F testove isto je


# OVERIDENTIFICATION, overidentifying restrictions

overident <- lm(residuals(mod3) ~ iq + educ_parent + dist_to_unitown, data = main)

overidentChi <- 
  linearHypothesis(
    overident, 
    c("educ_parent = 0", "dist_to_unitown = 0"),
    test = "Chisq"
  )

pchisq(overidentChi[2, 5], df = 1, lower.tail = FALSE)

#system.time(round(9.2389899))

#-------------------------------------------------------------------------------


# remember AER package has ivreg func
model <- 
  main %>%
    ivreg(log(wage) ~ iq | educ + I(exp**2) | dist_to_unitown + age + educ_parent, data = .)
summary(model)
msummary(model)

# https://stats.stackexchange.com/questions/134789/interpretation-of-ivreg-diagnostics-in-r
# --Weak instrument F test on first stage 2SLS
# Null is that the instruments have weak corr with endo, F tests it
# here rejected so instruments strong enough
# --Wu Hausman checks if IV is consistent as OLS
# H0 is that they are equally consistent so its preferable to use OLS
# here rejected so IV more consistent
# --Sargan tests for OVERIDENTIFICATION tests if all exogenous instruments are actually exogenous and thus all IVs are valid
# H0 is that they are exogenous
# here not sign, so not rejected, thus no overidentification 

# F statistic uses 2 DFs

#uz state
model2 <- 
  main %>%
    ivreg(log(wage) ~ iq + factor(state) | educ + I(exp**2) | dist_to_unitown + age + educ_parent, data = .)

summary(model2)
#msummary(model2)
stargazer(model, model2, type = "text")





# ODLICAN SOURCE ZA OVERIDENTIFICATION
# https://www.econometrics-with-r.org/12.4-attdfc.html



# SPORO HAOS !!!!
coeftest(model, vcov. = vcovHC, type = "HC1")
# The default is to use "HC1" for lm objects and "HC0" otherwise.
# HC1 heteroskedasticity consistent
# HC0 applies no small sample bias adjustment.
# HC1 applies a degrees of freedom-based correction,
# (n−1)/(n−k) where n is the number of observations
# and k is the number of explanatory or predictor variables in the model.
# HC1 is the most commonly used approach for linear models, and HC0 otherwise.
# https://sandwich.r-forge.r-project.org/reference/vcovCL.html#

# HC - Heteroskedasticity Consistent
