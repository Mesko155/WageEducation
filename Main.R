# LIBRARIES AND INFO -----------------------------------------------------------
#Dataset: data weekly earnings.csv
#Assume that this cross-sectional dataset is a random i.i.d.
#sample of 50.000 individuals from a single country, sampled in a recent year.
#It contains the following variables:
# • wage: individual’s weekly earnings
# • educ: years of education
# • educ parent: years of education of parents
# • born: date of birth
# • state: state the individual lives in
# • iq: result of a recent IQ test
# • dist to unitown: distance to the next city/town that offers higher education
library(tidyverse)
library(flextable)
library(ggthemes)
library(lubridate)
library(psych)
library(stargazer)
library(ivreg)
library(mgcv)


library(rddtools)
library(broom)


# ENVIRONMENT AND PREPWORK -----------------------------------------------------
rm(list=ls())
main <- read.csv("data_weekly_earnings.csv")


# https://stackoverflow.com/questions/27096485/change-a-column-from-birth-date-to-age-in-r

agefunction <- 
  function(dob, age.day = today(), units = "years", floor = TRUE) {
    calc.age =
      lubridate::interval(dob, age.day) /
      lubridate::duration(num = 1, units = units)
    if (floor)
      return(as.integer(floor(calc.age)))
    return(calc.age)
  }

# keep in mind ages are floored 47,9 -> 47 yo
main$age <- agefunction(main$born)

main$wage_mean <- main$wage - mean(main$wage)

main$logwage <- log(main$wage)

main$exp <- main$age - main$educ

#TODO: first quarter born dummy
#TODO: closetouni dummy if 4 std  -inf do -1std, -1std do mean, mean do +1std, 1+std to +inf
# ovde ce mozda trebat RDD


summary(main)
str(main)
any(is.na(main))

sub_3sd <- 
  main[
    main$wage < (mean(main$wage) + (3*sd(main$wage)))
    &
    main$wage > (mean(main$wage) - (3*sd(main$wage))),
  ]

#TODO:
#1-8 elementary
#9-12 highschool
#13-15 BA
#16-17 MA
#18+ postgrad

#TODO: T test for RDD near limits

#for loop za cutoff godinu -----------------------------------------------------
outputdf <- NULL

for (i in 1:length(unique(main$educ))) {
  x <- 
    main %>% 
    mutate(cutoffyear = ifelse(educ >= i, 1, 0)) %$%
    lm(wage ~ cutoffyear * (educ + educ_parent + age + iq + dist_to_unitown)) %>% 
    summary()
  
  outputdf <- 
    rbind(
      outputdf,
      data.frame(
        index = i,
        r2 = x$r.squared
      )
    )
}

outputdf
outputdf[outputdf$r2 == max(outputdf$r2),]
# 18 godina ko cutoff je optimalna



# TEST EDUC_PARENTS FOR IV -----------------------------------------------------
edu_par_iv1 <- 
  main %>%
    lm(educ ~ educ_parent, data = .)

cor(
  main$educ,
  main$educ_parent,
  method = "pearson"
  )

edu_par_iv2 <-   
  main %>%
    lm(wage ~ educ_parent, data = .)

edu_par_iv3 <- 
  main %>%
    lm(wage ~ educ_parent + educ, data = .)

summary(edu_par_iv1)
summary(edu_par_iv2)
summary(edu_par_iv3)

stargazer(
  edu_par_iv1,
  edu_par_iv2,
  edu_par_iv3,
  type = "text"
)

# 1 educ corrs with educ_parent
# 2,3 educ_parent corrs with wage only through educ

#TODO: jos dokazati da educ_parent nije corr sa errorom KROZ TEORIJU







# EDA EACH VARIABLE ------------------------------------------------------------

# adapted bins to range+1, binwidth to feel or if discrete to 1
describe(main$wage)

main %>%
  ggplot(aes(wage)) +
  geom_histogram(
    bins = 50,
    binwidth = 100
    ) +
  geom_vline(aes(xintercept = mean(wage)), color = "red") +
  geom_vline(aes(xintercept = median(wage)), color = "blue")



describe(main$educ)

main %>%
  ggplot(aes(educ)) +
  geom_histogram(
    bins = 31,
    binwidth = 1
  ) +
  geom_vline(aes(xintercept = mean(educ)), color = "red") +
  geom_vline(aes(xintercept = median(educ)), color = "blue") +
  geom_vline(aes(xintercept = mean(educ) + (3*sd(educ))), color = "green") +
  geom_vline(aes(xintercept = mean(educ) - (3*sd(educ))), color = "green")
#TODO: procenat iznad 3xSD i da li ga izbrisati kao outliera




describe(main$educ_parent)

main %>%
  ggplot(aes(educ_parent)) +
  geom_histogram(
    bins = 26,
    binwidth = 1
  ) +
  geom_vline(aes(xintercept = mean(educ_parent)), color = "red") +
  geom_vline(aes(xintercept = median(educ_parent)), color = "blue")



describe(main$age)

main %>%
  ggplot(aes(age)) +
  geom_histogram(
    bins = 11,
    binwidth = 1
  ) +
  geom_vline(aes(xintercept = mean(age)), color = "red") +
  geom_vline(aes(xintercept = median(age)), color = "blue")



describe(main$state)

main %>%
  ggplot(aes(state)) +
  geom_bar(
    bins = 56,
    binwidth = 1
  ) +
  geom_vline(aes(xintercept = mean(state)), color = "red") +
  geom_vline(aes(xintercept = median(state)), color = "blue")



describe(main$iq)

main %>%
  ggplot(aes(iq)) +
  geom_histogram(
    bins = 122,
    binwidth = 1
  ) +
  geom_vline(aes(xintercept = mean(iq)), color = "red") +
  geom_vline(aes(xintercept = median(iq)), color = "blue")



describe(main$dist_to_unitown)

main %>%
  ggplot(aes(dist_to_unitown)) +
  geom_histogram(
    bins = 72,
    binwidth = 1
  ) +
  geom_vline(aes(xintercept = mean(dist_to_unitown)), color = "red") +
  geom_vline(aes(xintercept = median(dist_to_unitown)), color = "blue")

#TODO: facetwrap multiple plots for report

#EDA relationships -------------------------------------------------------------
main %>%
  ggplot(aes(educ, wage)) +
  geom_point(alpha = 0.5) +
  geom_point(stat = "summary", fun = "mean", color = "red")


# plot recentered on mean average wage
main %>%
  ggplot(aes(mean_wage, educ)) +
  geom_point(alpha = 0.5) +
  geom_vline(aes(xintercept = 0), color = "red")

mean(main$wage)


main %>%
  ggplot(aes(educ, wage)) +
  geom_point(stat = "summary", fun = "mean")
#TODO: ovde RDD nakon BA




#boxplots
main %>%
  ggplot(aes(x=as.factor(educ), y=wage)) + 
  geom_boxplot()



main %>%
  ggplot(aes(iq, wage)) +
  geom_point(alpha = 0.5) +
  geom_point(stat = "summary", fun = "mean", color = "red")
#iq alone doesnt predict shit

main %>%
  ggplot(aes(educ_parent, wage)) +
  geom_point(alpha = 0.5) +
  geom_point(stat = "summary", fun = "mean", color = "red")

main %>%
  ggplot(aes(educ_parent, educ)) +
  geom_point(alpha = 0.5) +
  geom_point(stat = "summary", fun = "mean", color = "red")



# pre regression ---------------------------------------------------------------
corrmatrix <- cor(main[,c(2:4, 7:9)], method = "pearson", use = "complete.obs")


# razmilsiti sta cu sa IQ





# TEST
sub_3sd %>% 
  mutate(D = ifelse(educ >= 15, 1, 0)) %$%  #bez -15
  lm(wage ~ D * educ) %>% 
  summary()
  #sa 0.361 na 0.384 pojaca

sub_3sd %>% 
  mutate(D = as.factor(ifelse(educ >= 15, 1, 0))) %>% 
  ggplot(aes(x = educ, y = wage, color = D)) +
  geom_point() + 
  geom_smooth(method = "lm") #plot bolje izgleda od ovog dole

main %>% 
  mutate(D = as.factor(ifelse(educ >= 15, 1, 0))) %>% 
  ggplot(aes(x = educ, y = wage, color = D)) +
  geom_point() + 
  geom_smooth(method = "lm")

  

# PAAAAAAAAAAAZI main$born postane dummy pa calculatea 10 minuta
main %>%
  lm(
    wage ~ educ + educ_parent + age + state + iq + dist_to_unitown,
    data = .
    ) %>%
  summary()





#state procesljat
model_rdd_1 <- 
  main %>% 
    mutate(BA_or_more = ifelse(educ >= 15, 1, 0)) %>%  #bez -15
    lm(
      wage ~ BA_or_more * (educ + educ_parent + age + state + iq + dist_to_unitown),
      data = .
      ) %>%
  summary()

model_rdd_1
model_rdd_1$coefficients[,4] < 0.05
model_rdd_1$terms

sum(model_rdd_1$coefficients[,4] < 0.05)

# heteroskedasticity check -----------------------------------------------------

main %>%
  lm(log(wage) ~ educ, data = .) %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red")

main %>%
  ggplot(aes(x = educ, y = logwage)) +
  geom_point()

# log transform ----------------------------------------------------------------

#WITH
main %>%
  lm(log(wage) ~ educ, data = .) %>%
  summary()
#WITHOUT
main %>%
  lm(wage ~ educ, data = .) %>%
  summary()

# with controls
main %>%
  lm(log(wage) ~ educ + educ_parent + iq + exp, data = .) %>%
  summary()





# educ parent as IV
main %>%
  lm(educ ~ educ_parent, data = .) %>%
    summary() # samo da vidim da ima efekt na educ 0.15

cov(main$educ, main$educ_parent)
cor(main$educ, main$educ_parent)

# ovo je za u knjizi kako ofali zbog IV
# samo da vidim sa ivreg da je isto
main %>%
  lm(log(wage) ~ educ, data = .) %>%
  summary()

main %>%
  ivreg(logwage ~ educ, data = .) %>%
  summary()

# kad ubacis IV ofali
main %>%
  ivreg(logwage ~ educ | educ_parent, data = .) %>%
  summary()










main %>%
  ggplot(aes(educ, log(wage))) +
  geom_point() +
  geom_smooth(method = "lm")




# IQ aka ABILITY ---------------------------------------------------------------

main %>%
  lm(wage ~ iq, data = .) %>%
  summary()

main %>%
  lm(wage ~ iq + educ, data = .) %>%
  summary()

main %>%
  lm(wage ~ iq + educ + age, data = .) %>%
  summary()

corrmatrix <- cor(main[,c(2:4, 7:9)], method = "pearson", use = "complete.obs")

cov(main$iq, main$educ_parent)

main %>%
  lm(educ ~ iq, data = .) %>%
  summary()

main %>%
  lm(logwage ~ iq, data = .) %>%
  summary()

main %>%
  lm(logwage ~ educ + iq, data = .) %>%
  summary()



# making first quarter dummy ---------------------------------------------------
















