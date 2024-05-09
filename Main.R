#Dataset: data weekly earnings.csv
#Assume that this cross-sectional dataset is a random iid (independent and identically distributed)
#sample of 50.000 individuals from a single country, and that is was sampled in a recent year.
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

rm(list=ls())
main <- read.csv("data_weekly_earnings.csv")


# source of function
# https://stackoverflow.com/questions/27096485/change-a-column-from-birth-date-to-age-in-r

agefunction <- 
  function(dob, age.day = today(), units = "years", floor = TRUE) {
    calc.age = lubridate::interval(dob, age.day) / lubridate::duration(num = 1, units = units)
    if (floor)
      return(as.integer(floor(calc.age)))
    return(calc.age)
  }


# keep in mind ages are floored 47,9 -> 47 yo
main$age <- agefunction(main$born)



summary(main)
str(main)
any(is.na(main))


# adapted bins to range, binwidth to feel
describe(main$wage)

main %>%
  ggplot(aes(wage))+
  geom_histogram(
    bins = 50,
    binwidth = 100
    ) +
  geom_vline(aes(xintercept = mean(wage)), color = "red") +
  geom_vline(aes(xintercept = median(wage)), color = "blue")



describe(main$educ)

main %>%
  ggplot(aes(educ))+
  geom_histogram(
    bins = 30,
    binwidth = 1
  ) +
  geom_vline(aes(xintercept = mean(educ)), color = "red") +
  geom_vline(aes(xintercept = median(educ)), color = "blue")



describe(main$educ_parent)

main %>%
  ggplot(aes(educ_parent))+
  geom_histogram(
    bins = 26,
    binwidth = 1
  ) +
  geom_vline(aes(xintercept = mean(educ_parent)), color = "red") +
  geom_vline(aes(xintercept = median(educ_parent)), color = "blue")



describe(main$age)

main %>%
  ggplot(aes(age))+
  geom_histogram(
    bins = 10,
    binwidth = 1
  ) +
  geom_vline(aes(xintercept = mean(age)), color = "red") +
  geom_vline(aes(xintercept = median(age)), color = "blue")



describe(main$state)

main %>%
  ggplot(aes(state))+
  geom_bar(
    bins = 55,
    binwidth = 1
  ) +
  geom_vline(aes(xintercept = mean(state)), color = "red") +
  geom_vline(aes(xintercept = median(state)), color = "blue")



describe(main$iq)

main %>%
  ggplot(aes(iq))+
  geom_histogram(
    bins = 121,
    binwidth = 1
  ) +
  geom_vline(aes(xintercept = mean(iq)), color = "red") +
  geom_vline(aes(xintercept = median(iq)), color = "blue")



describe(main$dist_to_unitown)

main %>%
  ggplot(aes(dist_to_unitown))+
  geom_histogram(
    bins = 71,
    binwidth = 1
  ) +
  geom_vline(aes(xintercept = mean(dist_to_unitown)), color = "red") +
  geom_vline(aes(xintercept = median(dist_to_unitown)), color = "blue")




