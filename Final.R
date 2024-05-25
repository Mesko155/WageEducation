library(tidyverse)
library(flextable)
library(ggthemes)
library(lubridate)
library(psych)
library(stargazer)
library(ivreg)
library(modelsummary)
library(car)

rm(list=ls())
main <- read.csv("data_weekly_earnings.csv")

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
main$logwage <- log(main$wage)
main$exp <- main$age - main$educ - 6
main$exp2 <- main$exp^2
main$age2 <- main$age^2

main <- main[,c(2,9,3,4,10,13,11,12,7,8,6,5)]
names(main) <-
  c(
    "wage",
    "logwage",
    "edu",
    "edu_par",
    "age",
    "age2",
    "exp",
    "exp2",
    "iq",
    "dist",
    "state",
    "born"
  )

main <- tibble(main)

cormatrix <- 
  cor(
    main[,
         c(
           "wage",
           "logwage",
           "edu",
           "edu_par",
           "age",
           "age2",
           "exp",
           "exp2",
           "iq",
           "dist"
         )
    ],
    method = "pearson",
    use = "complete.obs"
  ) %>%
  round(4)


cormatrix <- data.frame(cormatrix)
cormatrix <- cbind(X = names(cormatrix), cormatrix)

cormatrix |>
  flextable() |>
  theme_apa() |>
  align(align = "center", part = "header") |>
  align(align = "right", part = "body") |>
  set_table_properties(align = "center", layout = "autofit") #|>
  #save_as_docx(path = "cormatrix.docx")
  #save_as_image(x, path = "cormatrix.png", expand = 10, res = 200)



