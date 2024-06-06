# 0.1 LOAD LIBRARIES -----------------------------------------------------------
library(tidyverse)
library(flextable)
library(ggthemes)
library(lubridate)
library(psych)
library(stargazer)
library(ivreg)
library(modelsummary)
library(car)

# 0.2 CLEAN ENV & LOAD DATA ----------------------------------------------------

rm(list=ls())
main <- read.csv("data_weekly_earnings.csv")

# 0.3 PREPARE DATA -------------------------------------------------------------
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

#rearrange
main <- 
  main[,c(
    "wage",
    "logwage",
    "educ",
    "educ_parent",
    "age",
    "age2",
    "exp",
    "exp2",
    "iq",
    "dist_to_unitown",
    "state",
    "born"
    )]

#rename
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
  # save_as_docx(path = "cormatrix2.docx")
  # save_as_image(x, path = "cormatrix.png", expand = 10, res = 200)

# TODO: poredjenje logwage i wage
# TODO: svaku varijablu i relationship?
# TODO: heterogeneity?

# 1 Without instruments ----------------------------------------------------------

# Edu along with controls individually onto logwage
simple_edu <-   
  main %>%
  ivreg(log(wage) ~ edu, data = .)

simple_exp <-   
  main %>%
  ivreg(log(wage) ~ exp, data = .)

simple_exp2 <-   
  main %>%
  ivreg(log(wage) ~ poly(exp, 2, raw = TRUE), data = .)

simple_iq <-   
  main %>%
  ivreg(log(wage) ~ iq, data = .)

summary(simple_edu)
summary(simple_exp)
summary(simple_exp2)
summary(simple_iq)

stargazer(
  simple_edu,
  simple_exp,
  simple_exp2,
  simple_iq,
  type = "text"
)

# Edu with individual controls
eduControls1 <-   
  main %>%
  ivreg(log(wage) ~ edu + iq, data = .)

eduControls2 <-   
  main %>%
  ivreg(log(wage) ~ edu + exp, data = .)

eduControls3 <-   
  main %>%
  ivreg(log(wage) ~ edu + poly(exp, 2, raw = TRUE), data = .)

# Edu with combined controls
eduControls4 <-   
  main %>%
  ivreg(log(wage) ~ edu + iq + exp, data = .)

eduControls5 <-   
  main %>%
  ivreg(log(wage) ~ edu + iq + poly(exp, 2, raw = TRUE), data = .)

summary(eduControls1)
summary(eduControls2)
summary(eduControls3)
summary(eduControls4)
summary(eduControls5)
# max 4 modela u stargazer


stargazer(
  eduControls1,
  eduControls2,
  eduControls3,
  eduControls5, #exp2
  type = "text"
)

stargazer(
  eduControls4,
  eduControls5,
  type = "text"
)


# 2 Introducing Instruments for Endogenous Variables ---------------------------


# check for exclusion
exclusionALL <-   
  main %>%
  ivreg(log(wage) ~ edu + edu_par + dist + age + iq + poly(exp, 2, raw = TRUE), data = .)

summary(exclusionALL)

exclusion1 <-   
  main %>%
  ivreg(log(wage) ~ edu + edu_par, data = .)

exclusion2 <-   
  main %>%
  ivreg(log(wage) ~ edu + dist, data = .)

exclusion3 <-   
  main %>%
  ivreg(log(wage) ~ exp + age, data = .)

exclusion4 <-   
  main %>%
  ivreg(log(wage) ~ poly(exp, 2, raw = TRUE) + poly(age, 2, raw = TRUE), data = .)

summary(exclusion1)
summary(exclusion2)
summary(exclusion3)
summary(exclusion4)

# 2.1 Endogenous "edu" ---------------------------------------------------------



# First stage

# edu (ed)
# With controls (wc)
# edu_par = ep
# dist = di
ed_FS_ep <-   
  main %>%
  ivreg(edu ~ edu_par + iq + poly(exp, 2, raw = TRUE), data = .)

ed_FS_di <-   
  main %>%
  ivreg(edu ~ dist + iq + poly(exp, 2, raw = TRUE), data = .)

ed_FS_ep_di <-   
  main %>%
  ivreg(edu ~ edu_par + dist + iq + poly(exp, 2, raw = TRUE), data = .)

# JUST A CHECK - F-test - compare with weak instruments
linearHypothesis(
  ed_FS_ep, 
  "edu_par = 0"
)

linearHypothesis(
  ed_FS_di, 
  "dist = 0"
)

linearHypothesis(
  ed_FS_ep_di, 
  c("edu_par = 0", "dist = 0")
)



#TSLS with controls (wc)
ed_TSLS_ep <-   
  main %>%
  ivreg(log(wage) ~ iq + poly(exp, 2, raw = TRUE) | edu | edu_par, data = .)

ed_TSLS_di <-   
  main %>%
  ivreg(log(wage) ~ iq + poly(exp, 2, raw = TRUE) | edu | dist, data = .)

ed_TSLS_ep_di <-   
  main %>%
  ivreg(log(wage) ~ iq + poly(exp, 2, raw = TRUE) | edu | edu_par + dist, data = .)

summary(ed_TSLS_ep)
summary(ed_TSLS_di)
summary(ed_TSLS_ep_di)

stargazer(
  ed_TSLS_ep,
  ed_TSLS_di,
  ed_TSLS_ep_di,
  type = "text"
)



#TSLS no controls (nc)
ed_TSLS_nc1 <-   
  main %>%
  ivreg(log(wage) ~ edu | edu_par, data = .)

ed_TSLS_nc2 <-   
  main %>%
  ivreg(log(wage) ~ edu | dist, data = .)

ed_TSLS_nc3 <-   
  main %>%
  ivreg(log(wage) ~ edu | edu_par + dist, data = .)

summary(ed_TSLS_nc1)
summary(ed_TSLS_nc2)
summary(ed_TSLS_nc3)

stargazer(
  ed_TSLS_nc1,
  ed_TSLS_nc2,
  ed_TSLS_nc3,
  type = "text"
)

# JUST A CHECK - J test for overidentification - compare with Sargan
# b <-   
#   main %>%
#   ivreg(log(wage) ~ iq | edu | edu_par + dist, data = .)
# 
# summary(b)
# 
# a <- 
#   lm(residuals(b) ~ main$edu_par + main$dist)
# 
# test_a <- 
#   linearHypothesis(
#     a,
#     c("main$edu_par = 0", "main$dist = 0"), 
#     test = "Chisq"
#   )
# 
# test_a
# pchisq(test_a[2, 5], df = 2, lower.tail = FALSE)


# 2.2 Endogenous "exp2" ---------------------------------------------------------

# TSLS with edu instruments (wei)

# exp2 (ex2)
# With controls (wc)
# edu_par = ep
# dist = di

ex2_TSLS_wei1_wc <-   
  main %>%
  ivreg(log(wage) ~ iq | poly(exp, 2, raw = TRUE) + edu | poly(age, 2, raw = TRUE) + edu_par, data = .)

ex2_TSLS_wei2_wc <-   
  main %>%
  ivreg(log(wage) ~ iq | poly(exp, 2, raw = TRUE) + edu | poly(age, 2, raw = TRUE) + dist, data = .)

ex2_TSLS_wei3_wc <-   
  main %>%
  ivreg(log(wage) ~ iq | poly(exp, 2, raw = TRUE) + edu | poly(age, 2, raw = TRUE) + edu_par + dist, data = .)

summary(ex2_TSLS_wei1_wc)
summary(ex2_TSLS_wei2_wc)
summary(ex2_TSLS_wei3_wc)

stargazer(
  ex2_TSLS_wei1_wc,
  ex2_TSLS_wei2_wc,
  ex2_TSLS_wei3_wc,
  type = "text"
)



# TSLS no edu instruments (nei) # IRRELEVANT

# ex_TSLS_nei1_nc <-   
#   main %>%
#   ivreg(log(wage) ~ iq + edu | poly(exp, 2, raw = TRUE) | poly(age, 2, raw = TRUE), data = .)
# 
# ex_TSLS_nei2_nc <-   
#   main %>%
#   ivreg(log(wage) ~ iq + edu | poly(exp, 2, raw = TRUE) | poly(age, 2, raw = TRUE), data = .)
# 
# ex_TSLS_nei3_wc <-   
#   main %>%
#   ivreg(log(wage) ~ iq + edu | poly(exp, 2, raw = TRUE) | poly(age, 2, raw = TRUE), data = .)
# 
# summary(ex_TSLS_nei1_nc)
# summary(ex_TSLS_nei2_nc)
# summary(ex_TSLS_nei3_nc)
# 
# stargazer(
#   ex_TSLS_nei1_nc,
#   ex_TSLS_nei2_nc,
#   ex_TSLS_nei3_nc,
#   type = "text"
# )



# 2.3 Endogenous "exp" --------------------------------------------------------

# TSLS with edu instruments (wei)

# exp (ex)
# With controls (wc)
# edu_par = ep
# dist = di

ex_TSLS_wei1_wc <-   
  main %>%
  ivreg(log(wage) ~ iq | exp + edu | age + edu_par, data = .)

ex_TSLS_wei2_wc <-   
  main %>%
  ivreg(log(wage) ~ iq | exp + edu | age + dist, data = .)

ex_TSLS_wei3_wc <-   
  main %>%
  ivreg(log(wage) ~ iq | exp + edu | age + edu_par + dist, data = .)

summary(ex_TSLS_wei1_wc)
summary(ex_TSLS_wei2_wc)
summary(ex_TSLS_wei3_wc)

stargazer(
  ex_TSLS_wei1_wc,
  ex_TSLS_wei2_wc,
  ex_TSLS_wei3_wc,
  type = "text"
)


# 3 Introducing "State" as control -------------------------------------------

# State is used as a factor to dummify
# with state as control (ws)
# no state as control (ns)

final1_ns <- ex2_TSLS_wei3_wc
# ivreg(log(wage) ~ iq | poly(exp, 2, raw = TRUE) + edu | poly(age, 2, raw = TRUE) + edu_par + dist, data = .)

final2_ns <- ex_TSLS_wei3_wc
# ivreg(log(wage) ~ iq | exp + edu | age + edu_par + dist, data = .)

final1_ws <-   
  main %>%
  ivreg(log(wage) ~ iq + I(factor(state)) | poly(exp, 2, raw = TRUE) + edu | poly(age, 2, raw = TRUE) + edu_par + dist, data = .)

final2_ws <-   
  main %>%
  ivreg(log(wage) ~ iq + I(factor(state)) | exp + edu | age + edu_par + dist, data = .)

#exp2
summary(final1_ns)
summary(final1_ws)
#exp
summary(final2_ns)
summary(final2_ws)

finals <- 
  stargazer(
    final1_ns,
    final1_ws,
    final2_ns,
    final2_ws,
    type = "text"
  )

# stargazer(
#   final1_ns,
#   final1_ws,
#   final2_ns,
#   final2_ws,
#   type = "latex",
#   out="finalsTable.html"
# )

# 3.1 Check Residuals ----------------------------------------------------------

final1_ns
final1_ws
final2_ns
final2_ws

fitRes1 <- data.frame(fit = final1_ns$fitted.values, res=final1_ns$residuals)
fitRes2 <- data.frame(fit = final1_ws$fitted.values, res=final1_ns$residuals)
fitRes3 <- data.frame(fit = final2_ns$fitted.values, res=final2_ns$residuals)
fitRes4 <- data.frame(fit = final2_ws$fitted.values, res=final2_ws$residuals)

# check for heteroskedasticity
# check for normality of residuals

fitRes1 %>%
  ggplot(aes(x = fit, y = res)) +
  geom_point(alpha = .1) +
  geom_hline(yintercept = 0, color = "#D81B60", linetype = "dashed", linewidth = 1)
  
fitRes1 %>%
  ggplot(aes(sample = res)) +
  stat_qq() +
  stat_qq_line(color = "#D81B60", linetype = "dashed", linewidth = 1)

 
fitRes2 %>%
  ggplot(aes(x = fit, y = res)) +
  geom_point(alpha = .1) +
  geom_hline(yintercept = 0, color = "#D81B60", linetype = "dashed", linewidth = 1)

 
fitRes2 %>%
  ggplot(aes(sample = res)) +
  stat_qq() +
  stat_qq_line(color = "#D81B60", linetype = "dashed", linewidth = 1)


fitRes3 %>%
  ggplot(aes(x = fit, y = res)) +
  geom_point(alpha = .1) +
  geom_hline(yintercept = 0, color = "#D81B60", linetype = "dashed", linewidth = 1)

  
fitRes3 %>%
  ggplot(aes(sample = res)) +
  stat_qq() +
  stat_qq_line(color = "#D81B60", linetype = "dashed", linewidth = 1)


fitRes4 %>%
  ggplot(aes(x = fit, y = res)) +
  geom_point(alpha = .1) +
  geom_hline(yintercept = 0, color = "#D81B60", linetype = "dashed", linewidth = 1)


fitRes4 %>%
  ggplot(aes(sample = res)) +
  stat_qq() +
  stat_qq_line(color = "#D81B60", linetype = "dashed", linewidth = 1)


# 3.2 Again Check Relevance For Each Endogenous For Each Model -----------------

#TODO: mozda







#stop

























# TEST SVE ISPOD-------------------------------------------------------------------------
