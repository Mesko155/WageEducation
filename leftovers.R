# 15 i nakon 
main %>% 
  mutate(D = ifelse(educ >= 15, 1, 0)) %$%  #mora $ u pipe
  lm(wage ~ D * I(educ - 15)) %>%
  # I je ko escaper lm formulu
  # * je cross ISTO STO i D + X + D:X
  summary()

main %>% 
  mutate(D = ifelse(educ >= 15, 1, 0)) %$%  #bez -15
  lm(wage ~ D * educ) %>% 
  summary()
# ovo mozda bolje D se samo promjeni bitno nam je u sustini samo 

#-------------------------------------------------------------------------------

# probni za druge godine za cutoff
#TODO: napraviti for loop za sve godine da vidim kad je najbolji model
main %>% 
  mutate(cutoffyear = ifelse(educ >= 16, 1, 0)) %$%  #bez -15
  lm(wage ~ cutoffyear * (educ + educ_parent + age + iq + dist_to_unitown)) %>% 
  summary() # JEBIGA BOLJI MODEL kad je cutoff na 18

#-------------------------------------------------------------------------------

# bez RDD
main %>%
  lm(wage ~ educ, data = .) %>%
  summary()
# obicni 0.30 a sa RDD 0.36
# ovo je tek prije ubacivanja svih kontrola


# prije 15
x <- main %>%
  filter(educ < 18) %>%
  lm(wage ~ educ, data = .)

summary(x)
#generalno slabiji jer koristis manje data, ovaj ispod inkorporira
#sav data i jaci je, a ovdje je isti koeficijent TO JE SVE STO NAS ZANIMA !!!


#https://rpubs.com/phle/r_tutorial_regression_discontinuity_design
#https://bookdown.org/carillitony/bailey/chp11.html

#rdd_data(main$educ, main$wage, cutpoint = 15) %>% 
#  rdd_reg_lm(slope = "separate") %>% 
#  summary()
#
#x <- rdd_data(main$educ, main$wage, cutpoint = 15) %>% 
#      rdd_reg_lm(slope = "separate")




#baseline wage ~ educ 0.308
# a ovaj sa svim kontrolnim 0.324
#ovako treba za 2 linije BEZ STATE ko integer
main %>% 
  mutate(BA_or_more = ifelse(educ >= 18, 1, 0)) %$%  #state nije ukljucen, jer ko integer insig
  lm(wage ~ BA_or_more * (educ + educ_parent + age + iq + dist_to_unitown)) %>% 
  summary() #3.858

# + STATE KO FAKTOR moram skontat sta cu states
main %>% 
  mutate(over = ifelse(educ >= 18, 1, 0)) %$% 
  lm(wage ~ over * (educ + educ_parent + age + iq + dist_to_unitown + factor(state))) %>% 
  summary() #poraste naravno na 0.3927



#-------------------------------------------------------------------------------


# PLOTTANJE NEUSPJELO

# ako hoces u plotu komplikovaniju formulu
forplot <- 
  main %>%
  lm(log(wage) ~ educ + educ_parent + iq + age, data = .)


main %>%
  ggplot(aes(educ, log(wage))) +
  geom_point() +
  geom_smooth(formula = forplot$model)

# test bez loga ne radi :/


forplot2 <- 
  main %>%
  lm(logwage ~ educ + educ_parent + iq + age, data = .)

main %>%
  ggplot(aes(educ, logwage)) +
  geom_point() +
  geom_line(aes(y = forplot2$fitted.values))

main %>%
  ggplot(aes(educ, logwage)) +
  geom_point() +
  geom_line(aes(y = predict(forplot2, type = "response")))


main %>%
  ggplot(aes(educ, logwage)) +
  geom_point() +
  geom_smooth(formula = logwage ~ educ)

#basic ne radi uopste
main$educ <- as.numeric(main$educ)

main %>%
  ggplot(aes(educ, wage), data = .) +
  geom_point() +
  geom_smooth(
    formula = y ~ x
  )

main %>%
  ggplot(aes(educ, wage), data = .) +
  geom_point() +
  geom_smooth(
    formula = y ~ x
  )













#-------------------------------------------------------------------------------













#-------------------------------------------------------------------------------











#-------------------------------------------------------------------------------