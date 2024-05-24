################################################################################
#########            Applied Statistics and Econometrics             ###########
#########                   Instrumental variables                   ###########
#########                    Suggested Solution                      ###########
################################################################################


#Source: Introduction to Econometrics with R by
#Christoph Hanck, Martin Arnold, Alexander Gerber, and Martin Schmelzer


#There are many studies in labor economics which deal with the issue of 
#estimating human capital earnings functions which state how wage income is
#determined by education and working experience.


#Dataset you will be using stems from a survey of high school graduates with variables
#coded for wages, education, average tuition and a number of socio-economic measures. 

#The data set also includes the distance from a college while the survey participants were in high school. 
#dataset "CollegeDistance" comes with the AER package.

###Task 1###

# attach the package `AER`
library(AER)           

# load the `CollegeDistance` data set
data("CollegeDistance")

?CollegeDistance
# get an overview over the data set
summary(CollegeDistance)
# or
str(CollegeDistance)
# or
head(CollegeDistance)

# generate a histogram of `distance`
hist(CollegeDistance$distance, main = "Distance to College", xlab = "Distance in 10 Miles")



#Regressing wage on education and control variables to estimate the human capital earnings
#function is problematic because education is not randomly assigned across the surveyed:
#individuals make their own education choices and so measured differences in earnings between 
#individuals with different levels of education depend on how these choices are made.
#In the literature this is referred to as a selection problem. This selection problem implies
#that education is endogenous so the OLS estimate will be biased and we cannot make valid inference
#regarding the true coefficient.


###Task 2###

# regress the log of `wage` on `education` 
wage_mod_1 <- lm(log(wage) ~ education, data = CollegeDistance)           

# Think of relevant controls and regress log of `wage` on `education` and controls
wage_mod_2 <- lm(log(wage) ~ unemp + ethnicity + gender + urban + education, data = CollegeDistance)     

# obtain robust coefficient summaries on both models
summary(wage_mod_1, vcov. = vcovHC, type = "HC1")
summary(wage_mod_2, vcov. = vcovHC, type = "HC1")

#In model specification with controls effect of education on wage is no more statistically significant.


#The above discussed selection problem renders the regression estimates in Task 2 implausible 
#One way to solve it is to try estimating instrumental variables regression
#that uses college distance as an instrument for education.

#Why use college distance as an instrument? The logic behind this is that 
#distance from a college will be correlated to the decision to pursue a college degree
#(instrument relevance)
#but may not predict wages apart from increased education (instrument exogeneity,
#which might be still questionable)
#so college proximity could be considered a valid instrument.


###Task 3###

# compute the correlation of the instrument with the endogenous regressor
cor(CollegeDistance$distance, CollegeDistance$education)

#Magnitude of correlation coefficient is quite low but sign of relationship meets expectation.

# perform the first stage regression and compute the fraction of explained variation
#Hint: use summary(reg_model results)$$r.squared
summary(lm(education ~ distance+unemp + ethnicity + gender + urban, data = CollegeDistance)) 
R2 <- summary(lm(education ~ distance+unemp + ethnicity + gender + urban, data = CollegeDistance))$r.squared 


# estimate the IV regression of `log(wage)` on `education` using distance as the instrument
#Hint: use function ivreg()
wage_mod_iv1 <- ivreg(log(wage) ~ education | distance, data = CollegeDistance)           

# perform TSLS (two-stages least squares) regression of `log(wage)` on `education` and controls using distance as the instrument 
wage_mod_iv2 <- ivreg(log(wage) ~ unemp + ethnicity + gender + urban + 
                      +education | . - education + distance, data = CollegeDistance) 

# obtain robust coefficient summaries on both models
coeftest(wage_mod_iv1, vcov. = vcovHC, type = "HC1")
coeftest(wage_mod_iv2, vcov. = vcovHC, type = "HC1")


#It is likely that the bias of the estimated coefficient on education in the
#simple regression model (first model) is substantial
#because the regressor is endogenous due to omitting variables from the model
#which correlate with education and impact wage income.

#Due to the selection problem, the estimate of the coefficient of interest is not
#trustworthy even in the multiple regression model (second model)
#which includes several socio-economic control variables. The coefficient on education
#is not significant and its estimate is close to zero.

#Instrumenting education by the college distance as done in first IV estimation yields
#the IV estimate of the coefficient of interest. 
#The result should, however, not be considered reliable because this simple model
#probably suffers from omitted variables bias. 
#Again, the coefficient on education is not statistically significant its estimate is quite small.

#The multiple regression model where we include demographic control variables and
#instrument education by distance delivers the most reliable estimate of the 
#impact of education on wage income among all the models considered. 
#The coefficient is highly significant and the estimate is about 0.067.
#The interpretation is that an additional year of schooling is expected to increases
#wage income by roughly 6.7%.


#Is obtained estimate of the coefficient on education trustworthy? This question is not easy to answer. 
#In any case, we should bear in mind that using an instrumental variables approach
#is problematic when the instrument is weak. 
#This could be the case here: Families with strong preference for education may 
#move into neighborhoods close to colleges. 
#Furthermore, neighborhoods close to colleges may have stronger job markets 
#reflected by higher incomes. 
#Such features would render the instrument invalid as they introduce unobserved
#variables which influence earnings
#but cannot be captured by years of schooling, our measure of education.

library(stargazer)

stargazer(wage_mod_1,
          wage_mod_2,
          wage_mod_iv1,
          wage_mod_iv2,
          type="text",
          keep.stat=c("n","rsq"))

