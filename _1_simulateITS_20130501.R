## Program to simulate explore the correct estimation of Interrupted-times-series designs ----
## this program is for experimentation to test things before implementing them in the function program



rm(list=ls())
library(lme4)
library(arm)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## specify parameter to generate data ====
# cohort size / number of students per year and school
stu.per.yr <- 100

# first and last year of data generation
start.yr <- 2005
end.yr <- 2012

# number of treatment and control schools
n.treatment.schools <- 1
n.control.schools <- 10

# vector of treatment years (for more than one year impact I think I have to adjust the code below)
impact.yrs <- c(2012)

# create a centered year variable so that the intercept of the ITS is more meaningful
center.yr <- 2011

# model parameters for the data generation
slope <- 1
overall.impact <- 0.5
treatment.impact <- 0.0

stu.error.sd <- 1
sch.error.sd <- 1
coh.error.sd <- 1


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## variables that are needed but that can be calculated based on the input above ====
start.id <- 1
total.yrs <- end.yr - start.yr + 1
n.schools <- n.treatment.schools + n.control.schools
n <- n.schools * total.yrs * stu.per.yr



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## create indicator variables and variables to estimate model ====

rep.yr <- rep(rep((start.yr:end.yr),each=stu.per.yr),n.schools)
school.id <- rep(start.id:n.schools, each=total.yrs * stu.per.yr)
c.yr <- rep.yr - center.yr

# its impact
impact.coeff <- rep(0, total.yrs * stu.per.yr * n.schools)
impact.coeff[rep.yr %in% impact.yrs] <- 1

# treatment indicator
treatment <- c(rep(1,n.treatment.schools * total.yrs * stu.per.yr), rep(0,n.control.schools * total.yrs * stu.per.yr))




#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## create different components of outcome variable and combine them into the y variables ====

true.score <- c.yr * slope
overall.impact.vector <- impact.coeff * overall.impact
treatment.impact.vector <- impact.coeff * treatment * treatment.impact

stu.error <- rnorm(c.yr, mean = 0, sd = stu.error.sd)
sch.error <- rep(rnorm( n.schools, sd = sch.error.sd)             ,each = total.yrs * stu.per.yr)
coh.error <- rep(rnorm( n.schools * total.yrs , sd = coh.error.sd),each = stu.per.yr)

y <- true.score + overall.impact.vector + treatment.impact.vector + stu.error + sch.error + coh.error




#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## check of generation looks correct ====

cbind(rep.yr,c.yr,treatment, impact.coeff, school.id, y, true.score, overall.impact.vector, treatment.impact.vector, stu.error, sch.error, coh.error)

## standard regression model
summary(lm(y ~ c.yr * treatment + impact.coeff * treatment))


## 


lmer (y ~ c.yr * treatment + impact.coeff * treatment + (1 | factor(rep.yr * school.id) ))


## empty model
M0 <- lmer (y ~ 1 + (1 | c.yr))
display (M0)

