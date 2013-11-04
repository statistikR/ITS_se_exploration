## Program to simulate explore the correct estimation of Interrupted-times-series designs ----

  library(lme4)
  library(arm)
  
createDataset <- function(
    ## specify parameter to generate data ====
  # cohort size / number of students per year and school
  stu.per.yr ,
  
  # first and last year of data generation
  start.yr ,
  end.yr ,
  
  # number of treatment and control schools
  n.treatment.schools ,
  n.control.schools ,
  
  # vector of treatment years (for more than one year impact I think I have to adjust the code below)
  impact.yrs ,
  
  # create a centered year variable so that the intercept of the ITS is more meaningful
  center.yr ,
  
  # model parameters for the data generation
  slope ,
  overall.impact ,
  treatment.impact ,
  
  stu.error.sd ,
  sch.error.sd ,
  coh.error.sd 
  
  ){

  
  end.yr = end.yr
  start.yr = start.yr
  n.treatment.schools = n.treatment.schools
  

 
 
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
  
  schoolYearRandomEffect <- factor(rep.yr * school.id) 
  
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## check if generation looks correct ====
  
  data <- as.data.frame(cbind(rep.yr,c.yr,treatment, impact.coeff, school.id,   schoolYearRandomEffect, y, true.score, overall.impact.vector, treatment.impact.vector, stu.error, sch.error, coh.error))
  
  return(data)
  


}

## this function compares the SEs of the impact estimates between MLM and OLS estimation

analyzeITSdataset <- function(dataSet){
    # extract p-value OLS regression
  
  
  .temp <- summary(lm(y ~ c.yr * treatment + impact.coeff * treatment, data = dataSet))
  p.OLS <- .temp$coeff["treatment:impact.coeff","Pr(>|t|)"]
  
  # extract p-values MLM model
  
  .temp.mlm <- display( lmer (y ~ c.yr * treatment + impact.coeff * treatment + (1 | schoolYearRandomEffect ), data = dataSet ) )
  p.MLM <- pt(-abs(.temp.mlm$t.value["treatment:impact.coeff"]),df=100)*2
  
  
  return(c(p.OLS,p.MLM))
  
}






## generate a dataset
 s1 <- createDataset(stu.per.yr = 100, start.yr = 2005, end.yr = 2012, n.treatment.schools = 1, n.control.schools = 1, impact.yrs = c(2012),
  center.yr = 2011, slope = 1, overall.impact = 0.5, treatment.impact = 1.0, stu.error.sd = 0.5, sch.error.sd = 0,
  coh.error.sd = 0)

analyzeITSdataset(s1)



# generate datasets and save p-values of simulation
pValues <- replicate(n = 10, expr =analyzeITSdataset( createDataset(stu.per.yr = 100, start.yr = 2005, end.yr = 2012, n.treatment.schools = 1, n.control.schools = 1, impact.yrs = c(2012),
  center.yr = 2011, slope = 1, overall.impact = 0.5, treatment.impact = 1.0, stu.error.sd = 0.5, sch.error.sd = 1,
  coh.error.sd = 1) ))

# change row-names
row.names(pValues) <- c("OLS","MLM")
# calculate significant results
sig <- apply(pValues,c(1,2),function(x) (x<0.05)*1 ) 

rowMeans(sig)
