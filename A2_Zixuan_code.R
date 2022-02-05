#=================================================================
# Set Directory and Import Data
#=================================================================
####Install####
library(tidyverse)
library(data.table)
library(plm)
rm(list = ls())
wd = "C:/Users/Qi zixuan/Desktop/Econ 613/Assignment 1"
setwd(wd)

datind2009 <- fread("./datind/datind2009.csv", header = T)
datind2009$idind <- as.character(datind2009$idind)
datind2009$idmen <- as.character(datind2009$idmen)

#=================================================================
# Exercise 1: OLS estimate
#=================================================================

####Question 1####
datind_use <- datind2009 %>%
  select(2, 3, 5, 9, 10) %>%
  filter(!is.na(wage) & !is.na(age))

X_Y_cor <- cor(x = datind_use[,"age"], y = datind_use[,"wage"])

####Question 2####
obj_OLS <- function(par, age, wage)
{
  xbeta = par[1] + par[2]*age
  epis  = wage - xbeta
  return(sum(epis^2))
}

start <- runif(2)
estimator <- optim(start, fn=obj_OLS, method="BFGS", age=datind_use$age,
                   wage=datind_use$wage, hessian=TRUE)

####Question 3####
# Using the standard formulas of the OLS
u_bar      <- datind_use$wage - estimator$par[1] - estimator$par[2]*datind_use$age
delta2_bar <- sum(u_bar^2)/(nrow(datind_use)-3)
X          <- cbind(rep(1,nrow(datind_use)), datind_use$age)
se_beta0   <- sqrt(delta2_bar*solve(t(X)%*%X)[1,1])
se_beta1   <- sqrt(delta2_bar*solve(t(X)%*%X)[2,2])
se_beta0
se_beta1
# Using bootstrap with 49 and 499 replications respectively
R1   <- 49;                      
nind <- nrow(datind_use);        
se_beta0_49 <- mat.or.vec(R1,1)
se_beta1_49 <- mat.or.vec(R1,1)
set.seed(123)
for (i1 in 1:R1)
{
  samp1        = sample(1:nind,nind,rep=TRUE)
  dat_samp1    = datind_use[samp1,]
  estimator1   = optim(start, fn=obj_OLS, method="BFGS", age=dat_samp1$age,
                     wage=dat_samp1$wage)
  u_bar1       = dat_samp1$wage - estimator$par[1] - estimator$par[2]*dat_samp1$age
  delta2_bar1   = sum(u_bar1^2)/(nrow(dat_samp1)-3)
  X1            = cbind(rep(1,nrow(dat_samp1)), dat_samp1$age)
  se_beta0_49[i1] = sqrt(delta2_bar1*solve(t(X1)%*%X1)[1,1])
  se_beta1_49[i1] = sqrt(delta2_bar1*solve(t(X1)%*%X1)[2,2])
}
mean(se_beta0_49)
mean(se_beta1_49)

R2   <- 499;                      
se_beta0_499 <- mat.or.vec(R2,1)
se_beta1_499 <- mat.or.vec(R2,1)
set.seed(122)
for (i2 in 1:R2)
{
  samp2            = sample(1:nind,nind,rep=TRUE)
  dat_samp2        = datind_use[samp2,]
  estimator2       = optim(start, fn=obj_OLS, method="BFGS", age=dat_samp2$age,
                     wage=dat_samp2$wage)
  u_bar2           = dat_samp2$wage - estimator$par[1] - estimator$par[2]*dat_samp2$age
  delta2_bar2      = sum(u_bar2^2)/(nrow(dat_samp2)-3)
  X2               = cbind(rep(1,nrow(dat_samp2)), dat_samp2$age)
  se_beta0_499[i2] = sqrt(delta2_bar2*solve(t(X2)%*%X2)[1,1])
  se_beta1_499[i2] = sqrt(delta2_bar2*solve(t(X2)%*%X2)[2,2])
}
mean(se_beta0_499)
mean(se_beta1_499)

#=================================================================
# Exercise 2: Detrend Data
#=================================================================

#Please separate the datasets to two, one is datind, the other is dathh
file_datind = list.files("datind")  
dir_datind = paste("./datind/", file_datind, sep="")  
n_datind = length(dir_datind)

datalist_datind <- vector("list", n_datind)

for (i in 1:16){
  datalist_datind[[i]] <- fread(dir_datind[i], header = T)
  datalist_datind[[i]]$idind <- as.character(datalist_datind[[i]]$idind)
  datalist_datind[[i]]$idmen <- as.character(datalist_datind[[i]]$idmen)
}

datind_2005_2018 <- datalist_datind[[2]]
for (i in 3:15){
  datind_2005_2018 <- rbind(datind_2005_2018, datalist_datind[[i]])
}
datind_2005_2018 <- datind_2005_2018 %>%
  select(2:5, 9, 10) %>%
  filter(!is.na(wage))

####Question 1####
datind_2005_2018_ag <- datind_2005_2018 %>%
  mutate(ag = 0,
         ag = ifelse(18 <= age & 25 >= age, 1, ag),
         ag = ifelse(26 <= age & 30 >= age, 2, ag),
         ag = ifelse(31 <= age & 35 >= age, 3, ag),
         ag = ifelse(36 <= age & 40 >= age, 4, ag),
         ag = ifelse(41 <= age & 45 >= age, 5, ag),
         ag = ifelse(46 <= age & 50 >= age, 6, ag),
         ag = ifelse(51 <= age & 55 >= age, 7, ag),
         ag = ifelse(56 <= age & 60 >= age, 8, ag),
         ag = ifelse(60 < age, 9, ag))
head(datind_2005_2018_ag)

####Question 2####
datind_2005_2018_ag_mean <- datind_2005_2018_ag %>%
  group_by(ag, year) %>%
  mutate(mean_wage = mean(wage)) %>%
  distinct(ag, year, .keep_all = TRUE) %>%
  ungroup() %>%
  filter(ag != 0)

p1 <- ggplot(datind_2005_2018_ag_mean,aes(x=year, y=mean_wage, color = ag, fill = ag)) + 
      geom_line() + facet_wrap(~ag)
p1
ggsave(p1,filename = "wage_each_group.png",height= 5, width= 5)

####Question 3####
reg1 <- lm(wage ~ age, data = datind_2005_2018)
reg1$coefficients
reg2 <- plm(wage ~ age, data = datind_2005_2018, index = "year", model = "within")
reg2$coefficients
# change from -182.4896 to -186.8793

#=================================================================
# Exercise 3: Numerical Optimization
#=================================================================
datind_2007 <- datalist_datind[[4]]%>%
  select(2:5, 9, 10) %>%
  filter(!is.na(wage))

####Question 1####
datind_2007 <- datind_2007 %>%
  filter(empstat != "Inactive")
head(datind_2007)

####Question 2####
datind_2007 <- datind_2007 %>%
  mutate(employ = ifelse(empstat == "Employed", 1, 0))

probit_like = function(par, age, empstat)
{
  xbeta           = par[1] + par[2]*age
  pr              = pnorm(xbeta)
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = empstat*log(pr) + (1-empstat)*log(1-pr)
  return(-sum(like))
}
# test the function
reg3     <- glm(employ ~ age, data = datind_2007, family = binomial(link = "probit"))
test_par <- reg3$coefficients
probit_like(test_par, datind_2007$age, datind_2007$employ)
logLik(reg3)

####Question 3####
int_value <- reg3$coefficients
res       <- optim(int_value, fn = probit_like, method = "BFGS", age = datind_2007$age,
               empstat = datind_2007$employ, hessian = TRUE)
res$par

####Question 4####
reg4 <- glm(employ ~ age + wage, data = datind_2007, family = binomial(link = "probit"))
# No. I think the reason is that including wages as a determinant of labor 
# market participation leads to an over-fitting problem: too many variables 
# in the model. The consequences is that model likelihood is not defined, 
# and thus I can't get the model to converge.

#=================================================================
# Exercise 4: Discrete choice
#=================================================================

datind_2005_2015 <- datalist_datind[[2]]
for (i in 3:12){
  datind_2005_2015 <- rbind(datind_2005_2015, datalist_datind[[i]])
}
datind_2005_2015 <- datind_2005_2015 %>%
  select(2:5, 9)

####Question 1####
datind_2005_2015 <- datind_2005_2015 %>%
  filter(empstat != "Inactive")
head(datind_2005_2015)

####Question 2####
datind_2005_2015 <- datind_2005_2015 %>%
  mutate(employ   = ifelse(empstat == "Employed", 1, 0),
         year2006 = ifelse(year == 2006, 1, 0),
         year2007 = ifelse(year == 2007, 1, 0),
         year2008 = ifelse(year == 2008, 1, 0),
         year2009 = ifelse(year == 2009, 1, 0),
         year2010 = ifelse(year == 2010, 1, 0),
         year2011 = ifelse(year == 2011, 1, 0),
         year2012 = ifelse(year == 2012, 1, 0),
         year2013 = ifelse(year == 2013, 1, 0),
         year2014 = ifelse(year == 2014, 1, 0),
         year2015 = ifelse(year == 2015, 1, 0)) 

# Probit Model
probit_like_FE = function(par, age, D1, D2, D3, D4, D5, D6, D7, D8, 
                          D9, D10, empstat)
{
  xbeta_FE              = par[1] + par[2]*age + par[3]*D1 + par[4]*D2 + 
                          par[5]*D3 + par[6]*D4 + par[7]*D5 + par[8]*D6 +
                          par[9]*D7 + par[10]*D8 + par[11]*D9 + par[12]*D10
  pr_FE                 = pnorm(xbeta_FE)
  pr_FE[pr_FE>0.999999] = 0.999999
  pr_FE[pr_FE<0.000001] = 0.000001
  like_FE               = empstat*log(pr_FE) + (1-empstat)*log(1-pr_FE)
  return(-sum(like_FE))
}

# optimize probit function
reg5                <- glm(employ ~ age+year2006+year2007+year2008+year2009+year2010+year2011+year2012+year2013+year2014+year2015,
                           data = datind_2005_2015, family = binomial(link = "probit"))
int_value_probit_FE <- reg5$coefficients
res_probit_FE       <- optim(int_value_probit_FE, fn = probit_like_FE, method = "BFGS", 
                             age = datind_2005_2015$age, D1 = datind_2005_2015$year2006,
                             D2 = datind_2005_2015$year2007, D3 = datind_2005_2015$year2008,
                             D4 = datind_2005_2015$year2009, D5 = datind_2005_2015$year2010,
                             D6 = datind_2005_2015$year2011, D7 = datind_2005_2015$year2012,
                             D8 = datind_2005_2015$year2013, D9 = datind_2005_2015$year2014,
                             D10 = datind_2005_2015$year2015, empstat = datind_2005_2015$employ, 
                             hessian = TRUE)
res_probit_FE$par[1:2]

#Logit Model
logit_like_FE = function(par, age, D1, D2, D3, D4, D5, D6, D7, D8, 
                          D9, D10, empstat)
{
  xbeta_FE              = par[1] + par[2]*age + par[3]*D1 + par[4]*D2 + 
                          par[5]*D3 + par[6]*D4 + par[7]*D5 + par[8]*D6 +
                          par[9]*D7 + par[10]*D8 + par[11]*D9 + par[12]*D10
  pr_FE                 = exp(xbeta_FE)/(1+exp(xbeta_FE))
  pr_FE[pr_FE>0.999999] = 0.999999
  pr_FE[pr_FE<0.000001] = 0.000001
  like_FE               = empstat*log(pr_FE) + (1-empstat)*log(1-pr_FE)
  return(-sum(like_FE))
}

# optimize probit function
reg6                <- glm(employ ~ age+year2006+year2007+year2008+year2009+year2010+year2011+year2012+year2013+year2014+year2015,
                           data = datind_2005_2015, family = binomial(link = "logit"))
int_value_logit_FE <- reg6$coefficients
res_logit_FE       <- optim(int_value_logit_FE, fn = logit_like_FE, method = "BFGS", 
                             age = datind_2005_2015$age, D1 = datind_2005_2015$year2006,
                             D2 = datind_2005_2015$year2007, D3 = datind_2005_2015$year2008,
                             D4 = datind_2005_2015$year2009, D5 = datind_2005_2015$year2010,
                             D6 = datind_2005_2015$year2011, D7 = datind_2005_2015$year2012,
                             D8 = datind_2005_2015$year2013, D9 = datind_2005_2015$year2014,
                             D10 = datind_2005_2015$year2015, empstat = datind_2005_2015$employ, 
                             hessian = TRUE)
res_logit_FE$par[1:2]
# LPM can use OLS instead of MLE
X_LPM <- cbind(rep(1,nrow(datind_2005_2015)), datind_2005_2015$age, datind_2005_2015$year2006, 
               datind_2005_2015$year2007,datind_2005_2015$year2008,datind_2005_2015$year2009,
               datind_2005_2015$year2010,datind_2005_2015$year2011,datind_2005_2015$year2012,
               datind_2005_2015$year2013,datind_2005_2015$year2014,datind_2005_2015$year2015)
Y_LPM <- datind_2005_2015$employ
res_LPM_FE <- solve(t(X_LPM)%*%X_LPM)%*%t(X_LPM)%*%Y_LPM
res_LPM_FE[1:2]

####Question 3####
# LPM
Xbeta_LPM   <- res_LPM_FE[1] + res_LPM_FE[2]*datind_2005_2015$age + res_LPM_FE[3]*datind_2005_2015$year2006 + 
               res_LPM_FE[4]*datind_2005_2015$year2007 + res_LPM_FE[5]*datind_2005_2015$year2008 + 
               res_LPM_FE[6]*datind_2005_2015$year2009 + res_LPM_FE[7]*datind_2005_2015$year2010 + 
               res_LPM_FE[8]*datind_2005_2015$year2011 + res_LPM_FE[9]*datind_2005_2015$year2012 + 
               res_LPM_FE[10]*datind_2005_2015$year2013 + res_LPM_FE[11]*datind_2005_2015$year2014 + 
               res_LPM_FE[12]*datind_2005_2015$year2015
epis      <-  datind_2005_2015$employ - Xbeta_LPM 
delta2_LPM <- sum(epis^2)/(nrow(datind_2005_2015)-12)
LPM_se_age   <- sqrt(delta2_LPM*solve(t(X_LPM)%*%X_LPM)[2,2])
LPM_se_int   <- sqrt(delta2_LPM*solve(t(X_LPM)%*%X_LPM)[1,1])
t_LPM_age <- res_LPM_FE[2]/LPM_se_age
t_LPM_age
#check
reg7 <- lm(employ ~ age+year2006+year2007+year2008+year2009+year2010+year2011+year2012+year2013+year2014+year2015,
           data = datind_2005_2015)
summary(reg7)

#probit
probit_est_se  <- sqrt(solve(res_probit_FE$hessian)[2,2])
t_probit_age   <- res_probit_FE$par[2]/probit_est_se
t_probit_age

#logit
logit_est_se   <- sqrt(solve(res_logit_FE$hessian)[2,2])
t_logit_age    <- res_logit_FE$par[2]/logit_est_se
t_logit_age 

#=================================================================
# Exercise 5: Marginal Effects
#=================================================================
####Question 1####
Xbeta_Probit <- res_probit_FE$par[1] + res_probit_FE$par[2]*datind_2005_2015$age + res_probit_FE$par[3]*datind_2005_2015$year2006 + res_probit_FE$par[4]*datind_2005_2015$year2007 + 
  res_probit_FE$par[5]*datind_2005_2015$year2008 + res_probit_FE$par[6]*datind_2005_2015$year2009 + res_probit_FE$par[7]*datind_2005_2015$year2010 + res_probit_FE$par[8]*datind_2005_2015$year2011 +
  res_probit_FE$par[9]*datind_2005_2015$year2012 + res_probit_FE$par[10]*datind_2005_2015$year2013 + res_probit_FE$par[11]*datind_2005_2015$year2014 + res_probit_FE$par[12]*datind_2005_2015$year2015
ME_Probit    <- mean(dnorm(Xbeta_Probit))*res_probit_FE$par
ME_Probit[2]

Xbeta_Logit  <- res_logit_FE$par[1] + res_logit_FE$par[2]*datind_2005_2015$age + res_logit_FE$par[3]*datind_2005_2015$year2006 + res_logit_FE$par[4]*datind_2005_2015$year2007 + 
  res_logit_FE$par[5]*datind_2005_2015$year2008 + res_logit_FE$par[6]*datind_2005_2015$year2009 + res_logit_FE$par[7]*datind_2005_2015$year2010 + res_logit_FE$par[8]*datind_2005_2015$year2011 +
  res_logit_FE$par[9]*datind_2005_2015$year2012 + res_logit_FE$par[10]*datind_2005_2015$year2013 + res_logit_FE$par[11]*datind_2005_2015$year2014 + res_logit_FE$par[12]*datind_2005_2015$year2015
ME_Logit    <- mean(dlogis(Xbeta_Logit))*res_logit_FE$par
ME_Logit[2]

####Question 2####
# probit
R_probit   <- 20                     
boots_probit <- matrix(rep(0, R_probit*length(res_probit_FE$par)), 
                             nrow=R_probit)
set.seed(12)
for (i in 1:R_probit)
{
  dat_samp_probit   = datind_2005_2015[sample(1:dim(datind_2005_2015)[1], replace=T, dim(datind_2005_2015)[1]),]
  estimator_probit  = optim(int_value_probit_FE, fn = probit_like_FE, method = "BFGS", 
                             age = dat_samp_probit$age, D1 = dat_samp_probit$year2006,
                             D2 = dat_samp_probit$year2007, D3 = dat_samp_probit$year2008,
                             D4 = dat_samp_probit$year2009, D5 = dat_samp_probit$year2010,
                             D6 = dat_samp_probit$year2011, D7 = dat_samp_probit$year2012,
                             D8 = dat_samp_probit$year2013, D9 = dat_samp_probit$year2014,
                             D10 = dat_samp_probit$year2015, empstat = dat_samp_probit$employ)
  Xbeta_samp_probit  = estimator_probit$par[1] + estimator_probit$par[2]*dat_samp_probit$age + estimator_probit$par[3]*dat_samp_probit$year2006 + estimator_probit$par[4]*dat_samp_probit$year2007 + 
                       estimator_probit$par[5]*dat_samp_probit$year2008 + estimator_probit$par[6]*dat_samp_probit$year2009 + estimator_probit$par[7]*dat_samp_probit$year2010 + estimator_probit$par[8]*dat_samp_probit$year2011 +
                       estimator_probit$par[9]*dat_samp_probit$year2012 + estimator_probit$par[10]*dat_samp_probit$year2013 + estimator_probit$par[11]*dat_samp_probit$year2014 + estimator_probit$par[12]*dat_samp_probit$year2015
  pdf_probit         = mean(dnorm(Xbeta_samp_probit))
  boots_probit[i,] = pdf_probit * estimator_probit$par
}
res_probit_se <- cbind(ME_Probit,apply(boots_probit,2,sd))
colnames(res_probit_se) <- c("marginal_effect","standard_error") 
res_probit_se[2,2]

# logit
R_logit   <- 20;                      
boots_logit <- matrix(rep(0, R_logit*length(res_logit_FE$par)), 
                      nrow=R_logit)
set.seed(13)
for (i in 1:R_logit)
{
  dat_samp_logit    = datind_2005_2015[sample(1:dim(datind_2005_2015)[1], replace=T, dim(datind_2005_2015)[1]),]
  estimator_logit   = optim(int_value_logit_FE, fn = logit_like_FE, method = "BFGS", 
                             age = dat_samp_logit$age, D1 = dat_samp_logit$year2006,
                             D2 = dat_samp_logit$year2007, D3 = dat_samp_logit$year2008,
                             D4 = dat_samp_logit$year2009, D5 = dat_samp_logit$year2010,
                             D6 = dat_samp_logit$year2011, D7 = dat_samp_logit$year2012,
                             D8 = dat_samp_logit$year2013, D9 = dat_samp_logit$year2014,
                             D10 = dat_samp_logit$year2015, empstat = dat_samp_logit$employ)
  Xbeta_samp_logit = estimator_logit$par[1] + estimator_logit$par[2]*dat_samp_logit$age + estimator_logit$par[3]*dat_samp_logit$year2006 + estimator_logit$par[4]*dat_samp_logit$year2007 + 
                     estimator_logit$par[5]*dat_samp_logit$year2008 + estimator_logit$par[6]*dat_samp_logit$year2009 + estimator_logit$par[7]*dat_samp_logit$year2010 + estimator_logit$par[8]*dat_samp_logit$year2011 +
                     estimator_logit$par[9]*dat_samp_logit$year2012 + estimator_logit$par[10]*dat_samp_logit$year2013 + estimator_logit$par[11]*dat_samp_logit$year2014 + estimator_logit$par[12]*dat_samp_logit$year2015
  pdf_logit = mean(dlogis(Xbeta_samp_logit))
  boots_logit[i,] = pdf_logit * estimator_logit$par
}
res_logit_se <- cbind(ME_Logit ,apply(boots_logit,2,sd))
colnames(res_logit_se) <- c("marginal_effect","standard_error") 
res_logit_se[2,2]