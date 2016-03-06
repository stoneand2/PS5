# Andy Stone
# Problem Set 5
# March 10, 2016

# Thanks and credit to Jonathan for the starter code

# Setting working directory
setwd("~/github/PS5")

# Packages to be utilized 
library(foreign); library(mice); library(arm)

# Setting seed so everyone randomizes similarly
set.seed(12435)
# For reading in the data
options(stringsAsFactors=F)

# read in data
anes <- read.dta("anes_timeseries_2012_stata12.dta")

## model Obama's feeling thermometer score as function
## of Clinton's feeling thermometer score
model1 <- lm(ft_dpc ~ ft_hclinton, anes)

## make a prediction for a single observation with
## hypothetical clinton score of 77
predict(model1, data.frame(ft_hclinton=77))
## we would expect a Obama score of 71.7

# Question 1: Partitioning data and fitting models of Obama's feeling thermometer score

# Setting the default ANES values for "Don't know", "Refused", "Other...", etc., to either NAs or
# an "Other" category for my variables of interest
# These codings differ by variable, so I couldn't do this all with an apply function

# Recoding the feeling thermometor to remove the negative codings and replace them with NAs
anes$ft_dpc <- ifelse(anes$ft_dpc == -8 | anes$ft_dpc == -9 | anes$ft_dpc == -2, NA, anes$ft_dpc)

# Recoding the PID variable to be either Democrat, Republican, Neither, or NAs
anes$pid_self <- as.factor(ifelse(anes$pid_self == "5. Other party {SPECIFY}" | anes$pid_self == "3. Independent" 
        | anes$pid_self == "0. No preference {VOL}", "Neither", ifelse(anes$pid_self == "1. Democrat", "Democrat",
        ifelse(anes$pid_self == "2. Republican", "Republican", NA))))

# Recoding the Hispanic variable to be either Hispanic or Non-Hispanic
anes$dem_hisp <- as.factor(ifelse(anes$dem_hisp == "-9. Refused" | anes$dem_hisp == "-8. Don't know", NA, ifelse(
  anes$dem_hisp == "1. Yes","Hispanic","Non-Hispanic")))

# Recoding the Black variable to be either Black or Non-Black
anes$dem_racecps_black <- as.factor(ifelse(anes$dem_racecps_black == "0. Not selected by R", "Not Black","Black"))

# Recoding the "who vote in 2008" variable to be Obama/McCain/Other/NA
anes$interest_whovote2008 <- as.factor(ifelse(anes$interest_whovote2008 == "1. Barack obama","Obama",
                                       ifelse(anes$interest_whovote2008 == "2. John mccain","McCain",
                                       ifelse(anes$interest_whovote2008 == "5. Other {SPECIFY}", "Other", 
                                       ifelse(anes$interest_whovote2008 == "-1. Inapplicable", "Didn't Vote", 
                                       NA)))))

# Recoding U.S. more/less secure since Obama took office
anes$presadm_secure <- as.factor(ifelse(anes$presadm_secure == "1. More secure","More Secure",
                                 ifelse(anes$presadm_secure == "2. Less secure","Less Secure",
                                 ifelse(anes$presadm_secure == "3. No change", "No Change",
                                 NA))))

# Recoding U.S. better/worse economy over past year
anes$econ_ecpast <- as.factor(ifelse(anes$econ_ecpast == "1. Gotten better","Better",
                                 ifelse(anes$econ_ecpast == "3. Gotten worse","Worse",
                                 ifelse(anes$econ_ecpast == "2. Stayed about the same", "Same",
                                 NA))))

# Recoding blame of Obama for poor economic conditions into high/some/no blame categories
anes$ecblame_pres <- as.factor(ifelse(anes$ecblame_pres == "1. A great deal" | anes$ecblame_pres == "2. A lot" ,"High Blame",
                               ifelse(anes$ecblame_pres == "3. A moderate amount" | anes$ecblame_pres == "4. A little","Some Blame",
                               ifelse(anes$ecblame_pres == "5. Not at all", "No Blame",
                               NA))))

# Making Obama's feeling therm. a proportion

# I do this because the variable is bounded between 0 and 100. We can treat this as bounded betweeen
# 0 and 1 (a proportion). Therefore, we should fit our models below using a functional form 
# appropriate for binary dependent variables (i.e., not linear models)

# Below, I will fit both linear and quasibinomial logistic regression models
# The linear models will make the substantive interpretation of our models straightforward, but the
# functional form of the logistic models ensures we obtain proportions bounded between 0 and 1
anes$ft_dpc <- anes$ft_dpc * 0.01

# Randomly subsetting data into partitions
# Getting number of observations
n.obs <- dim(anes)[1]
# Sampling indices to subset by
indices <- sample(x=1:n.obs, size=n.obs/2, replace=F)
# Creating training set using these indices
training.set <- anes[indices,]
# And creating test set
test.set <- anes[-indices,]

# Using training set to build 3 models of Obama's feeling thermometer score
# I will handle all missingness using multiple imputation

# Mice data
variables.to.use <- c("ft_dpc","pid_self","dem_edugroup_x","dem_hisp","dem_racecps_black",
                      "dem_parents","interest_whovote2008","presadm_secure","econ_ecpast",
                      "ecblame_pres")
# Multiple imputation to impute the NAs. m=5 multiple imputations (5 separate imputed datasets)
mice.training.set <- mice(training.set[,variables.to.use], 5)


# Model 1: A story of party identification

model.1 <- lm.mids(ft_dpc ~ pid_self + dem_hisp + dem_racecps_black, data=mice.training.set)
model.1.logit <- glm.mids(ft_dpc ~ pid_self + dem_hisp + dem_racecps_black, 
                         data=mice.training.set, family = quasibinomial(link = "logit"))
summary(pool(model.1))[,1:2]
summary(pool(model.1.logit))[,1:2]

# Model 2: A story of past voting behavior 

model.2 <- lm.mids(ft_dpc ~ interest_whovote2008 + dem_hisp + dem_racecps_black, 
                   data=mice.training.set)
model.2.logit <- glm.mids(ft_dpc ~ interest_whovote2008 + dem_hisp + dem_racecps_black, 
                          data=mice.training.set, family = quasibinomial(link = "logit"))
summary(pool(model.2))[,1:2]
summary(pool(model.2.logit))[,1:2]

# Model 3: A story of perceptions of Obama's first term in office

model.3 <- lm.mids(ft_dpc ~ presadm_secure + econ_ecpast + ecblame_pres + dem_hisp + 
                     dem_racecps_black, data=mice.training.set)
model.3.logit <- glm.mids(ft_dpc ~ presadm_secure + econ_ecpast + ecblame_pres + dem_hisp + 
                    dem_racecps_black, data=mice.training.set, family = quasibinomial(link = "logit"))
summary(pool(model.3))[,1:2]
summary(pool(model.3.logit))[,1:2]


# Question 2: Making predictions

# predict() can't handle an entire mice object. So, I feed it the first of the mice iterations

# Model 1
m1.predicted <- predict(model.1$analyses[[1]], test.set)
m1.logit.predicted <- invlogit(predict(model.1.logit$analyses[[1]], test.set))

# Model 2
m2.predicted <- predict(model.2$analyses[[1]], test.set)
m2.logit.predicted <- predict(model.2.logit$analyses[[1]], test.set)

# Model 3
m3.predicted <- predict(model.3$analyses[[1]], test.set)
m3.logit.predicted <- predict(model.3.logit$analyses[[1]], test.set)



# Questions 3 and 4: Function to take true outcomes and matrix of predictions, return matrix of fit 
# statistics by model. Allowing user to choose which of 5 fit statistics are calculated

Measures_of_Fit <- function(true.ys=c(), prediction.matrix=matrix(), RMSE=T, MAD=T, RMSLE=T, MAPE=T, 
                            MEAPE=T){
  if(class(true.ys) != "numeric" & class(true.ys) !="integer"){
    stop("Your true.ys values are invalid. Please pass the function a vector of observed Y values in numeric format.")
  }
  if(class(prediction.matrix) != "matrix"){
    stop("Your prediciton.matrix is invalid. Please pass the function a matrix of predictions.")
  }
  if(dim(prediction.matrix)[1] != length(true.ys)){
    stop("Each column of your prediction matrix should correspond to a prediction for each of 
          the values of Y specified in the true.ys vector. Ensure the number of rows in your 
          prediction matrix equals the number of Y observations.")
  }
  
  fit.statistics <- matrix(data=NA, nrow=dim(prediction.matrix)[2])
  rownames(fit.statistics) <- sapply(1:dim(prediction.matrix)[2], FUN=function(i)paste("Model",i))
  
  # Function for calculating RMSE
  RMSE_Function <- function(i){
    sqrt(mean(abs(prediction.matrix[,i] - true.ys)^2))
  }
  
  # Function for calculating MAD
  MAD_Function <- function(i){
    median(abs(prediction.matrix[,i] - true.ys))
  }
  
  # Function for calculating root mean squared log error (RMSLE)
  RMSLE_Function <- function(i){
    sqrt(mean((log(prediction.matrix[,i] + 1) - log(true.ys + 1))^2))
  }
  
  # Function for calculating mean absolute percentage error (MAPE)
  MAPE_Function <- function(i){
    (sum((abs(prediction.matrix[,i] - true.ys) / abs(true.ys))* 100) / length(true.ys))
  }
  
  # Function for calculating MEAPE
  MEAPE_Function <- function(i){
    median((abs(prediction.matrix[,i] - true.ys) / abs(true.ys)) * 100)
  }
  
  if(RMSE==T){
    fit.statistics <- cbind(fit.statistics,sapply(1:dim(prediction.matrix)[2], FUN=RMSE_Function))
    colnames(fit.statistics)[dim(fit.statistics)[2]] <- "RMSE"
  }
  
  if(MAD==T){
    fit.statistics <- cbind(fit.statistics,sapply(1:dim(prediction.matrix)[2], FUN=MAD_Function))
    colnames(fit.statistics)[dim(fit.statistics)[2]] <- "MAD"
  }
  
  if(RMSLE==T){
    fit.statistics <- cbind(fit.statistics,sapply(1:dim(prediction.matrix)[2], FUN=RMSLE_Function))
    colnames(fit.statistics)[dim(fit.statistics)[2]] <- "RMSLE"
  }
  
  if(MAPE==T){
    fit.statistics <- cbind(fit.statistics,sapply(1:dim(prediction.matrix)[2], FUN=MAPE_Function))
    colnames(fit.statistics)[dim(fit.statistics)[2]] <- "MAPE"
  }
  
  if(MEAPE==T){
    fit.statistics <- cbind(fit.statistics,sapply(1:dim(prediction.matrix)[2], FUN=MEAPE_Function))
    colnames(fit.statistics)[dim(fit.statistics)[2]] <- "MEAPE"
  }
  
  fit.statistics <- fit.statistics[,-1]
  return(fit.statistics)
  
}

practice.ys <- sample(1:10, 10, replace=T)
practice.mat <- matrix(sample(1:10, 40, replace=T), nrow=10)

Measures_of_Fit(practice.ys, practice.mat)






