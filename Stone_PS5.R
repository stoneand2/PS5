# Andy Stone
# Problem Set 5
# March 10, 2016

# Thanks and credit to Jonathan for the starter code

# Setting working directory
setwd("~/github/PS5")

# Packages to be utilized 
library(foreign); library(mice)

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

## Question 1
## randomly subset the data into two partitions
## use "training set" to build at least three models 
## of Obama's feeling thermometer score
## document carefully how you deal with missingness

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

mice.training.set <- mice(training.set[,variables.to.use], 5)


# Model 1: A story of party identification

model.1 <- lm.mids(ft_dpc ~ pid_self + dem_hisp + dem_racecps_black, data=mice.training.set)
summary(pool(model.1))[,1:2]


# Model 2: A story of past voting behavior 
model.2 <- lm.mids(ft_dpc ~ interest_whovote2008 + dem_hisp + dem_racecps_black, 
                   data=mice.training.set)
summary(pool(model.2))[,1:2]

# Model 3: A story of perceptions of Obama's first term in office
training.set$presadm_secure
training.set$econ_ecpast
training.set$ecblame_pres






