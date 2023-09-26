
#setwd("tutorialWeights")

# load (and install libraries)
library('fastDummies')
library("glmnet") # load glmnet

# download and import functions
system("wget https://raw.github.com/TabeaSchoeler/TS2021_UKBBweighting/main/tutorial/functionsWeights.R")
source("functionsWeights.R")

# Download list with variable names
system("wget https://raw.github.com/TabeaSchoeler/TS2021_UKBBweighting/main/data/obtainWeights.xlsx")
varsAll <- as.data.frame(readxl::read_excel("obtainWeights.xlsx", na="NA"))
varsLabel = subset(varsAll, is.na(varsAll$ID)==FALSE)

# Read in raw UKBB data
#UKBBall=readRDS( paste0(HOME, "/data/UKBB/UKBBdf.rds"))
UKBBall=fread("ukbXXXXXXX.csv")

# Select and rename relevant variables
UKBBs=subset(UKBBall, select = c("eid", paste0(varsLabel$ID, "-0.0")))
colnames(UKBBs) = c("eid", varsLabel$label)

# recode variables
UKBBrL=lapply(varsAll$label, function(x) recodeVar(df=UKBBs, var=x, varInfo=varsAll))
UKBBr=Reduce(function(x,y) dplyr::full_join(x = x, y = y, by = "eid", suffix=c("", "") ),  UKBBrL)

# Apply age restriction
UKBBe1=subset(UKBBr, age >=40 & age  <=69)

# Exclude individuals outside England
UKBBe2=subset(UKBBe1, assessment_center != 11004 & assessment_center != 11005 & assessment_center !=  11003 & assessment_center !=  11022 & assessment_center !=  11023) 
# 11004: Glasgow, 11005: Edinburgh , 11003: Cardiff, 11022: Swansea,  11023: Wrexham 

# Include white ethnicity
# | 1: White | 1001: British | 1002: Irish | 1003: Any other white background 
UKBB=subset(UKBBe2, ethnic_background == 1 | ethnic_background == 1001 | ethnic_background == 1002 | ethnic_background == 1003)


# Download and load glmnet output
system("wget https://raw.github.com/TabeaSchoeler/TS2021_UKBBweighting/main/data/cvfit.rda")
load("cvfit.rda")

# Create dummy variables
vars = subset(varsAll, includePrediction=="yes")
UKBBd=createDummy(df=UKBB, varInc=vars)
#UKBBd=sample_n(UKBBd, size=5000)
UKBBc=UKBBd[complete.cases(UKBBd),]


# Download variable names for glmnet model
system("wget https://raw.github.com/TabeaSchoeler/TS2021_UKBBweighting/main/data/varsModel.rds")
varPred=readRDS(paste0(HOME,"/results/rds/varsModel.rds"))

# include all possible two-way interaction terms among the dummy and continuous variables
f <- as.formula( ~ .*.)
xVal <- model.matrix(f, subset(UKBBc, select = varPred))

# get predicted probabilities and sampling weights
UKBBc$probs <- predict(cvfit, newx=xVal, s="lambda.min", type = "response")[,1]
UKBBc$IPSW=(1-UKBBc$probs)/UKBBc$probs # inverse probability weighte
UKBBc$IPSWnorm=UKBBc$IPSW/mean(UKBBc$IPSW) # normalized inverse probability weights


# DONE