#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
HOME=args[1]

source(paste0(HOME, "/analysis/input.R"))
source(paste0(HOME, "/analysis/functions.R"))
UKBBdir=UKBB

con <- file(paste0(HOME, "/output/log/UKBB_HSE_prep.log"))
sink(con, append=TRUE, type="output")


print("Load list of files to be included in GWA")
drop_auth(rdstoken = paste0(HOME, "/token.rds"))  # authentication for dropbox
drop_download(
  local_path = paste0(HOME,"/data/variableList.xlsx"),
  path = paste0(LOCAL,"/data/variableList.xlsx"), overwrite=TRUE)

library("readxl")
print("Columns to include for proposensity score derivation")
variableList <- readxl::read_excel(paste0(HOME, "/data/variableList.xlsx"))
colDesc=subset(variableList, includePrediction=="yes")$recodedName
varInc=subset(variableList, include=="yes")

print("Read in data")
UKBBall_raw=readRDS(paste0(HOME, "/results/rds/UKBBsel.rds"))
summary(UKBBall_raw$age)
summary(UKBBall_raw$household_size)
summary(UKBBall_raw$bmi)
summary(UKBBall_raw$weight)
table(UKBBall_raw$sex)

UKBBall_ageExcluded=subset(UKBBall_raw, age >=40 & age  <=69)
summary(UKBBall_raw$ethnic_background) # -1: Do not know | -3: Prefer not to answer | 1: White | 1001: British | 1002: Irish | 1003: Any other white background | 2: Mixed | 2001: White and Black Caribbean | 2002: White and Black African | 2003: White and Asian | 2004: Any other mixed background | 3: Asian or Asian British | 3001: Indian | 3002: Pakistani | 3003: Bangladeshi | 3004: Any other Asian background | 4: Black or Black British | 4001: Caribbean | 4002: African | 4003: Any other Black background | 5: Chinese | 6: Other ethnic group | 
# include only european ancestry, exlcude irish

print("Exclude individuals from Glasgow and Edinburgh")
UKBBall_scotExcluded=subset(UKBBall_ageExcluded, assessment_center != 11004 & assessment_center != 11005 & assessment_center !=  11003 & assessment_center !=  11022 & assessment_center !=  11023) 
# 11004: Glasgow, 11005: Edinburgh , 11003: Cardiff, 11022: Swansea,  11023: Wrexham 
CrossTable(UKBBall_scotExcluded$assessment_center)

# | 1: White | 1001: British | 1002: Irish | 1003: Any other white background 
UKBBall_europ=subset(UKBBall_scotExcluded, ethnic_background == 1 | ethnic_background == 1001 | ethnic_background == 1002 | ethnic_background == 1003)

# exclude people withdrawing from UK BB
# There are additional sample withdrawals in the UKBB. The list of withdrawn participants are in the w16389_20210201.csv file in the UKBB/org folder, both on JURA and HPC. Keep using your current exclusions for projects that started before this date and use the new exclusion for projects only starting now.
excludedPP <- read.csv(paste0(UKBB, "/org/w16389_20220222.csv"), header = FALSE)
UKBBall=subset(UKBBall_europ, !eid %in% unique(  excludedPP$V1))

print("Check missing data for UKBB")
checkMIss=lapply(colDesc, function(x) checkMissDat(x, data=UKBBall))

print("Read in HSE data")
HSEall=readRDS(paste0(HOME, "/data/HSE/HSEsel.rds"))
HSEall$eid=seq(1:NROW(HSEall))



print("Check missing data for HSE")
checkMIss=lapply(colDesc, function(x) checkMissDat(x, data=HSEall))
table(is.na(HSEall$income))

print("Columns to include only from UKBB")
varUKBBonlyAll=subset(variableList, include=="yes")$recodedName
varUKBBonlyAll = varUKBBonlyAll[!varUKBBonlyAll %in% colDesc]

print("Relabel variables")
UKBB=data.frame(eid=UKBBall$eid, sampleName=rep("UKBB", NROW(UKBBall)), sample=1, weight_individual=as.numeric(UKBBall$weight_individual ) )
HSE=data.frame(eid=HSEall$eid, sampleName=rep("HSE", NROW(HSEall)), sample=0, weight_individual=as.numeric(HSEall$weight_individual )  )

print("Recode variables")
for ( i in 1:NROW(varInc) ) {
       varName=varInc$recodedName[i]
              if(varInc$includePrediction[i]=="yes"){
              print(paste0("Included in prediction model ", varName))
                     if(varInc$needRecoding[i]=="no"){
                     print("no recoding required")
                     UKBB=cbind(UKBB, addedVar=UKBBall[[varName]])
                     names(UKBB)[names(UKBB) == "addedVar"] <- varName

                     HSE=cbind(HSE, addedVar=HSEall[[varName]])
                     names(HSE)[names(HSE) == "addedVar"] <- varName

                     if(varInc$levelsUKBB[i]=="continuous" ){
                            print("continuous variable")
                            UKBB[[varName]]=as.numeric(UKBB[[varName]])
                            HSE[[varName]]=as.numeric(HSE[[varName]])
                     } 
                     }
                     if(varInc$needRecoding[i]=="yes"){
                     print("Recoding required")
                     UKBB=cbind(UKBB, recodeVar(data=UKBBall, pheno=varName, sample="UKBB"))
                     HSE=cbind(HSE, recodeVar(data=HSEall, pheno=varName, sample="HSE"))
                     }
              }
              
              if(varInc$includePrediction[i]=="no"){
              print(paste0("Not included in prediction model ", varName))   
                     if(varInc$needRecoding[i]=="no"){
                     print("no recoding required")
                     UKBB=cbind(UKBB, addedVar=UKBBall[[varName]])
                     names(UKBB)[names(UKBB) == "addedVar"] <- varName
                            if(varInc$levelsUKBB[i]=="continuous" ){
                                   print("continuous variable")
                                   UKBB[[varName]]=as.numeric(UKBB[[varName]])
                            } 
                     }
                     if(varInc$needRecoding[i]=="yes"){
                     print("Recoding required")
                     UKBB=cbind(UKBB, recodeUKBB(data=UKBBall, pheno=varName))
                     }
              

       }

}
summary(HSE$age)
summary(HSE$weight)

saveRDS(UKBB , paste0(HOME,"/results/rds/UKBBall.rds"))
saveRDS(HSE , paste0(HOME,"/results/rds/HSEall.rds"))

print("Extract number of excluded individuals")
exclusion0=data.frame(navail=NROW(UKBBall_raw), text=paste0(NROW(UKBBall_raw), " UKBB data accessed"))
exclusion1=data.frame(navail=NROW(UKBBall_ageExcluded), text=paste0(NROW(UKBBall_raw)-NROW(UKBBall_ageExcluded), " exlcuded as younger than 40 or older than 69"))
exclusion2=data.frame(navail=NROW(UKBBall_scotExcluded), text=paste0(NROW(UKBBall_ageExcluded)-NROW(UKBBall_scotExcluded), " exlcuded as from Scotland/Wale"))
exclusion3=data.frame(navail=NROW(UKBBall_europ), text=paste0(NROW(UKBBall_scotExcluded)-NROW(UKBBall_europ), " exlcuded as non-european ancestry (self-reported)"))
exclusion4=data.frame(navail=NROW(UKBBall), text=paste0(NROW(UKBBall_europ)-NROW(UKBBall), " exlcuded as withdrewn consent"))
sumExclusion=rbind(exclusion0, exclusion1, exclusion2, exclusion3, exclusion4)
saveRDS(sumExclusion , paste0(HOME,"/results/rds/sumExclusion1.rds"))


HSEcheck=cbind(HSEall$year, HSE)
HSEcheck=HSE
HSEcheck$year=HSEall$year
descHSEList=lapply(levels(as.factor(HSEcheck$year)), function(x) checkVar(x, data=HSEcheck))
descHSEDF=do.call(rbind, descHSEList)
saveRDS(descHSEDF , paste0(HOME,"/results/rds/descHSEDF.rds"))
uploadDropbox(file="descHSEDF.rds", folder="rds")

print("Check missing data")
missingDat=data.frame(variable=colnames(UKBB),
           missing=do.call(rbind, lapply(colnames(UKBB), function(x) sum(is.na(UKBB[[x]]))))) 

saveRDS(missingDat , paste0(HOME,"/results/rds/missingDat.rds"))
uploadDropbox(file="missingDat.rds", folder="rds")

print("Remove variables with too much missing data")
colDescIncludedPropensity=colDesc[colDesc %in% subset(missingDat, missing<=50000)$variable]
saveRDS(colDescIncludedPropensity, paste0(HOME, "/results/rds/weightVariables.rds"))

print("Select columns")
varsIncModel=c("eid","sample", "sampleName", "weight_individual",colDescIncludedPropensity)
UKBB_inc=subset(UKBB, select=varsIncModel )
HSE_inc=subset(HSE, select=varsIncModel )

print("Normalize weights for HSE")
HSE_inc$weight_individual=normalizeWeights(HSE_inc$weight_individual)
UKBBHSEall=rbind( HSE_inc, UKBB_inc )

print("remove data with missing values")
UKBBHSE <- UKBBHSEall[complete.cases(UKBBHSEall), ]
print(paste0("Number of removed individuals: ", NROW(UKBBHSEall)-NROW(UKBBHSE)) )
saveRDS(UKBBHSE , paste0(HOME,"/results/rds/UKBBHSE.rds"))

set.seed(1234)
UKBBHSE_CV=createCV(UKBBHSE)
names(UKBBHSE_CV)=seq(1,5,1)
saveRDS(UKBBHSE_CV , paste0(HOME,"/results/rds/UKBBHSE_CV.rds"))


saveRDS(missingDat , paste0(HOME,"/results/rds/missingDat.rds"))
uploadDropbox(file="missingDat.rds", folder="rds")


print(Sys.time())


print("All done!")
print("All output saved")
uploadLog(file=paste0("/UKBB_HSE_prep.log"))


