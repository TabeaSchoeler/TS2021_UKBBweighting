#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
HOME=args[1]
source(paste0(HOME, "/analysis/input.R"))
source(paste0(HOME, "/analysis/functions.R"))


con <- file(paste0(HOME, "/output/log/census_prep.log"))
sink(con, append=TRUE)
sink(con, append=TRUE, type="output")


library(haven)
census=sjlabelled::read_spss(paste0(HOME, "/data/census/recodev12.sav"))
levels(as.factor(census$hlqupuk11))

print("Select variables")
library("readxl")
drop_auth(rdstoken = paste0(HOME, "/token.rds"))  # authentication for dropbox
drop_download(
  local_path = paste0(HOME,"/data/variableList.xlsx"),
  path = paste0(LOCAL,"/data/variableList.xlsx"), overwrite=TRUE)

print("Columns to include for proposensity score derivation")
variableList <- readxl::read_excel(paste0(HOME, "/data/variableList.xlsx"))
colDesc=subset(variableList, includePrediction=="yes")$recodedName
censusInc=subset(variableList, is.na(variableList$censusDat)==F)
censusInc$censusDat
censusInc$recodedName

print("Read in data")
UKBBall_raw=readRDS(paste0(HOME, "/results/rds/UKBBsel.rds"))
UKBBall_ageExcluded=subset(UKBBall_raw, age >=40 & age  <=69)
UKBBall_scotExcluded=subset(UKBBall_ageExcluded, assessment_center != 11004 & assessment_center != 11005 & assessment_center !=  11003 & assessment_center !=  11022 & assessment_center !=  11023) 
UKBBall_europ=subset(UKBBall_scotExcluded, ethnic_background == 1 | ethnic_background == 1001 | ethnic_background == 1002 | ethnic_background == 1003)
excludedPP <- read.csv(paste0(UKBB, "/org/w16389_20220222.csv"), header = FALSE)
UKBBall=subset(UKBBall_europ, !eid %in% unique(  excludedPP$V1))

# recode 
censusSel=selectCensus(census, censusInc)
censusAge=subset(censusSel, age %in% c("40-44",  "45-49",  "50-54",  "55-59",  "60-64",  "65-69"))
censusCountry=subset(censusAge, country=="England")
# The Office for National Statistics (ONS) is responsible for carrying out the census in England and Wales. Simultaneous but separate censuses took place in Scotland and Northern Ireland. 
censusAncestry=subset(censusCountry, ancestry == "White: Irish" | ancestry == "White:English/Welsh/Scottish/N Irish/British" | ancestry == "White: Other White")
censusAncestry$country=NULL
censusAncestry$ancestry=NULL

# Read in HSE
HSEall=readRDS(paste0(HOME, "/data/HSE/HSEsel.rds"))

HSE=data.frame(sampleName=rep("HSE", NROW(HSEall)), sample=1, weight_individual=as.numeric(HSEall$weight_individual )  )
CEN=data.frame( sampleName=rep("CEN", NROW(censusAncestry)), sample=0, weight_individual=rep(1, NROW(censusAncestry)) )
UKBB=data.frame(sampleName=rep("UKBB", NROW(UKBBall)), sample=1, weight_individual=as.numeric(UKBBall$weight_individual ) )

i=3
print("Recode variables")
for ( i in 1:NROW(colnames(censusAncestry)) ) {
       varName=colnames(censusAncestry)[i]
       
       if(censusInc$needRecoding[i]=="no"){
         print("no recoding required")
          CEN=cbind(CEN, addedVar=censusAncestry[[varName]])
          names(CEN)[names(CEN) == "addedVar"] <- varName

          HSE=cbind(HSE, addedVar=HSEall[[varName]])
          names(HSE)[names(HSE) == "addedVar"] <- varName

          UKBB=cbind(UKBB, addedVar=UKBBall[[varName]])
          names(UKBB)[names(UKBB) == "addedVar"] <- varName
       } else{
       print("Recoding required")
             CEN=cbind(CEN, recodeVar(data=censusAncestry, pheno=varName, sample="CEN"))
             HSE=cbind(HSE, recodeVar(data=HSEall, pheno=varName, sample="HSE"))
             UKBB=cbind(UKBB, recodeVar(data=UKBBall, pheno=varName, sample="UKBB"))
       }

  }


# 10 = No academic or professional qualifications
# 11= Level 1 (0-4 GCSE, O level, or equivalents)
# 12 = Level 2 (5+ GCSE, O level, 1 A level, or equivalents)
# 13 = Apprenticeship
# 14 = Level 3 (2+ A levels, or equivalents
# 15 = Level 4+ (degree, postgrad, professional quals)
# 16 = Other (vocational/foreign/outside UK quals)
head(CEN$education_age)

CEN$education=CEN$education_age
head(CEN$education)
CEN$education_age=ifelse(CEN$education==  "Other (vocational/foreign/outside UK quals)",  15, NA) #hse 2010: Qualification: Degree/degree level qualification (incl higher degree) (mentioned vs not mentioned). Other HSE: Which of the qualifications on this card do you have?  Degree/degree level qualification (including higher degree)
CEN$education_age=ifelse(CEN$education==  "Level 4+ (degree, postgrad, professional quals)",  19, CEN$education_age) #hse 2010: Qualification: Degree/degree level qualification (incl higher degree) (mentioned vs not mentioned). Other HSE: Which of the qualifications on this card do you have?  Degree/degree level qualification (including higher degree)
CEN$education_age=ifelse(CEN$education==  "Level 2 (5+ GCSE, O level, 1 A level, or equivalents)",  16, CEN$education_age) #hse 2010: Qualification: Degree/degree level qualification (incl higher degree) (mentioned vs not mentioned). Other HSE: Which of the qualifications on this card do you have?  Degree/degree level qualification (including higher degree)
CEN$education_age=ifelse(CEN$education==  "Apprenticeship",  17, CEN$education_age) #hse 2010: Qualification: Degree/degree level qualification (incl higher degree) (mentioned vs not mentioned). Other HSE: Which of the qualifications on this card do you have?  Degree/degree level qualification (including higher degree)
CEN$education_age=ifelse(CEN$education==  "Level 1 (0-4 GCSE, O level, or equivalents)",  15, CEN$education_age) #hse 2010: Qualification: Degree/degree level qualification (incl higher degree) (mentioned vs not mentioned). Other HSE: Which of the qualifications on this card do you have?  Degree/degree level qualification (including higher degree)
CEN$education_age=ifelse(CEN$education==  "No academic or professional qualifications",  14, CEN$education_age) # 14 or under [7 for no degree (assuming primary school)]
CEN$education_age=ifelse(CEN$education==  "Level 3 (2+ A levels, or equivalents)",  17, CEN$education_age) # 14 or under [7 for no degree (assuming primary school)]
CEN$education_age=as.numeric(CEN$education_age)
mean(CEN$education_age)

CEN$age=as.factor(censusAncestry$age)
CEN$age <- droplevels(CEN$age)

levels(CEN$age)
CENestL=lapply(colnames(censusAncestry), 
                     function(x) weightedEstimates(df=CEN, var=x, wt="weight_individual"))
CENest=do.call(rbind, CENestL)        
CENest=subset(CENest, select=c(cat, level, unweighted_estimate))      
colnames(CENest)=paste0(colnames(CENest), "_cen")
CENest$ID=paste0(CENest$cat, "_", CENest$level)
CENest$cat_cen=NULL;CENest$level_cen=NULL



# Coding REF
# REF: Reweighting the UK Biobank to reflect its underlying sampling population substantially reduces pervasive selection bias due to volunteering



UKBB$age=recodeAge(UKBB$age)
UKBBestL=lapply(colnames(censusAncestry), 
                     function(x) weightedEstimates(df=UKBB, var=x, wt="weight_individual"))
UKBBest=do.call(rbind, UKBBestL)  
UKBBest=subset(UKBBest, select=c(cat, level, unweighted_estimate))      
colnames(UKBBest)=paste0(colnames(UKBBest), "_ukbb")
UKBBest$ID=paste0(UKBBest$cat, "_", UKBBest$level)
UKBBest$cat_ukbb=NULL;UKBBest$level_ukbb=NULL

HSE$age=recodeAge(HSE$age)
HSEestL=lapply(colnames(censusAncestry), 
                     function(x) weightedEstimates(df=HSE, var=x, wt="weight_individual"))
HSEest=do.call(rbind, HSEestL)   
HSEest=subset(HSEest, select=c(cat, level, weighted_estimate))    
colnames(HSEest)=paste0(colnames(HSEest), "_hse")
HSEest$ID=paste0(HSEest$cat, "_", HSEest$level)


estCOmb=merge(HSEest, CENest, by="ID", all=T, sort=F)
estCOmbUKBB=merge(estCOmb, UKBBest, by="ID", all=T, sort=F)
estCOmbUKBB$ID=NULL

saveRDS(estCOmbUKBB , paste0(HOME,"/results/rds/censusCheck.rds"))
uploadDropbox(file="censusCheck.rds", folder="rds")



infoCensus=paste0("Available in census: ", NROW(census), ", remove individuals outside of age range: ", NROW(census)-NROW(censusAge), "' remove individuals not residing in England: ", NROW(censusAge)-NROW(censusCountry), ", removing non-European ancestry: ", NROW(censusCountry)-NROW(censusAncestry), "; Final estimate: out of n=", NROW(censusAncestry), "we included n=",NROW(censusAncestry), " (" ,(NROW(censusAncestry)/NROW(census))*100, ") were included" )
saveRDS(infoCensus , paste0(HOME,"/results/rds/infoCensus.rds"))
uploadDropbox(file="infoCensus.rds", folder="rds")



levels(censusAncestry$overallhealth)
prop.table(table(censusAncestry$sex))
prop.table(table(censusAncestry$employment_status))
prop.table(table(censusAncestry$long_standing_illness))
prop.table(table(censusAncestry$overallhealth))


# Correlations between variables
library(jtools )
library(survey )
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# HSE
hsedesign <- svydesign(ids=~1,   weights = ~weight_individual, data=HSE)
fcor=as.formula(paste0("~", paste0("as.numeric(", censusInc$recodedName, ")") , collapse=" + "))
corHSE=svycor(fcor, design = hsedesign, na.rm = T)$cors
colnames(corHSE)=censusInc$recodedName
rownames(corHSE)=censusInc$recodedName
corHSE_tri <- get_lower_tri(corHSE)
corHSEDF=reshape2::melt(as.matrix(corHSE_tri), na.rm = TRUE)

# UKBB
ukbbdesign <- svydesign(ids=~1,   weights = ~1, data=UKBB)
corUKBB=svycor(fcor, design = ukbbdesign, na.rm = T)$cors
colnames(corUKBB)=censusInc$recodedName
rownames(corUKBB)=censusInc$recodedName
corUKBB_tri <- get_lower_tri(corUKBB)
corUKBBDF=reshape2::melt(as.matrix(corUKBB_tri), na.rm = TRUE)

# CEN
cendesign <- svydesign(ids=~1,   weights = ~1, data=CEN)
corCEN=svycor(fcor, design = cendesign, na.rm = T)$cors
colnames(corCEN)=censusInc$recodedName
rownames(corCEN)=censusInc$recodedName
corCEN_tri <- get_lower_tri(corCEN)
corCENDF=reshape2::melt(as.matrix(corCEN_tri), na.rm = TRUE)


corExternalCensus=data.frame(p1=corCENDF$Var1, p2=corCENDF$Var2, corCEN=corCENDF$value, corHSE=corHSEDF$value, corUKBB=corUKBBDF$value)
saveRDS(corExternalCensus , paste0(HOME,"/results/rds/corExternalCensus.rds"))
uploadDropbox(file="corExternalCensus.rds", folder="rds")



print("All done!")

uploadLog(file=paste0("UKBB_HSE_CENSUS.log"))