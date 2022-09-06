#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
HOME=args[1]
source(paste0(HOME, "/analysis/input.R"))
source(paste0(HOME, "/analysis/functions.R"))

con <- file(paste0(HOME, "/output/log/recodeHSElog.log"))
sink(con, append=TRUE, type="output")



print("Select variables")
library("readxl")
drop_auth(rdstoken = paste0(HOME, "/token.rds"))  # authentication for dropbox
drop_download(
  local_path = paste0(HOME,"/data/variableList.xlsx"),
  path = paste0(LOCAL,"/data/variableList.xlsx"), overwrite=TRUE)

print("Columns to include for proposensity score derivation")
variableList <- readxl::read_excel(paste0(HOME, "/data/variableList.xlsx"))
datAvail=subset(variableList, includePrediction=="yes" | is.na(variableList$censusDat)==F)

colDesc=subset(datAvail, categorizeVar =="no")$recodedName
varInc=subset(datAvail, categorizeVar =="no")

print("Remove old files")
file.remove(paste0(HOME,"/data/HSE/HSEsel.rds"))


print("Read in data")
library(haven)
hse2006all=sjlabelled::read_spss(paste0(HOME, "/data/HSE2006/hse06ai.sav"))
hse2007all=sjlabelled::read_spss(paste0(HOME, "/data/HSE2007/hse07ai.sav"))
hse2008all=sjlabelled::read_spss(paste0(HOME, "/data/HSE2008/hse08ai.sav"))
hse2009all=sjlabelled::read_spss(paste0(HOME, "/data/HSE2009/hse09ai.sav"))
hse2010all=sjlabelled::read_spss(paste0(HOME, "/data/HSE2010/hse10ai.sav"))
 

nAvail=rbind(data.frame(nInc=NROW(hse2006all), year="2006"),
      data.frame(nInc=NROW(hse2007all), year="2007"),
      data.frame(nInc=NROW(hse2008all), year="2008"),
      data.frame(nInc=NROW(hse2009all), year="2009"),
      data.frame(nInc=NROW(hse2010all), year="2010"))

print("Exclude individuals")
hse2006=selectHSE(df=hse2006all, age="age", origin="ethinda", year="2006", vars=varInc)
hse2007=selectHSE(df=hse2007all, age="age", origin="ethinda", year="2007", vars=varInc)
hse2008=selectHSE(df=hse2008all, age="age", origin="origin" , year="2008", vars=varInc)
hse2009=selectHSE(df=hse2009all, age="age", origin="origin", year="2009", vars=varInc)
hse2010=selectHSE(df=hse2010all, age="age", origin="origin", year="2010", vars=varInc)


hse=rbind(hse2006,
          hse2007, 
          hse2008, 
          hse2009,
          hse2010)


descHSEList=lapply(levels(as.factor(hse$year)), function(x) checkVar(x, data=hse))
descHSEDF=do.call(rbind, descHSEList)
descHSEDF=merge(descHSEDF, nAvail, by="year", all.x=T)
descHSEDF$nAvailAllCohorts=NROW(hse2006all)+NROW(hse2007all)+NROW(hse2008all)+NROW(hse2009all)+NROW(hse2010all)
descHSEDF$nIncAllCohorts=NROW(hse2006)+NROW(hse2007)+NROW(hse2008)+NROW(hse2009)+NROW(hse2010)
descHSEDF$nNoMissingAllCohorts=NROW(hse[complete.cases(hse), ])


HSEinclusion=paste0("Total number of HSE individuals: ", descHSEDF$nAvailAllCohorts[1], "; Included individuals after applying UKBB filter: ", descHSEDF$nIncAllCohorts[1], ". Sample without missing data: ", descHSEDF$nNoMissingAllCohorts[1])
saveRDS(HSEinclusion , paste0(HOME,"/results/rds/HSEinclusion.rds"))
uploadDropbox(file="HSEinclusion.rds", folder="rds")

# Urbanisation
hse$urbanisation=revalue(as.factor(hse$urbanisation), c("Hamlet & Isolated Dwelling" ="village_hamlet", 
                                                    "Hamlet and Isolated Dwelling - sparse" = "village_hamlet",  
                                                    "Town & Fringe - less sparse" = "town_fringe",
                                                    "Town & Fringe - sparse"  = "town_fringe",
                                                     "Urban >= 10k - less sparse" = "urban",
                                                      "Urban >= 10k - sparse"  = "urban",
                                                      "Village - less sparse" ="village_hamlet", 
                                                      "Village - sparse" ="village_hamlet", 
                                                      "Town & fringe" = "town_fringe",
                                                      "Urban"  = "urban",
                                                      "Village, hamlet and isolated dwellings" ="village_hamlet"))

hse$urbanisation <- factor(hse$urbanisation, levels = c("village_hamlet", "town_fringe", "urban"))

# Recode
hse$alcfrequency=recodeVar(data=hse, pheno="alcfrequency", sample="HSE")$alcfrequency
hse$alcfrequency_con=as.numeric(hse$alcfrequency)


levels(as_factor(hse$income))
# Income
print("Recode income")
hse$incomeRec=recodeVar(data=hse, pheno="income", sample="HSE")$income
hse$income=hse$incomeRec
print("Check factor levels for income")
print(table(hse$income))

hse$income_con=as.numeric(ifelse(hse$income=="not_shared", NA, as.numeric(hse$income) ))

# Education
hse$education_ageRec=hse$education_age
hse$education_age=ifelse(hse$education_ageRec==  "19 or over",  19, NA) #hse 2010: Qualification: Degree/degree level qualification (incl higher degree) (mentioned vs not mentioned). Other HSE: Which of the qualifications on this card do you have?  Degree/degree level qualification (including higher degree)
hse$education_age=ifelse(hse$education_ageRec==  "18",  18, hse$education_age) #hse 2010: Qualification: Degree/degree level qualification (incl higher degree) (mentioned vs not mentioned). Other HSE: Which of the qualifications on this card do you have?  Degree/degree level qualification (including higher degree)
hse$education_age=ifelse(hse$education_ageRec==  "17",  17, hse$education_age) #hse 2010: Qualification: Degree/degree level qualification (incl higher degree) (mentioned vs not mentioned). Other HSE: Which of the qualifications on this card do you have?  Degree/degree level qualification (including higher degree)
hse$education_age=ifelse(hse$education_ageRec==  "16",  16, hse$education_age) #hse 2010: Qualification: Degree/degree level qualification (incl higher degree) (mentioned vs not mentioned). Other HSE: Which of the qualifications on this card do you have?  Degree/degree level qualification (including higher degree)
hse$education_age=ifelse(hse$education_ageRec==  "15",  15, hse$education_age) #hse 2010: Qualification: Degree/degree level qualification (incl higher degree) (mentioned vs not mentioned). Other HSE: Which of the qualifications on this card do you have?  Degree/degree level qualification (including higher degree)
hse$education_age=ifelse(hse$education_ageRec==  "14 or under",  14, hse$education_age) #hse 2010: Qualification: Degree/degree level qualification (incl higher degree) (mentioned vs not mentioned). Other HSE: Which of the qualifications on this card do you have?  Degree/degree level qualification (including higher degree)

# Bmi
hse$bmi=hse$weight/(hse$height/100)^2


# RECODE CONTINUOUS VARIABLES
hse$age_cat=recodeAge(hse$age)
levels(hse$age_cat)

# Education age: Create categorical variable
hse$education_age_cat=as.factor(hse$education_age)
levels(hse$education_age_cat)

# sex
hse$sex=recodeVar(data=hse, pheno="sex", sample="HSE")$sex
table(hse$sex)

print("Summary household size")
hse$household_sizeCoded=ifelse(as.numeric(hse$household_size)>=7, "7", NA)
hse$household_sizeCoded=ifelse(as.numeric(hse$household_size)<7, as.character(hse$household_size), hse$household_sizeCoded)
hse$household_size=hse$household_sizeCoded
hse$household_size=as.factor(hse$household_size)
print("household levels")
print(table(hse$household_sizeCoded))

print("missing data for household levels")
print(table(is.na(hse$household_sizeCoded)))


checkCat=function(df, checkVar){
  print(checkVar)
  print(table(df[[checkVar]], df[["year"]]))
}

factorCheck=unique(subset(descHSEDF, cat=="prop", select=category))
lapply(factorCheck$category, function(x) checkCat(df=hse, checkVar=x))

print("Check missing data per dummy variable")
checkMIss=lapply(colDesc, function(x) checkMissDat(x, data=hse))

print("Save selected data")
saveRDS(hse, paste0(HOME, "/data/HSE/HSEsel.rds"))



descHSEList=lapply(levels(as.factor(hse$year)), function(x) checkVar(x, data=hse))
descHSEDF=do.call(rbind, descHSEList)
saveRDS(descHSEDF , paste0(HOME,"/results/rds/descHSEDFmissingRaw.rds"))
uploadDropbox(file="descHSEDFmissingRaw.rds", folder="rds")


print("All done !")
print("All done !")
print("All done !")
print("All done !")
print("All done !")

print("All output saved")
uploadLog(file=paste0("recodeHSElog.log"))


