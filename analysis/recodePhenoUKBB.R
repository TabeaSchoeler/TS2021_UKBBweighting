#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
HOME=args[1]
source(paste0(HOME, "/analysis/input.R"))
source(paste0(HOME, "/analysis/functions.R"))
UKBBdir=UKBB

con <- file(paste0(HOME, "/output/log/recodeUKBBlog.log"))
sink(con, append=TRUE, type="output")

print("Read in UKBB pheno data")
UKBBdf=readRDS( paste0(HOME, "/data/UKBB/UKBBdf.rds"))

print("Check number of cases with migrained (should be around 10,647)")
print(NROW(subset(UKBBdf, UKBBdf[["20002-0.0"]]=="1265")))

print("Check number of cases with hypertension (should be around 93,560)")
print(NROW(subset(UKBBdf, UKBBdf[["20002-0.0"]]=="1065")))

print("Check number of cases with anxiety/depression (should be around 42,000)")
print(NROW(subset(UKBBdf, UKBBdf[["2100-0.0"]]=="1")))

print("Remove files to be overwritten")
file.remove(paste0(HOME,"/results/rds/UKBBsel.rds"))

print("Select variables")
library("readxl")
drop_auth(rdstoken = paste0(HOME, "/token.rds"))  # authentication for dropbox
drop_download(
  local_path = paste0(HOME,"/data/variableList.xlsx"),
  path = paste0(LOCAL,"/data/variableList.xlsx"), overwrite=TRUE)
  
print("Columns to include for proposensity score derivation")
variableList <- as.data.frame(readxl::read_excel(paste0(HOME, "/data/variableList.xlsx")))
varInc=subset(variableList, labelUKBB_raw!="NA")

print("Select variables")
UKBBsel=subset(UKBBdf, select=paste0(unique(varInc$labelUKBB_raw), "-0.0"))
colnames(UKBBsel)=varInc$recodedName
UKBBsel$eid=UKBBdf$eid

print("Extract variable info")
dataDic=fread(paste0(HOME, "/data/UKBB/Data_Dictionary_Showcase.csv"))
dataDicSel=subset(dataDic, FieldID %in% varInc$labelUKBB_raw, select=c(FieldID, Field, Coding, Participants, ValueType, Instances, Notes, Link))

print("Read in file containing coding info of the variables")
codingDic=read.csv(paste0(HOME, "/data/UKBB/Codings.csv"))

for ( i in 1:NROW(varInc$recodedName) ) {
    var=varInc$recodedName[i]
    varIncSub=subset(varInc, recodedName==var)
    if(varIncSub$binary=="no"){
          print(paste0("recode ", var))
           UKBBsel[[var]]=as.numeric( UKBBsel[[var]])
           }
  }

UKBBsel$cancer=revalue(as.factor(UKBBsel$cancer), c("1"="yes",
                                                    "0"="no",
                                                    "-1" = NA,
                                                    "-3" = NA))

UKBBsel$cancer <- factor(UKBBsel$cancer, levels = c("no", "yes"))

# URBANISATION
UKBBsel$urbanisation=revalue(as.factor(UKBBsel$urbanisation), c("1"="urban", # 1: England/Wales - Urban - sparse 
                                                     "2" = "town_fringe",  # 2: England/Wales - Town and Fringe - sparse 
                                                     "3" = "village_hamlet", # 3: England/Wales - Village - sparse
                                                     "4" = "village_hamlet", # 4: England/Wales - Hamlet and Isolated dwelling - sparse 
                                                     "5" = "urban", # 5: England/Wales - Urban - less sparse 
                                                     "6" = "town_fringe", # 6: England/Wales - Town and Fringe - less sparse 
                                                     "7" = "village_hamlet", # 7: England/Wales - Village - less sparse 
                                                     "8" = "village_hamlet", # 8: England/Wales - Hamlet and Isolated Dwelling - less sparse 
                                                     "9" = NA, # 9: Postcode not linkable 
                                                     "11" = NA, #  11: Scotland - Large Urban Area 
                                                     "12" = NA, # 12: Scotland - Other Urban Area
                                                     "13" = NA, # 13: Scotland - Accessible Small Town 
                                                     "14" = NA, # 14: Scotland - Remote Small Town
                                                     "15" = NA, # 15: Scotland - Very Remote Small Town
                                                     "16" = NA, # 16: Scotland - Accessible Rural
                                                     "17" = NA, # 17: Scotland - Remote Rural
                                                     "18" = NA)) #  18: Scotland - Very Remote Rural 
 UKBBsel$urbanisation <- factor(UKBBsel$urbanisation, levels = c("village_hamlet", "town_fringe", "urban"))


# Average weekly alcohol consumption
#1588	Average weekly beer plus cider intake	Field 1588 was collected from participants who indicated they drink alcohol more often than once or twice a week, as defined by their answers to Current Field
#1578	Average weekly champagne plus white ...	Field 1578 was collected from participants who indicated they drink alcohol more often than once or twice a week, as defined by their answers to Current Field
#1608	Average weekly fortified wine intake	Field 1608 was collected from participants who indicated they drink alcohol more often than once or twice a week, as defined by their answers to Current Field
#5364	Average weekly intake of other alcoh ...	Field 5364 was collected from participants who indicated they drink alcohol more often than once or twice a week, as defined by their answers to Current Field
#	Average weekly red wine intake	Field 1568 was collected from participants who indicated they drink alcohol more often than once or twice a week, as defined by their answers to Current Field
#1598	Average weekly spirits intake

weeklyFreq=c("1588", "1578", "1608", "5364", "1568", "1598")
UKBBalc=subset(UKBBdf, select=paste0(weeklyFreq, "-0.0"))
UKBBalcFreq=data.frame(weeklyAlc=rowSums(UKBBalc, na.rm = TRUE))
UKBBalcFreq$missingDat=rowSums(is.na(UKBBalc) | UKBBalc == "")
UKBBalcFreq$weeklyAlcClean=ifelse(UKBBalcFreq$missingDat==length(weeklyFreq), NA, UKBBalcFreq$weeklyAlc)

UKBBsel$alcfrequencyweekly=NA
UKBBsel$alcfrequencyweekly=ifelse(UKBBsel$alcfrequency<=3, UKBBalcFreq$weeklyAlcClean, NA) # [-3: Prefer not to answer | 1: Daily or almost daily | 2: Three or four times a week | 3: Once or twice a week | 4: One to three times a month | 5: Special occasions only | 6: Never ]
UKBBsel$alcfrequencyweekly=ifelse(UKBBsel$alcfrequency>3, 0, UKBBsel$alcfrequencyweekly)
UKBBsel$alcfrequencyweekly=ifelse(UKBBsel$alcfrequency=="-3", NA, UKBBsel$alcfrequencyweekly)
UKBBsel$alcfrequencyweekly=ifelse(UKBBsel$alcfrequencyweekly<0, NA, UKBBsel$alcfrequencyweekly)
UKBBsel$alcfrequencyweekly=ifelse(UKBBsel$alcfrequencyweekly>250, NA, UKBBsel$alcfrequencyweekly)
UKBBsel$alcfrequencyweekly=as.numeric(UKBBsel$alcfrequencyweekly)
summary(UKBBsel$alcfrequencyweekly)
# If answer < 0 then rejected, If answer > 250 then rejected

# Vegetable intake
UKBBsel$vegetable_intake_con=ifelse(UKBBsel$vegetable_intake_con < 0, NA, UKBBsel$vegetable_intake_con)
UKBBsel$vegetable_intake_con=as.numeric(UKBBsel$vegetable_intake_con)

# Fresh fruit intake
UKBBsel$fruit_intake_con=ifelse(UKBBsel$fruit_intake_con < 0, NA, UKBBsel$fruit_intake_con)
UKBBsel$fruit_intake_con=as.numeric(UKBBsel$fruit_intake_con)

# Cigarette frequency
UKBBsel$smoking_frequency=NA
UKBBsel$smoking_frequency=ifelse(UKBBsel$smoking_status=="0", "0", UKBBsel$smoking_frequency)
UKBBsel$smoking_frequency=ifelse(UKBBsel$smoking_status=="1", UKBBsel$smoking_frequency_former, UKBBsel$smoking_frequency) # 1: Previous smoker
UKBBsel$smoking_frequency=ifelse(UKBBsel$smoking_status=="2", UKBBsel$smoking_frequency_current, UKBBsel$smoking_frequency) # 2: Current smoker
UKBBsel$smoking_frequency=as.numeric(UKBBsel$smoking_frequency)
# Remove redundant variables
UKBBsel$smoking_frequency_current=NULL
UKBBsel$smoking_frequency_former=NULL

# create new continuous variables
UKBBsel$alcfrequency=recodeVar(data=UKBBsel, pheno="alcfrequency", sample="UKBB")$alcfrequency
UKBBsel$alcfrequency_con=as.numeric(as.factor(UKBBsel$alcfrequency))

# bmi
UKBBsel$bmi=UKBBsel$weight/(UKBBsel$height/100)^2

# create new continuous variables
UKBBsel$income=as.factor(recodeVar(data=UKBBsel, pheno="income", sample="UKBB")$income)
UKBBsel$income_con=as.numeric(ifelse(UKBBsel$income=="not_shared", NA, as.numeric(UKBBsel$income) ))

# recode household sice
UKBBsel$household_size=ifelse(UKBBsel$household_size=="-3" | UKBBsel$household_size=="-1", NA, UKBBsel$household_size)
UKBBsel$household_sizeCoded=ifelse(UKBBsel$household_size>=7, "7", NA)
UKBBsel$household_sizeCoded=ifelse(UKBBsel$household_size<7, UKBBsel$household_size, UKBBsel$household_sizeCoded)
UKBBsel$household_size=UKBBsel$household_sizeCoded
UKBBsel$household_size=as.factor(UKBBsel$household_size)
print("household levels")
print(table(UKBBsel$household_sizeCoded))

# recode bmi
UKBBsel$bmi=round(UKBBsel$bmi,0)

# recode coffee_intake
UKBBsel$coffee_intake=ifelse(UKBBsel$coffee_intake<0, NA, UKBBsel$coffee_intake)


head(UKBBsel$education)
print("Recode education age")
UKBBsel$education_age=ifelse(UKBBsel$education=="1", 20, UKBBsel$education_age)
UKBBsel$education_age=ifelse(as.numeric(UKBBsel$education_age)<0, NA, as.numeric(UKBBsel$education_age))
UKBBsel$education_age=ifelse(UKBBsel$education_age<= 14, 14, UKBBsel$education_age)
UKBBsel$education_age=ifelse(UKBBsel$education_age>=19, 19, UKBBsel$education_age)
table(UKBBsel$education_age)

table(is.na(UKBBsel$education_age))

# binary measure of education
UKBBsel$education_attainment=ifelse(UKBBsel$education_age>19, "uni_college", NA) # 1: College or University degree
UKBBsel$education_attainment=ifelse(UKBBsel$education_age=<19, "no_uni_college", UKBBsel$education_attainment)
UKBBsel$education_attainment <- factor(UKBBsel$education_attainment, levels = c("no_uni_college", "uni_college"))

# Recode years education
# 1.   College or University degree	                     20
# 2.   A levels/AS levels or equivalent                        13
# 3.   O levels/GCSEs or equivalent		              10
# 4.   CSEs or equivalent	              	              10
# 5.   NVQ or HND or HNC or equivalent		              19
# 6.   Other professional qualifications eg: nursing, teaching	15
# &.   None of the above	                            	7


# add non-cancer illness coding
drop_auth(rdstoken = paste0(HOME, "/token.rds"))  # authentication for dropbox
drop_download(
  local_path = paste0(HOME,"/data/UKBB/recodingNames.csv"),
  path = paste0(LOCAL,"/data/UKBB/recodingNames.csv"), overwrite=TRUE)

varNames <- read.csv(paste0(HOME, "/data/UKBB/recodingNames.csv"))

UKBBillness=subset(UKBBsel, select=c(eid, noncancer_illness_number, noncancer_illness, illness_icd10))

# Self-reported
varNames20002=subset(varNames, select=c(coding_sr, name_sr))
varNames20002 <- na.omit(varNames20002) 
UKBBillness=merge(UKBBillness, varNames20002, by.x="noncancer_illness", by.y="coding_sr", all.x=T, sort = FALSE)

UKBBillness$noncancer_illness_coded=ifelse(UKBBillness$noncancer_illness_number==0, "0:no_illness", UKBBillness$noncancer_illness)
UKBBillness$noncancer_illness_coded=ifelse(UKBBillness$noncancer_illness_number>0, as.character(UKBBillness$name_sr), UKBBillness$noncancer_illness_coded)
UKBBillness$noncancer_illness_binary=ifelse(UKBBillness$noncancer_illness_number==0, "no_illness", "illness")

UKBBillness$hypertension=ifelse(is.na(UKBBillness$noncancer_illness_number)==F, "0:no_hypertension", NA)
UKBBillness$hypertension=ifelse(UKBBillness$noncancer_illness_coded=="hypertension", "1:hypertension", UKBBillness$hypertension)

UKBBillness$migraine=ifelse(is.na(UKBBillness$noncancer_illness_number)==F, "0:no_migraine", NA)
UKBBillness$migraine=ifelse(UKBBillness$noncancer_illness_coded=="migraine", "1:migraine", UKBBillness$migraine)

UKBBillness$schizophrenia=ifelse(is.na(UKBBillness$noncancer_illness_number)==F, "0:no_schizophrenia", NA)
UKBBillness$schizophrenia=ifelse(UKBBillness$noncancer_illness_coded=="schizophrenia", "1:schizophrenia", UKBBillness$schizophrenia)

UKBBillness$manic_disorder=ifelse(is.na(UKBBillness$noncancer_illness_number)==F, "0:no_manic", NA)
UKBBillness$manic_disorder=ifelse(UKBBillness$noncancer_illness_coded=="mania/bipolar disorder/manic depression", "1:manic_disorder", UKBBillness$manic_disorder)

UKBBillness$alc_dependence=ifelse(is.na(UKBBillness$noncancer_illness_number)==F, "0:no_alc_dependence", NA)
UKBBillness$alc_dependence=ifelse(UKBBillness$noncancer_illness_coded=="alcohol dependency", "1:alc_dependence", UKBBillness$alc_dependence)

UKBBillness$eating_disorder=ifelse(is.na(UKBBillness$noncancer_illness_number)==F, "0:no_eating_disorder", NA)
UKBBillness$eating_disorder=ifelse(UKBBillness$noncancer_illness_coded=="anorexia/bulimia/other eating disorder", "1:eating_disorder", UKBBillness$eating_disorder)

UKBBillness$ptsd=ifelse(is.na(UKBBillness$noncancer_illness_number)==F, "0:no_ptsd", NA)
UKBBillness$ptsd=ifelse(UKBBillness$noncancer_illness_coded=="post-traumatic stress disorder", "1:ptsd", UKBBillness$ptsd)

UKBBillness$back_pain=ifelse(is.na(UKBBillness$noncancer_illness_number)==F, "0:no_back_pain", NA)
UKBBillness$back_pain=ifelse(UKBBillness$noncancer_illness_coded=="back pain", "1:back_pain", UKBBillness$back_pain)

UKBBsel=merge(UKBBsel, UKBBillness, by="eid", all.x=T, sort = FALSE, suffixes = c("",".y") )


# recode athma
UKBBsel$asthma=ifelse(UKBBsel$medical_condition=="-7" , "no", NA)
UKBBsel$asthma=ifelse(UKBBsel$medical_condition=="8" , "yes", UKBBsel$asthma)



#Job class was coded as skilled versus unskilled as in the Tyrrell (2016, (12))
# A skilled job was defined as ones in the following categories: 
#1.	Managers and Senior Officials
#2.	Professional Occupations
#3.	Associate Professional and Technical Occupations
#4.	Administrative and Secretarial Occupations
#5.	Skilled Trades Occupations
# Unskilled jobs were defined as ones in the following categories:
#6.	Personal Service Occupations
#7.	Sales and Customer Service Occupations
#8.	Process, Plant and Machine Operatives
#9.	Elementary Occupations

UKBBsel$job_class_coded=ifelse(substr(UKBBsel$job_class,1,1)=="1" , 1, NA) # managers_senior_officials
UKBBsel$job_class_coded=ifelse(substr(UKBBsel$job_class,1,1)=="2" , 1, UKBBsel$job_class_coded) # professional_occupations
UKBBsel$job_class_coded=ifelse(substr(UKBBsel$job_class,1,1)=="3" , 1, UKBBsel$job_class_coded) # associate_professional_technical_occupations
UKBBsel$job_class_coded=ifelse(substr(UKBBsel$job_class,1,1)=="4" , 1, UKBBsel$job_class_coded) # administrative_secretarial_occupations
UKBBsel$job_class_coded=ifelse(substr(UKBBsel$job_class,1,1)=="5" , 1, UKBBsel$job_class_coded) # skilled_trades_occupations
UKBBsel$job_class_coded=ifelse(substr(UKBBsel$job_class,1,1)=="6" , 0, UKBBsel$job_class_coded) # personal_service_occupations
UKBBsel$job_class_coded=ifelse(substr(UKBBsel$job_class,1,1)=="7" , 0, UKBBsel$job_class_coded) # sales_customer_service_occupations
UKBBsel$job_class_coded=ifelse(substr(UKBBsel$job_class,1,1)=="8" , 0, UKBBsel$job_class_coded) # "Process, Plant and Machine Operatives"
UKBBsel$job_class_coded=ifelse(substr(UKBBsel$job_class,1,1)=="8" , 0, UKBBsel$job_class_coded) # Elementary Occupations

print("Check categorical variables")
UKBBsel$smoking_status=as.factor(UKBBsel$smoking_status)

# Recode employment status
UKBBsel$employment_status_binary=revalue(as.factor(UKBBsel$employment_status), c("-7"=NA, 
                                         "-3"=NA,
                                         "1"="employed",
                                         "2"= NA, 
                                         "3"=NA,
                                         "4"=NA,
                                         "5"="unemployed",
                                         "6" = NA,
                                         "7"= NA) )

# recode insomnia
UKBBsel$insomnia=revalue(as.factor(UKBBsel$insomnia), c(
                                         "-3"=NA,
                                         "1"="rarely",
                                         "2"= "sometimes", 
                                         "3"="usually") )
UKBBsel$insomnia <- factor(UKBBsel$insomnia, levels = c("rarely", "sometimes", "usually")) # [-3: Prefer not to answer | 1: Never/rarely | 2: Sometimes | 3: Usually | ]
UKBBsel$insomnia_con=as.numeric(UKBBsel$insomnia)


# recode handedness
UKBBsel$handedness_left=revalue(as.factor(UKBBsel$handedness_left), c(
                                         "-3"=NA,
                                         "1"="0:right",
                                         "2"= "1:left", 
                                         "3"=NA) ) # [-3: Prefer not to answer | 1: Right-handed | 2: Left-handed | 3: Use both right and left hands equally | ]



UKBBsel$physical_activity=ifelse(UKBBsel$physical_activity<0, NA, UKBBsel$physical_activity)
UKBBsel$weight_individual=1

print("Remove old variables")
UKBBsel$illness_icd10=NULL
UKBBsel$noncancer_illness=NULL

# RECODE CONTINUOUS VARIABLES
UKBBsel$age_cat=recodeAge(UKBBsel$age)
levels(UKBBsel$age_cat)

# Education age: Create categorical variable
UKBBsel$education_age_cat=as.factor(UKBBsel$education_age)
levels(UKBBsel$education_age_cat)

# BMI
# If your BMI is less than 18.5, it falls within the underweight range.
# If your BMI is 18.5 to <25, it falls within the healthy weight range.
# If your BMI is 25.0 to <30, it falls within the overweight range.
# If your BMI is 30.0 or higher, it falls within the obesity range.
UKBBsel$bmi_cat=recodeBMI(UKBBsel$bmi)
levels(UKBBsel$bmi_cat)
table(UKBBsel$bmi_cat)




print("Import PCA data")
# see here for description: https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=531
# Sample QC; This corresponds to the "ukb_sqc_v2.txt" file available from the UKBB portal
sample_qc <- as.data.frame(fread(paste0(UKBBdir,"/geno/ukb_sqc_v2.txt"), header = F, select = c(3:68)))
colnames(sample_qc) <- c("array", "batch", "plate", "well", "cluster_CR", "dQC", "dna_concentration", "submitted_gender", "inferred_gender", "X_intensity", "Y_intensity", "submitted_plate", "submitted_well", "missing_rate", "heterozygosity", "heterozygosity_pc_corrected", "heterozygosity_missing_outlier", "PSCA", "in_kinship", "excluded_kinship_inference", "excess_relatives", "white_british", "pca_calculation", paste0("PC", seq(1,40)), "phasing_autosome", "phasing_X", "phasing_Y")

# Sample eid; This corresponds to a list of all sample eids (with sex information), retrieved from a ".fam" file available from the UKBB portal
sample_eid <- as.data.frame(fread(paste0(UKBBdir, "/plink/_001_ukb_cal_chr1_v2.fam"), header = F, select = c(1,5), col.names = c("eid", "sex")))
geno <- cbind(sample_eid, sample_qc)

#remove related samples (pca_calculation = 1)
geno$removeRelated=ifelse(geno$pca_calculation==1, "include", NA)
geno$removeRelated=ifelse(geno$pca_calculation==0, "exclude", geno$removeRelated)

# remove gender mismatch
geno$genderMismatch=ifelse(geno$submitted_gender==geno$inferred_gender, "include", NA) # 
geno$genderMismatch=ifelse(geno$submitted_gender!=geno$inferred_gender, "exclude", geno$genderMismatch)

# remove non-european ancestry
geno$removeAncestry=ifelse(geno$white_british==1, "include", NA)
geno$removeAncestry=ifelse(geno$white_british==0, "exclude", geno$removeAncestry)

# remove non-european ancestry
geno$removeHetmiss=ifelse(geno$heterozygosity_missing_outlier==0, "include", NA)
geno$removeHetmiss=ifelse(geno$heterozygosity_missing_outlier==1, "exclude", geno$removeHetmiss) #  (0/1)	(no/yes) Indicates samples identified as outliers in heterozygosity and missing rates, which indicates poor-quality genotypes 

geno$includeGeno=ifelse(geno$removeRelated=="include" &
                        geno$genderMismatch=="include" &
                        geno$removeAncestry=="include" &
                        geno$removeRelated=="include" &
                        geno$removeHetmiss=="include", "include", "exclude")

# select for inclusion
genoInc=subset(geno, select=c(eid, array, batch, removeRelated, genderMismatch, removeAncestry, removeHetmiss, includeGeno, PC1, PC2, PC3, PC4, PC5))
head(genoInc)


UKBBselPCA=merge(UKBBsel, genoInc, by="eid", all.x=T)
head(UKBBselPCA)

print("Save selected variables")
saveRDS(UKBBselPCA, paste0(HOME, "/results/rds/UKBBsel.rds"))


print("Check coding of the variables")
for ( i in 1:NROW(dataDicSel) ) {
      coding=dataDicSel$Coding[i]
      print(paste0("Check ", dataDicSel$Field[i]))
      print(paste0("Variable type ", dataDicSel$ValueType[i]))
      varType=c("Categorical multiple", "Categorical single")

      if( any(varType %in% dataDicSel$ValueType[i]) ){
          print("Get label for categorical variable")
          codingSel=subset(codingDic, Coding==coding)
          meaningPasted=paste0(codingSel$Value, ": ", codingSel$Meaning, " | ")
          dataDicSel$Label[i]=paste0(meaningPasted, collapse="")
      } else {
            dataDicSel$Label[i]=NA
      }
} 


write.table(dataDicSel,
              file= paste0(HOME, "/data/UKBB/dataDicExtracted.csv"),
              sep="\t",
              row.names = FALSE,
              col.names=T,
              quote=F)


print("All done!")


print("All output saved")
uploadLog(file=paste0("recodeUKBBlog.log"))

