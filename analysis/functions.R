# Install old glmnet package
#setwd(paste0(HOME, "/programs/R"))
#system("wget https://cran.r-project.org/src/contrib/Archive/glmnet/glmnet_4.1.tar.gz")
#install.packages(paste0(HOME, "/programs/R/glmnet_4.1.tar.gz"), repos = NULL, type ='source')
#library(devtools)
#install_local(paste0(HOME, "/programs/R/glmnet_4.1-4.tar.gz"))


print("Check with variables are continous")
drop_auth(rdstoken = paste0(HOME, "/token.rds"))  # authentication for dropbox
drop_download(
  local_path = paste0(HOME,"/data/variableList.xlsx"),
  path = paste0(LOCAL,"/data/variableList.xlsx"), overwrite=TRUE)

library("readxl")
variableList <- read_excel(paste0(HOME, "/data/variableList.xlsx"))
variabesCon=subset(variableList, levelsUKBB=="continuous")$recodedName



# Rename variables
renameVar=function(df, varOld, varNew){
      print(paste0("Rename ", varOld, " to ", varNew))
      names(df)[names(df) == varOld] <- varNew
      return(df)
}



recodeVar=function(data, pheno, sample){ 
    print(paste0("Recode ", pheno))

    varType=typeof(data[[pheno]])
    variable_rec=data[[pheno]]
     dfout=data.frame( variable_rec)
     colnames(dfout)=pheno

    if(pheno=="noncancer_illness_coded" | pheno=="illness_icd10_coded" | pheno=="noncancer_illness_binary"){
      print("no recoding done")
      variable_rec=data[[pheno]]
      dfout=data.frame( variable_rec)
      colnames(dfout)=pheno
      next 
    }

    if((pheno %in% variabesCon)==TRUE ){
      print("Continuous variable")
      variable_rec=as.numeric(data[[pheno]])
      dfout=data.frame( as.numeric(variable_rec))
      colnames(dfout)=pheno
     } else {

    variable=as.character(data[[pheno]])

    if(pheno=='sex'){
        if(sample=="UKBB"){ # 0: Female | 1: Male | 
        print("recode UKBB")
        variable_rec=revalue(variable, c("0"="female", 
                                         "1"="male") )
        }
        if(sample=="CEN"){ # 0: Female | 1: Male | 
        print("recode UKBB")
        variable_rec=revalue(variable, c("Female"="female", 
                                         "Male"="male") )
        }
        if(sample=="HSE"){ 
        print("recode HSE")
        variable_rec=revalue(variable, c("Men"="male", 
                                         "Women"="female",
                                          "Female"="female",
                                          "Male"="male",
                                           "Refused/not obtained" = NA,
                                          "Schedule not obtained" = NA,
                                          "Schedule not applicable" = NA ) )

        }
        variable_rec <- factor(variable_rec, levels = c("male", "female"))
    }

    if(pheno=='long_standing_illness'){
        if(sample=="UKBB"){ # 0: Female | 1: Male | 
        print("recode UKBB")
        variable_rec=revalue(variable, c("0"="no", 
                                         "1"="yes") )
        }
        if(sample=="HSE"){ 
        print("recode HSE")
        variable_rec=revalue(variable, c("Yes"="yes", 
                                         "No" = "no") )
        }
        variable_rec <- factor(variable_rec, levels = c("no", "yes"))
    }




    if(pheno=='overallhealth'){
        if(sample=="UKBB"){ #  #-1: Do not know | -3: Prefer not to answer | 1: Excellent | 2: Good | 3: Fair | 4: Poor | (\"In general how would you rate your overall health?\)
        print("recode UKBB")
        variable_rec=revalue(variable, c("-1"=NA, 
                                         "-3"=NA,
                                         "1" = "good",
                                         "2" = "good",
                                         "3" = "fair",
                                         "4" = "poor") )
        }
        if(sample=="HSE"){ 
        print("recode HSE")
        variable_rec=revalue(variable, c("...very good,"="good",
                       "Good,"="good",
                        "good,"  = "good",
                       "Fair,"= "fair",
                       "fair," = "fair",
                       "Bad, or" = "poor",
                       "bad, or" = "poor",
                       "very bad?" = "poor",
                       "Very bad?" = "poor",
                       "Very good/good" = "good",
                       "Fair" = "fair",
                       "Bad/very bad" = "poor") )
        }
    if(sample=="CEN"){ 
                variable_rec=revalue(variable, c("Very good health"="good",
                                 "Very bad health" = "poor",
                                 "Good health" = "good",
                                 "Fair health" = "fair",
                                 "Bad health" = "poor") )

    }
        variable_rec <- factor(variable_rec, levels = c("poor", "fair", "good"))
    }

if(pheno=='smoking_status'){
        if(sample=="UKBB"){ #  | 0: Never | 1: Previous | 2: Current | 
        print("recode UKBB")
        variable_rec=revalue(variable, c("-3"=NA,
                                         "0" = "never",
                                         "1" = "previous",
                                         "2" = "current") )
        }
        if(sample=="HSE"){ 
        print("recode HSE")
        variable_rec=revalue(variable, c("Never smoked cigarettes at all"="never",
                       "Used to smoke cigarettes occasionally"="previous",
                       "Used to smoke cigarettes regularly"= "previous",
                       "Current cigarette smoker" = "current") )
        }
        variable_rec <- factor(variable_rec, levels = c("never", "previous", "current"))
    }
    


      if(pheno=='education'){

        if(sample=="UKBB"){  #-3: Prefer not to answer | -7: None of the above | 1: College or University degree | 2: A levels/AS levels or equivalent | 3: O levels/GCSEs or equivalent | 4: CSEs or equivalent | 5: NVQ or HND or HNC or equivalent | 6: Other professional qualifications eg: nursing, teaching | 
        print("recode UKBB")
        variable_rec=revalue(variable, c("-1"=NA, 
                                         "-3"=NA,
                                         "-7"="none",
                                         "1" = "college_or_university_degree",
                                         "2" = "highschool_diploma",
                                         "3" = "highschool_diploma", # CSE = The Certificate of Secondary Education (CSE) was a qualification offered from 1965 until the introduction of the General Certificate of Secondary Education (GCSE) in 1986. 
                                         "4" = "highschool_diploma",
                                         "5" = "highschool_diploma",
                                         "6" = "highschool_diploma") )
        }
        if(sample=="HSE"){ 
        print("recode HSE")
        variable_rec=revalue(variable, c("Higher ed below degree"="qualification",
                                         "NVQ3/GCE A Level equiv"="qualification",
                                         "NVQ1/CSE other grade equiv" = "qualification",
                                         "Foreign/other" = "qualification",
                                         "No qualification" = "none",
                                         "FT Student" = "qualification",
                                         "NVQ2/GCE O Level equiv" = "qualification",
                                         "NVQ4/NVQ5/Degree or equiv"  = "qualification") ) # NVQ4+ (a degree-level or equivalent qualification or above).
        }
  if(sample=="CEN"){ 
    variable_rec=revalue(variable, c("Level 1 (0-4 GCSE, O level, or equivalents)" = "qualification",
                                             "Apprenticeship" = "qualification",
                                             "Level 2 (5+ GCSE, O level, 1 A level, or equivalents)"="qualification",
                                             "Level 4+ (degree, postgrad, professional quals)"  = "qualification",
                                             "Other (vocational/foreign/outside UK quals)" = "qualification",
                                             "Level 3 (2+ A levels, or equivalents)"="qualification",
                                             "No academic or professional qualifications" = "none") ) 
  }

        variable_rec <- factor(variable_rec, levels = c("none", "qualification", "qualification"))

    }
    if(pheno=='vegetable_intake'){

        if(sample=="UKBB"){  
        print("recode UKBB")
        variable_rec=revalue(as.factor(variable), c("-1"=NA, 
                                         "-3"=NA,
                                         "0"="no",
                                          "1"="yes") )
        }

        if(sample=="HSE"){ 
        print("recode HSE")
        variable_rec=revalue(as.factor(variable), c("No"="no",
                                          "Yes"= "yes") )
        }
        variable_rec <- factor(variable_rec, levels = c("no", "yes"))

    }

    if(pheno=='fruit_intake'){

        if(sample=="UKBB"){  
        print("recode UKBB")
        variable_rec=revalue(variable, c("-1"=NA, 
                                         "-3"=NA,
                                         "0"="no",
                                          "1"="yes") )
        }
        if(sample=="HSE"){ 
        print("recode HSE")
        variable_rec=revalue(variable, c("No"="no",
                                          "Yes"= "yes") )
        }
        variable_rec <- factor(variable_rec, levels = c("no", "yes"))

    }

   if(pheno=='employment_status'){

        if(sample=="UKBB"){  # -3: Prefer not to answer | -7: None of the above | 1: In paid employment or self-employed | 2: Retired | 3: Looking after home and/or family | 4: Unable to work because of sickness or disability | 5: Unemployed | 6: Doing unpaid or voluntary work | 7: Full or part-time student | 
        print("recode UKBB")
        variable_rec=revalue(variable, c("-7"=NA, 
                                         "-3"=NA,
                                         "1"="employed",
                                         "2"= "retired", 
                                         "3"="economically_inactive",
                                         "4"="economically_inactive",
                                         "5"="unemployed",
                                         "6" ="economically_inactive",
                                         "7"="economically_inactive") )
        }
        if(sample=="HSE"){ 
        print("recode HSE")
        variable_rec=revalue(variable, c("ILO unemployed"="unemployed",
                                         "In employment"= "employed",
                                         "Other economically inactive" = "economically_inactive",
                                         "Retired" = "retired") )
        }
    if(sample=="CEN"){ 
        variable_rec=revalue(variable,  c(
  "Econ active (exc FT students) SE with emps, FT"= "employed",
  "Econ active (exc FT students) SE without emps, FT"  = "employed",
  "Econ active (exc FT students) seek/will take emp in 2 wks"= "unemployed",
  "Econ active (exc FT students), PT employee" = "employed",
  "Econ active (exc FT students) SE with emps, PT" = "employed",
  "Econ active (exc FT students) SE without emps, PT"  = "employed",
  "Econ active (exc FT students),  FT employee" = "employed",
  "Econ active FT students, in employment" = "employed",
  "Economically Inactive, Looking after home/family" = "economically_inactive",
  "Economically Inactive, Permanently sick/disabled"= "economically_inactive",
  "Economically Inactive, Student"  = "economically_inactive",
  "Economically Inactive, Other" = "economically_inactive",
  "Economically Inactive, Retired" = "retired",
  "Econ active FT students, unemp, seek/will take emp in 2 wks"="economically_inactive") )
    }
        variable_rec <- factor(variable_rec, levels = c("unemployed", "employed","economically_inactive",  "retired" ))

    }


        if(pheno=='income'){

        if(sample=="UKBB"){  # -1: Do not know | -3: Prefer not to answer | 1: Less than 18,000 | 2: 18,000 to 30,999 | 3: 31,000 to 51,999 | 4: 52,000 to 100,000 | 5: Greater than 100,000 | 
        print("recode UKBB")
        variable_rec=revalue(variable, c("-1"= "not_shared",
                                         "-3"= "not_shared",
                                         "1"="<18k",
                                         "2"="18k-31k",
                                         "3"="31k-52k",
                                         "4"="52k-100k",
                                         "5"= ">100k"   ) )
        }
        if(sample=="HSE"){ 
        print("recode HSE")
        variable_rec=revalue(as_factor(variable), c("<£520" = "<18k",
                                          "£1,600<£2,600"="<18k",
                                          "£10,400<£13,000"="<18k",
                                          "£13,000<£15,600" = "<18k",
                                          ">=£150,000" = ">100k",
                                          "£130,000<£140,000" = ">100k",
                                          "£100,000<£110,000" = ">100k",
                                          "£110,000<£120,000" = ">100k",
                                          "£140,000<£150,000" = ">100k",
                                          "£120,000<£130,000" = ">100k",
                                          "£15,600<£18,200" = "<18k",
                                          "£18,200<£20,800" = "18k-31k",
                                          "£2,600<£3,600" = "<18k",
                                          "£20,800<£23,400" = "<18k",
                                          "£23,400<£26,000" = "18k-31k",
                                          "£26,000<£28,600" = "18k-31k",
                                          "£28,600<£31,200" = "18k-31k",
                                          "£3,600<£5,200" = "<18k",
                                          "£31,200<£33,800" = "31k-52k",
                                          "£33,800<£36,400" = "31k-52k",
                                          "£36,400<£41,600" = "31k-52k",
                                          "£41,600<£46,800" = "31k-52k",
                                          "£46,800<£52,000" = "31k-52k",
                                          "£5,200<£7,800" = "<18k",
                                          "£52,000<£60,000" = "52k-100k",
                                          "£520<£1,600" = "<18k",
                                          "£60,000<£70,000" = "52k-100k",
                                          "£7,800<£10,400" = "<18k",
                                          "£70,000<£80,000" = "52k-100k",
                                          "£80,000<£90,000" = "52k-100k",
                                          "£90,000<£100,000" = "52k-100k",
                                          "Do not know" = "not_shared",
                                          "Refused"= "not_shared") )
        }
        variable_rec <- factor(variable_rec, levels = c("not_shared","<18k", "18k-31k", "31k-52k", "52k-100k", ">100k"))

    }




      if(pheno=='alcfrequency'){ #  "Alcohol intake frequency.",501580,575012,"Complete","Categorical single","","Data","Primary","Unisex",4,1,100402,"ACE touchscreen question \"About how often do you drink alcohol?\" If the participant activated the Help button they were shown the message:     If this varies a lot, please provide an average considering your intake over the last year     ","http://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=1558"

        if(sample=="UKBB"){  
        print("recode UKBB")
        variable_rec=revalue(as.factor(variable), c("-3"=NA,
                                         "6"="never",
                                         "5" = "few_times_year",
                                         "4" = "monthly",
                                         "3" = "once_twice_weekly", 
                                         "2" = "three_four_times_weekly",
                                         "1" = "daily") )
        }
        if(sample=="HSE"){ 
        print("recode HSE")
        variable_rec=revalue(variable, c("Not at all in the last 12 months/Non-drinker"="never",
                                         "Once or twice a year"="few_times_year",
                                         "Once every couple of months" = "few_times_year",
                                         "Once or twice a month" = "monthly",
                                         "Once or twice a week" = "once_twice_weekly",
                                         "Five or six days a week" = "daily",
                                         "Three or four days a week" = "three_four_times_weekly",
                                         "Almost every day" = "daily") )
        }
        variable_rec <- factor(variable_rec, levels = c("never", "few_times_year", "monthly", "once_twice_weekly", "three_four_times_weekly", "daily"))

    }


    

    print(table(as.factor(variable_rec)))
    dfout=data.frame(variable_rec)
    colnames(dfout)=pheno

        }
      return(dfout)
}           




recodeUKBB=function(data, pheno, type="factor"){
    varType=typeof(data[[pheno]])
    variable=data[[pheno]]

    if(levels(as.factor(variable))[1] == "-3" & levels(as.factor(variable))[2] == "-1" & levels(as.factor(variable))[3] == "0" & levels(as.factor(variable))[4] == "1"){
      print("recode binary")
    variable_rec=revalue(as.character(variable), c("-1"=NA, 
                                         "-3"=NA,
                                         "0" = "no",
                                         "1" = "yes") )
    variable_rec <- factor(variable_rec, levels = c("no", "yes"))
    }

    if(pheno=="overallhealth"){
    variable_rec=revalue(as.character(variable), c("-1"=NA, 
                                         "-3"=NA,
                                         "1" = "good",
                                         "2" = "good",
                                         "3" = "fair",
                                         "4" = "poor") )
    variable_rec <- factor(variable_rec, levels = c("poor", "fair", "good"))
    }

    if(pheno=="vegetable_intake" | pheno=="fruit_intake"){
      variable_rec=revalue(as.character(variable), c("-1"=NA, 
                                         "-3"=NA,
                                         "0" = "no",
                                         "1" = "yes") )
       variable_rec <- factor(variable_rec, levels = c("no", "yes"))

    }


    print(table(as.factor(variable_rec)))
    dfout=data.frame(variable_rec)
    colnames(dfout)=pheno
    return(dfout)
        
  
}


describeData=function(df1, df2, colname){

  df1_sel=df1[[colname]]
  df2_sel=df2[[colname]]

  if(is.numeric(df1_sel)==TRUE){
    print(paste0("use ", colname, " as a continuous variable"))
    dfOut=data.frame(hse=round( mean(df1_sel, na.rm=T),2),
                  ukbb=round( mean(df2_sel, na.rm=T),2) )
    dfOut$levels="mean"
  } else {
    print(paste0("use ", colname, " as a categorical variable"))
    p1=as.data.frame(round(prop.table(table(as.factor(df1_sel) )),3))
    names(p1)[names(p1) == 'Freq'] <- "hse"

    p2=as.data.frame(round(prop.table(table(as.factor(df2_sel) )),3))
    names(p2)[names(p2) == 'Freq'] <- "ukbb"
  
    dfOut=merge(p1, p2, by.x="Var1", by.y="Var1", all = T, sort = FALSE)
    names(dfOut)[names(dfOut) == 'df1_sel'] <- "levels"
    }
        
  dfOut$category=colname
  dfOut$missing_hse=paste0(sum(is.na(df1_sel)), "/", NROW(df1_sel))
  dfOut$missing_ukbb=paste0(sum(is.na(df2_sel)), "/", NROW(df2_sel))
  names(dfOut)[names(dfOut) == 'Var1'] <- "levels"

  dfOut_sel=subset(as.data.frame(dfOut), select=c(category, levels, hse, ukbb, missing_hse, missing_ukbb))
  return(dfOut_sel)
}


library(modelsummary)
library(kableExtra)
library(gt)

# Save summary table and upload to dropbox
saveTable=function(df, name, type="other"){
 print(paste0("Save ", name))

if(type=="glm"){
  print("glm output")
  modelsummary(df, output=paste0(HOME, "/results/tables/", name, ".html"),
  statistic=NULL,
  estimate="{estimate} ({std.error}){stars}")
}
if(type=="other"){
 kable(df, "html", , booktabs = TRUE) %>%
  kable_styling("striped") %>%
  save_kable(paste0(HOME, "/results/tables/", name,".html"))
}
 print(paste0("Upload ", name))
 drop_upload(paste0(HOME, "/results/tables/", name,".html"), 
 path = paste0(LOCAL, "/results/tables"), 
 mode = "overwrite")
}

uploadDropbox=function(file, folder){
   print(paste0("Upload ", file))
 drop_upload(paste0(HOME, "/results/",folder,"/", file), 
 path = paste0(LOCAL, "/results/",folder), 
 mode = "overwrite")
}

uploadLog=function(file){
   print(paste0("Upload ", file))
   print(Sys.time())
 drop_upload(paste0(HOME, "/output/log/", file), 
 path = paste0(LOCAL, "/output/log"), 
 mode = "overwrite")
}




weightedEstimates=function(df, var, wt){
  print(paste0("Read in " , var))
  variable=df[[var]]
  weights=df[[wt]]
  type=typeof(variable)
  na_var=sum(is.na(variable))
  na_weights=sum(is.na(weights))
  print(paste0("Missing data in variable: ", na_var))
  print(paste0("Missing data in weights: ", na_weights))

  dfout=data.frame(variable=variable,
                 weights=weights
                 )

  dfClean=dfout[complete.cases(dfout),]
  variable=dfClean$variable
  weights=dfClean$weights
  n_complete_data=NROW(dfClean)

  print(paste0("Removed ", NROW(df)-NROW(dfClean) , " from data"))

  if(type=="double"){
    print("Estimate (weighted) mean")
    #mean_est=mean(variable*weights, na.rm=T )
    mean_est=sum(variable*weights)/sum(weights)
    var_est=sum(weights *(variable-mean_est)^2, na.rm=T)*(sum(weights, na.rm=T)/(sum(weights, na.rm=T)^2-sum(weights^2, na.rm=T)))  # taken from: https://rdrr.io/cran/SDMTools/src/R/wt.mean.R
    

    
    sd_est=sqrt(var_est)
    outWeighted=paste0(round(mean_est, 2), " (", round(sd_est, 2), ")")
    outUnweighted=paste0(round(mean(variable, na.rm=T), 2), " (", round(sd(variable, na.rm=T), 2), ")")

    MeanoutWeighted_raw=mean_est
    MeanoutUnweighted_raw=mean(variable, na.rm=T)
    sdUnweighted_raw =sd(variable, na.rm=T)

    df_out=data.frame(cat=rep(var, 1),
                      level="mean",
                      weighted_estimate=outWeighted,
                      unweighted_estimate=outUnweighted,
                      outWeighted_raw=MeanoutWeighted_raw,
                      outUnweighted_raw=MeanoutUnweighted_raw,
                      totalN=n_complete_data,
                      sdweighted=sd_est,
                      sdunweighted=sdUnweighted_raw)
    return(df_out)

  } else {
    print("Estimate (weighted) proportion")
    splitDf <- split(dfClean, as.factor(variable))  
    catNames=names(splitDf)
    listFreqWeighted=list()
    listFreqUneighted=list()

    for ( i in 1:length(catNames) ) {
      print(paste0("Get frequencies for: ", catNames[i]))
      listFreqWeighted[[i]]=round(sum(splitDf[[catNames[i]]]$weights),0)
      listFreqUneighted[[i]]=NROW(splitDf[[catNames[i]]])
      }
      FreqWeigthed=data.frame(freq_weighted=do.call(rbind, listFreqWeighted))
      FreqUneigthed=data.frame(freq_unweighted=do.call(rbind, listFreqUneighted))
      Freq=cbind(FreqWeigthed, FreqUneigthed)
      Freq$prop_weighted=Freq$freq_weighted/sum(FreqWeigthed$freq_weighted)
      Freq$prop_unweighted=Freq$freq_unweighted/sum(FreqUneigthed$freq_unweighted)
      Freq$levels=catNames
      Freq$outWeighted=paste0(round(Freq$prop_weighted*100, 2), "% (n=", round(Freq$freq_weighted, 2), ")" )
      Freq$outUnweighted=paste0(round(Freq$prop_unweighted*100, 2), "% (n=", round(Freq$freq_unweighted, 2), ")" )

      Freq$outWeighted_raw=round(Freq$prop_weighted*100, 2)
      Freq$outUnweighted_raw=round(Freq$prop_unweighted*100, 2)

      df_out=data.frame(cat=rep(var, NROW(Freq)),
                      level= Freq$levels,
                      weighted_estimate=Freq$outWeighted,
                      unweighted_estimate=Freq$outUnweighted,
                      outWeighted_raw=Freq$outWeighted_raw,
                      outUnweighted_raw= Freq$outUnweighted_raw,
                      totalN=n_complete_data,
                      sdweighted=NA,
                      sdunweighted=NA)
    return(df_out)
  }
}






corTest=function(a, b){
       cor=cor.test(a , b, method = c("pearson"), use = "complete.obs")
       dfOUTcor=data.frame(cor=cor$estimate, pval= formatC(cor$p.value, digits=4) )
       return(dfOUTcor)
}


print("normalized weights")
normalizeWeights=function(weights){
  normalized=weights/mean(weights, na.rm = TRUE)
  return(normalized)
}



remList=function(df){
      if("eid" %in% names(df)==TRUE){
       print("keep")
       return(df)
      } else {
       print("remove")
       df=NULL
      }
}




GWAS_modified <- function(formula, 
                          data, 
                          method = "glm", 
                          i = seq_len(nrow(geno(data))), 
                          j = seq_len(ncol(geno(data))), 
                          chunkSize = NULL, 
                          nCores = getOption("mc.cores", 2L), 
                          weighting,
                          verbose = FALSE, 
                          binary = "no", ...) {


getCoefficients.glm <- function(x) {
    c(coef(summary(x))[2L, ], N=round(nobs(x), 0))
}


getResponse <- function(formula) {
    # Extract component from parse tree (see https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Language-objects)
    sym <- formula[[2L]]
    # Convert symbol to character
    as.character(sym)
}

i <- convertIndex(geno(data), i, "i")
j <- convertIndex(geno(data), j, "j")

  if(weighting=="yes" | weighting=="weighted"){
    print("Use survey package")
     surveyGWA=function(data){

        dataC <- data[complete.cases(data$PS_w), ]

       design.ps <- svydesign(ids=~1,   weights = ~PS_w, data=dataC)
       svyglm(GWAS.model, design=design.ps)
     }
       FUN <- match.fun(surveyGWA)
         #FUN <- match.fun(method)
  } else {
    print("Use glm")
    FUN <- match.fun(method)
  }


GWAS.model <- update(formula, ".~z+.")

print(paste0("Weighting applied: ", weighting))

OUT <- chunkedApply(X = geno(data), MARGIN = 2L, FUN = function(col, ...) {
            df <- pheno(data)[i, , drop = FALSE]
            df[["z"]] <- col
            if(weighting=="yes" | weighting=="weighted"){
              print("Weighting variable: PS_w")
              if(binary=="no"){
                print("Outcome type: Continuous")
                fm <- FUN(data = df)
                #fm <- FUN(GWAS.model, data = df, weights=PS_w, ...)
              }
              if(binary=="yes"){
                  print("Outcome type: Binary")
                  fm <- FUN(GWAS.model, data = df, weights=PS_w,  family = binomial(logit), ...)
              }

            }
            if(weighting=="no" |  weighting=="unweighted"){
                if(binary=="no"){
                print("Outcome type: Continuous")
                fm <- FUN(GWAS.model, data = df, ...)
                }
                if(binary=="yes"){
                  print("Outcome type: Binary")
                  fm <- FUN(GWAS.model, data = df, family = binomial(logit), ...)
              }
            }
            getCoefficients.glm(fm)

        }, i = i, j = j, chunkSize = chunkSize, nCores = nCores, verbose = verbose, ...)

        OUT <- as.data.frame(t(OUT))
        OUT$SNP <- colnames(geno(data))[j]
        print(OUT)

        if(binary=="no"){
        OUTdf=data.frame(SNP=OUT[["SNP"]],
                         CHR = data@map$chromosome[j],
                         BP=data@map$base_pair_position[j],
                         A1=data@map$allele_1[j], 
                         A2=data@map$allele_2[j], 
                         BETA = OUT[['Estimate']],
                         SE = OUT[['Std. Error']],
                         P = OUT[['Pr(>|t|)']],
                         N = OUT[['N']])
        }
        if(binary=="yes"){
        # note: p-value column is different
        OUTdf=data.frame(SNP=OUT[["SNP"]],
                         CHR = data@map$chromosome[j],
                         BP=data@map$base_pair_position[j],
                         A1=data@map$allele_1[j], 
                         A2=data@map$allele_2[j], 
                         BETA = OUT[['Estimate']],
                         SE = OUT[['Std. Error']],
                         P = OUT[['Pr(>|z|)']],
                         N = OUT[['N']])
        }

    return(OUTdf)
}



#standardBeta=function(df, beta, se, N){
 # print("Derive standardized beta")
#  zscore = beta/ se
#  df$beta_std = zscore / sqrt(N)  # Use effective sample size
#  print("Get standard error")
#  df$se_std = sqrt(1/N)
#  return(df)
#}efeffe


standardBeta=function(df, beta, se, N){
    df$maf <- ifelse(df$maf > 0.5, 1 - df$maf, df$maf)
     # make sure they are numeric numbers
    snp_freq = as.numeric(as.character(df$maf))
    b = as.numeric(as.character(beta))
    se = as.numeric(as.character(se))
    n = as.numeric(as.character(N))
    zscore = b / se
    df$beta_std = zscore / sqrt(2*snp_freq*(1-snp_freq)*(n+zscore^2))
    df$se_std  = 1 / sqrt(2*snp_freq*(1-snp_freq)*(n+zscore^2))
  return(df)
}



# Slurm function to get correlation estimates

corFunc <- function(loopsCor) {
    print(paste0("Run job ", loopsCor))
    nRep=seq(1:2)
    listCorRan=list()
    start.time <- Sys.time()

    print(HOME)
    library(BGData)
    source(paste0(HOME, "/analysis/input.R"))
    source(paste0(HOME, "/analysis/functions.R"))
    load.BGData(paste0(HOME, "/data/UKBB/genoFile.RData"))
    phenoIn=pheno(bg)
    dfInc=pheno(bg)[[pheno]]


        for(i in 1:max(nRep) ){
            if(binary=="no"){
                print("Generate random distribution for continous phenotypes")

                pheno(bg)$phenoRandom1=rnorm(NROW(dfInc), mean=mean(dfInc, na.rm=T), sd=sd(dfInc, na.rm=T) )
                pheno(bg)$phenoRandom2=rnorm(NROW(dfInc), mean=mean(dfInc, na.rm=T), sd=sd(dfInc, na.rm=T) )

                glmFormulaRandom <- as.formula(paste("phenoRandom1 ~ sex + PC1 + PC2 + PC3 + PC4 + PC5 + batch"))
                } 
        if(binary=="yes"){
                print("Generate random distribution for binary phenotypes")
                pheno(bg)$phenoRandom1=rbinom(n=NROW(dfInc), size=1, prob=prevalence)
                pheno(bg)$phenoRandom2=rbinom(n=NROW(dfInc), size=1, prob=prevalence)
                glmFormulaRandom <- as.formula(paste("as.factor(phenoRandom1) ~ sex + PC1 + PC2 + PC3 + PC4 + PC5 + batch"))
                 }

        print(paste0("Start iteration ", nRep[i]))
        print("unweighted")
        gwasRawRandom <- GWAS_modified(formula = glmFormulaRandom, data = bg, weighting="no", method="glm", j=sel_snps, binary=binary )
        print("weighted")
        gwasWeightedRandom <- GWAS_modified(formula = glmFormulaRandom, data = bg, weighting="yes", method="glm", j=sel_snps, binary=binary ) #j = !exclusions j=1:10 to test 10 SNPs
    
        gwasRandom=merge(gwasRawRandom, gwasWeightedRandom, by="SNP", all.x=T, suffixes=c("", "_wt"))
        gwasRandom$iteration=nRep[i]
        listCorRan[[i]]=gwasRandom
        }
        CorRandDF=do.call(rbind, listCorRan)
        end.time <- Sys.time()
        runningTime_h=round(difftime(start.time,  end.time, units="hours"), 2)
        print(   paste0("Runnig time for ",  NROW(sel_snps) , " included SNps and k=", nRep , " repititions: ",runningTime_h , " hours"))

         return(CorRandDF)
}




selectHSE=function(df, age, origin, year, vars){

  varSelect=vars[[grep(year, names(vars), value = T)]]


  for ( i in 1:NROW(varSelect) ) {
    check=varSelect[[i]]
    print(check)
    dfcheck=df[,   check ]
  }


  df_sel <- df[,   varSelect ]
  colnames(df_sel)=vars$labelUKBB_recoded

  print("clean ancestry")
  labels = as.data.frame(  df[[origin]] %>% attr('labels'))
  labelsDF=data.frame(num=labels[,1], label=rownames(labels))
  df[[origin]]=labelsDF$label[ match(df[[origin]],labelsDF$num) ]

  #df=as.data.frame(df)
library(sjlabelled)

  for ( i in 1:NROW(vars) ) {
    print(vars$labelUKBB_recoded[i])
    if(vars$codedHSE[i]=="con"){
      print("Continuous")
          df_sel[,i]=as.numeric(  df_sel[,i])
          print(summary( df_sel[,i]))
    }
      if(vars$codedHSE[i]=="cat"){
      print("Categorical")
          labels = as.data.frame(  df_sel[,i] %>% attr('labels'))
          #labelsDF=data.frame(num=labels[,1], label=rownames(labels))
          labelsDF=data.frame(num=labels[,1], label=get_labels(df_sel[,i]))
          df_sel[,i]=labelsDF$label[ match(df_sel[,i],labelsDF$num) ]
          df_sel[,i]=as.factor(  df_sel[,i])
          print(table( df_sel[,i]))
    }
  }

  df_sel$weight_individual=as.numeric(as.character(df[,  "wt_int" ])) 
  df_sel$ancestry=df[[origin]]

  df_age=subset(df_sel, age >=40 & age  <=69)
  print(paste0(NROW(df_sel)-NROW(df_age), " excluded as outside the UKBB age range"))
  df_origin=subset(df_age, ancestry == "White - British" | ancestry == "White - Irish" | ancestry == "Any other white background" | ancestry == "White")
  print(paste0(NROW(df_age)-NROW(df_origin), " excluded as non-european ancestry"))
  print(paste0("Included are ", NROW(df_origin), " participants"))
  df_origin$year=year

  return(df_origin)
}




#incomeRecode=function(var){
#varOut=revalue(as.factor(as.numeric(var)), c(
#                                          "1" = "<18k","2" = "<18k","3" = "<18k","4" = "<18k","5" = "<18k","6" = "<18k","7" = "<18k","8" = "<18k","9" = "<18k","10"= "<18k",
#                                          "11"="18k-31k","12"="18k-31k", "13"="18k-31k","14"="18k-31k","15"="18k-31k",
#                                          "16"="31k-52k","17"="31k-52k","18"="31k-52k","19"="31k-52k","20"="31k-52k", 
#                                          "21"="52k-100k","22"="52k-100k", "23"="52k-100k","24"="52k-100k","25"="52k-100k",
#                                          "26"=">100k",  "27"=">100k",  "28"=">100k",  "29"=">100k",  "30"=">100k", "31"=">100k",
 #                                         "32"="not_shared",  "33"="not_shared", "96"="not_shared",  "97"="not_shared"))
#return(varOut)
#}


sexRecode=function(var){
varOut=revalue(as.factor(var), c("Male"="Men",
                                  "Female"="Women",
                                          "Refused/not obtained" = NA,
                                          "Schedule not obtained" = NA,
                                          "Schedule not applicable" = NA))


return(varOut)
}



checkVar=function(yearSel, dfOutList=list(), data){
  hse_year=subset(data, year==yearSel)
      for ( i in 1:NCOL(hse_year) ) {

        if(is.numeric(hse_year[,i])==TRUE){
          dfOut=data.frame(Freq=round( mean(hse_year[,i], na.rm=T),2) )
          dfOut$Var1=colnames(hse_year)[i]
          dfOut$measure="mean"
          } else {
          dfOut=as.data.frame(round(prop.table(table(as.factor(hse_year[,i]) )),3))
          dfOut$measure="prop"
          }
          dfOut$category=colnames(hse_year)[i]
          dfOut$missing_hse=sum(is.na(hse_year[,i]))
          dfOut$samplesAvail=NROW(hse_year[,i])
          dfOut$missing_prop=sum(is.na(hse_year[,i]))/ NROW(hse_year[,i])
          dfOutList[[i]]=data.frame(variable=dfOut$Var1, category =dfOut$category, cat=dfOut$measure, mean=dfOut$Freq,   samplesAvail=dfOut$samplesAvail,  missing= dfOut$missing_hse, missing_prop = dfOut$missing_prop )
          }
     dfOutDF=do.call(rbind, dfOutList)
     dfOutDF$year=yearSel
  return(dfOutDF)
}




library(crochet)
compareModel<- function(formula, 
                          data, 
                          method = "glm", 
                          i = seq_len(nrow(geno(data))), 
                          j = seq_len(ncol(geno(data))), 
                          chunkSize = NULL, 
                          nCores = getOption("mc.cores", 2L), 
                          weighting,
                          verbose = FALSE, 
                          binary = "no", ...) {


getCoefficients.glm <- function(x) {
    c(coef(summary(x))[2L, ], N=round(nobs(x), 0))
}


getResponse <- function(formula) {
    # Extract component from parse tree (see https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Language-objects)
    sym <- formula[[2L]]
    # Convert symbol to character
    as.character(sym)
}

i <- convertIndex(geno(data), i, "i")
j <- convertIndex(geno(data), j, "j")

FUN <- match.fun(method)
GWAS.model <- update(formula, ".~z+.")

print(paste0("Weighting applied: ", weighting))

OUT <- chunkedApply(X = geno(data), MARGIN = 2L, FUN = function(col, ...) {
            df <- pheno(data)[i, , drop = FALSE]
            df[["z"]] <- col
            if(weighting=="yes" | weighting=="weighted"){
              if(binary=="no"){
                print("Outcome type: Continuous")
                fm <- FUN(GWAS.model, data = df, weights=PS, ...)
              }
              if(binary=="yes"){
                  print("Outcome type: Binary")
                  fm <- FUN(GWAS.model, data = df, weights=PS,  family = binomial(logit), ...)
              }

            }
            if(weighting=="no" |  weighting=="unweighted"){
                if(binary=="no"){
                print("Outcome type: Continuous")
                fm <- FUN(GWAS.model, data = df, ...)
                }
                if(binary=="yes"){
                  print("Outcome type: Binary")
                  fm <- FUN(GWAS.model, data = df, family = binomial(logit), ...)
                  
                  GWAS.model.int <- update(glmFormula, ".~z*PS+.")
                  fw <- FUN(    GWAS.model.int , data = df, family = binomial(logit), ...)
                  fmOut=getCoefficients.glm(fm)
                  #outmod <- as.data.frame(t(fmOut))
                  #outmod$SNP = colnames(geno(data))[j]
                  #anova(fm, fw, test="F")[2L, ]
                  anova(fm, fw, test="F")$`Pr(>F)`
                  #[2L, ]
                  # $`Pr(>F)`[2]
                  #as.data.frame(pvalFtest)
                  #as.data.frame(t(pvalFtest))
                  #outmodFtest$SNP <- colnames(geno(data))[j]
                  #print(anova(fm, fw, test="F")[2L, ])
                  #merge(outmod,outmodFtest, by="SNP")
                  # 
                 # list(outmod, outmodFtest)
              }
            }
           # as.data.frame(outmodFtest)
            # 
             #compare.glm <- function(fm=fm, fw=fw) {
             #anova(fm,fw)[2L, ]
            #  }
           # compare.glm()

            #compare.glm <- function(x) {
            #  anova(x,x)[2L, ]
            #  }
            #compare.glm(fm)

        }, i = i, j = j, chunkSize = chunkSize, nCores = nCores, verbose = verbose, ...)

        OUT <- as.data.frame(t(OUT))
        OUT$V1 <- colnames(geno(data))[j]
        

    return(OUT)
}

trimPS=function(weight, l, u){
  design <- svydesign(ids = ~1, data = data.frame(weight = weight), 
        weights = ~weight)
  trimmed <- trimWeights(design, lower = l, upper = u, 
        strict = T)
   return(weights(trimmed))
}



extractPrev=function(ID){
  info=gwasinfo(id=ID) 
  print(info$trait)
  if("ncase" %in% colnames(info)){
     df=data.frame(ieugwasrID=ID, sample.prev=info$ncase/(info$ncontrol+info$ncase), N= info$ncontrol+info$ncase, coding="binary")
  } else {
  df=data.frame(ieugwasrID=ID, sample.prev=NA, N= info$sample_size, coding="nonbinary")
  }
  #print(head(df))
  return(df)
}

mungeData=function(name, clean="no"){
  if(file.exists(paste0(HOME, "/output/ldsc/", name, ".sumstats.gz"))==F | clean == "yes"){
  paste0("start munging for: ", name)
  setwd(paste0(HOME,"/output/ldsc"))

  munge(files = paste0(HOME, "/output/ldsc/", name),
        hm3 = paste0(HOME,"/data/w_hm3.snplist"),
        trait.names=name,
        info.filter = 0.9,
        maf.filter = 0.01)
  }
    paste0("Munging done for: ", name)
}


recodeGLM=function(var, model){
       print(var)
       modelSel=subset(model, grepl(var, model$variable)==T)
       modelSel$cat=var
       modelSel$level=str_replace(modelSel$variable, var, "")  
       modelOut=subset(modelSel, select=c(variable, cat, level))
       return(modelOut)
}




extractRegression=function(model, label, HSE_n, UKBB_n, group){
  
       #modelOut=as.data.frame(coef(beta(model)))
       modelOutDF=model
       colnames(modelOutDF)[colnames(modelOutDF) == "Estimate"] <- 'est'
       colnames(modelOutDF)[colnames(modelOutDF) == "Std. Error"] <- 'se'
       colnames(modelOutDF)[colnames(modelOutDF) == "Pr(>|z|)"] <- 'p'
       modelOutDF$uCI=modelOutDF$est + 1.96 * modelOutDF$se
       modelOutDF$lCI=modelOutDF$est - 1.96 * modelOutDF$se
       modelOutDF$label=label
       modelOutDF$group=group
       modelOutDF$HSE_n=HSE_n
       modelOutDF$UKBB_n=UKBB_n
       return(modelOutDF)
}


scaleVar=function(variable, df){
       print(variable)
       print(levels(as.factor(df[[variable]])))
       print("")
       varIn=as.numeric(as.factor(df[[variable]]))
       varScale=(varIn - mean(varIn, na.rm=T)) / sd(varIn, na.rm=T)
       dfOut=as.data.frame(varScale)
       colnames(dfOut)=variable
       return(dfOut)
}


addq <- function(x) paste0("`", x, "`")
library(qdap)


glmUni=function(variable, weights="no", weightInc=NULL, outcome, df, fam = gaussian){
              glmFormulaPheno <- as.formula(paste0("scale(as.numeric(", outcome, ")) ~ scale(as.numeric(", addq(variable), "))"))
              print(glmFormulaPheno)

       if(weights=="no"){
              regModelPheno <- glm(glmFormulaPheno, data = df, family=fam)
       } else{
         print("Weights included")
              design.ps <- svydesign(ids=~1,   weights = ~df[[weightInc]], data=df)
              regModelPheno=svyglm(glmFormulaPheno, design=design.ps)
              #regModelPheno <- glm(glmFormulaPheno, data = df, weights=df[[weightInc]], family=fam) 
              print("Regression done")
       }
       modNames=names(model.frame(regModelPheno, data = df))
       modNames=gsub("scale", "", modNames)
       modNames=gsub("as.numeric", "", modNames)
       #modNames=gsub("as.factor", "", modNames)
       modNames=gsub("))", "", modNames)
       modNames=str_replace(modNames, "[((]", "")
       modNames=str_replace(modNames, "[((]", "")


       regModelPhenoSum=as.data.frame(summary(regModelPheno)$coefficients)
       regModelPhenoSum$variable=rownames(regModelPhenoSum)
       regModelPhenoSumOut=subset(regModelPhenoSum, variable!="(Intercept)")
       intercept=subset(regModelPhenoSum, variable=="(Intercept)")$Estimate
       regModelPhenoSumOut$intercept=intercept

       recodeGLMdf=do.call(rbind, lapply(modNames[2], function(x) recodeGLM(x, regModelPhenoSumOut)))
       regModelPhenoSumOutM=merge(regModelPhenoSumOut,recodeGLMdf, by="variable", all.x=T )
       regModelPhenoSumOutM$cat=ifelse(is.na(regModelPhenoSumOutM$cat)==T, regModelPhenoSumOutM$variable, regModelPhenoSumOutM$cat)
       regModelPhenoSumOutM$level=ifelse(is.na(regModelPhenoSumOutM$level)==T, "", regModelPhenoSumOutM$level)

       return(regModelPhenoSumOutM)
}



# Read in SNP labels
readInrsID=function(chr){
        print(paste0("read in chromosome ", chr))
        print("SNPs obtained from QCd list (MAF, HWE, INFO)")
        rsID=bigreadr::fread2(paste0(UKBBgeno, "/genoQC/chr", chr, ".bim"), showProgress = FALSE)
        rsIDClean=data.frame(SNPlabel=paste0(rsID$V1, ":", rsID$V4, "_", rsID$V5, "_", rsID$V6), SNP=rsID$V2)
        return(rsIDClean)
} 

# process results from ldak
readInSNPs=function(chr, weight="", pheno){
    print(paste0("read in chromosome ", chr))
    progress=read.table(paste0(HOME, "/output/ldak/regressRes/", pheno, "_chr", chr ,weight, ".progress"), header=T)

    if(weight!=""){
        sandwich=fread(paste0(HOME, "/output/ldak/regressRes/", pheno, "_chr", chr ,weight,".alternatives"), header=T)
        chrIn=fread(paste0(HOME, "/output/ldak/regressRes/", pheno, "_chr", chr ,weight,".assoc"), header=T)
        chrDF=merge(chrIn, subset(sandwich, select=c(Predictor, Sandwich_Beta, Sandwich_SD, Sandwich_P, LRT_P)), by="Predictor", all.x=T)
        chrDF=subset(chrDF, select=c(Predictor, Chromosome, Basepair, A1, A2, Effect, SD, MAF, Sandwich_Beta, Sandwich_SD, Sandwich_P, LRT_P))
        colnames(chrDF)=c("SNPlabel","CHR", "BP" ,"A1", "A2", "BETA", "SE", "MAF", "BETA_sw", "SE_sw", "P_sw", "LRT_P")
    } else{
       chrIn=fread(paste0(HOME, "/output/ldak/regressRes/", pheno, "_chr", chr ,weight,".assoc"), header=T)
       chrDF=subset(chrIn, select=c(Predictor, Chromosome, Basepair, A1, A2, Effect, SD, MAF))
       colnames(chrDF)=c("SNPlabel", "CHR", "BP" ,  "A1", "A2", "BETA", "SE", "MAF")
       chrDF$BETA_sw=NA
       chrDF$SE_sw=NA
       chrDF$P_sw=NA
       chrDF$LRT_P=NA
    }

    sum=read.table(paste0(HOME, "/output/ldak/regressRes/", pheno, "_chr", chr ,weight, ".summaries"), header=T)
    addN=subset(sum, select=c(Predictor, n))
    chrDF$P=2*pnorm(abs(chrDF$BETA/chrDF$SE), lower.tail=FALSE)
     
    if(weight!=""){
        print("estimate effective sample size")
        nEFFdf= na.omit(data.frame(pheno=phenoFile[[pheno]], PS_w=phenoFile$PS_w, genoInc=phenoFile$genoInc ))
        addN$n=round((sum(nEFFdf$PS_w)^2)/sum(nEFFdf$PS_w^2),0)
     }

    rsLabel=snpNamesList[[chr]]

    if(NROW(rsLabel)-NROW(chrDF)==0){
        print("all SNPs processed")
    } else {
          print(paste0(NROW(rsLabel)-NROW(chrDF), " SNPs missing"))
    }
    GWAmerge=merge(chrDF, rsLabel, by="SNPlabel", all.y=T)
    print(paste0(NROW(chrDF)-NROW(GWAmerge), " SNPs removed after merging with QCd SNP IDs (MAF > 0.01, INFO > 0.9"))

    GWAmergeN=merge(GWAmerge, addN, by.x="SNPlabel", by.y="Predictor", all.x=T)
    GWAmergeOut=subset(GWAmergeN, select=c(SNPlabel, SNP, CHR, BP, A1, A2, BETA, SE, P, n, MAF, BETA_sw, SE_sw, P_sw, LRT_P))
    names(GWAmergeOut)[names(GWAmergeOut) == 'n'] <- 'N'
    print("Remove SNPs with MAF < 0.01 (after GWA in subset)")
    GWAmergeMAF=subset(GWAmergeOut, MAF > 0.01)

    if(weight==""){
    print(paste0("GWA sig hits: ", NROW(subset(GWAmergeMAF, P<5e-8))))
    } else {
      print(paste0("GWA sig hits: ", NROW(subset(GWAmergeMAF, P_sw<5e-8)), " (sandwich estimator) "))
          #print(paste0("GWA sig hits: ", NROW(subset(GWAmergeMAF, P<5e-8)), " (lm estimator) "))
    }
    print(paste0("SNPs available: ", NROW(GWAmergeMAF)))
    return(GWAmergeMAF)
}


# clump ldak output

clumpData=function(df, estimate, pvalSel=5e-8){
 


if(estimate=="_wt"){
    print("Weighted data")
    clump_input=subset(df, P_sw < pvalSel)
    dfFormat= format_data( clump_input, 
    type = "exposure",
    snp_col = "SNP", beta_col = "BETA_sw", se_col = "SE_sw",
    effect_allele_col = "A1", other_allele_col = "A2",
    pval_col = "P_sw",
    samplesize_col = "N",
    min_pval = 1,
    chr_col = "CHR", pos_col = "BP")
} else{
    print("Unweighted data")
    clump_input=subset(df, P< pvalSel)
    dfFormat= format_data( clump_input, 
    type = "exposure",
    snp_col = "SNP", beta_col = "BETA", se_col = "SE",
    effect_allele_col = "A1", other_allele_col = "A2",
    pval_col = "P",
    samplesize_col = "N",
    min_pval = 1,
    chr_col = "CHR", pos_col = "BP")
}
 
    
    print(paste0("Number of GWA sig included: ",  NROW(dfFormat)))

    dfClump=clump_data(dfFormat,
    clump_kb = 10000, clump_r2 = 0.001,
    clump_p1 = 1,clump_p2 = 1)

    
    print(paste0("Number of hits after clumping: ",  NROW(dfClump)))
    dfClumpSel=subset(clump_input, SNP %in% unique(  dfClump$SNP))

    dfClumpSel$gwaSig="yes"
    colnames(dfClumpSel)=paste0(colnames(dfClumpSel), estimate)
    return(dfClumpSel)
}





evalReg=function(formula, subsetN=3000, data=UKBBHSE, IPSWNorm="yes"){
  print(formula)
  if(subsetN>0){
  HSEsub=subset(data, sampleName=="HSE")
  UKBBsubsel=subset(data, sampleName=="UKBB")
  UKBBsub=sample_n(UKBBsubsel, subsetN)
  UKBBHSEsub=rbind(HSEsub, UKBBsub)
  } else{
       UKBBHSEsub=data  
        HSEsub=subset(data, sampleName=="HSE")
  }

  psmodel = glm(formula, 
                data = UKBBHSEsub,
                family = binomial(link = "logit"),
                weights = weight_individual) 
  print("Calculate design weights (propensity score model)")
  print(coef(summary(psmodel)))
  
  UKBBHSEsub$probs = predict(psmodel, type="response")
  
  UKBBHSEsub$IPSW_raw=(1-UKBBHSEsub$probs)/UKBBHSEsub$probs
  UKBBHSEsub$PS_raw=1-UKBBHSEsub$probs
  UKBBHSEsub$IP_raw=1/UKBBHSEsub$probs
  
  UKBBHSEsub$IPSWNorm=c(rep(1, NROW(HSEsub)), normalizeWeights(subset(UKBBHSEsub, sampleName=="UKBB")$IPSW_raw))
  UKBBHSEsub$PSNorm=c(rep(1, NROW(HSEsub)), normalizeWeights(subset(UKBBHSEsub, sampleName=="UKBB")$PS_raw))
  UKBBHSEsub$IPNorm=c(rep(1, NROW(HSEsub)), normalizeWeights(subset(UKBBHSEsub, sampleName=="UKBB")$IP_raw))
  
  getN=function(weight){
       N=round((sum(weight)^2)/sum(weight^2),0)
       return(N)
  }

  sumPSmod=data.frame(typePS=c("IPSWNorm", "PSNorm", "IPNorm"), minWeight=NA, maxWeight=NA)
  sumPSmod$minWeight=c(min(UKBBHSEsub$IPSWNorm), min(UKBBHSEsub$PSNorm), min(UKBBHSEsub$IPNorm))
  sumPSmod$maxWeight=c(max(UKBBHSEsub$IPSWNorm), max(UKBBHSEsub$PSNorm), max(UKBBHSEsub$IPNorm))
  sumPSmod$nEFF=c(getN(UKBBHSEsub$IPSWNorm), getN(UKBBHSEsub$PSNorm), getN(UKBBHSEsub$IPNorm))

  if(IPSWNorm=="yes"){
      IPSWNormElim=modelPerform(weightGen="IPSWNorm", data=UKBBHSEsub)
      sumPSmod$predEliminated=c(IPSWNormElim$predEliminated)
      sumPSmod$maxEst=c(IPSWNormElim$maxEst)
  } else{
    IPSWNormElim=modelPerform(weightGen="IPSWNorm", data=UKBBHSEsub)
    PSNormElim=modelPerform(weightGen="PSNorm", data=UKBBHSEsub)
    IPWNormElim=modelPerform(weightGen="IPNorm", data=UKBBHSEsub)
    sumPSmod$predEliminated=c(IPSWNormElim$predEliminated, PSNormElim$predEliminated, IPWNormElim$predEliminated)
    sumPSmod$maxEst=c(IPSWNormElim$maxEst, PSNormElim$maxEst, IPWNormElim$maxEst)
  }
  sumPSmod$formula=formula

  print(  head(sumPSmod))
  return(sumPSmod)
}



modelPerform=function(weightGen, data){
   NormModelList=lapply(colDescPS, function(x) glmUni(x, outcome="sample", df=data, weights="yes", weightInc=weightGen, fam=gaussian))
   NormModel=do.call(rbind, NormModelList)
   print(NormModel)
   maxEst=max( abs(NormModel$Estimate))
   predEliminated=NROW(NormModel)-NROW(subset(NormModel, `Pr(>|t|)` <0.05))
   lmOut=data.frame(predEliminated, maxEst)
  return(lmOut)
}


cleanString=function(df, label){
        df$labelShort=df$cat
        df$levelDesc=df$cat
for ( i in 1:NROW(label) ) {
       df$labelShort=ifelse(str_detect(df$cat, label[i])==T, label[i],   df$labelShort)
       df$levelDesc=ifelse(str_detect(df$cat, label[i])==T, gsub(paste0(label[i], "_"), "", df$cat),   df$levelDesc)
}

df$levelDesc=gsub(paste0("`"), "", df$levelDesc)
df$levelDesc=gsub(paste0("_"), " ", df$levelDesc)
return(df)
}





selectCensus=function(df, vars){
  
  varSelect=vars$censusDat
  df_sel <- df[,   varSelect ]
  colnames(df_sel)=vars$labelUKBB_recoded
  head(df_sel)
  print("clean ancestry")
  labels = as.data.frame(  df[["ethnicityew"]] %>% attr('labels'))
  labelsDF=data.frame(num=labels[,1], label=rownames(labels))
  df[["ethnicityew"]]=labelsDF$label[ match(df[["ethnicityew"]],labelsDF$num) ]
  
  labels = as.data.frame(  df[["country"]] %>% attr('labels'))
  labelsDF=data.frame(num=labels[,1], label=rownames(labels))
  df[["country"]]=labelsDF$label[ match(df[["country"]],labelsDF$num) ]

  library(sjlabelled)
  
  for ( i in 1:NROW(vars) ) {
    print(vars$labelUKBB_recoded[i])
    if(vars$codedCensus[i]=="con"){
      print("Continuous")
      df_sel[,i]=as.numeric(  df_sel[,i])
      print(summary( df_sel[,i]))
    }
    if(vars$codedCensus[i]=="cat"){
      print("Categorical")
      labels = as.data.frame(  df_sel[,i] %>% attr('labels'))
      #labelsDF=data.frame(num=labels[,1], label=rownames(labels))
      labelsDF=data.frame(num=labels[,1], label=get_labels(df_sel[,i]))
      df_sel[,i]=labelsDF$label[ match(df_sel[,i],labelsDF$num) ]
      df_sel[,i]=as.factor(  df_sel[,i])
      print(table( df_sel[,i]))
    }
  }
  
  df_sel$ancestry=df[["ethnicityew"]]
  df_sel$country=df[["country"]]
  return(df_sel)
}


recodeBMI=function(variable){
    newVar=ifelse(variable<18.5,"underweight", NA )
    newVar=ifelse(variable>=18.5 & variable<25,"healthyweight", newVar )
    newVar=ifelse(variable>=25 & variable<30, "overweight", newVar )
    newVar=ifelse(variable>=30, "obese", newVar )
    newVar <- factor(newVar, levels = c("underweight", "healthyweight", "overweight","obese"))
  return(as.factor(newVar))
}


recodeAge=function(variable){
    newVar=ifelse(variable>=40 & variable<=44,"40-44", NA )
    newVar=ifelse(variable>=45 & variable<=49,"45-49", newVar )
    newVar=ifelse(variable>=50 & variable<=54, "50-54", newVar )
    newVar=ifelse(variable>=55 & variable<=59,  "55-59", newVar )
    newVar=ifelse(variable>=60 & variable<=64, "60-64", newVar )
    newVar=ifelse(variable>=65 & variable<=69, "65-69", newVar )
    newVar <- factor(newVar, levels = c("40-44", "45-49", "50-54","55-59","60-64","65-69" ))
  return(as.factor(newVar))
}


recodeEducation=function(variable){
varOut=revalue(as.factor(variable), c("other_none"="other_none",
                                      "highschool_diploma"="highschool_diploma_uni",
                                          "college_or_university_degree" = "highschool_diploma_uni"))
  return(as.factor(varOut))
}

recodeEducationAge=function(variable){
varOut=revalue(as.factor(variable), c("No academic or professional qualifications"="7", # 7 for no degree (assuming primary school), 
                                      "Level 1 (0-4 GCSE, O level, or equivalents)"="10", # # 10 for a level 1 or level 2 degree,
                                          "Level 2 (5+ GCSE, O level, 1 A level, or equivalents)" = "10",
                                          "Level 3 (2+ A levels, or equivalents)" = "13", # 13 for a level 3 degree
                                          "Level 4+ (degree, postgrad, professional quals)" = "19", #  20 for a level 4+ degree
                                          "Apprenticeship" = "12", # We assigned 12 years to the “apprenticeship” (reflecting the fact that it requires continuing one’s education after GCSEs, but does not require an A/AS-level degree) 
                                          "Other (vocational/foreign/outside UK quals)" = "15"))
varOut <- factor(varOut, levels = c("7", "10", "13", "12", "15","19"))
return(as.factor(varOut))
}


# function for ldscore
h2ldsc=function(h2df, listJK=list()){
for ( i in 1:NROW(h2df) ) {
  print("Estimate heritability")
  setwd( paste0(HOME, "/output/ldsc/"))
  blocks=200
  ldsch2=NULL
  ldsch2=ldsc(traits=rep(paste0("ldak_", h2df$pheno[i], ".sumstats.gz"), 2),
              #sample.prev= rep(as.numeric(h2df$sample.prev[i] ), 2),
              sample.prev=c(NA,NA),
              population.prev=c(NA,NA),
              #population.prev=rep(as.numeric(h2df$population.prev[i] ),2) ,
                           ld=h2df$w[i],
                        wld=h2df$w[i],
              trait.names = rep(h2df$pheno[i], 2),
              stand=T,
              n.blocks=blocks)


  H2=ldsch2[["S"]]
  ldsch2[["S"]]
  SE=NULL
  SE<-matrix(0, nrow(ldsch2[["S"]]), nrow(ldsch2[["S"]]))
  SE[lower.tri(SE,diag=TRUE)] <-sqrt(diag(ldsch2[["V"]]))
  INT=ldsch2[["I"]]
  h2df$h2[i]=H2[1,1]
  h2df$h2_se[i]=SE[1,1]
  h2df$I[i]=INT[1,1]
  h2df$N=ldsch2[["N"]][1]



  ldscJK=ldscMod(traits=rep(paste0("ldak_", h2df$pheno[i], ".sumstats.gz"), 2),
              #sample.prev= rep(as.numeric(h2df$sample.prev[i] ), 2),
              sample.prev=c(NA,NA),
              population.prev=c(NA,NA),
              #population.prev=rep(as.numeric(h2df$population.prev[i] ),2) ,
                           ld=h2df$w[i],
                        wld=h2df$w[i],
              trait.names = rep(h2df$pheno[i], 2),
              stand=T,
              n.blocks=blocks)

jackknife.cov <- cov(ldscJK$jackknife)/200 
JK.se <- sqrt(diag(jackknife.cov))
JK.est <- t(as.matrix(colMeans(ldscJK$jackknife))) # means per column: column 1=used for h2, column 2: intercept
h2df$JK.intercept=JK.est[1,2] # intercept estimates
h2df$JK.intercept_se=JK.se[2]
h2df$JK.est.raw=JK.est[1] # estimate used to derive h2
# h2 estimate and se (STEP 2)
# estimate of h2, regression slope scaled by M and N
h2df$JK.h2 = ( ldscJK$m * JK.est.raw / ldscJK$N[1])

 listJK[[i]]=data.frame(pheno=h2df$pheno[i], JK=ldscJK$jackknife[,1])

}
return(  list(h2df, listJK))
}





extractJK=function(data, model){

   dataJK=data[["JKList"]]
   names(dataJK)=model
    corMat=matrix(nrow=length(phenoIn), ncol=length(phenoIn))
   colnames(corMat)=model
    rownames(corMat)=model
  for ( i in 1:NROW(model) ) {
  }

for (i in 1:length(model)) {
  
   est1=JKlist[[model[[i]]]][,1]
  for (j in 1:length(model)) {
     est2=JKlist[[model[[j]]]][,1]

     corOut=cor.test(est1, est2)
  }

  

  str(data)
  jackknife.cov <- cov(jackknife)/200 
  JK.se <- sqrt(diag(jackknife.cov))
  JK.est <- t(as.matrix(colMeans(ldscJK$jackknife))) # means per column: column 1=used for h2, column 2: intercept
  JK.intercept=JK.est[1,2] # intercept estimates
  JK.intercept_se=JK.se[2]
  JK.est.raw=JK.est[1] # estimate used to derive h2
# h2 estimate and se (STEP 2)
# estimate of h2, regression slope scaled by M and N
  JK.h2 = ( m * JK.est.raw / ldscJK$N[1])

}

}































ldscLDAK=function(model, est="n", prevDat, jkIt=NULL){

if(est=="w"){
  print("Use corrected LD scores")
  w=paste0(HOME,"/data/ldscUKBB_M/") # ldscUKBB_M
  wld=w
  sample.prev=prevDat$population.prev
  population.prev=prevDat$population.prev
  estimation="weighted"
}
if(est=="w_100"){
  print("Use 1000 genome reference panel")
  w=paste0(HOME,"/data/eur_w_ld_chr/")
  wld=w
  sample.prev=prevDat$population.prev
  population.prev=prevDat$population.prev
  estimation="weighted_1000"
}
if(est=="n"){ # use standard ldsc
   w=paste0(HOME,"/data/eur_w_ld_chr/")
   wld=w
  sample.prev=prevDat$sample.prev
  population.prev=prevDat$population.prev
    estimation="unweighted"
}
if(est=="jk_n"){
    print(paste0("Jackknife block (unweighted) number ", jkIt))
    w=paste0(HOME,"/data/eur_w_ld_chr/JK/", jkIt)
    wld=w
    sample.prev=prevDat$sample.prev
    population.prev=prevDat$population.prev
    estimation="unweighted"
}
if(est=="jk_w"){
    print(paste0("Jackknife block (weighted) number ", jkIt))
    w=paste0(HOME,"/data/ldscUKBB_M/JK/", jkIt) # ldscUKBB_M
    wld=w
    sample.prev=prevDat$population.prev
    population.prev=prevDat$population.prev
    estimation="weighted"
}
setwd(paste0(HOME, "/output/ldsc/"))
ldscReg=ldsc(traits=paste0(model, ".sumstats.gz"),
              #sample.prev= c(as.numeric(h2df$sample.prev2[i] ), as.numeric(h2df$sample.prev2[i] ) ),
              sample.prev= NA,
              population.prev=NA,
              #population.prev=rep(as.numeric(h2df$population.prev[i] ),2) ,
                           ld= w,
                        wld= wld,
              trait.names = model,
              stand=T)

  print("Extract hertiability estimates ")
  h2=diag(ldscReg[["S"]])
  SE=NULL
  SE<-matrix(0, nrow(ldscReg[["S"]]), nrow(ldscReg[["S"]]))
  SE[lower.tri(SE,diag=TRUE)] <-sqrt(diag(ldscReg[["V"]]))
  h2_se=diag(SE)
  int=diag(ldscReg[["I"]])
  mat <- matrix(0, nrow = length(h2), ncol = length(h2))
  mat[lower.tri(mat, diag = TRUE)] <- ldscReg[["N"]]
  n=diag(mat)

# convert to liability scale
zv <- dnorm(qnorm(population.prev))
h2_liab <- h2 * population.prev^2 * ( 1 - population.prev)^2 / sample.prev / (1-sample.prev) / zv^2
h2_se_liab <- sqrt(( h2_se * population.prev^2 * ( 1 - population.prev)^2 / sample.prev / (1-sample.prev) / zv^2) ^2)

h2DF1=data.frame(pheno=model, h2=h2, h2_se=h2_se, I=int, N=n, sample.prev, population.prev, scale="observed", model=estimation)
h2DF2=data.frame(pheno=model, h2=h2_liab, h2_se=h2_se_liab, I=int, N=n, sample.prev, population.prev, scale="liability", model=estimation)
h2DF=rbind(h2DF1, h2DF2)


  print("Get ldscore regression estimates ")
  k<-nrow(ldscReg[["S_Stand"]])
  SE<-matrix(0, k, k)
  SE[lower.tri(SE,diag=TRUE)] <-sqrt(diag(ldscReg[["V_Stand"]]) )
  rg_se=SE

for ( i in 1:NROW(rg_se) ) {
  rg_se[i,]=rg_se[,i]
}
  rg=ldscReg[["S_Stand"]]

#ldscJK=ldscMod(traits=paste0("ldak_", model, ".sumstats.gz"),
#              #sample.prev= c(as.numeric(h2df$sample.prev2[i] ), as.numeric(h2df$sample.prev2[i] ) ),
#              sample.prev=c(NA,NA),
#              population.prev=c(NA,NA),
 #             #population.prev=rep(as.numeric(h2df$population.prev[i] ),2) ,
 #                          ld= w,
 #                       wld= w,
 #             trait.names = model,
 ##             stand=T)

#JKlist=ldscJK[["JKList"]]
#names(JKlist)=model

rgOut=list(rg,rg_se,h2DF)
names(rgOut)=c("rg", "rg_se", "h2")
str(rgOut)
return(rgOut)
}




selectPrev=function(variable, w="no"){
  finGen=subset(downloadSS, UKBB==variable)
  finGen$sample.prev= as.numeric(finGen$cases)/ as.numeric(finGen$Ntot)
  finGen$population.prev=finGen$sample.prev
  
  UKBB=subset(gwaDat, recodedName==variable)

  if(w=="no"){
  dfOUT=data.frame(model=c( paste0("ldak_", UKBB$recodedName), finGen$fileName),
             sample.prev=c(UKBB$sample.prev,  finGen$sample.prev),
             population.prev=c(UKBB$population.prev,  finGen$population.prev))
  } else{
        dfOUT=data.frame(model=c(paste0("ldak_", UKBB$recodedName, "_weighted"), finGen$fileName),
             sample.prev=c(UKBB$sample.prev,  finGen$sample.prev),
             population.prev=c(UKBB$population.prev,  finGen$population.prev))
  }
  
  return(dfOUT)
}



extractRG=function(x){
 dfOut=data.frame(ldakPheno= colnames( x$rg)[1], finnGen= colnames( x$rg)[2], rg=  x$rg[2,1], rg_se=  x$rg_se[2,1], ref= x$h2$model[1])
 dfOut$h2_finnGen= x$h2$h2[4]
 dfOut$h2_ldak=ifelse(is.na( x$h2$h2[3])==T,  x$h2$h2[1], x$h2$h2[3])
 
 return(dfOut)
}



set.seed(123) # set a seed for reproducibility
#library("biglasso")
#library("extraTrees")
library("glmnet")
library("WeightIt")
library(SuperLearner)

# create dummy variables


createDummy=function(df, varInc, return="data"){
  library('fastDummies')
  dummyCol=as.data.frame(subset(variableList, levelsUKBB!="continuous" & includePrediction=="yes" & labelUKBB_recoded!="education"))$recodedName
  dummyCol <- unique(varInc[varInc %in% dummyCol])
  nonDummyInc <- unique(varInc[!varInc %in% dummyCol])
  UKBBHSEDummyIn=subset(df, select=c("eid",dummyCol))

  print("Separate binary from categorical")
  colLevels=UKBBHSEDummyIn %>%  sapply(levels)
  colLevelsDF=as.data.frame(do.call(rbind, lapply(colLevels, function(x) NROW(x))))
  colLevelsDF$variable=rownames(colLevelsDF)

  colLevelsBinary=subset(colLevelsDF, V1==2)
  colLevelsCategorical=subset(colLevelsDF, V1>2)
  UKBBHSEDummyBinary <- dummy_cols(UKBBHSEDummyIn, select_columns = colLevelsBinary$variable, remove_first_dummy = TRUE, ignore_na=TRUE)
  UKBBHSEDummyCat <- dummy_cols(UKBBHSEDummyIn, select_columns = colLevelsCategorical$variable, remove_first_dummy = FALSE, ignore_na=TRUE)
  UKBBHSEDummy=merge(UKBBHSEDummyBinary, UKBBHSEDummyCat, by="eid", all.x=T, sort=F, suffixes=c("", ".rem"))


  #UKBBHSEDummy <- dummy_cols(UKBBHSEDummy, select_columns = dummyCol, remove_first_dummy = FALSE, ignore_na=TRUE)
  nonDummyIncRegCat <- unique(colnames(UKBBHSEDummy)[!colnames(UKBBHSEDummy) %in% "eid"])
  nonDummyIncRegCat <- unique(nonDummyIncRegCat[!nonDummyIncRegCat %in% dummyCol])
  remVar=data.frame(varCheck=nonDummyIncRegCat)
  remVar$remove=ifelse(grepl('.rem', remVar$varCheck)==T, "remove", remVar$varCheck)
  nonDummyIncRegCat=subset(remVar, remove!="remove")$varCheck
  nonDummyIncRegCon <- unique(varInc[!varInc %in% dummyCol])
  nonDummyIncReg=c(nonDummyIncRegCon, nonDummyIncRegCat)
  UKBBHSED=merge(df, UKBBHSEDummy, by="eid", all.x=T, sort=F, suffixes=c("", ".rem2"))

if(return=="data"){
  return(UKBBHSED)
}
if(return=="variable"){
  return(nonDummyIncReg)
}
}



createCV=function(df, n = 5, crossValList=list()){
  nr <- nrow(df)
  splitData=split(df, sample(1:n, nrow(df), replace=T))

  for ( i in 1:n)  {
    print(paste0("Prop HSE/UKBB in full sample: ",   prop.table(table(df$sample))))
    print(paste0("Prop HSE/UKBB in validation sample: ",      prop.table(table( splitData[[i]]$sampleName)) ))

    dfValidate=splitData[[i]]
    dfTrain=subset(df, !eid %in% dfValidate$eid)
    crossValList[[i]]=list(dfTrain, dfValidate)
    names(crossValList[[i]])=c("train", "validate")
  }
  return(crossValList)
}


checkMissDat=function(var, data){
  print(var)
  print(table(is.na(data[[var]])))
}


# GENERATE WEIHTS
getWeights=function(varInc, return="data", validation="all", data=NULL, listCV=NULL){

print("Start regression")

if(validation=="validation"){
data=listCV[["train"]]
#data=sample_n(data, 3000)
print(paste0("Number of individuals included in training model: ", NROW(data)))
validate=listCV[["validate"]]
#validate=sample_n(data, 3000)
print(paste0("Number of individuals included in validation model: ", NROW(validate)))

}

#varInc=colDescIncludedPropensity
#varInc=c("sex", "age", "age_cat", "education_age", "income")

print("Run interaction model")
  #formulaGLM=as.formula(paste0("sample ~ ", paste0(varInc, collapse=" * ")))
  # remove continuous

start_time <- Sys.time()
library('fastDummies')
#data=sample_n(data, 20000)
UKBBHSED=createDummy(df=data, varInc, return="data")
nonDummyIncReg=createDummy(df=data, varInc, return="variable")


saveRDS(nonDummyIncReg , paste0(HOME,"/results/rds/varsModel.rds"))
uploadDropbox(file="varsModel.rds", folder="rds")

print("Number of UKBB/HSE individuals")
print(table(UKBBHSED$sampleName))


print("Check missing data per dummy variable")
checkMIss=lapply(nonDummyIncReg, function(x) checkMissDat(x, data=UKBBHSED))

#UKBBHSED=sample_n(UKBBHSED, 20000)
y = as.factor(UKBBHSED$sample)
f <- as.formula( ~ .*.)
#f <- as.formula( ~ .)
x <- model.matrix(f,UKBBHSED[, nonDummyIncReg])
#saveRDS(UKBBHSED, "origRDS.rds")

print(paste0("Variables included in the model: ", NROW(colnames(x)) ))
w=UKBBHSED$weight_individual
varModel=data.frame(varModelIn=colnames(x))
varModel=subset(varModel, varModelIn!="(Intercept)")

# increasing the foldes increases the trainsing data
set.seed(1234)
cvfit = cv.glmnet(x, y, # we need to find a model configuration, and thus the value of λ, which minimizes the the residuals (difference between observations and prediction). The glmnet package ships with a build-in cross-validation function. The function cv.glmnet() performs by default 10-fold cross-validation
    family = "binomial", 
    type.measure = "class" ,
    nfolds = 5,
    weights=w,
    nlambda=100,
    parallel = F)

if(validation=="all"){
  print("Upload glmnet object")
  #saveRDS(cvfit , paste0(HOME,"/results/rds/glmnetW.rds"))
  #uploadDropbox(file="glmnetW.rds", folder="rds")


  #save(cvfit, file=paste0(HOME,"/results/rds/glmnetW.RData"))
  #uploadDropbox(file="glmnetW.RData", folder="rds")

  save(cvfit, file=paste0(HOME,"/results/rds/cvfit.rda"))
  uploadDropbox(file="cvfit.rda", folder="rds") # has to be the same name as the glmnet object

  

}

coef_names_GLMnet <- coef(cvfit, s="lambda.min")
keepVars=row.names(coef_names_GLMnet)[coef_names_GLMnet@i+1]
effectVar=coef_names_GLMnet@x
LassoOut=data.frame(keepVars=keepVars, effectVar=effectVar)
head(varModel)


varModelOut=merge(varModel, LassoOut, by.x="varModelIn", by.y="keepVars", all.x=T)
varModelOut=varModelOut[order(varModelOut$effectVar, decreasing = TRUE), ] 

saveRDS(varModelOut , paste0(HOME,"/results/rds/varSelection_", validation, ".rds"))
uploadDropbox(file=paste0("varSelection_", validation, ".rds"), folder="rds")


print(paste0("Non-dummay variables: ", NROW(varInc), ", all variables: ",NROW(nonDummyIncReg),". Among ", NROW(rownames(varModel)), " (main and interaction terms), ", NROW(LassoOut), " are selected"))

print("Lasso estimates")
print(LassoOut)





if(validation=="validation"){
  UKBBHSEDvalidate=createDummy(df=validate, varInc, return="data")
  print("Get predicted values in test dataset")
  xVal <- model.matrix(f,UKBBHSEDvalidate[, nonDummyIncReg])
  #UKBBHSEDvalidate$probs <- predict(cvfit, xVal, s = "lambda.min", type = "response")[,1]
  UKBBHSEDvalidate$probs <- predict(cvfit,newx=xVal, s="lambda.min", type = "response")[,1]
}

if(validation=="all"){
  print("Get predicted values for the whole sample")
  UKBBHSEDvalidate=UKBBHSED
  UKBBHSEDvalidate$probs <- predict(cvfit, newx=x, s="lambda.min", type = "response")[,1]
  
  # Reload glmnet to get coef
  library(glmnet) 
  print("Generate rep weights from glmnet object")
  rm(cvfit)
  load(paste0(HOME,"/results/rds/cvfit.rda"))
  UKBBHSEDvalidate$probs_rep <- predict(cvfit, newx=x, s="lambda.min", type = "response")[,1]
  UKBBHSEDvalidateExp=subset(UKBBHSEDvalidate, sampleName=="UKBB")
  saveRDS(subset(UKBBHSEDvalidateExp, select=c("eid", "probs", "probs_rep")), paste0(HOME, "/results/rds/propsRep.rds"))
  uploadDropbox(file="propsRep.rds", folder="rds")

}

UKBBHSEDvalidate$IPSW=(1-UKBBHSEDvalidate$probs)/UKBBHSEDvalidate$probs
#UKBBHSE$IPSW=1/UKBBHSE$probs
UKBBHSEDvalidate$IPSWNorm=c(normalizeWeights(subset(UKBBHSEDvalidate, sampleName=="HSE")$IPSW), normalizeWeights(subset(UKBBHSEDvalidate, sampleName=="UKBB")$IPSW))
UKBBHSEDvalidate$propensity.weight.normalized=ifelse(UKBBHSEDvalidate$sampleName=="HSE", UKBBHSEDvalidate$weight_individual, UKBBHSEDvalidate$IPSWNorm)

# Effective sample size from superlearner
UKBBEffn=subset(UKBBHSEDvalidate, sampleName=="UKBB")
UKBBHSEDvalidate$nEFF=(sum(UKBBEffn$propensity.weight.normalized)^2)/sum(UKBBEffn$propensity.weight.normalized^2)

print("Effective sample size")
print(UKBBHSEDvalidate$nEFF[1])
UKBBHSEDvalidate$nVarKept=NROW(LassoOut)
UKBBHSEDvalidate$ID=paste0(UKBBHSEDvalidate$eid, "_", UKBBHSEDvalidate$sampleName)

end_time <- Sys.time()
print(paste0("Running time: ", end_time - start_time))

if(return=="data"){
       print("Return data.frame")
       return(UKBBHSEDvalidate)
}
if(return=="weights"){
       print("Return weights")
       return(UKBBHSEDvalidate$propensity.weight.normalized)
}
}




# ============ CORRELATIONS BETWEEN PHENOTYPES
library(jtools )
library(survey )
library(reshape2)
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}



nearestN=function(listCV){

print("Start iteration")
       set.seed(1234) 
       library(caret)
       train=listCV[["train"]]
       print(paste0(NROW(train), " individuals included in training dataset"))
       #train=sample_n(train, 1000)
       train$sample =as.factor(train$sample)
       test=listCV[["validate"]]
       test$sample =as.factor(test$sample)
       print(paste0(NROW(test), " individuals included in testing dataset"))

       #test=sample_n(test, 1000)
print("Run KNN on training dataset")
knnFit <- train(as.formula(paste0("sample ~ ", paste0(varInc, collapse=" + "))),
               data = train, 
               method = "knn",
               preProcess = c("center","scale"),
               trControl = trainControl("cv", number = 10) )

print("Predict on new test set")
knnPred=predict(knnFit, type = "prob", test)
outProb=data.frame(probs=knnPred["1"])
test$probs=outProb$X1
test$ID=paste0(test$eid, "_", test$sampleName)

print("Done with iteration")
return(subset(test, select=c(ID, probs)))
#return(subset(train, select=c(eid, probs)))

}


getCorrs=function(data, weights, vars, sample, weighting=""){

if(sample=="UKBB"){
       datAll=UKBBall
       nEFF=data$nEFF[1]
}
if(sample=="HSE"){
       datAll=HSEall
        nEFF=NROW(data)

}
if(weighting==""){
   data[[weights]]=1
   nEFF=NROW(data)
}

# Create survey design object
datSel <- data[data$sampleName %in% sample, ]
#dataDummy=createDummy(df=datSel, varInc, return="data")
#nonDummyIncReg=createDummy(df=datSel, varInc, return="variable")

datSel=subset(datSel, select=c("ID", weights))

datCor=merge(datAll, datSel, by="ID", all.x=T, sort=F, suffixes=c("", ".x"))
library('fastDummies')

datCorcomplete <-  datCor[complete.cases(datCor[[weights]]),]
datCorcomplete=subset(datCorcomplete, select=c(vars, weights))

fcor=as.formula(paste0(" ~ ", paste0("as.numeric(",vars, ")", collapse=" + ")))
designW <- svydesign(ids=~1,   weights = ~datCorcomplete[[weights]], data=datCorcomplete)
cor_w=svycor(fcor, design = designW, na.rm = T)$cors
cor_w_tri <- get_lower_tri(cor_w)
colnames(cor_w_tri)=vars
rownames(cor_w_tri)=vars
cor_wDF=reshape2::melt(as.matrix(cor_w_tri), na.rm = TRUE)

names(cor_wDF)[names(cor_wDF) == 'Var1'] <- 'p1'
names(cor_wDF)[names(cor_wDF) == 'Var2'] <- 'p2'
cor_wDF$ID=paste0(cor_wDF$p1, "_", cor_wDF$p2)
names(cor_wDF)[names(cor_wDF) == 'value'] <- paste0("cor",sample ,weighting)
cor_wDF$nEFF=nEFF
names(cor_wDF)[names(cor_wDF) == 'nEFF'] <- paste0("nEFF_",sample ,weighting)

cor_wDF=subset(cor_wDF, p1!=weights & p2!=weights)
cor_wDF=subset(cor_wDF, p1!=p2)
head(cor_wDF)


return(cor_wDF)
}




weightedDist=function(data, method){
       print("Create dummy variables")
       library('fastDummies')

       dataDummy=createDummy(df=data, varInc, return="data")
       nonDummyIncReg=createDummy(df=data, varInc, return="variable")
       print("Check if predictors are no longer signidicant in weighted model")
       # unweighted - all
       #x=nonDummyIncReg[1]
       psmodelunweightedList=lapply(nonDummyIncReg, function(x) glmUni(x, outcome="sample", df=dataDummy, weights="yes", weightInc="weight_individual", fam=gaussian))
       psmodelunweighted=do.call(rbind, psmodelunweightedList)
       psmodelunweighted=cleanString(df=psmodelunweighted, label=nonDummyIncReg)

       # weighted - all
       psmodelweightedList=lapply(nonDummyIncReg, function(x) glmUni(x, outcome="sample", df=dataDummy, weights="yes", weightInc="propensity.weight.normalized", fam=gaussian))
       psmodelweighted=do.call(rbind, psmodelweightedList)
       psmodelweighted=cleanString(df=psmodelweighted, label=nonDummyIncReg)
      
      print("Sex specific analysis")
      regIncSex <- nonDummyIncReg[!nonDummyIncReg %in% c("sex_female")]
      #  male
      psmodelMaleList=lapply(regIncSex, function(x) glmUni(x, outcome="sample", df=subset(dataDummy, sex_female=="0"), weights="yes", weightInc="weight_individual", fam=gaussian))
      psmodelMale=do.call(rbind, psmodelMaleList)
      psmodelMale=cleanString(df=psmodelMale, label=nonDummyIncReg)
      
      #  femalse
      psmodelFemaleList=lapply(regIncSex, function(x) glmUni(x, outcome="sample", df=subset(dataDummy, sex_female=="1"), weights="yes", weightInc="weight_individual", fam=gaussian))
      psmodelFemale=do.call(rbind, psmodelFemaleList)
      psmodelFemale=cleanString(df=psmodelFemale, label=nonDummyIncReg)


       print("Unweighted model")
       ps_Extract=extractRegression(model=psmodelunweighted, 
                                 label="unweighted", 
                                 UKBB_n= NROW(subset(data, sampleName=="UKBB")), 
                                 HSE_n= NROW(subset(data, sampleName=="HSE")),
                                 group="all")
                                 
       print("Weighted model")
       psW_Extract=extractRegression(model=psmodelweighted, 
                                 label="weighted", 
                                 UKBB_n= NROW(subset(data, sampleName=="UKBB")), 
                                 HSE_n= NROW(subset(data, sampleName=="HSE")),
                                 group="all")
       
      print("Unweighted model - male")
      ps_ExtractMale=extractRegression(model=psmodelMale, 
                                 label="unweighted", 
                                 UKBB_n= NROW(subset(UKBBHSE, sampleName=="UKBB" & sex=="male")), 
                                 HSE_n= NROW(subset(UKBBHSE, sampleName=="HSE" & sex=="male")),
                                 group="male")
                                 
      print("Unweighted model - female")
      ps_ExtractFemale=extractRegression(model=psmodelFemale, 
                                 label="unweighted", 
                                 UKBB_n= NROW(subset(UKBBHSE, sampleName=="UKBB" & sex=="female")), 
                                 HSE_n= NROW(subset(UKBBHSE, sampleName=="HSE" & sex=="female")),
                                 group="female")


       print("Combine weighted and unweighted regression results")
       psmodeSum=rbind(ps_Extract, psW_Extract)


colSELps=c("darkgreen", "plum", "lightblue", "darkgreen")

weightReg <- ggplot(psmodeSum, aes(x = cat, 
                         y = est, 
                         ymin = lCI, 
                         ymax = uCI,
                         col=label)) +
    scale_color_manual("",values = colSELps) +
    geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)+ 
    geom_linerange(aes( ymin = lCI,
                       ymax = uCI),  lwd = .2, position = position_dodge(width = 0.5)) + 
    geom_pointrange(aes(ymin = lCI,
                        ymax = uCI, size=1), lwd = 0.4, position = position_dodge(width = 0.5)) + 
    theme_classic() + 
    coord_flip() +
    guides( shape = "none") +
  theme(legend.position="top", axis.text.x=element_text(size=8),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        panel.grid.minor = element_blank())  + 
    scale_x_discrete(position = "top") +
    labs(x =  "", 
         y = expression(paste( italic(beta[STD]) )),
         title = "") 
print(weightReg)

ggsave(file=paste0(HOME,"/results/figures/weightReg_", method, ".pdf"), plot = weightReg, width = 23, height = 20, units = "cm")
uploadDropbox(file=paste0("weightReg_", method, ".pdf"), folder="figures")

colnames(ps_ExtractFemale)
print("Combine weighted and unweighted regression results")
psmodeSum=rbind(ps_Extract, psW_Extract, ps_ExtractMale, ps_ExtractFemale)
print("Export to dropbox")
saveRDS(psmodeSum , paste0(HOME,"/results/rds/psmodeSumAll_", method, ".rds"))
uploadDropbox(file=paste0("psmodeSumAll_", method, ".rds"), folder="rds")


}




meanChange=function(df, method){
       print("Apply weights to data")
       UKBB_raw=as.data.frame(subset(df, sampleName=="UKBB"))
       HSE_raw=as.data.frame(subset(df, sampleName=="HSE"))
       print("Weighted estimates for UKBB")
       UKBB$weight_individual=NULL
       UKBBallweighted=merge(UKBBall, subset(UKBB_raw, select=c(eid, probs, propensity.weight.normalized, IPSW)), by="eid", all.x=T)
       
       incVariables=subset(variableList, include=="yes")$recodedName
       removeVar=c("noncancer_illness_coded", "batch", "alcfrequency_con","vegetable_intake", "fruit_intake", "income_con", "employment_status_binary","coordinate_north_birth" ,  "coordinate_east_birth" ,  "PC2", "PC3", "PC4" , "PC5" , "weight_individual", "cholesterol" )
       incVariables <- unique(incVariables[!incVariables %in% removeVar])

       UKBBweighted=lapply(incVariables, 
                     function(x) weightedEstimates(df=UKBBallweighted, var=x, wt="propensity.weight.normalized"))
                     
       UKBBweightedDF=do.call(rbind, UKBBweighted)
       names(UKBBweightedDF)[names(UKBBweightedDF) == "unweighted_estimate"] <- "unweighted_estimate_UKBB"
       names(UKBBweightedDF)[names(UKBBweightedDF) == "weighted_estimate"] <- "weighted_estimate_UKBB"
       
       UKBBweightedDF$Increase = UKBBweightedDF$outWeighted_raw - UKBBweightedDF$outUnweighted_raw
       UKBBweightedDF$propChange = (UKBBweightedDF$Increase/UKBBweightedDF$outUnweighted_raw) * 100
       
       UKBBweightedDF$meanDiffstd=UKBBweightedDF$Increase/UKBBweightedDF$sdunweighte
       UKBBweightedDF$propMeanChange=ifelse(UKBBweightedDF$level=="mean", UKBBweightedDF$meanDiffstd, UKBBweightedDF$propChange)
       
       UKBBweightedDF$mergeID=paste0(UKBBweightedDF$cat, "_", UKBBweightedDF$level)
       names(UKBBweightedDF)[names(UKBBweightedDF) == "totalN"] <- "totalN_UKBB"
       
       print('Get estimates HSE')
       HSEallweighted=merge(HSEall, subset(HSE_raw, select=c(eid)), by="eid", all.x=T)
       varIncPS=subset(variableList, includePrediction=="yes")$recodedName


       recodLabDF=data.frame(newLab=varIncPS, oldLab=NA)
       for ( i in 1:NROW(varIncPS) ) {
      varIncSel=varIncPS[i]
      getLab=grep(varIncSel, varInc, value = TRUE)
      if(length(getLab)>0){
        recodLabDF$oldLab[i]= getLab
      } else{
       recodLabDF$oldLab[i]= "notincluded"     
      }
      }
      
      HSEweighted=lapply(varIncPS, 
                     function(x) weightedEstimates(df=HSEallweighted, var=x, wt="weight_individual"))
      HSEweightedDF=do.call(rbind, HSEweighted)
      
      HSEweightedDF=merge(HSEweightedDF,recodLabDF, by.x="cat", by.y="newLab", all.x=T, sort=F)
      HSEweightedDF$usedForPS=ifelse(HSEweightedDF$oldLab != "notincluded", "yes", "no")
      HSEweightedDF$mergeID=paste0(HSEweightedDF$cat, "_", HSEweightedDF$level)
      
      HSEweightedDF$cat=NULL
      HSEweightedDF$level=NULL
      
      names(HSEweightedDF)[names(HSEweightedDF) == "totalN"] <- "totalN_HSE"
      names(HSEweightedDF)[names(HSEweightedDF) == "unweighted_estimate"] <- "unweighted_estimate_HSE"
      names(HSEweightedDF)[names(HSEweightedDF) == "weighted_estimate"] <- "weighted_estimate_HSE"
      
      HSE_UKBBweightedDF=merge(HSEweightedDF, UKBBweightedDF, by="mergeID", all.y=T)
      HSE_UKBBweightedDFsel=subset(HSE_UKBBweightedDF, select=c(cat, level, usedForPS, unweighted_estimate_HSE, weighted_estimate_HSE, unweighted_estimate_UKBB, weighted_estimate_UKBB, totalN_HSE, totalN_UKBB, propChange, propMeanChange))
      
      saveRDS(HSE_UKBBweightedDFsel , paste0(HOME,"/results/rds/HSE_UKBBweightedALL_", method, ".rds"))
      uploadDropbox(file=paste0("HSE_UKBBweightedALL_", method, ".rds"), folder="rds")

}



filterScores=function(chr, folder, iteration){
  print(paste0("Iteration: ", iteration, ", chromosome: ", chr))
  setwd(folder)

  ld=fread(paste0(chr, ".l2.ldscore.gz"))
  ldSplit=split(ld, sample(1:200, nrow(ld), replace=T))
  ldSplit[[iteration]]=NULL
  ldSplitJK=do.call(rbind, ldSplit)

  print(paste0("SNPs removed: ", NROW(ld)-NROW(ldSplitJK)))
  folderOut <- paste0(getwd(), "/JK/", iteration)
        if (file.exists(folderOut)) {
          cat("The folder already exists")
          } else {
            dir.create(folderOut)
            } 
  write.table(ldSplitJK,
              file= paste0(folderOut, "/", chr, ".l2.ldscore.gz" ),
              sep="\t",
              row.names = F,
              col.names=T,
              quote=F)

}




# ================= FUNCTION TO PERFORM MR ===========================

funMR=function(estimation,  mrResList=list(), jk="no"){

for ( i in 1:NROW(getMR) ) {
    exposure=getMR[i]
    print("========================================")
    print(paste0("Start MR for exposure ", exposure))
    print("========================================")

    dfW_sig=subset(gwaInW[[exposure]], P_sw < 5e-8)
    if(NROW(dfW_sig)==0) {
          print("No significant hits in weighted")
          dfWClumpInc=data.frame(SNPlabel=NA, SNP=NA, CHR=NA, BP=NA, A1=NA, A2=NA, BETA=NA, SE=NA, P=NA, N=NA, maf=NA, data="weighted")
    } else{
      dfWClumpInc=data.frame(SNPlabel= dfW_sig$SNPlabel, SNP=dfW_sig$SNP, CHR=dfW_sig$CHR, BP=dfW_sig$BP, A1=dfW_sig$A1, A2=dfW_sig$A2, BETA=dfW_sig$BETA_sw, SE=dfW_sig$SE_sw, P=dfW_sig$P_sw, N=dfW_sig$N, maf=dfW_sig$MAF, data="weighted")
    }

    df_sig=subset(gwaIn[[exposure]], P < 5e-8)
    if(NROW(df_sig)==0) {
      print("No significant hits in unweighted")
      dfClumpInc=data.frame(SNPlabel=NA, SNP=NA, CHR=NA, BP=NA, A1=NA, A2=NA, BETA=NA, SE=NA, P=NA, N=NA, maf=NA, data="weighted")
    } else {
         dfClumpInc=data.frame(SNPlabel= df_sig$SNPlabel, SNP=df_sig$SNP, CHR=df_sig$CHR, BP=df_sig$BP, A1=df_sig$A1, A2=df_sig$A2, BETA=df_sig$BETA, SE=df_sig$SE, P=df_sig$P, N=df_sig$N, maf=df_sig$MAF, data="unweighted")
    }
    dfInAll=rbind(dfWClumpInc, dfClumpInc)
    dfIn=subset(dfInAll, !is.na(SNPlabel))
    dfClump=NULL
    dfClumpAll=clumpData(df=dfIn, estimate="") 
    dfClump=dfClumpAll[!duplicated(dfClumpAll$SNPlabel), ]
    
    if(NROW(dfClump)<5) next

    if(estimation=="weighted"){
      print("Run MR for weighted data")
      gwaClump=merge(gwaInW[[exposure]], subset(dfClump, select=c(SNPlabel, data)),by="SNPlabel", all.y=T, sort=F, suffixes=c("", "_wt"))
      gwaClumpInc=data.frame(SNPlabel= gwaClump$SNPlabel, SNP=gwaClump$SNP, CHR=gwaClump$CHR, BP=gwaClump$BP, A1=gwaClump$A1, A2=gwaClump$A2, BETA=gwaClump$BETA_sw, SE=gwaClump$SE_sw, P=gwaClump$P_sw, N=gwaClump$N, maf=gwaClump$MAF)
     } 

    if(estimation=="unweighted"){
      print("Run MR for unweighted data")
      gwaClump=merge(gwaIn[[exposure]], subset(dfClump, select=c(SNPlabel, data)),by="SNPlabel", all.y=T, sort=F)
      gwaClumpInc=data.frame(SNPlabel= gwaClump$SNPlabel, SNP=gwaClump$SNP, CHR=gwaClump$CHR, BP=gwaClump$BP, A1=gwaClump$A1, A2=gwaClump$A2, BETA=gwaClump$BETA, SE=gwaClump$SE, P=gwaClump$P, N=gwaClump$N, maf=gwaClump$MAF)
    }
 
    print("Standardize beta")
    gwaClumpSTD=standardBeta(df=gwaClumpInc, beta=gwaClumpInc$BETA, se = gwaClumpInc$SE, N = gwaClumpInc$N) # use effective sample size
    # remove rows with inf
    gwaClumpSTD <- gwaClumpSTD[!is.infinite(gwaClumpSTD$se_std),]

    print("Estimate correction factor")
     beta=gwaClumpSTD$beta_std
     betaM=mean(beta)
     beta_var=gwaClumpSTD$se_std^2
     m=NROW(beta)
     w2_w=1/(m-1) * sum(  (beta-betaM )^2)  # variance among beta for the exposure
     sig2_w= w2_w - (1/m) * sum(beta_var)
     correctionFac=w2_w/sig2_w
                 

    print("Format exposure data")
    exposureFormat=format_data(gwaClumpSTD,
                           type="exposure",
                           snp_col="SNP", 
                           beta_col = "beta_std",
                           se_col = "se_std",
                           effect_allele_col = "A1",
                           other_allele_col = "A2",
                           pval_col = "P",
                           samplesize_col = "N")
    exposureFormat$exposure=exposure       
    minPexposure=min(exposureFormat$pval.exposure)
    maxPexposure=max(exposureFormat$pval.exposure)

   mrOutsaveList=list()
   SNPsel=merge(data.frame(SNP=gwaClumpInc$SNP, SNPlabel=gwaClumpInc$SNPlabel), data.frame(SNP=exposureFormat$SNP), by="SNP", all.x=T, sort=F)
   

   for ( j in 1:NROW(getMR) ) {
     outcome=getMR[j]
     print(paste0("Start MR for outcome ", outcome))
      if(exposure==outcome){
        next
         }

           if(estimation=="weighted"){
              gwaOut=subset(gwaInW[[outcome]], SNPlabel %in% unique( SNPsel$SNPlabel)) # select SNPs significant in unweighted analysis from sandwich estimstor GWA
              gwaOutcome=data.frame(SNP=gwaOut$SNP, CHR=gwaOut$CHR, BP=gwaOut$BP, A1=gwaOut$A1, A2=gwaOut$A2, BETA=gwaOut$BETA_sw, SE=gwaOut$SE_sw, P=gwaOut$P_sw, N=gwaOut$N, maf=gwaOut$MAF)
           } 
          if(estimation=="unweighted"){
              gwaOut=subset(gwaIn[[outcome]], SNPlabel %in% unique( SNPsel$SNPlabel)) # select SNPs significant in unweighted analysis from sandwich estimstor GWA
              gwaOutcome=data.frame(SNP=gwaOut$SNP, CHR=gwaOut$CHR, BP=gwaOut$BP, A1=gwaOut$A1, A2=gwaOut$A2, BETA=gwaOut$BETA, SE=gwaOut$SE, P=gwaOut$P, N=gwaOut$N, maf=gwaOut$MAF)
           }

        print("Standardize beta (outcome)")
        gwaOutcomeSTD=standardBeta(df=gwaOutcome, beta=gwaOutcome$BETA, se = gwaOutcome$SE, N = gwaOutcome$N) # use effective sample size
        gwaOutcomeSTD <- gwaOutcomeSTD[!is.infinite(gwaOutcomeSTD$se_std),]

        print("Format outcome data")
        outcomeMR=format_data(gwaOutcomeSTD,
                           type="outcome",
                           snp_col="SNP", 
                           beta_col = "beta_std",
                           se_col = "se_std",
                           effect_allele_col = "A1",
                           other_allele_col = "A2",
                           pval_col = "P",
                           samplesize_col = "N")
        outcomeMR$outcome=outcome      

        print("Harmonize data")
        datMR <- harmonise_data( exposure_dat = exposureFormat, 
                                 outcome_dat = outcomeMR)
        print("Perform MR")
        mrOut=mr(datMR, method_list=c( "mr_ivw"))
        mrOut$model=estimation
        mrOut$exposure=exposure
        mrOut$outcome=outcome

        print("Save output")
        mrOutsave=subset(mrOut, select=c(exposure, outcome, b, se, pval, model, nsnp ))
        mrOutsave$exposureNEFF=round(mean(exposureFormat$samplesize.exposure),0)
        mrOutsave$outcomeNEFF=round(mean(outcomeMR$samplesize.outcome),0)
        mrOutsaveList[[j]]=mrOutsave

        if(jk=="yes"){
          print("Run jacknife")
          nSNP=exposureFormat$SNP
          MR_jn=data.frame(beta=numeric(NROW(datMR)) )

          for(k in 1:NROW(datMR)){
            exposureFormat_JK=exposureFormat[-k, ]
            datMR_JK <- harmonise_data( exposure_dat = exposureFormat_JK, 
                                 outcome_dat = outcomeMR)
            MR_jn$beta[k]=mr(datMR_JK, method_list=c( "mr_ivw"))$b
            }
            MR_jn$exposure=exposure
            MR_jn$outcome=outcome
            MR_jn$model=estimation
            mrOutsaveList[[j]]=MR_jn
            }
    
   }
  mrResDF=do.call(rbind, mrOutsaveList)
  mrResDF$correctionFac=correctionFac
  mrResDF$minPexposure=minPexposure
  mrResDF$maxPexposure=maxPexposure
  mrResList[[i]]=mrResDF
  print("========================================")
  print(paste0("Finnished MR for exposure ", exposure))
  print("========================================")
}
  return(mrResList)
}


naAux=function(df, var, sample){
       nAll=NROW(df)
       nMiss=NROW(subset(df, is.na(df[[var]])==T))
       percMiss=round((nMiss/nAll)*100,2)
       dfOut=data.frame(nAll, nMiss, percMiss, var, sample)
       return(dfOut)
}




runRandomR=function(df, varInc, return){
       print("Run random forest")
       library(WeightIt)
       f <- as.formula( paste0("sample ~ ",  paste0(varInc, collapse=" + ")) )
       
       (W1 <- weightit(f, 
                data = df,
                method = "super", 
                estimand = "ATC",
                SL.library = c("SL.randomForest")))
       df$probs =W1$ps
       df$IPSW=(1-df$probs)/df$probs
       df$IPSWNorm=c(normalizeWeights(subset(df, sampleName=="HSE")$IPSW), normalizeWeights(subset(df, sampleName=="UKBB")$IPSW))
       df$propensity.weight.normalized=ifelse(df$sampleName=="HSE", df$weight_individual, df$IPSWNorm)
       print(head(df))
       print("Check sum PS")
       print(summary(df$propensity.weight.normalized))
if(return=="data"){
       print("Return data.frame")
       return(df)
}
if(return=="weights"){
       print("Return weights")
       return(df$propensity.weight.normalized)
}
}
