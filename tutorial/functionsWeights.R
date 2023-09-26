
# Functions


recodeVar=function(df, var, varInfo){
  print(var)
  funcApply <- eval(parse(text = paste(subset(varInfo, label==var)$recode)))
  varRec=funcApply(df=df, var)
  dfRec=data.frame(eid=df$eid, varRec)
  colnames(dfRec)=c("eid", var)
  return(dfRec)
}




createDummy=function(df, varInc, return="data"){
  dummyCat=subset(varInc, type=="cat")$label
  dummyBin=subset(varInc, type=="bin")$label
  varCon=subset(varInc, type=="con")$label


  dfBin <- dummy_cols(subset(df, select=c(c( dummyBin))), select_columns = dummyBin, remove_first_dummy = TRUE, ignore_na=TRUE)
  dfCat <- dummy_cols(subset(df, select=c(c( dummyCat))), select_columns = dummyCat, remove_first_dummy = FALSE, ignore_na=TRUE)

  remVar=c(colnames(dfCat), colnames(dfBin))
  dfCon=subset(df, select=c("eid",varCon))
  dfM=cbind(dfCon, dfCat, dfBin)
  
  varPred <- c(varCon, unique(remVar[!remVar %in% c( dummyCat, dummyBin)]))

if(return=="data"){
  return(dfM)
}
if(return=="variable"){
  return(varPred)
}

}


removeNegative=function(var, df){
  print(paste0("Recode ", var))
  varE=df[[var]]
  varOut=ifelse(varE<0, NA, varE)
  return(varOut)
}



recodeEdu=function(var, df=df){
  print(paste0("Recode ", var))
  varOut=ifelse(as.numeric(df[["education_degree"]])==1, 20, as.numeric(df[[var]]))
  varOut=ifelse(varOut<0, NA, varOut)
  varOut=ifelse(varOut<= 14, 14, varOut)
  varOut=ifelse(varOut >=19, 19,  varOut)
  print(table(varOut))
  return(varOut)
}




recodeHouse=function(var, df=df){
    varIn=df[[var]]
    varOut=ifelse(varIn=="-3" | varIn=="-1", NA, varIn)
    varOut=ifelse(varOut>=7, "7", varOut)
  print(table(varOut))
  return(varOut)
}


recodeAlc=function(var, df=df){
  print(paste0("Recode ", var))
  varOut=revalue(as.factor(df[[var]]), c("-3"=NA,
                                            "6"="never",
                                            "5" = "few_times_year",
                                            "4" = "monthly",
                                            "3" = "once_twice_weekly", 
                                            "2" = "three_four_times_weekly",
                                            "1" = "daily") )
  print(table(varOut))
  return(varOut)
}

recodeSmoke=function(var, df=df){
  print(paste0("Recode ", var))
  varOut=revalue(as.factor(df[[var]]), c("-3"=NA,
                                 "0" = "never",
                                 "1" = "previous",
                                 "2" = "current") )
  print(table(varOut))
  return(varOut)
  
}

recodeIncome=function(var, df=df){
  print(paste0("Recode ", var))
  varOut=revalue(as.factor(df[[var]]), c("-1"= "not_shared",
                                 "-3"= "not_shared",
                                 "1"="<18k",
                                 "2"="18k-31k",
                                 "3"="31k-52k",
                                 "4"="52k-100k",
                                 "5"= ">100k"   ) )
  print(table(varOut))
  return(varOut)
}

recodeEmployment=function(var, df=df){
  # -3: Prefer not to answer | -7: None of the above | 1: In paid employment or self-employed | 2: Retired | 3: Looking after home and/or family | 4: Unable to work because of sickness or disability | 5: Unemployed | 6: Doing unpaid or voluntary work | 7: Full or part-time student | 
  print(paste0("Recode ", var))
  varOut=revalue(as.factor(df[[var]]), c("-7"=NA, 
                                   "-3"=NA,
                                   "1"="employed",
                                   "2"= "retired", 
                                   "3"="economically_inactive",
                                   "4"="economically_inactive",
                                   "5"="unemployed",
                                   "6" ="economically_inactive",
                                   "7"="economically_inactive") )

  print(table(varOut))
  return(varOut)
}

recodeBMIcon=function(var, df=df){
  varOut=round(df$weight/(df$height/100)^2,0)
  return(varOut)
  }


recodeBMI=function(var, df=df){
  print(paste0("Recode ", var))
  variable=round(df$weight/(df$height/100)^2,0)
  varOut=ifelse(variable<18.5,"underweight", NA )
  varOut=ifelse(variable>=18.5 & variable<25,"healthyweight", varOut )
  varOut=ifelse(variable>=25 & variable<30, "overweight", varOut )
  varOut=ifelse(variable>=30, "obese", varOut )
  varOut <- factor(varOut, levels = c("underweight", "healthyweight", "overweight","obese"))
  print(table(varOut))
  return(as.factor(varOut))
}

recodeHealth=function(var, df=df){
  print(paste0("Recode ", var))
  #  #-1: Do not know | -3: Prefer not to answer | 1: Excellent | 2: Good | 3: Fair | 4: Poor | (\"In general how would you rate your overall health?\)
  varOut=revalue(as.factor(df[[var]]), c("-1"=NA, 
                                   "-3"=NA,
                                   "1" = "good",
                                   "2" = "good",
                                   "3" = "fair",
                                   "4" = "poor") )
  print(table(varOut))
  return(as.factor(varOut))
}


recodeUrban=function(var, df=df){
  print(paste0("Recode ", var))
  varOut=revalue(as.factor(df[[var]]), c("1"="urban", # 1: England/Wales - Urban - sparse 
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
  varOut <- factor(varOut, levels = c("village_hamlet", "town_fringe", "urban"))
  print(table(varOut))
  return(as.factor(varOut))
}


recodeSex=function(var, df=df){
  print(paste0("Recode ", var))
  varOut=revalue(as.factor(df[[var]]), c("0"="female", 
                                 "1"="male") )
  varOutF <- factor(varOut, levels = c("male", "female"))
  print(table(varOutF))
  return(as.factor(varOutF))
}
