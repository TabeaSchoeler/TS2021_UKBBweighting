
#######################################################
# =================== LOAD LIBRARIES ==================
#######################################################
# remove all of the objects that are stored in the global environment
.libPaths()
.libPaths("~/Dropbox/progs/R/library")

# Load and install libraries
.libPaths( c( .libPaths(), "~/Dropbox/progs/R/library") )
load.lib=c('pheatmap', 'ggthemes', 'ggpubr', 'Qtlizer', 'devtools', 'GenomicSEM', 'data.table', 'Matrix',
           'sem', 'Matrix', 'stats', 'semTools', 'ggcorrplot', 'grex', 'openxlsx', 'rJava', 'qqman', 'gmodels', 'ggplot2',
           'ggcorrplot', 'tidyverse', 'reshape2', 'pdftools', 'plyr', 'phenoscanner','ggpubr',
           'biomaRt', 'viridis', 'rdrop2', 'lavaan', 'gprofiler2', 'knitr',
           'gridExtra', 'CMplot', 'configr', 'purrr', 'TwoSampleMR', 'GOexpress', 'ieugwasr', 'xlsx',
           'survey', 'gtsummary', 'srvyr', 'foreign', 'epigraphdb', 'kable', 'ggpubr', 'sjPlot')


install.lib<-load.lib[!load.lib %in% installed.packages()]

# Install missing packages
for(lib in install.lib) install.packages(lib, dependencies=TRUE)
# Load all packages
sapply(load.lib,require,character=TRUE)
# Sampling bias
# Compare the proportions in each age group between your sample and the source population. This is important to be able to highlight potential sampling bias. You could similarly repeat this looking at distributions by sex.




# Function to prepare data for manhattan plot
dataManHplot=function(df, genes, topn){

  plotDF=data.frame(
    SNP=df$SNP,
    Chromosome = df$CHR,
    Position = df$BP,
    PS = df$P )
  
  # Add row break in resID/gene name variable
  genes=genes %>% top_n(topn, -(as.numeric(P)))
  
  SNPsComFacText=as.character(paste0(genes$SNP, "\n(" ,genes$hgnc, ")"))
  # Derive colour scheme for mediated vs non-mediated SNPs
  SNPsComFac=genes$SNP
  SNPsComFacCol="black"
  # Save in list
  listGWA_manhattanOut=list(plotDF=plotDF, SNPs=SNPsComFac, SNPsText=SNPsComFacText, SNPsCol=as.character(SNPsComFacCol))
  return(listGWA_manhattanOut)
}

# Function to plot manhattan plot
ManHplot=function(list, width, height, type, ylimit=NULL){
  plotOut=CMplot(list[["plotDF"]],
                 type="p",
                 plot.type=type,
                 LOG10=TRUE,
                 file="jpg",
                 dpi=300,
                 main="",
                 cex=0.5, # size of the points (all)
                 amplify=TRUE,
                 signal.cex=0.5, # size of the points (significant)
                 threshold=c(5e-8),
                 #threshold.col=c("lightblue2","grey"),
                 highlight=unlist(list[["SNPs"]]),
                 highlight.text=unlist(list[["SNPsText"]]),
                 highlight.text.cex=0.7,
                 highlight.text.col=unlist(list[["SNPsCol"]]),
                 highlight.col= "midnightblue",
                 col=c("lightblue2","lightblue4"),
                 ylab=expression(-log[10](italic(p))),
                 highlight.text.font=4,
                 file.output=FALSE,
                 width=width,
                 ylim=ylimit,
                 height=height)
  return(plotOut)
}




# standardize beta
standardBeta=function(df, beta, se, N, maf){
  maf <- ifelse(maf > 0.5, 1 - maf, maf)
  # make sure they are numeric numbers
  snp_freq = as.numeric(as.character(maf))
  b = as.numeric(as.character(beta))
  se = as.numeric(as.character(se))
  n = as.numeric(as.character(N))
  zscore = b / se
  df$beta_std = zscore / sqrt(2*snp_freq*(1-snp_freq)*(n+zscore^2))
  df$se_std  = 1 / sqrt(2*snp_freq*(1-snp_freq)*(n+zscore^2))
  return(df)
}


zScoreDiff=function(beta1, beta2,se1,se2, df){
  df$ZscoreDiff=(sqrt(beta1^2)-sqrt(beta2^2) )/sqrt((se1^2 + se2^2))
  df$ZscoreDiff_pval=pnorm(q=df$ZscoreDiff, lower.tail=TRUE)
  return(df)
}



snpPlot=function(df,type="perc"){
  if(type=="perc"){
    plot=ggplot(aes(x=beta_wt_abs, y=beta_rep_abs, colour=as.factor(outlieSNP2)),data=df) +
      geom_point(data=subset(df, outlieSNP2=="nochange"), size=2, colour=subset(df, outlieSNP2=="nochange")$colourScheme2) + 
      geom_point(data=subset(df, outlieSNP2=="20decrease"), size=2, colour=subset(df, outlieSNP2=="20decrease")$colourScheme2) + 
      geom_point(data=subset(df, outlieSNP2=="20increase"), size=2, colour=subset(df, outlieSNP2=="20increase")$colourScheme2) 
  } 
  if(type=="sig"){
    print("BB")

    plot=ggplot(aes(x=beta_wt_abs, y=beta_rep_abs, colour=as.factor(colourSchemeSig)),data=df) +
      geom_point(data=subset(df, source=="both"), size=2, colour=subset(df, source=="both")$colourSchemeSig) + 
      geom_point(data=subset(df, source=="weighted"), size=2, colour=subset(df, source=="weighted")$colourSchemeSig) + 
      geom_point(data=subset(df, source=="unweighted"), size=2, colour=subset(df, source=="unweighted")$colourSchemeSig) 
  }
  plot=plot+
  #scale_color_brewer(palette="Dark2") + 
  geom_abline(intercept = 0, slope = 1, color="black", linetype="solid", size=.5) +
    
    theme_classic() +
  theme(legend.position = "none")  +
  xlim(0, max(c(df$beta_wt_abs, df$beta_rep_abs) )) +
    ylim(0, max(c(df$beta_wt_abs, df$beta_rep_abs) )) +
  labs(x = expression(paste("|" ,italic(beta[w]), "|" )), 
       y = expression(paste( "|" ,italic(beta), "|" )) ,
       title =   recodeHarmVar2(df$pheno)[1])  #+
    
    #scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
   # scale_y_continuous(breaks = scales::pretty_breaks(n = 3))
  


  print(plot)
  return(plot)
}


mrPlot=function(df){
  plot <- ggplot(df, aes(x = outcome, 
                                     y = b, 
                                     ymin = lCI, 
                                     ymax = uCI,
                                     fill=changeEst_cat,
                                     colour=changeEst_cat,
                                     shape = factor(model))) +
    geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)+ 
    geom_linerange(aes(x = outcome, ymin = lCI,
                       ymax = uCI),  lwd = .2, position = position_dodge(width = 0.5), colour=df$colourSchemeMR) + 
    geom_pointrange(aes(x = outcome, y = b, ymin = lCI,
                        ymax = uCI, size=1), lwd = 0.4, position = position_dodge(width = 0.5), colour=df$colourSchemeMR) + 
    coord_flip() +
    theme_classic() + 
    theme(legend.position="none")  + 
    labs(x =  "", 
         y = expression(paste( italic(beta[STD]) )),
         title = df$exposure[1]) +
    geom_text(mapping = aes(x=outcome, y=b, label = label_codedNEW), size = 3, colour = "black", nudge_y = ifelse(df$b > 0, 0.01, -0.01))
print(plot)
  return(plot)
}




MRplotIndiv=function(df, scaleMR = "free"){
  
  dfns=subset(df, changeEst_cat=="none")
  dfsig_over=subset(df, changeEst_cat=="overEst")
  dfsig_under=subset(df, changeEst_cat=="underEst")
  dfsig_other=subset(df, changeEst_cat=="changeDir")

  plorMRAllsupp <- ggplot(df, aes(x = outcome, 
                                  y = b, 
                                  ymin = lCI, 
                                  ymax = uCI,
                                  shape = factor(model))) +
    geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)+ 
    geom_linerange(aes(x = outcome_clean, ymin = lCI,
                       ymax = uCI),  lwd = .2, position = position_dodge(width = 0.5), colour=df$colourSchemeMR) + 
    geom_pointrange(data=dfns, aes( x = outcome_clean, y = b, ymin = lCI,
                                          ymax = uCI, size=1), lwd = 0.4, position = position_dodge(width = 0.5), colour=  dfns$colourSchemeMR[1]) +
    geom_pointrange(data=dfsig_under, aes( x = outcome_clean, y = b, ymin = lCI,
                                          ymax = uCI, size=1), lwd = 0.4, position = position_dodge(width = 0.5), colour=  dfsig_under$colourSchemeMR[1]) + 
    geom_pointrange(data=dfsig_over, aes( x = outcome_clean, y = b, ymin = lCI,
                        ymax = uCI, size=1), lwd = 0.4, position = position_dodge(width = 0.5), colour=  dfsig_over$colourSchemeMR[1]) + 
    geom_pointrange(data=dfsig_other, aes( x = outcome_clean, y = b, ymin = lCI,
                                          ymax = uCI, size=1), lwd = 0.4, position = position_dodge(width = 0.5), colour=  dfsig_other$colourSchemeMR[1]) + 
    
    coord_flip() +
    theme_classic() + 
    theme(legend.position="none")  + 
    labs(x =  "", 
         y = expression(paste( italic(alpha[STD]) )),
         title = "") +
    geom_text(mapping = aes(x=outcome_clean, y=b, label = label_codedNEW), size = 3, colour = "black", nudge_y = ifelse(df$b > 0, 0.01, -0.01)) + 
    facet_grid(cols = vars(exposure_clean), scales = scaleMR) + 
    theme( axis.line = element_line(colour = "white"), strip.background = element_rect(colour="white", fill="white" )) + xlab("Outcome") + 
    labs(title="")
  print(plorMRAllsupp)
}


RGplotIndiv=function(df){
  
  dfns=subset(df, changeEst_cat=="ns_change")
  dfsig_over=subset(df, changeEst_cat=="change_orig_overestimated")
  dfsig_under=subset(df, changeEst_cat=="change_orig_underestimated")
  
  plorMRAllsupp <- ggplot(df, aes(x = pheno1, 
                                  y = rg, 
                                  ymin = lCI, 
                                  ymax = uCI,
                                  fill=changeEst_cat,
                                  #colour=changeEst_cat,
                                  shape = factor(model))) +
    geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)+ 
    
    geom_linerange(aes(x = pheno1, ymin = lCI,
                       ymax = uCI),  lwd = .2, position = position_dodge(width = 0.5), colour=df$colourSchemeMR) + 
    #geom_pointrange(aes(x = outcome_clean, y = b, ymin = lCI,
    #                    ymax = uCI, size=1), lwd = 0.4, position = position_dodge(width = 0.5), colour=df$colourSchemeMR) + 
    
    
    geom_pointrange(data=dfns, aes( x = pheno1, y = rg, ymin = lCI,
                                    ymax = uCI, size=1), lwd = 0.4, position = position_dodge(width = 0.5), colour=  dfns$colourSchemeMR[1]) + 
    theme( legend.position = "bottom")  +
    
    
    geom_pointrange(data=dfsig_under, aes( x = pheno1, y = rg, ymin = lCI,
                                           ymax = uCI, size=1), lwd = 0.4, position = position_dodge(width = 0.5), colour=  dfsig_under$colourSchemeMR[1]) + 
    
    theme( legend.position = "bottom")  +
    
    geom_pointrange(data=dfsig_over, aes( x = pheno1, y = rg, ymin = lCI,
                                          ymax = uCI, size=1), lwd = 0.4, position = position_dodge(width = 0.5), colour=  dfsig_over$colourSchemeMR[1]) + 
    theme( legend.position = "bottom")  +
    
    
    coord_flip() +
    theme_classic() + 
    theme(legend.position="none")  + 
    labs(x =  "", 
         y = expression(paste( italic(r[g]) )),
         title = "") +
    #geom_text(mapping = aes(x=Pheno1clean, y=rg, label = Pheno1clean), size = 3, colour = "black", nudge_y = ifelse(df$b > 0, 0.01, -0.01)) + 
    facet_grid(cols = vars(pheno2), scales = "free") + 
    theme( axis.line = element_line(colour = "white"), strip.background = element_rect(colour="white", fill="white" )) + xlab("") + 
    labs(title="") + geom_text(aes(label =sigFDR), size=8, colour=  "black",   nudge_x = 0.01,  nudge_y = 0.01) 
  print(plorMRAllsupp)
}


  

extractCorest=function(df, return="sum", corW, nEff){
  # Get correlations
  corWoutClean=data.frame(pheno1=df$p1.x, pheno2= df$p2.x, corHSEw=df$corHSEw, corUKBB=df$corUKBB, corUKBBw=df[[corW]], nEff=df[[nEff]])
  corWoutClean=subset(corWoutClean, pheno1!= pheno2 )
  corWoutClean$discUKBB=corWoutClean$corHSEw - corWoutClean$corUKBB
  corWoutClean$discUKBB_w=corWoutClean$corHSEw - corWoutClean$corUKBBw
  
  corWoutClean$changetype_w=ifelse((abs(corWoutClean$discUKBB) > abs(corWoutClean$discUKBB_w))==T, "reducederror", NA)
  corWoutClean$changetype_w=ifelse((abs(corWoutClean$discUKBB) <= abs(corWoutClean$discUKBB_w))==T, "increasederror",  corWoutClean$changetype_w)
  corWoutClean$changetype_w=ifelse(abs(corWoutClean$discUKBB)>0.05 | abs(corWoutClean$discUKBB_w)>0.05,  corWoutClean$changetype_w, "littlediff")
  
  percReduced_w=(NROW(subset(corWoutClean, changetype_w=="reducederror"))/NROW(corWoutClean))*100
  percIncreased_w=(NROW(subset(corWoutClean, changetype_w=="increasederror"))/NROW(corWoutClean))*100
  
  corWoutClean$biasReduction=(abs(corWoutClean$discUKBB)-abs(corWoutClean$discUKBB_w))/abs(corWoutClean$discUKBB)
  corWoutClean$biasReduction=ifelse(  corWoutClean$changetype_w == "littlediff", NA,   corWoutClean$biasReduction)
  
  corWoutCleanSub=subset(corWoutClean, abs(discUKBB) >0.05 | abs(discUKBB_w) >0.05)
  
  largestDiff=as.data.frame(corWoutCleanSub %>%
                              top_n(5, abs(corWoutCleanSub$discUKBB)))
  largestDiff <-largestDiff[order(abs(largestDiff$discUKBB), decreasing = TRUE),]
  
  largestDiffpheno=paste0(paste0(largestDiff$pheno1, "~~", largestDiff$pheno2, " (rDiff=", round(largestDiff$discUKBB,2), ", rHSE=",round(largestDiff$corHSEw,2), ", rUKBB=",round(largestDiff$corUKBB,2)) , ")", collapse="; ")
  
  datOut=data.frame(percReducedError_w=percReduced_w,
                    percIncreasedError_w= percIncreased_w, 
                    maxDisc=max(abs(corWoutClean$discUKBB)),
                    maxDiscPheno=largestDiffpheno, 
                    maxDisc_w=max(abs(corWoutClean$discUKBB_w)),
                    medianDisc=median(abs(corWoutClean$discUKBB)),
                    medianDisc_w=median(abs(corWoutClean$discUKBB_w)),
                    meanDisc=mean(abs(corWoutClean$discUKBB)),
                    meanDisc_w=mean(abs(corWoutClean$discUKBB_w)),
                    nEff=corWoutClean$nEff[1],
                    biasReductionMedian= median(corWoutCleanSub$biasReduction),
                    biasReductionMean= mean(corWoutCleanSub$biasReduction),
                    range=paste0( round(min((corWoutCleanSub$biasReduction)), 4), "; ",  round(max((corWoutCleanSub$biasReduction)), 4)))
  
  if(return=="sum"){
    return(datOut)
  }
  if(return=="data"){
    return(corWoutClean)
  }
  
  if(return=="name"){
    return(df$model[1])
  }
  
}




recodeParticipation=function(variable){
  variableOut=revalue(  variable, c("foodParticipation"="participation",
                                    "emailParticipation"="participation",
                                    "mhParticipation"="participation",
                                    "sexParticipation"="participation"))
 return(variableOut)
}

recodeHarmVar1=function(variable){
  variableOut=revalue(  variable, c("age"="Age (continuous)",    "alcfrequency_never"="Alcohol frequency (never)",
                                                                 "alcfrequency_daily"="Alcohol frequency (daily)",
                                                                 "alcfrequency_few_times_year"="Alcohol frequency (few times/year)",
                                                                 "alcfrequency_monthly"="Alcohol frequency (monthly)",
                                                                 "alcfrequency_once_twice_weekly"="Alcohol frequency (once/twice weekly)",
                                                                 "alcfrequency_three_four_times_weekly"="Alcohol frequency (three/four weekly)",
                                                                 "bmi"="BMI",
                                                                 "education_age"="Education age (continuous)",
                                                                 "employment_status_economically_inactive"="Employment status (economically inactive)",
                                                                 "employment_status_employed"="Employment status (employed)",
                                                                 "employment_status_retired"="Employment status (retired)",
                                                                 "height"="Height (continuous)",
                                                                 "household_size_1"= "Household size (1)",
                                                                 "household_size_2"= "Household size (2)",
                                                                 "household_size_3"= "Household size (3)",
                                                                 "household_size_4"= "Household size (4)",
                                                                 "household_size_5"= "Household size (5)",
                                                                 "household_size_6"= "Household size (6)",
                                                                 "household_size_7"= "Household size (7 or more)",
                                                                 "income_<18k"="Income (<18k)",
                                                                 "income_>100k"="Income (>100k)",
                                                                 "income_18k-31k"="Income (18k-31k)",
                                                                 "income_31k-52k"="Income (31k-52k)",
                                                                 "income_52k-100k"="Income (52k-100k)",
                                                                 "sex_female"="Sex (female)",
                                                                  "income_not_shared"="Income (not shared)",
                                                                 "employment_status_unemployed" ="Employment status (unemployed)",
                                                                  "smoking_status_never"="Smoking status (never)",
                                                                 "smoking_status_current"="Smoking status (current)",
                                                                 "smoking_status_previous"="Smoking status (previous)",
                                                                 "urbanisation_town_fringe"="Urbanisation (town/fringe)",
                                                                 "urbanisation_urban"="Urbanisation (urban)",
                                                                  "urbanisation_village_hamlet"="Urbanisation (village/hamlet)",
                                                                "bmi_cat_underweight" = "BMI (underweight)",
                                                                  "bmi_cat_healthyweight" = "BMI (healthy weight)",
                                                                 "bmi_cat_overweight" = "BMI (overweight)",
                                                                 "bmi_cat_obese" = "BMI (obese)",
                                                                 "overallhealth_poor"= "Overall health (poor)",
                                                                 "overallhealth_fair" = "Overall health (fair)",
                                                                 "overallhealth_good" = "Overall health (good)",
                                                                 "weight"="Weight (continuous)") )
  return(variableOut)
}




recodeHarmVar2=function(variable){
  variableOut=revalue(  variable, c("age"="Age",
                                    "alcfrequency"="Alcohol frequency",
                                    "bmi"="BMI",
                                    "education_age"="Education (age)",
                                    "employment_status"="Employment status",
                                    "height"="Height",
                                    "household_size"= "Household size",
                                    "income"="Income",
                                    "sex"="Sex",
                                    "cholesterol_ldl" = "Cholesterol (LDL)",
                                    "smoking_status"="Smoking status",
                                    "urbanisation"="Urbanisation",
                                    "overallhealth"="Overall health",
                                    "alc_dependence" = "Alcohol (dependence)",
                                    "alcfrequencyweekly" = "Alcohol use (frequency)",
                                    "asthma" = "Asthma",
                                    "back_pain" = "Back pain",
                                    "bmi_cat" = "BMI",
                                    "cancer"  = "Cancer",
                                    "cholesterol_hdl" =  "Cholesterol (HDL)",
                                    "coffee_intake" = "Coffee intake",
                                    "coordinate_east_now" = "Coordinate (east)",
                                    "coordinate_north_now" = "Coordinate (north)",
                                    "depression_anxiety" = "Depression/anxiety",
                                    "deprivation_index" = "Deprivation index",
                                    "diabetes" =  "Diabetes",
                                    "eating_disorder" =   "Eating disorder",
                                    "handedness_left" = "Handedness (left)",
                                    "hypertension"  = "Hypertension",
                                    "insomnia_con" = "Insomnia",
                                    "manic_disorder" = "Manic disorder",
                                    "long_standing_illness" = "Long standing illness",
                                    "migraine" = "Migraine" ,
                                    "mood_swings" = "Mood swings",
                                    "job_class_coded" = "Job class",
                                    "neuroticism" = "Neuroticism",
                                    "noncancer_illness_number" = "Number of illnesses (non-cancer)",
                                    "PC1" = "Principal component (1)",
                                    "physical_acticity_MET" = "Physical acticity (MET)",
                                    "physical_activity" = "Physical activity",
                                    "ptsd" =  "PTSD",
                                    "reaction_time" =  "Reaction time",
                                    "risk_taking" =   "Risk taking",
                                    "loneliness" = "Loneliness",
                                    "fruit_intake_con" = "Fruit intake",
                                    "schizophrenia"  = "Schizophrenia",
                                    "smoking_frequency" = "Smoking frequency",
                                    "systolic_blood_pressure" =  "Systolic blood pressure",
                                    "vegetable_intake_con" =  "Vegetable intake" ,
                                    "weight"="Weight") )
  return(variableOut)
}


recodeLevel=function(variable){
  variableOut=revalue(  variable, c("alc_dependence"="present",
                                    "few_times_year"="few times/year",
                                    "once_twice_weekly"="once, twice/weekly",
                                    "back_pain"="present",
                                    "healthyweight" = "healthy weight",
                                    "eating_disorder" = "present",
                                    "economically_inactive" = "economically inactive",
                                    "hypertension"="present",
                                    "not_shared"="not shared",
                                    "manic_disorder" = "present",
                                    "migraine" = "present",
                                    "schizophrenia"  = "present",
                                    "ptsd" = "present",
                                    "three_four_times_weekly" = "three, four times/weekly",
                                    "yes" = "present",
                                    "town_fringe" = "town/fringe",
                                    "village_hamlet"= "village/hamlet"))
                                  
                                    
                                    return(variableOut)
}



plotLASSO=function(plotDat, model, col){

  plotDat$corModel=paste0(tolower(recodeHarmVar2(plotDat$pheno1)), "~", tolower(recodeHarmVar2(plotDat$pheno2)))
  
  
  
  PlotForest=rbind(data.frame(cor=plotDat$corHSE,  group="HSE", model=plotDat$corModel, change=plotDat$changetype_w),
                   data.frame(cor=plotDat$corUKBB,  group="UKBB", model=plotDat$corModel, change=plotDat$changetype_w),
                   data.frame(cor=plotDat$corUKBBw,  group="UKBB (weighted)", model=plotDat$corModel, change=plotDat$changetype_w))
  
  PlotForest$change=revalue(  PlotForest$change, c("littlediff"="similar r",
                                                   "reducederror" = "differential r"))
  
  forestModels <- ggplot(PlotForest, aes(y = model, x = cor, col=change, shape=group)) +
    geom_point( size = 3) +     
    theme_classic()   + 
    scale_color_manual(values=c( col, "darkgrey"),
                       labels = expression( (r[diff] >0.05) ,(r[diff] <=0.05) ), name=NULL) + # +  xlim(-0.6, 0.6)
    #scale_shape_discrete(labels=c("fail","pass", "XXX"),name="result") +
    scale_shape_manual(values = c(0, 5, 6, 15), name="") + #labels=c("fail","pass", "XXX")
  
    theme(
          legend.position = c(0.5, 0.85),
          legend.direction="horizontal",
          plot.margin = margin(t=0, r=1, b=0, l=2, "cm"), 
          axis.text.x=element_text(size=8, angle=45, hjust=1))  + 
    coord_flip() +
    labs(x =expression(paste( italic(r) )), 
         y = "",
         title = "") 
  
  name=paste0("lassoMod_", model)
  ggsave(file=paste0(HOME,"/results/figures/",name,".pdf"), plot = forestModels, width = 35, height = 15, units = "cm")
  
  return(forestModels)
  
}




# ========= Differences in H2 estimates ================
h2Diff=function(df1, df2, corH2, h2adjust=NROW(phenoLG) ){
  h2_diff= abs(df1$h2) - abs(df2$h2)
  h2_diff_se= sqrt( df1$h2_se^2 + df2$h2_se^2 - 2*corH2 * df1$h2_se * df2$h2_se )
  ZscoreDiff=h2_diff/h2_diff_se
  ZscoreDiff_pval=2*pnorm(-abs(ZscoreDiff)) # two sided
  dfOut=df1
  dfOut$scheme="h2diff"
  dfOut$modelClass=NA
  dfOut$h2=h2_diff
  dfOut$h2_se=h2_diff_se
  dfOut$uCI=dfOut$h2 + 1.96 * dfOut$h2_se
  dfOut$lCI=dfOut$h2 - 1.96 * dfOut$h2_se
  padjust= do.call(rbind, lapply(ZscoreDiff_pval, function(x) p.adjust(x, "fdr", h2adjust)))
  dfOut$pFDRh2=padjust
  dfOut$sigFDR=ifelse(padjust<0.05, "*", "")
  return(dfOut)
}



# ========= Plot for h2 ================
h2PlotFunc=function(df){
  h2Plot = ggplot(df, aes(x = fct_reorder(phenoClean, -h2), 
                          y = h2, 
                          ymin = lCI, 
                          ymax = uCI,
                          col=scheme)) +
    scale_color_manual("",values = c("hotpink4", "mediumpurple4", "midnightblue") , labels = c("GWA(h2)", "wGWA(h2)", "GWA(h2) - wGWA(h2)")) +
    facet_grid(cols = vars(scheme), scales = "fixed") +
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
          panel.grid.minor = element_blank(),
          strip.text.x = element_blank())  + 
    
    scale_x_discrete(position = "top") +
    labs(x =  "", 
         y = expression(paste( italic(h^2) )),
         title = "") + geom_text(aes(label =sigFDR), size=8, colour=  "black",   nudge_x = 0.01,  nudge_y = 0.01) 
  print(h2Plot)
  return(h2Plot)
}



# ======== HEATMAP genetic correlations
heatM=function(df, text){
  heatRG=ggcorrplot::ggcorrplot(df,
                                type = "lower",
                                ggtheme = ggplot2::theme_minimal,
                                colors = c("#6D9EC1", "white", "mediumorchid4"),
                                lab = TRUE,
                                tl.cex=16,
                                outline.color = "white") +
    labs(title=text, subtitle = "") + 
    theme(
      plot.title = element_text(color = "darkgrey", size = 20, face = "bold"))
  
  print(heatRG)
}


# ======== Save image
saveFigure=function(fileName, plotName, w, h){
  print("Save PDF")
  ggsave(file=paste0(fileName, ".pdf"), plot = plotName, width = w, height = h, units = "cm", bg='transparent')
  print("Save SVG")
  save_plot(paste0(fileName, ".svg"), fig = plotName, width=w, height=h)
  print("Save PNG")
  ggsave(file=paste0(fileName, ".png"), plot = plotName, width = w, height = h, units = "cm", bg='white',   dpi = 1000)
}


# Get lower triangle of the correlation matrix
library(reshape2)
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# melt
meltIndividual=function(data){
  rownames(data)=colnames(data)
  dataM=melt(as.matrix(data), na.rm = TRUE)
  return(dataM)
}

# sex GWA - compare Beta
compareBeta=function(df, b1, b2, se1, se2){
  df$diffBeta=abs(df[[b1]]) - abs(df[[b2]])
  df$changeDir=ifelse( df$diffBeta > 0, "reducedBias", "increasedBias")
  df$percChange= (abs(df[[b2]]) - abs(df[[b1]]))  / abs(df[[b1]])
  df$diffSE=sqrt( abs(df[[se1]])^2 + abs(df[[se2]])^2)
  ZscoreDiff=df$diffBeta/df$diffSE
  df$diff_pval=2*pnorm(-abs(ZscoreDiff)) # two sided
  df$uCI_diff=   df$diffBeta+ 1.96 *   df$diffSE
  df$lCI_diff=  df$diffBeta - 1.96 *   df$diffSE
  df$col=ifelse(df$diff_pval<0.05, "sig", "ns")
  return(df)
}

