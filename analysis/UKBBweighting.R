

#######################################################
# =================== FUNCTIONS =======================
#######################################################

rm(list = ls())
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
HOME=getwd()
source(paste0(HOME, "/analysis/functionsProcessing.R"))


#######################################################
# =================== ANALYSES ========================
#######################################################

# read in clean labels
variableList <- as.data.frame(readxl::read_excel(paste0(HOME, "/data/variableList.xlsx")))
variablesUKBB=subset(variableList, select=c(labelUKBB_recoded, label_clean, label_clean_short ))

# ============= UKBB missing data ===================
sumExclusion=readRDS(paste0(HOME,"/results/rds/sumExclusion.rds"))


# ============= HSE missing data ===================
descHSEraw=readRDS(paste0(HOME,"/results/rds/descHSEDFmissingRaw.rds")) # missing data before recoding
descHSE=readRDS(paste0(HOME,"/results/rds/descHSEDF.rds"))
HSEinclusion=readRDS(paste0(HOME,"/results/rds/HSEinclusion.rds"))

# ============= Census data check ===================
infoCensus=readRDS(paste0(HOME,"/results/rds/infoCensus.rds"))
census=readRDS(paste0(HOME,"/results/rds/censusCheck.rds"))
variablesUKBBshort=subset(variablesUKBB, select=c(labelUKBB_recoded, label_clean ))
census=merge(census, variablesUKBBshort, by.x= "cat_hse", by.y="labelUKBB_recoded", all.x=T, sort=F)
census$cat_hse=census$label_clean
census$label_clean=NULL
names(census)[names(census) == 'cat_hse'] <- 'Variable'
names(census)[names(census) == 'level_hse'] <- 'Category'
names(census)[names(census) == 'weighted_estimate_hse'] <- 'HSE'
names(census)[names(census) == 'unweighted_estimate_cen'] <- 'Census'
names(census)[names(census) == 'unweighted_estimate_ukbb'] <- 'UKBB'


# Correlations among harmonized variables
corExternalCensus=readRDS(paste0(HOME, "/results/rds/corExternalCensus.rds")) 
corExternalCensus=subset(corExternalCensus, p1!=p2)

corExternalCensusGG=data.frame(corCEN=c(corExternalCensus$corCEN, corExternalCensus$corCEN),
                               corHSEUKBB=c(corExternalCensus$corHSE, corExternalCensus$corUKBB),
                               group=c(rep("HSE",NROW(corExternalCensus) ), rep("UKBB",NROW(corExternalCensus) )),
                               label=c(paste0(tolower(recodeHarmVar2(corExternalCensus$p1)) ,"~", tolower(recodeHarmVar2(corExternalCensus$p2)) ), paste0(tolower(recodeHarmVar2(corExternalCensus$p1)) ,"~", tolower(recodeHarmVar2(corExternalCensus$p2)) ) ))
corExternalCensusGG=subset(corExternalCensusGG, group!="UKBB")

scatterHSE_Census <- ggplot(corExternalCensusGG, aes(x = corCEN, y = corHSEUKBB, col=group)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, colour="grey") +    
  theme_classic() +
  scale_color_manual(values=c( "black")) + 
  geom_text(aes(label = label), na.rm = TRUE, hjust = -0.05, vjust=-0.05, size=3, colour="darkmagenta") +
  
  theme(legend.position="none")  + 
  labs(x =expression(paste( italic(r[CENSUS]) ) ), 
       y = expression(paste( italic(r[HSE]) ) ),
       title = "") +   xlim(-.5, 1) +   ylim(-.5, 1)

# Save as image
saveFigure(fileName=paste0(HOME,"/results/figures/scatterHSE_Census"), plotName=scatterHSE_Census, w=15, h=15)


# ============= Predictors included in LASSO  ===================
varModelOut=readRDS(paste0(HOME,"/results/rds/varSelection_all.rds"))
LASSOvarInc=paste0("Of all included predictors (n=", NROW(varModelOut), ", n=",   NROW(subset(varModelOut, is.na(effectVar)!=T)), " were included, while n=", NROW(subset(varModelOut, is.na(effectVar)==T)), " where removed")

# ============= Model performance  ===================
corAllcomb=readRDS(paste0(HOME,"/results/rds/corAllcomb.rds"))
corAllcomb=subset(corAllcomb, p1.x!="bmi_cat" & p2.x!="bmi_cat") # remove BMI cat as already included as continuous

# no validation
plotLASSOall=extractCorest(corAllcomb, corW="corUKBBw", nEff="nEFF_UKBBw", return="data") 
sumLASSOall=extractCorest(corAllcomb, corW="corUKBBw", nEff="nEFF_UKBBw", return="sum") 
plotOutLASSO=plotLASSO(plotLASSOall, model="all", col="darkslateblue")

weightingCorHSEsum=paste0("The largest difference in correlation estimate |corhse-corukk| was rdiff=|", round(sumLASSOall$maxDisc, 3), "|. The largest discrepancies in correlations estimates between HSE and (unweighted) UKBB was present for: ", sumLASSOall$maxDiscPheno)

# cross validation
plotLASSOcv=extractCorest(corAllcomb, corW="corUKBBw_cv", nEff="nEFF_UKBB_cv", return="data") 
sumLASSOcv=extractCorest(corAllcomb, corW="corUKBBw_cv", nEff="nEFF_UKBB_cv", return="sum") 
plotOutLASSOcv=plotLASSO(plotLASSOcv, model="cv", col="darkgreen")

# Cobine plots
LASSOComb=ggarrange(plotOutLASSO, plotOutLASSOcv, heights = c(0.5, 0.5), nrow=2,  labels=c("A", "B")) 

# Save as image
saveFigure(fileName=paste0(HOME,"/results/figures/LASSOComb"), plotName=LASSOComb, w=40, h=30)
saveFigure(fileName=paste0(HOME,"/results/figures/LASSOmain"), plotName=plotOutLASSO, w=33, h=15)


# =============== Density curve PS
datPlot=readRDS(paste0(HOME,"/results/rds/densityPS.rds"))
datPlot=subset(datPlot, sampleName=="UKBB")
summary(datPlot$propensity.weight.normalized)


PSplot= ggplot(datPlot, aes(x=propensity.weight.normalized, fill=sampleName)) + geom_density(alpha=.3) +
  scale_fill_manual("",values = c("cornflowerblue", "darkgreen") ) + 
  theme(legend.position="none") +   
  theme_classic() + 
  labs(
    x = expression(paste('Normalized propensity weights (', italic(w[i][n]) , ")")),
    y="") +
  xlim(0, 10) +  theme(legend.position="none")  


sumDensity=summary(datPlot$propensity.weight.normalized)
print(PSplot)

#the sum of the total of the normalized weights within one group the sample size
checkWeigh=subset(datPlot, sample == 0)
sum(checkWeigh$probs_norm, na.rm=T)
sumWeights=summary(checkWeigh$probs_norm)

# Save as plot
saveFigure(fileName=paste0(HOME,"/results/figures/PSplot"), plotName=PSplot, w=15, h=15)


# ============= Predictors for belonging to either HSE or UKBB ===================
usedPSpred=readRDS(paste0(HOME,"/results/rds/HSE_UKBBweightedAll.rds"))
usedPSpred$usedForPSCoded=ifelse(is.na(usedPSpred$usedForPS)==T, "no", usedPSpred$usedForPS)
usedPSpredSub=subset(usedPSpred, select=c(cat, usedForPSCoded))

psmodeSumAll=readRDS(paste0(HOME,"/results/rds/psmodeSumAll_noCV.rds"))
psmodeSumAll$labelRecoded=recodeHarmVar1(psmodeSumAll$labelShort)
psmodeSum=subset(psmodeSumAll, variable!="(Intercept)")
psmodeSum$order=seq(1, NROW(psmodeSum), 1)
psmodeSum$group_sep=ifelse(psmodeSum$group=="all", "mixed", "male/female")
psmodeSum$label_group=paste0(psmodeSum$label, " \n(", psmodeSum$group_sep, ")")
psmodeSum$label_group_long=psmodeSum$group

colSELps=c("darkgreen", "plum", "lightblue", "darkgreen")

weightReg <- ggplot(psmodeSum, aes(x = fct_reorder(labelRecoded, -order), 
                         y = est, 
                         ymin = lCI, 
                         ymax = uCI,
                         col=label_group_long)) +
    scale_color_manual("",values = colSELps) +
    geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)+ 
  facet_grid(cols = vars(label),scales = "fixed") +
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

# Save Figure
saveFigure(fileName=paste0(HOME,"/results/figures/weightReg"), plotName=weightReg, w=20, h=17)
psmodeSumText=subset(psmodeSum, group_sep=="mixed" & label == "unweighted")
psmodeSumTextTop=psmodeSumText %>% top_n(4, (as.numeric(abs(est)))) 
TextTopPredicors= paste0(psmodeSumTextTop$labelShort, collapse=", ")


# ===============  change in proportions as a function of weighting
summaryWeightingAll=readRDS(paste0(HOME,"/results/rds/HSE_UKBBweightedAll_noCV.rds"))
summaryWeightingAll$label_clean=recodeHarmVar2(summaryWeightingAll$cat)

# check which variables are included in prediction
summaryWeighting=merge(summaryWeightingAll, subset(variableList, select=c(includePrediction,labelUKBB_recoded)), by.x="cat", by.y="labelUKBB_recoded")
summaryWeighting=subset(summaryWeighting, grepl("0:", level)!=T)
summaryWeighting=subset(summaryWeighting, grepl("0:no", level)!=T)
summaryWeighting=subset(summaryWeighting, level!="no")


library(stringr)
summaryWeighting$level=str_replace_all(summaryWeighting$level, "1:", "")  
summaryWeighting$propChangeLog=log(abs(summaryWeighting$propChange))
summaryWeighting$level=recodeLevel(summaryWeighting$level)
summaryWeighting$label=paste0(summaryWeighting$label_clean, " (", summaryWeighting$level, ")")
summaryWeighting$usedForPSCoded=ifelse(summaryWeighting$includePrediction=="no", "no", summaryWeighting$includePrediction)

library(forcats)
summaryWeightingProp=subset( summaryWeighting, level!="mean")

PSchange=ggplot(summaryWeightingProp, aes(x=fct_reorder(label, -propChange), y=propChange, 
                                          fill=usedForPSCoded)) + 
  scale_fill_manual("",values = c("chocolate", "steelblue") , labels = c("not used for prediction", "used for prediction")) + 
  geom_bar(stat='identity', width=.5)  +
  theme_classic() + 
 labs(x =   expression(paste( "Percentage change [", italic( (p[w]-p) / p), "]" )) , 
       y =  "",
       title = "") +

  theme( 
         legend.title= element_blank(), 
         legend.position = c(0.8, 0.8),
         legend.direction="horizontal",
         plot.margin = margin(t=0, r=1, b=0, l=2, "cm"), 
         axis.text.x=element_text(size=8, angle=45, hjust=1)) 

summaryWeightingMean=subset( summaryWeighting, level=="mean")

PSMeanchange=ggplot(summaryWeightingMean, aes(x=fct_reorder(label, -propMeanChange), y=propMeanChange, fill=usedForPSCoded)) + 
  scale_fill_manual("",values = c("chocolate", "steelblue") ) + 
  geom_bar(stat='identity', width=.5)  +
  theme_classic() + 
  labs(x =  "", 
       y =  "",
       title = "") +
  theme( legend.direction="horizontal",
         legend.title= element_blank(), 
         legend.position="none",
         plot.margin = margin(t=0, r=1, b=0, l=2, "cm"), 
         axis.text.x=element_text(size=8, angle=45, hjust=1)) +
  theme( plot.margin = margin(t=5, r=0, b=1, l=0, "cm")) + 
  xlab( expression(paste( "Mean change [", italic( (m[w]-m) / sd), "]" )))



PSchangeMeanComb=ggarrange(PSchange, PSMeanchange, widths = c(0.7, 0.4)) 
saveFigure(fileName=paste0(HOME,"/results/figures/PSchangeProp"), plotName=PSchange, w=20, h=15)
saveFigure(fileName=paste0(HOME,"/results/figures/PSchangeMean"), plotName=PSMeanchange, w=15, h=18)
saveFigure(fileName=paste0(HOME,"/results/figures/PSchange"), plotName=PSchangeMeanComb, w=40, h=15)


# ================= Create Table for supplement
weightingChange=data.frame("Variable"=summaryWeighting$label,
                           "HSE_raw"=summaryWeighting$unweighted_estimate_HSE,
                           "HSE_w"=summaryWeighting$weighted_estimate_HSE,
                           "HSE_n"=summaryWeighting$totalN_HSE,
                           "UKBB_raw"=summaryWeighting$unweighted_estimate_UKBB,
                           "UKBB_w"=summaryWeighting$weighted_estimate_UKBB,
                           "UKBB_n"=summaryWeighting$totalN_UKBB)

names(weightingChange)[names(weightingChange) == 'HSE_raw'] <- 'HSE (unweighted)'
names(weightingChange)[names(weightingChange) == 'HSE_w'] <- 'HSE (weighted)'
names(weightingChange)[names(weightingChange) == 'HSE_n'] <- 'HSE (n)'
names(weightingChange)[names(weightingChange) == 'UKBB_raw'] <- 'UKBB (unweighted)'
names(weightingChange)[names(weightingChange) == 'UKBB_w'] <- 'UKBB (weighted)'
names(weightingChange)[names(weightingChange) == 'UKBB_n'] <- 'UKBB (n)'


# sex prev
sexPrevDFcensus=subset(census, Category=="female")
sexPrevDFHSE=subset(summaryWeightingAll, level=="female")
sexPrevWeighting=paste0("UKBB participations were over-proportional female [UKBB=", sexPrevDFcensus$UKBB[1], ", HSE=", sexPrevDFcensus$HSE[1], ", CENSUS=", sexPrevDFcensus$Census[1], "]. Probability weighting recovered the populaton prevalence in the UKBB ", sexPrevDFHSE$weighted_estimate_UKBB[1])








##############################################################################################
# ========================================== WEIGHTED GWAS ===================================
##############################################################################################

# ===================== SNP effects 
### Weighted versus unweighted estimates
weighSNPDiff=readRDS(paste0(HOME, "/results/rds/SNPeffectsCor.rds"))
weighSNPDiff=subset(weighSNPDiff,  pheno!="PS")

# Text for manuscript
sampleSizeSum=paste0(" obtained from wGWA (Neffective=",min(weighSNPDiff$N_wt), "-", max(weighSNPDiff$N_wt),") to traditional GWA analyses (N=",min(weighSNPDiff$N), "-",max(weighSNPDiff$N),")")
weighSNPDiff$cor_independentBeta=weighSNPDiff$cor

# Get p-values for differences between beta
weighSNPDiff$beta_diff=weighSNPDiff$BETA - weighSNPDiff$BETA_wt
weighSNPDiff$ZscoreDiff=weighSNPDiff$beta_diff / sqrt( weighSNPDiff$SE^2+weighSNPDiff$SE_wt^2 - 2*weighSNPDiff$cor_independentBeta * weighSNPDiff$SE * weighSNPDiff$SE_wt )
weighSNPDiff$ZscoreDiff_pval=2*pnorm(-abs(weighSNPDiff$ZscoreDiff)) # two sided

# estimate percentage change
weighSNPDiff$nameLabel = paste0(weighSNPDiff$pheno, "_", weighSNPDiff$SNP)
weighSNPDiff$changePerc = ( (abs(weighSNPDiff$BETA) - abs(weighSNPDiff$BETA_wt)) / abs(weighSNPDiff$BETA) ) * 100
weighSNPDiff$beta_wt_abs = abs( weighSNPDiff$BETA_wt)
weighSNPDiff$beta_rep_abs = abs( weighSNPDiff$BETA)


# check if differential dirction of effects
weighSNPDiff$changeSign="sameSign"
weighSNPDiff$changeSign=ifelse(weighSNPDiff$BETA > 0 & weighSNPDiff$BETA_wt < 0, "diffSign",   weighSNPDiff$changeSign)
weighSNPDiff$changeSign=ifelse(weighSNPDiff$BETA < 0 & weighSNPDiff$BETA_wt > 0, "diffSign",   weighSNPDiff$changeSign)
subset(weighSNPDiff, changeSign=="diffSign")


# plot proportions
phenoSubList=list()
propTable=list()
phenoDatList=list()
phenoSigList=list()


colSchemeSig=function(variable){
  variableout=revalue( variable, c(
    "unweighted"= "lightseagreen",
    "weighted" ="forestgreen",
    "both"="lightgreen") )
  return(variableout)
}

colSchemeChange=function(variable){
  variableout=revalue( variable, c(
    "20increase"= "steelblue4",
    "20decrease" ="turquoise3",
    "nochange"="lightgrey")  )
  return(variableout)
}


for ( i in 1:length(levels(as.factor(weighSNPDiff$pheno))) ) {
  phenoName=levels(as.factor(weighSNPDiff$pheno))[i]
  print(paste0("Read ", phenoName))
  phenoSub=subset(weighSNPDiff, pheno==phenoName)
  phenoSub$pval_fdr <- do.call(rbind, lapply(phenoSub$ZscoreDiff_pval, function(x) p.adjust(x, "fdr",  NROW(phenoSub))))
  phenoSub$outlieSNP=ifelse(phenoSub$pval_fdr < 0.05, "sig", "ns")
  
  phenoSub$changeSign="sameSign"
  phenoSub$changeSign=ifelse(phenoSub$BETA > 0 & phenoSub$BETA_wt < 0, "diffSign",   phenoSub$changeSign)
  phenoSub$changeSign=ifelse(phenoSub$BETA < 0 & phenoSub$BETA_wt > 0, "diffSign",   phenoSub$changeSign)
  
  phenoSub$outlieSNP2="nochange"
  phenoSub$outlieSNP2=ifelse(phenoSub$changePerc >=20, "20decrease", phenoSub$outlieSNP2 )
  phenoSub$outlieSNP2=ifelse(phenoSub$changePerc <= -20, "20increase", phenoSub$outlieSNP2 )
  phenoSub$outlieSNP2=ifelse(phenoSub$changeSign =="diffSign", "nochange", phenoSub$outlieSNP2 ) # in case beta estimates are in different directions
  
  #add colour scheme
  phenoSub$colourScheme2=colSchemeChange( phenoSub$outlieSNP2)


   tableSum2=as.data.frame( matrix(nrow=3, ncol=3))
   colnames(tableSum2)=c("pheno", "factor", "nSNP")
   tableSum2$pheno=phenoName
   tableSum2$factor=c("20increase", "20decrease",  "nochange")
   tableSum2$factorSig=c("both", "weighted",  "unweighted")

   tableSum2$nSig=ifelse(  tableSum2$factorSig == "both", NROW(subset(phenoSub, source=="both")), NA)
   tableSum2$nSig=ifelse(  tableSum2$factorSig == "unweighted", NROW(subset(phenoSub, source=="unweighted")),    tableSum2$nSig)
   tableSum2$nSig=ifelse(  tableSum2$factorSig == "weighted", NROW(subset(phenoSub, source=="weighted")),    tableSum2$nSig)
   tableSum2$nSig=ifelse(tableSum2$nSig==0, "",    tableSum2$nSig)

   tableSum2$propSig=ifelse(  tableSum2$factorSig == "both", NROW(subset(phenoSub, source=="both"))/NROW(phenoSub), NA)
   tableSum2$propSig=ifelse(  tableSum2$factorSig == "unweighted", NROW(subset(phenoSub, source=="unweighted"))/NROW(phenoSub),    tableSum2$propSig)
   tableSum2$propSig=ifelse(  tableSum2$factorSig == "weighted", NROW(subset(phenoSub, source=="weighted"))/NROW(phenoSub),    tableSum2$propSig)
   
   tableSum2$SigOrder=   NROW(subset(phenoSub, source=="weighted" |  source=="both"))
   
   tableSum2$nSNPChange=ifelse(  tableSum2$factor == "20increase", NROW(subset(phenoSub, outlieSNP2=="20increase")), NA)
   tableSum2$nSNPChange=ifelse(  tableSum2$factor == "20decrease", NROW(subset(phenoSub, outlieSNP2=="20decrease")),    tableSum2$nSNPChange)
   tableSum2$nSNPChange=ifelse(  tableSum2$factor == "nochange", NROW(subset(phenoSub, outlieSNP2=="nochange")),    tableSum2$nSNPChange)
   tableSum2$nSNPChange=ifelse(tableSum2$nSNPChange==0, "",    tableSum2$nSNPChange)
   
   tableSum2$propChange=ifelse(  tableSum2$factor == "20increase", NROW(subset(phenoSub, outlieSNP2=="20increase"))/NROW(phenoSub), NA)
   tableSum2$propChange=ifelse(  tableSum2$factor == "20decrease",  NROW(subset(phenoSub, outlieSNP2=="20decrease"))/NROW(phenoSub),    tableSum2$propChange)
   tableSum2$propChange=ifelse(  tableSum2$factor == "nochange", NROW(subset(phenoSub, outlieSNP2=="nochange"))/NROW(phenoSub),    tableSum2$propChange)
   tableSum2$propChange=round(   tableSum2$propChange, 2)
   tableSum2$propChangeOrder=   NROW(subset(phenoSub, outlieSNP2=="nochange"))

   tableSum2$changeSign=c("sameSign", "diffSign", NA)
   tableSum2$changeSignSNPs=ifelse(  tableSum2$changeSign == "sameSign", NROW(subset(phenoSub, changeSign=="sameSign")), NA)
   tableSum2$changeSignSNPs=ifelse(  tableSum2$changeSign == "diffSign", NROW(subset(phenoSub, changeSign=="diffSign")), tableSum2$changeSignSNPs)
   
   tableSum2$changeSignProp=ifelse(  tableSum2$changeSign == "sameSign", NROW(subset(phenoSub, changeSign=="sameSign"))/NROW(phenoSub), NA)
   tableSum2$changeSignProp=ifelse(  tableSum2$changeSign == "diffSign", NROW(subset(phenoSub, changeSign=="diffSign"))/NROW(phenoSub),    tableSum2$changeSignProp)
   tableSum2$changeSignSNPname=ifelse( tableSum2$changeSign == "diffSign", paste0( subset(phenoSub, changeSign=="diffSign")$SNP, collapse=", "), NA)
   
   tableSum2$nSNP=NROW(phenoSub)
   
   phenoSub$colourSchemeSig=colSchemeSig(phenoSub$source)

   
  propTable[[i]]=tableSum2
  phenoSubList[[i]]=snpPlot(phenoSub)
  phenoSigList[[i]]=snpPlot(phenoSub, type="sig")
  phenoDatList[[i]]=phenoSub
}

propTableDF=do.call(rbind, propTable)


# ============= GWAS significant hits
propTableDF$sigLabel=paste0(propTableDF$nSig, " (", round(propTableDF$propSig*100,0), "%)")
propTableDF$sigLabel=ifelse(propTableDF$propSig==0, "", propTableDF$sigLabel)
propTableDF$factorSigClean=revalue( propTableDF$factorSig, c(
  "unweighted"= "GWA",
  "weighted" ="wGWA",
  "both"= "GWA & wGWA" ))

propTableDF$factorSig <- factor(propTableDF$factorSig, levels = c("weighted", "both", "unweighted"))
propTableDF$colourSchemeSig=colSchemeSig(propTableDF$factorSig)
propTableDF$colourSchemeSig=fct_rev(propTableDF$colourSchemeSig)
propTableDF$label_clean=recodeHarmVar2(propTableDF$pheno)
propTableDF$order=seq(1,NROW(propTableDF),1)

propSNPsSig=ggplot(propTableDF, aes( y=propSig, x=fct_reorder(label_clean, order),fill=propTableDF$factorSigClean )) + 
  geom_bar(position="stack", stat="identity", show.legend=T) +
  theme(legend.position = "bottom") +
  ggtitle("") +
  theme_classic() +
  theme(axis.text.x=element_text(size=10, angle=45, hjust=1)) +
  xlab("") + ylab("") + labs(title="Number of genome-wide variants") +
  scale_fill_manual(values=levels(propTableDF$colourSchemeSig) ) +
  geom_text(aes(label = sigLabel), position = position_stack(0.5), color = "black", size=2) + 
  theme(legend.position = "bottom", legend.title=element_blank(),
        plot.margin = margin(t=1, r=1, b=0, l=2, "cm"))


# ============= Degree of change between weighted and unweigthed
propTableDF$factor <- factor(propTableDF$factor, levels = c("nochange", "20decrease", "20increase"))
levels(as.factor(propTableDF$factor))
SNPchangeEXPLabel=expression(paste(""),
                             paste("Overestimation due to participation bias [(|", italic(beta),"| - |",italic(beta[w]), "|) / |",italic(beta) ,"|",phantom(x) > .2,"]" ,sep=""), 
                             paste("Underestimation due to participation bias [(|", italic(beta), "| - |",italic(beta[w]), "|) / |",italic(beta),"|" ,phantom(x) < -.2,"]", sep=""))

propTableDF$colourScheme2=colSchemeChange(propTableDF$factor)
propTableDF$colourScheme2=as.factor(revalue(propTableDF$colourScheme2, c(
  "lightgrey"= "white")))

propTableDF$changeLabel=paste0(propTableDF$nSNPChange, " (", round(propTableDF$propChange*100,0), "%)")
propTableDF$changeLabel=ifelse(propTableDF$factor=="nochange", "", propTableDF$changeLabel)
propTableDF$order=seq(1,NROW(propTableDF),1)

propSNPs=ggplot(propTableDF, aes( y=propChange, x=fct_reorder(label_clean, order), fill=factor  )) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("") +
  theme_classic() +
  theme(axis.text.x=element_text(size=10, angle=45, hjust=1)) +
  xlab("") + ylab("") + 
  labs(title="Change in effect estimates for genome-wide variants") +
  scale_fill_manual("", values=levels(propTableDF$colourScheme2),  labels = SNPchangeEXPLabel) +
  xlab("") + 
  geom_text(aes(label = changeLabel), position = position_stack(0.5), color = "black", size=2) +
  theme(legend.position=c(0.5,-.91),
        legend.box="vertical", 
        legend.title=element_blank(),
        plot.margin = margin(t=0, r=1, b=1.5, l=2, "cm"))




# ==== SELECT EXAMPLES
snpExamples=c("smoking_status", "cholesterol_ldl", "bmi", "education_age")
names(phenoSubList)=levels(as.factor(weighSNPDiff$pheno))
plotlistShort=list(phenoSubList[[snpExamples[1] ]], phenoSubList[[snpExamples[2] ]], phenoSubList[[snpExamples[3] ]], phenoSubList[[snpExamples[4]  ]])
snPlotCombinedShort=ggarrange(plotlist=plotlistShort, 
                              ncol=2,
                              nrow=2)

names(phenoSigList)=levels(as.factor(weighSNPDiff$pheno))
plotSigShort=list(phenoSigList[[snpExamples[1] ]], phenoSigList[[snpExamples[2] ]], phenoSigList[[snpExamples[3] ]], phenoSigList[[snpExamples[4]  ]])
snPlotSigShort=ggarrange(plotlist=plotSigShort, 
                              ncol=2,
                               nrow=2)

snPlotProp=ggarrange(propSNPs, snPlotCombinedShort, ncol=2, widths=c(4,2))
snPlotSig=  ggarrange(propSNPsSig, snPlotSigShort, ncol=2, widths=c(4,2))
snPlotCombinedProp= ggarrange(snPlotProp, snPlotSig, heights=c(3,3), 
                             nrow=2,
                             labels=c("A", "B"), common.legend = TRUE)


saveFigure(fileName=paste0(HOME,"/results/figures/snPlotCombinedProp"), plotName=snPlotCombinedProp, w=35, h=25)
saveFigure(fileName=paste0(HOME,"/results/figures/snPlotProp"), plotName= snPlotProp , w=35, h=12)
saveFigure(fileName=paste0(HOME,"/results/figures/snPlotSig"), plotName= snPlotSig , w=35, h=12)



#  ======== Supplement Figures
library("gridExtra")
names(phenoSubList)=levels(as.factor(weighSNPDiff$pheno))

propSNPsLEGEND=ggplot(propTableDF, aes( y=propChange, x=fct_reorder(label_clean, order), fill=factor  )) + 
  geom_bar(position="stack", stat="identity")  +
  scale_fill_manual("", values=levels(propTableDF$colourScheme2),  labels = SNPchangeEXPLabel) +
  theme(legend.position="bottom",
        legend.box="vertical", 
        legend.title=element_blank(),
        plot.margin = margin(t=0, r=1, b=1.5, l=2, "cm"))


legSNPsig <- cowplot::get_legend(propSNPsSig)
legSNPsigout=as_ggplot(legSNPsig)
legSNPprop <- cowplot::get_legend(propSNPsLEGEND)
legSNPpropout=as_ggplot(legSNPprop)

# Plot 1
snPlotCombined=ggarrange(ggarrange(plotlist=phenoSubList), legSNPpropout, nrow = 2, heights=c(1, 0.1))
saveFigure(fileName=paste0(HOME,"/results/figures/snPlotCombined"), plotName= snPlotCombined , w=40, h=50)

# Plot 2
snPlotSigCombined=ggarrange(ggarrange(plotlist=phenoSigList), legSNPsigout, nrow = 2, heights=c(1, 0.1))
saveFigure(fileName=paste0(HOME,"/results/figures/snPlotSigCombined"), plotName= snPlotSigCombined , w=40, h=50)


# Get estimates for text
topIncrease=subset(propTableDF, factor=="20increase") %>% 
  top_n(3, (as.numeric(propChange))) %>% 
  arrange(-propChange) 

topDecrease=subset(propTableDF, factor=="20decrease") %>% 
  top_n(3, (as.numeric(propChange))) %>% 
  arrange(-propChange) 

topSignificance=subset(propTableDF, factorSig=="weighted") %>% 
  top_n(3, (as.numeric(propSig))) %>% 
  arrange(-propSig) 

increaseInfo=paste0(tolower(topIncrease$label_clean), " (",round(topIncrease$propChange, 2)*100, "% of SNPs)" ,collapse = ", ")
decreaseInfo=paste0(tolower(topDecrease$label_clean), " (",round(topDecrease$propChange, 2)*100, "% of SNPs)" ,collapse = ", ")
signifianceInfo=paste0(tolower(topSignificance$label_clean), " (",round(topSignificance$propSig, 2)*100, "% of SNPs)" ,collapse = ", ")


propTableDF$nSNPChangeSum=ifelse(propTableDF$nSNPChange=="", 0, propTableDF$nSNPChange)
propTableDF$nSigSum=ifelse(propTableDF$nSig=="", 0, propTableDF$nSig)
sum(as.numeric(propTableDF$nSNPChangeSum))
propTableDF$factor
nTotallPheno=subset(propTableDF, factor=="20increase")$nSNP
nSigAdditionalPheno=subset(propTableDF, factorSig=="weighted")$nSigSum
nOverestimatPheno=subset(propTableDF, factor=="20increase")$nSNPChangeSum
nUnderestimatPheno=subset(propTableDF, factor=="20decrease")$nSNPChangeSum


sumSNPgwaWeighted=paste0("First, with respect to genome-wide discovery, we found that of all identified SNPs in either the weighted or unweighted GWA analyses on the included phenotypes (n=", sum(as.numeric(propTableDF$nSigSum)), "), " , sum(as.numeric(nSigAdditionalPheno)), " SNPs (", round(sum(as.numeric(nSigAdditionalPheno))/sum(as.numeric(nTotallPheno)) *100, 2), "%) were missed as a result of participation bias, as these SNPs reached significance only in the weighted analyses. With respect to changes in effect size between the weighted and unweighted SNP estimates, we found that overestimation of SNP effects was present for ", sum(as.numeric(nOverestimatPheno)), " SNPs (", round(sum(as.numeric(nOverestimatPheno))/sum(nTotallPheno) *100,2), "% of all identified SNPs) as a result of participation bias, while underestimation of SNP effects was present for ", sum(as.numeric(nUnderestimatPheno)), " SNPs (", round(sum(as.numeric(nUnderestimatPheno))/sum(nTotallPheno) *100,2), "% of all identified SNPs)")
sumSNPweighting=paste0("With respect to the individual phenotypes, the phenotypes most affected by false negative findings due to participation bias included: ", signifianceInfo, ". Underestimation  in effect size due to participation bias were most prominent for ", decreaseInfo, ", whereas overestomation in effect size due to participation bias were most notably present for ", increaseInfo, ". ")

changeEffsDF=weighSNPDiff
changeEffsDF$changeSign="noChange"
changeEffsDF$changeSign=ifelse(changeEffsDF$BETA > 0 & changeEffsDF$BETA_wt < 0, "diffSign",   changeEffsDF$changeSign)
changeEffsDF$changeSign=ifelse(changeEffsDF$BETA < 0 & changeEffsDF$BETA_wt > 0, "diffSign",   changeEffsDF$changeSign)
changeEffsign=subset(changeEffsDF, changeSign=="diffSign")

totalSNPs=sum(as.numeric(propTableDF$nSig), na.rm = T)
changeEffsignText=paste0("Of all genome-wide hits (",totalSNPs, "), only ", NROW(changeEffsign), " SNP estimate change direction of effect [", paste0(changeEffsign$pheno, " (", changeEffsign$SNP, "; b=", changeEffsign$BETA, "; bw=", changeEffsign$BETA_wt,"). However, the two effects were not significant in both GWA (p=", formatC(changeEffsign$P, digits = 3), " versus pw=", formatC(changeEffsign$P_wt, digits =3) , collapse = ", "), "]")

newSNPsW=paste0("All genome-wide hits: (",totalSNPs, ". Of the tested phenotypes (", NROW(subset(propTableDF, factorSig=="weighted"))  , ") new SNPs were identified for ", NROW(subset(propTableDF, factorSig=="weighted" & nSig !="")), ")")



#######################################################
# =================== PROCESS GWA ON PS ===============
#######################################################
source(paste0(HOME, "/analysis/processPSgwa.R"))
inSNPsSum=print(paste0("The GWAS on UKBB participation was conducted on nEFF=", clump_input$N[1], " participants, Included were ", nSNPsTotal, " SNPs, of which ", nSNPsbeforeClumping, " reached genome-wide significance. After clumping, ", nSNPsafterClumping, " SNPs remained."))

# Heritability estimate for PS GWA
h2outPS=subset(readRDS(paste0(HOME, "/results/rds/h2outDF.rds")), file=="ldak_PS_weighted")
h2outPStext=paste0("SNP heritability for UKBB participation was estimated to be h2=", round(h2outPS$h2, 3), " (se=", round(h2outPS$h2_se, 3), "), with an LD-score intercept of ", round(h2outPS$intercept, 3))

# Text for LD score regression results
topRG=ldscExternalClean %>% 
  arrange( dplyr::desc(abs(rg))) %>%
  top_n(6, (abs(rg))) 
trait=paste0(paste0(tolower(topRG$labelClean), " (rg=", round(topRG$rg,2), ")"), collapse=", ")

topRGtext=paste0(" LD score regression analyses implicated substantial genetic correlations between UKBB participation and phenotypes related to cognition, socioeconomic factors and behaviour [", trait, "]")


# =======================================================================================================
# ===================================== LDSC regression  ================================================
# =======================================================================================================
rgList=readRDS(paste0(HOME, "/results/rds/rgComb.rds")) 
rgListW=readRDS(paste0(HOME, "/results/rds/rgCombW.rds"))

# jackknife correlations
h2JK=readRDS( paste0(HOME, "/results/rds/h2Cor.rds"))
rgJK=readRDS( paste0(HOME, "/results/rds/rgCor.rds"))

rg=as.data.frame(rgList[["rg"]])
phenoNamesM=colnames(rg)
rgW=as.data.frame(rgListW[["rg"]])

# extract number of tests
rgTests<- get_lower_tri(rgList[["rg"]]) # get p-values
rownames(rgTests)=phenoNamesM
rgTestsform <- melt(rgTests, na.rm = TRUE)
rgTestsN=subset(rgTestsform, Var1!=Var2 & Var1!="ldak_sex" & Var2!="ldak_sex")
rgFDRadjust=NROW(rgTestsN)

rownames(rg)=colnames(rg) %>% str_replace("ldak_", "")
rownames(rg)=recodeHarmVar2(rownames(rg))
colnames(rg)=rownames(rg)

rownames(rgW)=colnames(rgW) %>% str_replace("ldak_", "")
rownames(rgW)=rownames(rgW) %>% str_replace("_weighted", "")
rownames(rgW)=recodeHarmVar2(rownames(rgW))
colnames(rgW)=rownames(rgW)

rg_se=as.data.frame(rgList[["rg_se"]])
rg_seW=as.data.frame(rgListW[["rg_se"]])


# ================= Heritability estimates
# read in LDSC results
h2DFw=rgListW[["h2"]]; h2DFw$scheme="weighted" # weighted results
h2DF=rgList[["h2"]]; h2DF$scheme="unweighted" # unweighted results
h2LDAK=rbind(h2DF, h2DFw)

# clean label
h2LDAK$pheno=h2LDAK$pheno %>% str_replace("_weighted", "")
h2LDAK$pheno=h2LDAK$pheno %>% str_replace("ldak_", "")
h2LDAK$modelClass="ldsc"
h2LDAK$method="ldsc"
h2LDAK$phenoModel=paste0(h2LDAK$pheno, "_", h2LDAK$model)


# change: observed h2  = liab h2 for continuous
h2LDAKOBS=subset(h2LDAK, scale=="observed", select=c(phenoModel, h2, h2_se))
h2LDAK=merge(h2LDAK, h2LDAKOBS, by="phenoModel", all.x=T, sort=F, suffixes = c("", "_obs"))
h2LDAK$h2=ifelse(is.na(h2LDAK$h2)==T, h2LDAK$h2_obs, h2LDAK$h2)
h2LDAK$h2_se=ifelse(is.na(h2LDAK$h2_se)==T, h2LDAK$h2_se_obs, h2LDAK$h2_se)

# Get CI
h2LDAK$uCI=h2LDAK$h2 + 1.96 * h2LDAK$h2_se
h2LDAK$lCI=h2LDAK$h2 - 1.96 * h2LDAK$h2_se
h2LDAKSex=h2LDAK
h2LDAKSex=subset(h2LDAK, pheno=="sex")
h2LDAK=subset(h2LDAK, pheno!="sex")


# ========= Differences in H2 estimates ================
LGdiffList=list()
phenoLG=levels(as.factor(h2LDAK$pheno))

for ( i in 1:length(phenoLG) ) {
  phenoLGin=phenoLG[i]
  h2CombPheno=subset(h2LDAK, pheno==phenoLGin)

  h2CombPheno$cor=subset(h2JK, pheno==paste0("ldak_", phenoLGin))$cor
  if(NROW(subset(h2CombPheno, method=="ldsc" ))==0) next
  
  ldscDiffOBS=h2Diff(df1=subset(h2CombPheno, method=="ldsc" & scheme=="unweighted" & scale == "observed"),
                     df2=subset(h2CombPheno, method=="ldsc" & scheme=="weighted" & scale == "observed"),
                     corH2=subset(h2CombPheno, method=="ldsc" & scheme=="unweighted" & scale == "observed")$cor)
  
  ldscDiffLIAB=h2Diff(df1=subset(h2CombPheno, method=="ldsc" & scheme=="unweighted" & scale == "liability"),
                      df2=subset(h2CombPheno, method=="ldsc" & scheme=="weighted" & scale == "liability"),
                      corH2=subset(h2CombPheno, method=="ldsc" & scheme=="unweighted" & scale == "liability")$cor)
  
  
  
  LGdiffList[[i]]=rbind(ldscDiffOBS, ldscDiffLIAB)
  
}

h2LDAK$sigFDR=""
h2LDAK$pFDRh2=NA
h2DiffDF=do.call(rbind, LGdiffList)
h2DiffDF$cor=NULL
h2CombDiff=rbind(h2LDAK, h2DiffDF)



# ======= PLot for checking 
h2CombDiff$scheme <- factor(h2CombDiff$scheme, levels = c("unweighted", "weighted", "h2diff"))
h2CombDiff$phenoClean=recodeHarmVar2(h2CombDiff$pheno)

# Plot for observed and liability scale
h2PlotLiab <- h2PlotFunc(df=subset(h2CombDiff, scale=="liability"))
h2PlotObs <- h2PlotFunc(df=subset(h2CombDiff, scale=="observed"))
h2PlotComb=ggarrange(h2PlotObs, h2PlotLiab, widths = c(1, 0.7)) 

# Save PLot
saveFigure(fileName=paste0(HOME,"/results/figures/h2PlotLiab"), plotName= h2PlotLiab , w=35, h=15)


# TEXT
h2CombDiffLDSC=subset(h2CombDiff, scale=="liability" & scheme =="h2diff" )
h2SigDiff=subset(h2CombDiffLDSC, pFDRh2<0.05)
h2OverEstimation=NROW(subset(h2SigDiff, h2>0))
h2UnderEstimation=NROW(subset(h2SigDiff, h2<0))

h2Top=h2CombDiffLDSC %>% 
  arrange( dplyr::desc(abs(h2))) %>%
  top_n(4, (as.numeric(abs(h2)))) 

h2ChangeText=paste0("Participation bias distorted heritability estimates (average change: ", round(mean(abs(h2CombDiffLDSC$h2)), 4), ", range ", round(min(abs(h2CombDiffLDSC$h2)), 3), "-", round(max(abs(h2CombDiffLDSC$h2)),3), "). h2diff was highest for ", 
                    h2Top$pheno[1], " (hdiff=", round(h2Top$h2[1], 3), " ", paste0("[", round(subset(h2CombDiff, scale=="liability" & scheme =="unweighted" & pheno==h2Top$pheno[1])$h2,2), " (unweighted) vs. ",round(subset(h2CombDiff, scale=="liability" & scheme =="weighted" & pheno==h2Top$pheno[1])$h2,2), " (weighted)] ) " ), 
                    h2Top$pheno[2], " (hdiff=", round(h2Top$h2[2], 3), " ", paste0("[", round(subset(h2CombDiff, scale=="liability" & scheme =="unweighted" & pheno==h2Top$pheno[2])$h2,2), " (unweighted) vs. ",round(subset(h2CombDiff, scale=="liability" & scheme =="weighted" & pheno==h2Top$pheno[2])$h2,2), " (weighted)] ) " ), 
                    h2Top$pheno[3], " (hdiff=", round(h2Top$h2[3], 3), " ", paste0("[", round(subset(h2CombDiff, scale=="liability" & scheme =="unweighted" & pheno==h2Top$pheno[3])$h2,2), " (unweighted) vs. ",round(subset(h2CombDiff, scale=="liability" & scheme =="weighted" & pheno==h2Top$pheno[3])$h2,2), " (weighted)] ) " ), 
                    "). of all assessed traits included in LDSC regression (", NROW(h2CombDiffLDSC), ") ", NROW(h2SigDiff), " showed significant (pfdr<0.05) h2diff estimates, of which ", h2OverEstimation, " (",  round(h2OverEstimation/NROW(h2SigDiff)*100, 2), "%) were over-estimated and ", h2UnderEstimation, " (",  round(h2UnderEstimation/NROW(h2SigDiff)*100, 2), "%) were under-estimated as a result of participation bias"  )






# ==================== GENETIC CORRELATIONS
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
rgWplot=subset(rgW, rownames(rgW)!="Sex")
rgWplot$Sex=NULL
rgplot=subset(rg, rownames(rg)!="Sex")
rgplot$Sex=NULL

rgWheat=heatM(rgWplot, text="Weighted genetic correlations") 
rgheat=heatM(rgplot, text="Unweighted genetic correlations")  

rgCombPlot=ggarrange(rgheat, rgWheat,nrow=2) 
saveFigure(fileName=paste0(HOME,"/results/figures/rgCombPlot"), plotName= rgCombPlot , w=60, h=55)


# ~======= Compare estimates
rgCorDF=data.frame(pheno=colnames(rg), ID=seq(1, NROW(rg), 1))
rgDiff = matrix(NA, NROW(rg),NCOL(rg))
rgPval = matrix(NA, NROW(rg),NCOL(rg)) # FDR corrected pval for rgDiff
rgPvalunadjusted = matrix(NA, NROW(rg),NCOL(rg)) # unorrected pval for rgDiff
rgSEDiff = matrix(NA, NROW(rg),NCOL(rg))
rgDir = matrix(NA, NROW(rg),NCOL(rg)) # direction of effects
rgPvalraw = matrix(NA, NROW(rg),NCOL(rg))
rgPvalWeighted = matrix(NA, NROW(rg),NCOL(rg))


for (i in 1:NCOL(rg)) {
  print(colnames(rg)[i])
  # Add Jackknife correlations
  rgCorDF$mergeID=paste0(phenoNamesM, "_", phenoNamesM[i])
  rgCorDFM=merge(rgCorDF, rgJK, by.x="mergeID", by.y="pheno", all.x=T, sort=F )
  rg_diff= rg[,i] - rgW[,i]
  rg_diff_se= sqrt( (rg_se[,i]^2+rg_seW[,i]^2) - (2*rgCorDFM$cor * rg_se[,i] * rg_seW[,i]) )
  ZscoreDiff=rg_diff/rg_diff_se
  ZscoreDiff_pval=2*pnorm(-abs(ZscoreDiff)) # two sided
  rgDiff[,i]=rg_diff
  rgSEDiff[,i]=rg_diff_se
  rgPvalunadjusted[,i]=ZscoreDiff_pval
  rgPval[,i]= do.call(rbind, lapply(ZscoreDiff_pval, function(x) p.adjust(x, "fdr", rgFDRadjust)))
  
  # get pvalues
  rgPvalraw[,i]=2*pnorm(-abs(rg[,i]/rg_se[,i])) # two sided
  rgPvalWeighted[,i]=2*pnorm(-abs(rgW[,i]/rg_seW[,i])) # two sided
  
  # check changes of direction in rg
  rgDirection=rep(NA, NROW(rg[,i]))
  rgDirection=ifelse(abs(rg[,i]) >  abs(rgW[,i]), "overEst", rgDirection)
  rgDirection=ifelse(abs(rg[,i]) <  abs(rgW[,i]), "underEst", rgDirection)
  rgDirection=ifelse(abs(rg_diff) < 0.1, "noChange", rgDirection)
  rgDirection=ifelse(round(rg[,i],2) < 0 &  round(rgW[,i],2) > 0, "changeDir", rgDirection)
  rgDirection=ifelse(round(rg[,i],2) > 0 &  round(rgW[,i],2) < 0, "changeDir", rgDirection)
  rgDir[,i]=rgDirection
}

colnames(rgDiff)=colnames(rg)
rownames(rgDiff)=colnames(rg)
rownames(rgPval)=colnames(rg)
colnames(rgPval)=colnames(rg)
rownames(rgSEDiff)=colnames(rg)
colnames(rgSEDiff)=colnames(rg)
rownames(rgDir)=colnames(rg)
colnames(rgDir)=colnames(rg)
rownames(rgPvalraw)=colnames(rg)
colnames(rgPvalraw)=colnames(rg)
rownames(rgPvalWeighted)=colnames(rg)
colnames(rgPvalWeighted)=colnames(rg)
rownames(rgPvalunadjusted)=colnames(rg)
colnames(rgPvalunadjusted)=colnames(rg)



# extract p-values (differences between weighted and standard)
rgPval_tri <- get_lower_tri(rgPval) # get p-values
rgPvalform <- melt(rgPval_tri, na.rm = TRUE)
rgPvalform$pvalFDR=rgPvalform$value
rgPvalform$labelM1=paste0(rgPvalform$Var1, "_", rgPvalform$Var2)
rgPvalform$Var1=NULL
rgPvalform$Var2=NULL
rgPvalform$value=NULL

# extract p-values (standard)
rgPvalraw_tri <- get_lower_tri(rgPvalraw) # get p-values
rgPvalrawform <- melt(rgPvalraw_tri, na.rm = TRUE)
rgPvalrawform$labelM1=paste0(rgPvalrawform$Var1, "_", rgPvalrawform$Var2)

# extract p-values (weighted)
rgPvalWeighted_tri <- get_lower_tri(rgPvalWeighted) # get p-values
rgPvalWeightedform <- melt(rgPvalWeighted_tri, na.rm = TRUE)
rgPvalWeightedform$labelM1=paste0(rgPvalWeightedform$Var1, "_", rgPvalWeightedform$Var2)

# extract rg difference
rgDiff_tri <- get_lower_tri(rgDiff) # get correlations
rgDiffform <- melt(rgDiff_tri, na.rm = TRUE)
rgDiffform$value=ifelse(rgDiffform$Var1==rgDiffform$Var2, NA, rgDiffform$value)
rgDiffform$labelM1=paste0(rgDiffform$Var1, "_", rgDiffform$Var2)
rgDiffform=merge(rgDiffform, rgPvalform, by="labelM1", all.x=T, sort=F)

rgDiffform$ID  <- 1:nrow(rgDiffform)
rgDiffform$Var1 = rgDiffform$Var1 %>% str_replace("ldak_", "")
rgDiffform$Var2 = rgDiffform$Var2 %>% str_replace("ldak_", "")

rgDiffform$Pheno1clean=recodeHarmVar2(rgDiffform$Var1)
rgDiffform$Pheno2clean=recodeHarmVar2(rgDiffform$Var2)
rgDiffform$type="correlation"
rgDiffform$label=paste0(rgDiffform$Pheno1clean, "_", rgDiffform$Pheno2clean)

# extract label for change in rg
rgDir_tri <- get_lower_tri(rgDir) # get correlations
rgDirform <- melt(rgDir_tri, na.rm = TRUE)
rgDirform$value=ifelse(rgDirform$Var1==rgDirform$Var2, NA, rgDirform$value)
rgDirformSel=data.frame(labelM1=paste0(rgDirform$Var1, "_", rgDirform$Var2), changeClass=rgDirform$value)

# Merge rg Diff and vector indexing type of change (overestimation versus underestimation versus change of direction of effects)
rgDiffformDir=merge(rgDiffform, rgDirformSel, by="labelM1", all.x=T, sort=F)


# ========= Supplement Table
# Standard errors for rgDiff
rgSEDiff_tri <- get_lower_tri(rgSEDiff) # get correlations
rgSEDiffform <- melt(rgSEDiff_tri, na.rm = TRUE)
rgSEDiffform$labelM1=paste0(rgSEDiffform$Var1, "_", rgSEDiffform$Var2)

# extract unweighted rg
rg_tri <- get_lower_tri(as.matrix(rg)) # get correlations
rgform <- melt(rg_tri, na.rm = TRUE)
rgform$labelM1=paste0(rgform$Var1, "_", rgform$Var2)

# extract unweighted rgse
colnames(rg_se)=colnames(rg)
rownames(rg_se)=rownames(rg)
rg_se_tri <- get_lower_tri(as.matrix(rg_se)) # get correlations
rg_seform <- melt(rg_se_tri, na.rm = TRUE)
rg_seform$labelM1=paste0(rg_seform$Var1, "_", rg_seform$Var2)

# extract weighted rg
rgW_tri <- get_lower_tri(as.matrix(rgW)) # get correlations
rgWform <- melt(rgW_tri, na.rm = TRUE)
rgWform$labelM1=paste0(rgWform$Var1, "_", rgWform$Var2)

# extract weighted SE
colnames(rg_seW)=colnames(rgW)
rownames(rg_seW)=rownames(rgW)
rgWse_tri <- get_lower_tri(as.matrix(rg_seW)) # get correlations
rgWseform <- melt(rgWse_tri, na.rm = TRUE)
rgWseform$labelM1=paste0(rgWseform$Var1, "_", rgWseform$Var2)

# extract pvalue (rgDIFF fdr corrected)
rgPval_tri <- get_lower_tri(as.matrix(rgPval)) # get correlations
rgPvalform <- melt(rgPval_tri, na.rm = TRUE)
rgPvalform$labelM1=paste0(rgPvalform$Var1, "_", rgPvalform$Var2)

# extract pvalue (rgDIFF uncorrected)
rgDiffPvalunadjusted_tri <- get_lower_tri(as.matrix(rgPvalunadjusted)) # get correlations
rgDiffPvalunadjustedform <- melt(rgDiffPvalunadjusted_tri, na.rm = TRUE)
rgDiffPvalunadjustedform$labelM1=paste0(rgDiffPvalunadjustedform$Var1, "_", rgDiffPvalunadjustedform$Var2)


mergeRG=list(
    data.frame(label=rgDiffformDir$label, Pheno1=rgDiffformDir$Var1, Pheno2=rgDiffformDir$Var2,
               rgDiff=rgDiffformDir$value, changeClass=rgDiffformDir$changeClass, ID=rgDiffformDir$ID),
     data.frame(label=rgSEDiffform$labelM1, rg_seDiff=rgDiffform$value),
    data.frame(label=rgDiffPvalunadjustedform$labelM1, pvalDiff=rgDiffPvalunadjustedform$value),
    data.frame(label=rgPvalform$labelM1, pvalFDR=rgPvalform$value),
     data.frame(label=rgform$labelM1, rg=rgform$value),
     data.frame(label=rg_seform$labelM1, rg_se=rg_seform$value),
    data.frame(label=rgPvalrawform$labelM1, rg_p=rgPvalrawform$value),
     data.frame(label=rgWform$labelM1, rgW=rgWform$value),
     data.frame(label=rgWseform$labelM1, rgW_se=rgWseform$value),
    data.frame(label=rgPvalWeightedform$labelM1, rgW_p=rgPvalWeightedform$value)
)


suppRGtable=Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "label", all.x = TRUE),
       mergeRG)

suppRGtableClean=subset(suppRGtable, Pheno1!=Pheno2 & Pheno1!="Sex" & Pheno2!="Sex")
suppRGtableClean$label=NULL
suppRGtableClean$ID=NULL
names(suppRGtableClean)[names(suppRGtableClean) == 'rgDiff'] <- 'rg difference (standard-weighted)'
names(suppRGtableClean)[names(suppRGtableClean) == 'rg_seDiff'] <- 'SE(rg difference)'
names(suppRGtableClean)[names(suppRGtableClean) == 'pvalDiff'] <- 'p-value (rg difference)'
names(suppRGtableClean)[names(suppRGtableClean) == 'pvalFDR'] <- 'p-value, FDR (rg difference)'
names(suppRGtableClean)[names(suppRGtableClean) == 'rg'] <- 'rg (standard)'
names(suppRGtableClean)[names(suppRGtableClean) == 'rg_se'] <- 'SE(rg standard)'
names(suppRGtableClean)[names(suppRGtableClean) == 'rg_p'] <- 'P(rg standard)'
names(suppRGtableClean)[names(suppRGtableClean) == 'rgW'] <- 'rg (weighted)'
names(suppRGtableClean)[names(suppRGtableClean) == 'rgW_se'] <- 'SE(rg weighted)'
names(suppRGtableClean)[names(suppRGtableClean) == 'rgW_p'] <- 'P(rg weighted)'
names(suppRGtableClean)[names(suppRGtableClean) == 'changeClass'] <- 'Description (change in rg)'



# ------------ COMBINE RG and H2
h2CombDiffuse=subset(h2CombDiff, scheme=="h2diff" & scale=="liability", select=c(pheno,h2, pFDRh2))
h2CombDiffuse$label=paste0(Pheno1clean=recodeHarmVar2(h2CombDiffuse$pheno), "_", recodeHarmVar2(h2CombDiffuse$pheno))
suppRGtablePlot=subset(suppRGtable, Pheno1!="Sex" & Pheno2!="Sex")
h2CombDiffCorComb=merge(suppRGtablePlot, h2CombDiffuse, by="label", all.x=T, sort=F)

h2CombDiffCorComb$rgDiffabs=abs(h2CombDiffCorComb$rg)-abs(h2CombDiffCorComb$rgW) # plot absolute diff
h2CombDiffCorComb$rgDiffabs=ifelse(is.na(h2CombDiffCorComb$rgDiff)==TRUE, NA, h2CombDiffCorComb$rgDiffabs)


h2CombDiffCorComb$corh2Diff=ifelse(is.na(h2CombDiffCorComb$rgDiff)==TRUE, h2CombDiffCorComb$h2, h2CombDiffCorComb$rgDiffabs)
h2CombDiffCorComb$type=ifelse(is.na(h2CombDiffCorComb$rgDiff)==TRUE, "h2", "correlation")
h2CombDiffCorComb$pvalFDR=ifelse(is.na(h2CombDiffCorComb$rgDiff)==TRUE, h2CombDiffCorComb$pFDRh2, h2CombDiffCorComb$pvalFDR) # add p=value for h2
h2CombDiffCorComb$pvalFDR=as.numeric(h2CombDiffCorComb$pvalFDR)

h2CombDiffCorComb$typeCol=NULL
h2CombDiffCorComb$changeClass=ifelse(is.na(h2CombDiffCorComb$changeClass)==T, "none", h2CombDiffCorComb$changeClass)
h2CombDiffCorComb$typeCol=ifelse(h2CombDiffCorComb$type=="h2" & h2CombDiffCorComb$corh2Diff > 0.02, "h2Overest", "none")
h2CombDiffCorComb$typeCol=ifelse(h2CombDiffCorComb$type=="h2" & h2CombDiffCorComb$corh2Diff < -0.02, "h2Underest", h2CombDiffCorComb$typeCol)
h2CombDiffCorComb$typeCol=ifelse( h2CombDiffCorComb$changeClass=="underEst", "rgUnderest", h2CombDiffCorComb$typeCol)
h2CombDiffCorComb$typeCol=ifelse(h2CombDiffCorComb$changeClass=="overEst", "rgOverest", h2CombDiffCorComb$typeCol)
h2CombDiffCorComb$typeCol=ifelse(h2CombDiffCorComb$changeClass=="changeDir", "changeDir", h2CombDiffCorComb$typeCol)

h2CombDiffCorComb$corh2DiffRec=round(h2CombDiffCorComb$corh2Diff, 2)
h2CombDiffCorComb$corh2DiffRec=ifelse(h2CombDiffCorComb$changeClass=="changeDir", paste0(round(h2CombDiffCorComb$rg,2), " \n", round(h2CombDiffCorComb$rgW, 2) ), h2CombDiffCorComb$corh2DiffRec)
h2CombDiffCorComb$corh2DiffRec=ifelse(h2CombDiffCorComb$pvalFDR<0.05, paste0(h2CombDiffCorComb$corh2DiffRec, "*"), h2CombDiffCorComb$corh2DiffRec )

# ============= Plot
levels(as.factor(h2CombDiffCorComb$typeCol))
h2CombDiffCorComb$typeCol <- factor(h2CombDiffCorComb$typeCol, levels=c("h2Overest", "h2Underest", "rgOverest", "rgUnderest", "none", "changeDir" ))

colH2RD=c("chocolate1", "darkgoldenrod1", "cadetblue3", "palegreen3", "white", "paleturquoise1")

colH2RDLabel=expression(paste("Overestimation ",italic(h^2)), 
                        paste("Underestimation ",italic(h^2)),
                        paste("Overestimation ",italic(rg)),
                        paste("Underestimation ",italic(rg)),
                        paste(""),
                        paste("Change in direction of ",italic(rg)))


# Heatmap 
heatRG=ggplot(h2CombDiffCorComb, aes(fct_reorder(Pheno1, ID), fct_reorder(Pheno2, ID), fill= typeCol)) + 
  theme_minimal() +
  scale_fill_manual("",values = colH2RD, label=colH2RDLabel) +
  geom_tile() +
  geom_text(aes(label = corh2DiffRec ), size=2) +
  theme(panel.grid = element_blank(),  axis.text.x = element_text(angle = 45, hjust=1,  size=12),
        axis.text.y = element_text( size=12) ) +
  labs(x =  "", 
       y = "",
       title = "") +
theme(legend.position="none")


# ============= Examples: two traits
rgUnweightedAll=data.frame(pheno1=meltIndividual(rg)$Var1, pheno2=meltIndividual(rg)$Var2, rg=meltIndividual(rg)$value, se=meltIndividual(rg_se)$value, model="unweighted")
rgUnweighted=subset(rgUnweightedAll, pheno1!="Sex" & pheno2!="Sex")
rgWeightedAll=data.frame(pheno1=meltIndividual(rg)$Var1, pheno2=meltIndividual(rg)$Var2, rg=meltIndividual(rgW)$value, se=meltIndividual(rg_seW)$value, model="weighted")
rgWeighted=subset(rgWeightedAll, pheno1!="Sex" & pheno2!="Sex")

rgUnweighted$uCI=rgUnweighted$rg + 1.96 * rgUnweighted$se
rgUnweighted$lCI=rgUnweighted$rg - 1.96 * rgUnweighted$se
rgWeighted$uCI=rgWeighted$rg + 1.96 * rgWeighted$se
rgWeighted$lCI=rgWeighted$rg - 1.96 * rgWeighted$se
plotPheno=strsplit(rgJK$pheno, "_ldak_")
rgJK$pheno1=recodeHarmVar2(sapply(plotPheno, "[[" , 1) %>% str_replace("ldak_", ""))
rgJK$pheno2=recodeHarmVar2(sapply(plotPheno, "[[" , 2))

# Add Jackknife correlations
rgJK$phenoM=paste0(rgJK$pheno1, "_", rgJK$pheno2)
rgWeightedM=rgWeighted
rgWeightedM$phenoM=paste0(rgWeightedM$pheno1, "_", rgWeightedM$pheno2)
rgWeightedM$ID=seq(1,NROW(rgWeightedM),1)
rgWeightedM1=merge(rgWeightedM, rgJK, by="phenoM", all.x=T, sort=F, suffixes = c("", ".rem"))
rgWeightedMOrder=rgWeightedM1[order(rgWeightedM1$ID),]

rg_diff= rgUnweighted$rg - rgWeightedMOrder$rg
rg_diff_se= sqrt( (rgUnweighted$se^2+rgWeightedMOrder$se^2) - (2*rgWeightedMOrder$cor * rgUnweighted$se * rgWeightedMOrder$se) )
ZscoreDiff=rg_diff/rg_diff_se
ZscoreDiff_pval=2*pnorm(-abs(ZscoreDiff)) # two sided
rgUnweighted$pval_fdr <- do.call(rbind, lapply(ZscoreDiff_pval, function(x) p.adjust(x, "fdr", NROW( ZscoreDiff_pval))))
rgUnweighted$sigFDR=ifelse(rgUnweighted$pval_fdr<0.05, "*", "")

rgWeighted$pval_fdr=rgUnweighted$pval_fdr
rgWeighted$sigFDR=""
rg_diff_abs= abs(rgUnweighted$rg) - abs(rgWeightedMOrder$rg)
rgUnweighted$changeEst_cat=ifelse(rg_diff_abs > .10, "change_orig_overestimated", "ns_change" )
rgUnweighted$changeEst_cat=ifelse(rg_diff_abs < -.10, "change_orig_underestimated", rgUnweighted$changeEst_cat )
rgWeighted$changeEst_cat=rgUnweighted$changeEst_cat


rgComb=rbind(rgUnweighted, rgWeighted)
levels(as.factor(h2CombDiffCorComb$typeCol))
levels(as.factor(rgComb$changeEst_cat))
rgComb$colourSchemeMR=revalue(  rgComb$changeEst_cat, c("change_orig_overestimated"="cadetblue3",
                                                        "change_orig_underestimated"="palegreen3",
                                                        "ns_change"= "lightgrey") )



rgCombselEx=subset(rgComb, pheno2 %in% c("Education (age)", "Smoking status"))
plotRGshort=RGplotIndiv(rgCombselEx)
plotRGshort = plotRGshort+  scale_x_discrete(position = 'top')  + theme(plot.margin = margin(t=0, r=0, b=4, l=0, "cm")) 


# SAVE COMBINDED PPLOT
legendRG <- cowplot::get_legend(heatRG + theme(legend.position="top"))
legendPlotRG=as_ggplot(legendRG)

heatRGComb=ggarrange(ggarrange(heatRG, plotRGshort, nrow = 1, widths=c(0.9, 0.7), labels=c("A", "B" )), legendPlotRG, nrow = 2, heights=c(1, 0.1))

# Save plot
saveFigure(fileName=paste0(HOME,"/results/figures/heatRGComb"), plotName= heatRGComb , w=40, h=22)


# TEXT
suppRGtableSel=subset(suppRGtable, Pheno1!=Pheno2 & Pheno1!="Sex" & Pheno2!="Sex")

topUnderRG=subset(suppRGtableSel, changeClass=="underEst") %>% 
  top_n(3, (as.numeric(abs(rgDiff)))) %>% 
  arrange(-desc((rgDiff)))

topOverRG=subset(suppRGtableSel, changeClass=="overEst")  %>% 
  top_n(3, (as.numeric((rgDiff)))) %>% 
  arrange(desc((rgDiff)))


rgDiffSig=subset(suppRGtableSel, pvalFDR<0.05)
rgDiffSigFreq=as.data.frame(table(c(rgDiffSig$Pheno1, rgDiffSig$Pheno2 )))
rgDiffSigFreqOrder <- rgDiffSigFreq[order(-rgDiffSigFreq$Freq),]


rgChangeText=paste0("Average change: ", round(mean(abs(suppRGtableSel$rgDiff), na.rm=T), 3), "; median: ", round(median(abs(suppRGtableSel$rgDiff), na.rm=T), 3) ,"; range ", round(min(abs(suppRGtableSel$rgDiff), na.rm=T), 3), "-", round(max(abs(suppRGtableSel$rgDiff), na.rm=T),3), "). Bias leading to overestimation (rgdiff>0) was largerst for ",
                    paste0(paste0(topOverRG$Pheno1, "~", topOverRG$Pheno2, " [rgDiff=", round(topOverRG$rgDiff, 2) , ", rgStandard=", round(topOverRG$rg, 2),  ", rgW=", round(topOverRG$rgW, 2)  ), collapse="; "), 
                    ". Bias leading to underestimation was largest for ",
                    paste0(paste0(topUnderRG$Pheno1, "~", topUnderRG$Pheno2, " [rgDiff=", round(topUnderRG$rgDiff, 2) , ", rgStandard=", round(topUnderRG$rg, 2),  ", rgW=", round(topUnderRG$rgW, 2) ), collapse="; "), ". ", 
                    NROW(rgDiffSig), " (", round( NROW(rgDiffSig)/NROW(suppRGtableSel) *100, 2), "%) of the assessed trait-pairs (n=", NROW(suppRGtableSel), ")  were significantly (pFDR<0.05) different, of which",  paste0(paste0(rgDiffSigFreqOrder$Freq, " involved ", rgDiffSigFreqOrder$Var1), collapse= "; ") )

# change in direction of rg
topDirChangeRG=subset(suppRGtableSel, changeClass=="changeDir")  %>% 
  top_n(2, (as.numeric((rgDiff)))) %>% 
  arrange(desc((rgDiff)))

topDirChangeRG_Diff=subset(suppRGtableSel, changeClass=="changeDir" & pvalDiff < 0.05) 
topDirChangeRG_sig=subset(suppRGtableSel, changeClass=="changeDir" & rg_p < 0.05 & rgW_p < 0.05) 

changeRGdirText=paste0("A number of genetic correlations were in opposite directions bewteen rg and rgw (", NROW(subset(suppRGtableSel, changeClass=="changeDir") ), " out of the ", NROW(suppRGtableSel), " assessed trait-pairs, but most did not show significant (p<0.05) rg_DIFF. The largest rgDiff in opposite directions were present for ", paste0(topDirChangeRG$Pheno1, "~", topDirChangeRG$Pheno2, " (rg=", round(topDirChangeRG$rg, 2), "; prg=", formatC(topDirChangeRG$rg_p,2), " versus rgW=",  round(topDirChangeRG$rgW, 2), "; prgW=", formatC(topDirChangeRG$rgW_p,2), ", prgDIFF=", formatC(topDirChangeRG$pvalDiff,2) , ", prgDiff_fdr=", formatC(topDirChangeRG$pvalFDR,2),")", collapse="  //  "  ) )



# ===========================================================================
# ============================ Sex check ====================================
# ===========================================================================
sexGWACheck=readRDS(paste0(HOME,"/results/rds/sexGWACheck.rds"))

# Reduction in Beta estimates
compSexRaw23=compareBeta(df=sexGWACheck, b1="BETA_23", b2="BETA", se1="SE_23", se2="SE")
compSexRaw23$group="GWA"

compSexW23=compareBeta(df=sexGWACheck, b1="BETA_23", b2="BETA_sw", se1="SE_23", se2="SE_sw")
compSexW23$group="wGWA"

compSexComb=rbind(compSexRaw23, compSexW23)

sigRawSex=subset(compSexRaw23, col=="sig")
sigWSex=subset(compSexW23, col=="sig")

effect23andME=paste0("here ", NROW(sigWSex), " (", round(NROW(sigWSex)/NROW(compSexW23)*100, 2), "%) SNPs showed significanlt differential effects in the weigthed analysys, of which all reduced sex-associated SNP effects. In contrast, only ", NROW(sigRawSex), " (", round(NROW(sigRawSex)/NROW(compSexRaw23)*100, 2), "% of effects estimated in standard GWA deviated from previously sex-associated effects)")

sexBetaChange = ggplot(compSexComb, aes(x = fct_reorder(SNP, -diffBeta), 
                        y = diffBeta, 
                        ymin = lCI_diff, 
                        ymax = uCI_diff,
                        col=col,
                        shape=group)) +
  scale_colour_manual("",values = c("grey", "purple4" ), labels = c("", ""   ), guide = "none" ) +
  scale_shape_discrete(name="") +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)+ 
  geom_linerange(aes( ymin = lCI_diff,
                      ymax = uCI_diff),  lwd = .2, position = position_dodge(width = 0.5)) + 
  geom_pointrange(aes(ymin = lCI_diff,
                      ymax = uCI_diff, size=1), lwd = 0.4, position = position_dodge(width = 0.5)) + 
  theme_classic() + 
  coord_flip() +
  theme(legend.position="none", 
        axis.text.x=element_text(size=8),
        axis.text.y=element_text(size=6),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t=1, r=0, b=0, l=1, "cm"))  + 
  scale_x_discrete(position = "top") +
  
  labs(x = "" , 
       y = expression(paste( "23andMe(",italic(beta["STD"]), ") - UKBB(", italic(beta["STD"]), ")" )))


# get h2 estimates
sexH2=subset(h2LDAKSex, pheno=="sex" & scale == "liability")
sexH2$h2_pval=2*pnorm(-abs(sexH2$h2/sexH2$h2_se))
h2GWADF=data.frame(model=sexH2$scheme, type="h2", est=sexH2$h2, lCI=sexH2$lCI, uCI=sexH2$uCI, pval=sexH2$h2_pval)
h2GWADF$tickLabel=ifelse(h2GWADF$model=="unweighted", "GWA", "wGWA")

sexBiasPlot <- ggplot(h2GWADF, aes(x = tickLabel, 
                                     y = est, 
                                     ymin = lCI, 
                                     ymax = uCI,
                                     col=model,
                                    shape = model)) +
  geom_linerange(aes( ymin = lCI,
                      ymax = uCI),  lwd = .2, position = position_dodge(width = 0.5)) + 
  geom_pointrange(aes(ymin = lCI,
                      ymax = uCI, size=1), lwd = 0.4, position = position_dodge(width = 0.5)) + 
  theme_classic() + 
  guides( shape = "none") +
  theme(legend.position="none", 
        axis.text.x=element_text(size=10),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        panel.grid.minor = element_blank())  + 
  scale_color_manual("",values = c("darkslateblue", "darkslateblue"), labels = c("", ""   ), guide = "none"  ) +
  scale_x_discrete(position = "bottom") +
  labs(x =  expression(paste( italic(h["sex"]^2))) , 
       y = "",
       title = "") +    

  geom_abline(intercept = 0, slope = 0, color="black", linetype="solid", size=0.8)  + 
  theme(plot.margin = margin(t=0, r=1, b=0, l=0, "cm")) 
print(sexBiasPlot)

legendSex <- cowplot::get_legend(sexBetaChange+theme(legend.position="top"))
legendPlotSex=as_ggplot(legendSex)

sexBiasComb=ggarrange(sexBiasPlot, sexBetaChange, widths = c(0.7, 0.5)) 
sexBiasComb=ggarrange(legendPlotSex, nrow = 2, heights=c(0.1, 1), ggarrange(sexBiasPlot, sexBetaChange, nrow = 2, heights=c(0.4, 0.8), labels=c("A", "B")))

# Save plot
saveFigure(fileName=paste0(HOME,"/results/figures/sexBiasPlot"), plotName= sexBiasComb , w=9, h=22)


# Extract estimates for TEXT
weightedH2=subset(sexH2, scheme=="weighted")$h2
weightedH2_p=subset(sexH2, scheme=="weighted")$h2_pval
unweightedH2=subset(sexH2, scheme=="unweighted")$h2
unweightedH2_p=subset(sexH2, scheme=="unweighted")$h2_pval
h2TextSex=paste0("we find that wGWA recuded h2 by ", round((unweightedH2-weightedH2)/unweightedH2,2)*100, "% (h2 on liability scale= ", round(weightedH2, 3), "%, p=", formatC(weightedH2_p,2), " in wGWA versus ", round(unweightedH2, 3), "%, p=", formatC(unweightedH2_p,2), " in GWA). ")


#######################################################
# ============= MENDELIAN RANDOMIZATION ===============
#######################################################
MRdfall=readRDS(paste0(HOME, "/results/rds/MRldak.rds"))
MRdfall$corJK=MRdfall$cor

# apply correction for inclusion of ns instruments
MRdfall$correctVar=ifelse(MRdfall$minPexposure>=5e-8, MRdfall$correctionFac, 1 )
MRdfall$b_corr=MRdfall$b*MRdfall$correctVar
MRdfall$b_var=MRdfall$se^2
MRdfall$b_varcor=MRdfall$b_var*MRdfall$correctVar
MRdfall$se_corr=sqrt(MRdfall$b_varcor)

MRdfall$b=MRdfall$b_corr
MRdfall$se=MRdfall$se_corr
MRdfall$uCI=MRdfall$b + 1.96 * MRdfall$se
MRdfall$lCI=MRdfall$b - 1.96 * MRdfall$se
MRdfall$exposure_outcome=paste0(MRdfall$exposure,"_",MRdfall$outcome)

# select phenotypes with at least 10 instruments
MRdfsig=subset(MRdfall, nsnp>=10)
MRdf=subset(MRdfsig, exposure_outcome!="PS_PS" & outcome!="PS" & exposure!="PS")
nadjustMR=NROW(MRdf$exposure_outcome)/2

# get pvalues for differences between beta estimates
listBetaDiffMR=list()
for ( i in 1:length(levels(as.factor(MRdf$exposure_outcome))) ) {
  phenoin=levels(as.factor(MRdf$exposure_outcome))[i]
  
  beta1=subset(MRdf, exposure_outcome==phenoin & model == "unweighted")$b
  beta2=subset(MRdf, exposure_outcome==phenoin & model == "weighted")$b
  se1=subset(MRdf, exposure_outcome==phenoin & model == "unweighted")$se
  se2=subset(MRdf, exposure_outcome==phenoin & model == "weighted")$se
  pval1=subset(MRdf, exposure_outcome==phenoin & model == "unweighted")$pval
  pval2=subset(MRdf, exposure_outcome==phenoin & model == "weighted")$pval
  cor=subset(MRdf, exposure_outcome==phenoin & model == "weighted")$corJK
  uCI1= beta1+ 1.96 * se1
  lCI1=beta1 - 1.96 * se1
  uCI2= beta2+ 1.96 * se2
  lCI2=beta2 - 1.96 * se2
  # Get p-values for differences between beta
  beta_diff=beta1 - beta2
  beta_diff_abs=abs(beta1) - abs(beta2)
  beta_diff_se= sqrt( se1^2 + se2^2 - 2*cor * se1 * se2 )
  ZscoreDiff=beta_diff/beta_diff_se
  ZscoreDiff_pval=2*pnorm(-abs(ZscoreDiff)) # two sided
  uCI_diff= beta_diff+ 1.96 * beta_diff_se
  lCI_diff=beta_diff - 1.96 * beta_diff_se
  ZscoreDiff_pval_fdr=p.adjust(ZscoreDiff_pval, "fdr",nadjustMR)
  
  changeEst_cat="none"
  changeEst_cat=ifelse(beta_diff_abs >0.1, "overEst", changeEst_cat)
  changeEst_cat=ifelse(beta_diff_abs < -0.1, "underEst", changeEst_cat)
  changeEst_cat=ifelse(beta1>0 & beta2<0, "changeDir", changeEst_cat)
  changeEst_cat=ifelse(beta1<0 & beta2>0, "changeDir", changeEst_cat)
  
  listBetaDiffMR[[i]]=data.frame(exposure_outcome=phenoin, beta1, beta2, se1, se2, pval1, pval2, beta_diff_abs, beta_diff, beta_diff_se, uCI_diff, lCI_diff, changeEst_cat, ZscoreDiff_pval, ZscoreDiff_pval_fdr, cor, lCI1, uCI1, lCI2, uCI2)
}


# =========== individual MR 
listBetaDiffDF <- do.call(rbind, listBetaDiffMR)
MRDiff=merge(MRdf, listBetaDiffDF, by="exposure_outcome", all.x=T)
MRDiff$pDiff_sig= ifelse(MRDiff$ZscoreDiff_pval_fdr <0.05 , "*", "")
MRDiff$pval1_fdr <- do.call(rbind, lapply(MRDiff$pval1, function(x) p.adjust(x, "fdr", nadjustMR)))
MRDiff$pval2_fdr <- do.call(rbind, lapply(MRDiff$pval2, function(x) p.adjust(x, "fdr", nadjustMR)))
MRDiff$label_coded=ifelse(MRDiff$ZscoreDiff_pval_fdr <0.05,  "*", "")
MRDiff$ID=seq(1,NROW(MRDiff), 1)

remDup=subset(MRDiff, select=c(exposure_outcome, label_coded, ID))
remDupInc=remDup[!duplicated(remDup[c("exposure_outcome", "label_coded")]), ]
remDupIncSel=data.frame(ID=remDupInc$ID, label_codedNEW=remDupInc$label_coded)
MRDiff=merge(MRDiff, remDupIncSel, by="ID", all.x=T)

# relabel
exposureLabel=data.frame(exposure=variablesUKBB$labelUKBB_recoded, exposure_clean=variablesUKBB$label_clean, exposure_clean_short=variablesUKBB$label_clean_short )
outcomeLabel=data.frame(outcome=variablesUKBB$labelUKBB_recoded, outcome_clean=variablesUKBB$label_clean, outcome_clean_short=variablesUKBB$label_clean_short )

MRDiff_exp=merge(MRDiff, exposureLabel, by="exposure", all.x=T, sort = F)
MRDiff_epx_out=merge(MRDiff_exp, outcomeLabel, by="outcome", all.x=T, sort = F)
MRDiff=MRDiff_epx_out[order(MRDiff_epx_out$ID), ]

levels(as.factor(MRDiff$changeEst_cat))
MRexpLabel=expression(paste(""),
                      paste(""), # remove from plot: differential MR effects + no effects
                      paste("Overestimation due to participation bias"), 
                      paste("Underestimation due to participation bias"))

# ============ heatmap MR
MR_heat=ggplot(MRDiff, aes(x=exposure_clean, y=outcome_clean)) + 
  geom_tile(aes(fill = changeEst_cat), colour =   "white")  +  
  scale_x_discrete(limits = rev(levels(MRDiff$changeEst_cat))) +
  scale_fill_manual("", values= c("white","white","purple4","darkcyan"),  labels = MRexpLabel)  +
  geom_text(aes(label =label_coded), size=2.5, colour=  "white") +
  ylab("Outcome") + 
  xlab("Exposure") +
  theme_classic() + 
  labs(title="") +
  theme(axis.text.x=element_text(size=12, angle=45, hjust=1),
        axis.text.y=element_text(size=12,hjust=1),
        legend.position = "none",  axis.line = element_line(colour = "white")) + 
  theme( legend.position = "none",
         plot.margin = margin(t=0, r=0, b=0, l=0, "cm")) 

# ============ Scatter plot
MRDiff$colourSchemeMR=revalue(  MRDiff$changeEst_cat, c("overEst"="purple4",
                                                        "underEst"="darkcyan",
                                                        "none"= "lightgrey",
                                                        "changeDir" = "lightgrey" ) )

plotERRORns=subset(MRDiff, changeEst_cat=="none" | changeEst_cat=="changeDir") 
plotERROR=subset(MRDiff, changeEst_cat!="none" & changeEst_cat!="changeDir") 

MRDiff$label_coded1=ifelse(MRDiff$changeEst_cat != "none",  paste0(MRDiff$exposure_clean_short, " (E) \n", MRDiff$outcome_clean_short, " (O)" ), NA)
MRDiff$label_coded2=paste0(MRDiff$label_coded1, MRDiff$label_coded)
MRDiff$label_coded3=ifelse(duplicated(MRDiff$label_coded2)==T, NA, MRDiff$label_coded2)

scatterMR=ggplot(aes(y=beta1, x=beta2, color=as.factor(changeEst_cat)), data=MRDiff) +
  geom_abline(intercept = 0, slope = 1, color="black", linetype="solid", size=1) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey", size = 0.5) +
  geom_vline(xintercept=0, linetype="dashed", color = "grey", size = 0.5) +
  geom_point(data=plotERRORns, size=2, colour=plotERRORns$colourScheme) +
  geom_point(data=plotERROR, size=3, colour=plotERROR$colourScheme) + 
  
  geom_errorbar(data=plotERROR, aes(ymin=lCI1, ymax=uCI1), colour=plotERROR$colourScheme,height=0,  position=position_dodge(.9)) +
  geom_errorbarh(data=plotERROR,aes(xmin=lCI2, xmax=uCI2), colour=plotERROR$colourScheme, width=0,  position=position_dodge(.9)) +
  
  theme_classic() +
  theme(legend.position = "none" )  +
  labs(x = expression(paste( italic(alpha[w]) )), 
       y = expression(paste( italic(alpha) )) ,
  ) 
scatterMRnoLab=scatterMR

library(ggrepel)
scatterMROut = scatterMR +   geom_label_repel(data = subset(MRDiff, is.na(label_coded1)==F & changeEst_cat == "overEst"  ), 
                                              aes(label = label_coded3, size = NULL), 
                                              nudge_y = 0.7,
                                              nudge_x = -0.3,
                                              segment.size  = 0.3,
                                              fontface = 'bold', 
                                              size=1.5) +   scale_colour_manual(values = c( "purple4"  ,"darkcyan", "grey") )

scatterMROut = scatterMROut +   geom_label_repel(data = subset(MRDiff, is.na(label_coded1)==F &  changeEst_cat == "underEst"  ), 
                                                 aes(label = label_coded3, size = NULL), 
                                                 nudge_y = -0.7,
                                                 nudge_x = 0.3,
                                                 segment.size  = 0.3,
                                                 fontface = 'bold', 
                                                 size=1.5) +   scale_colour_manual(values = c( "purple4"  ,"darkcyan", "grey") )




legendMR <- cowplot::get_legend(MR_heat+theme(legend.position="top"))
legendPlotMR=as_ggplot(legendMR)
MRcombOut=ggarrange(legendPlotMR, ggarrange(ggarrange(MR_heat, scatterMROut, nrow = 1, labels=c("A", "B"))), nrow = 2, heights=c(0.1, 1) )

# Save plot
saveFigure(fileName=paste0(HOME,"/results/figures/MRcombOut"), plotName= MRcombOut , w=30, h=16)
saveFigure(fileName=paste0(HOME,"/results/figures/scatterMRnoLab"), plotName= scatterMRnoLab , w=15, h=16)
saveFigure(fileName=paste0(HOME,"/results/figures/MR_heat"), plotName= MR_heat , w=15, h=12)


# ==== Supplementary Figures
MRmodelName=levels(as.factor(MRDiff$exposure))
nMRmodels=length(levels(as.factor(MRDiff$exposure)))
MRround1=round(nMRmodels/2,0)

inPotMR1=MRmodelName[1:MRround1]
inPotMR2=MRmodelName[(MRround1+1):nMRmodels]


MRDiff$exposure_clean=recodeHarmVar2(MRDiff$exposure)
MRDiff$outcome_clean=recodeHarmVar2(MRDiff$outcome)
inPotMR1df=subset(MRDiff, exposure %in% inPotMR1)
inPotMR2df=subset(MRDiff, exposure %in% inPotMR2)

inPotMR1out=MRplotIndiv(inPotMR1df)
inPotMR2out=MRplotIndiv(inPotMR2df)

inPotMR2df$tickLabel=ifelse(inPotMR2df$model=="unweighted", "GWA", "wGWA")

legendMRsupp <- cowplot::get_legend(ggplot(inPotMR2df, aes(x = outcome, 
                                                           y = b,
                                                           shape=tickLabel) ) + geom_point() +theme_classic() + labs(shape = "") + theme(legend.position="top") )
legendPlotMRsupp=as_ggplot(legendMRsupp)
library(gdata )
SuppPotMRcombined=ggarrange(ggarrange(legendPlotMR, legendPlotMRsupp, heights = c(0.1, 0.1), nrow=2), inPotMR1out,inPotMR2out, nrow = 3, heights=c(0.1, 1, 1) ) 

# Save Plot
saveFigure(fileName=paste0(HOME,"/results/figures/plorMRAllsupp"), plotName= SuppPotMRcombined , w=45, h=60)


# ============== TEXT ================
changeMR=MRDiff[!duplicated(MRDiff$exposure_outcome), ]
MRTop=changeMR %>% 
  arrange( dplyr::desc(abs(beta_diff))) %>%
  top_n(4, (as.numeric(abs(beta_diff)))) 


meanMRtext=paste0("On average, participation bias led to a |",round(mean(abs(changeMR$beta_diff)), 3) ,"|, min=",round(min(abs(changeMR$beta_diff)), 3), ", max=" ,round(max(abs(changeMR$beta_diff)), 3) ," change in standardized beta estimates. Maximum bias: ", round(max(abs(changeMR$beta_diff)), 3) )
MRmostAffected=
  paste0("Most affected were lifestyle choices, including ",  MRTop$exposure[1], " on ", MRTop$outcome[1] , " (beta=", round(MRTop$beta1[1], 2), ", betaW=", round(MRTop$beta2[1],2), "),",
         MRTop$exposure[2], " on ", MRTop$outcome[2] , " (beta=", round(MRTop$beta1[2], 2), ", betaW=", round(MRTop$beta2[2],2), ")",
         MRTop$exposure[3], " on ", MRTop$outcome[3] , " (beta=", round(MRTop$beta1[3], 2), ", betaW=", round(MRTop$beta2[3],2), ")",
         MRTop$exposure[4], " on ", MRTop$outcome[4] , " (beta=", round(MRTop$beta1[4], 2), ", betaW=", round(MRTop$beta2[4],2), ")")

MRover=subset(changeMR, changeEst_cat=="overEst")
MRunder=subset(changeMR, changeEst_cat=="underEst")
MRdiffSig=subset(changeMR, ZscoreDiff_pval_fdr <0.05)
overUnderText=paste0("Of all exposure-outcome associtions tested (k=", NROW(changeMR), "), ",NROW(MRover)+NROW(MRunder)," (", round((NROW(MRover)+NROW(MRunder))/ NROW(changeMR), 2)*100, "%) estimates were either over-estimated (", round(NROW(MRover)/ NROW(changeMR), 2)*100, "%)) or under-estimated (", round(NROW(MRunder)/ NROW(changeMR), 2)*100, "%)). However, significant (pFDR<0.05) differential effects were only present for ", NROW(MRdiffSig), " of the exposure-outcome associations tested (", paste0(MRdiffSig$exposure_outcome, collapse=", "), ")")
MRtext=paste0(meanMRtext, MRmostAffected, overUnderText)

# Change in direction
changeDirMR=subset(MRDiff, changeEst_cat=="changeDir" & model=="unweighted")
NROW(changeDirMR)

MRTopChangeDir=changeDirMR %>% 
  arrange( dplyr::desc(abs(beta_diff))) %>%
  top_n(2, (as.numeric(abs(beta_diff)))) 


MRTopChangeText=paste0("The largest differences between α and αw resulting from opposite effects was present for: ", paste0( MRTopChangeDir$exposure, " on ", MRTopChangeDir$outcome, " (a=", round(MRTopChangeDir$beta1, 2), "; palpha=", formatC(MRTopChangeDir$pval1, 2), " versus aw=", round(MRTopChangeDir$beta2, 2), "; palpha_we=", formatC(MRTopChangeDir$pval2, 2), collapse=" /// "))






################################################################
# ====================== Create output tables ==================
################################################################
source(paste0(HOME, "/analysis/createTables.R"))



