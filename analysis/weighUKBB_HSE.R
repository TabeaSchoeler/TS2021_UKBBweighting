#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
HOME=args[1]
validation=args[2]

source(paste0(HOME, "/analysis/input.R"))
source(paste0(HOME, "/analysis/functions.R"))
UKBBdir=UKBB
con <- file(paste0(HOME, "/output/log/UKBB_HSE",validation, ".log"))
sink(con, append=TRUE, type="output")
set.seed(1234) 

print("read in variables for weighting")
varInc=readRDS(paste0(HOME, "/results/rds/weightVariables.rds"))
print("read in raw data")
UKBBall=readRDS(paste0(HOME,"/results/rds/UKBBall.rds"))
UKBBall$ID=paste0(UKBBall$eid, "_", rep("UKBB", NROW(UKBBall)))

HSEall=readRDS(paste0(HOME,"/results/rds/HSEall.rds"))
HSEall$ID=paste0(HSEall$eid, "_", rep("HSE", NROW(HSEall)))

print("read in data with processed variables")
UKBBHSE=readRDS( paste0(HOME,"/results/rds/UKBBHSE.rds"))
UKBBHSE$ID=paste0(UKBBHSE$eid, "_", UKBBHSE$sampleName)


print("Load list of files to be included in GWA")
drop_auth(rdstoken = paste0(HOME, "/token.rds"))  # authentication for dropbox
drop_download(
  local_path = paste0(HOME,"/data/variableList.xlsx"),
  path = paste0(LOCAL,"/data/variableList.xlsx"), overwrite=TRUE)

library("readxl")
variableList <- readxl::read_excel(paste0(HOME, "/data/variableList.xlsx"))


if(validation=="all"){
print("Generate weights")
UKBBHSEgen=getWeights(data=UKBBHSE, varInc=varInc, validation="all", return="data")
print("save output")
saveRDS(UKBBHSEgen , paste0(HOME,"/results/rds/datHSEUKBB.rds"))
print("All output saved")
uploadLog(file=paste0("UKBB_HSE",validation, ".log"))
}



if(validation=="validation"){
       print("Run 5-fold cross validation")
       UKBBHSE_CV=readRDS(paste0(HOME,"/results/rds/UKBBHSE_CV.rds"))
       UKBBHSElist=lapply(UKBBHSE_CV, function(x) getWeights(listCV=x, varInc=varInc, validation="validation", return="data"))
       
       UKBBHSElistSel=lapply(UKBBHSElist, function(x) subset(x, select=c(ID ,probs)))
       UKBBHSE_CV_df=Reduce(function(x, y) merge(x=x, y=y, by="ID", all=T), UKBBHSElistSel)
       UKBBHSE_CV_df$probs= rowMeans(UKBBHSE_CV_df[c(2:6)], na.rm = TRUE)
       UKBBHSE_CV_df$IPSW=(1-UKBBHSE_CV_df$probs)/UKBBHSE_CV_df$probs
       UKBBHSE_CV_Out=merge(UKBBHSE, subset(UKBBHSE_CV_df, select=c(ID, IPSW, probs)), by="ID", all.x=T, sort=F)
       UKBBHSE_CV_Out$IPSWNorm=c(normalizeWeights(subset(UKBBHSE_CV_Out, sampleName=="HSE")$IPSW), normalizeWeights(subset(UKBBHSE_CV_Out, sampleName=="UKBB")$IPSW))
       UKBBHSE_CV_Out$propensity.weight.normalized=ifelse(UKBBHSE_CV_Out$sampleName=="HSE", UKBBHSE_CV_Out$weight_individual, UKBBHSE_CV_Out$IPSWNorm)
       UKBBEffn=subset(UKBBHSE_CV_Out, sampleName=="UKBB")
       UKBBHSE_CV_Out$nEFF=(sum(UKBBEffn$propensity.weight.normalized)^2)/sum(UKBBEffn$propensity.weight.normalized^2)
       print(paste0("Effective sample size: ", head( UKBBHSE_CV_Out$nEFF[1])))
       saveRDS(UKBBHSE_CV_Out , paste0(HOME,"/results/rds/UKBBHSE_PS_CV.rds"))

       print("All output saved")
       uploadLog(file=paste0("UKBB_HSE",validation, ".log"))

}


if(validation=="getcor"){
       UKBBHSEgen=readRDS(paste0(HOME,"/results/rds/datHSEUKBB.rds"))
       UKBBHSEgen$ID=paste0(UKBBHSEgen$eid, "_", UKBBHSEgen$sampleName)
       UKBBHSE_CV =readRDS(paste0(HOME,"/results/rds/UKBBHSE_PS_CV.rds"))

       UKBBcor=getCorrs(data=UKBBHSEgen, weights="propensity.weight.normalized", vars=varInc, sample="UKBB", weighting="")
       UKBBcorW=getCorrs(data=UKBBHSEgen, weights="propensity.weight.normalized", vars=varInc, sample="UKBB", weighting="w")
       UKBBcorCVW=getCorrs(data=UKBBHSE_CV, weights="propensity.weight.normalized", vars=varInc, sample="UKBB", weighting="w")
       names(UKBBcorCVW)[names(UKBBcorCVW) == 'corUKBBw'] <- 'corUKBBw_cv'
       names(UKBBcorCVW)[names(UKBBcorCVW) == 'nEFF_UKBBw'] <- "nEFF_UKBB_cv"

       HSEcorW=getCorrs(data=HSEall, weights="weight_individual", vars=varInc, sample="HSE", weighting="w")
       
       corAllComb=Reduce(function(x, y) merge(x=x, y=y, by="ID", all=T), list(HSEcorW, UKBBcor, UKBBcorW, UKBBcorCVW))
       saveRDS(corAllComb , paste0(HOME,"/results/rds/corAllComb.rds"))
       uploadDropbox(file="corAllComb.rds", folder="rds")
       print("All output saved")
       uploadLog(file=paste0("UKBB_HSE",validation, ".log"))
}




# ==================================================================================
# ==================== UNIVARIATE REGRESSION MODELS ================================
# ========================= (Predicting sample) ====================================
# ==================================================================================

if(validation=="RegCheck"){
       print("Check if predictors are no longer signidicant in weighted model")
       print("Check results without cross-validation")
       datIn=readRDS(paste0(HOME,"/results/rds/datHSEUKBB.rds"))
       weightedDist(data=datIn, method="noCV")
       meanChange(df=datIn, method="noCV")

       print("Check out-of-sample cross-validation")
       datIncv=readRDS(paste0(HOME,"/results/rds/UKBBHSE_PS_CV.rds"))
       weightedDist(data=datIncv, method="CV")
       meanChange(df=datIncv, method="CV")
       print("All output saved")
       uploadLog(file=paste0("UKBB_HSE",validation, ".log"))

}




# ==================================================================================
# ==================== Prepare data for GWA ========================================
# ==================================================================================


if(validation=="prepGWA"){
       print("Prepare data for GWAs")
       print("Read in data including the sample weights")
       UKBBHSE=readRDS(paste0(HOME,"/results/rds/datHSEUKBB.rds"))

       saveRDS(UKBBHSE , paste0(HOME,"/results/rds/densityPS.rds"))
       uploadDropbox(file="densityPS.rds", folder="rds")
       
       print("Generate dataset for GWAS")
       UKBBallweighted=merge(UKBBall, subset(UKBBHSE, select=c(eid, probs, propensity.weight.normalized)), by="eid", all.x=T)
       UKBB_inc1=subset(UKBBallweighted, sampleName=="UKBB", select=c("eid","eid", "propensity.weight.normalized","probs") )
       colnames(UKBB_inc1)=c("IID", "FID", "PS_w", "PS")
       UKBB_inc2=subset(UKBBallweighted, sampleName=="UKBB")
       UKBB_inc2$eid=NULL; UKBB_inc2$propensity.weight.normalized=NULL; UKBB_inc2$probs=NULL
       # Note: PS_w used for weighting // PS used for GWA
       
       print("export data for gwa")
       phenoFile=cbind(UKBB_inc1, UKBB_inc2)
       phenoFile$batch=as.factor(phenoFile$batch)
       
       print("Remove all individuals with missing data on PS")
       phenoFilenoMiss=phenoFile[complete.cases(phenoFile$PS),]
       print(paste0(NROW(phenoFile)-NROW(phenoFilenoMiss), " excluded with missing PS data"))
       phenoFilenoMiss$PS_w=normalizeWeights(phenoFilenoMiss$PS_w) # normalize weights again
       
       print("add info on whether genotype data is used per individual")
       print("Remove related individuals, sex mismatch, ancestry etc")
       UKBBall_raw=readRDS(paste0(HOME, "/results/rds/UKBBsel.rds"))
       unrelatedGenoSel=subset(UKBBall_raw, includeGeno=="include", select=c(eid))
       unrelatedGenoSel$genoInc="yes"
       completeGenoM=data.frame(FID=unrelatedGenoSel$eid, genoInc=unrelatedGenoSel$genoInc)
       phenoFilenoMiss2=merge(phenoFilenoMiss, completeGenoM, by.x="IID", by.y="FID", all.x=T)
       phenoFileGeno=subset(phenoFilenoMiss2, genoInc=="yes") # include only people with complete genotype
       

       write.table(phenoFileGeno,
              file= paste0(HOME, "/data/UKBB/phenoFile"),
              sep="\t",
              row.names = FALSE,
              col.names=T,
              quote=F)
              
       print("CHECK IF SAMPLE SELECTED FOR GWA IS SIGNIFICANTLY DIFFERENT TO PHENOTYPE SAMPLE")
       phenoFileGeno$age=as.numeric(phenoFileGeno$age)
       phenoFileGeno$bmi=as.numeric(phenoFileGeno$bmi)
       phenoFileGeno$education_age=as.numeric(phenoFileGeno$education_age)
       
       UKBBweightedGeno=lapply(varInc, 
                     function(x) weightedEstimates(df=phenoFileGeno, var=x, wt="PS_w"))
                     UKBBweightedGenoDF=do.call(rbind, UKBBweightedGeno)
       names(UKBBweightedGenoDF)[names(UKBBweightedGenoDF) == "unweighted_estimate"] <- "unweighted_estimate_UKBB_geno"
       names(UKBBweightedGenoDF)[names(UKBBweightedGenoDF) == "weighted_estimate"] <- "weighted_estimate_UKBB_geno"
       names(UKBBweightedGenoDF)[names(UKBBweightedGenoDF) == "totalN"] <- "totalN_UKBB_geno"
       UKBBweightedGenoDF$mergeID=paste0(UKBBweightedGenoDF$cat, "_", UKBBweightedGenoDF$level)

       UKBBweighted=lapply(varInc, 
                     function(x) weightedEstimates(df=UKBBallweighted, var=x, wt="propensity.weight.normalized"))
       UKBBweightedDF=do.call(rbind, UKBBweighted)
       UKBBweightedDF$mergeID=paste0(UKBBweightedDF$cat, "_", UKBBweightedDF$level)   
       names(UKBBweightedDF)[names(UKBBweightedDF) == "unweighted_estimate"] <- "unweighted_estimate_UKBB"
       names(UKBBweightedDF)[names(UKBBweightedDF) == "weighted_estimate"] <- "weighted_estimate_UKBB"
       names(UKBBweightedDF)[names(UKBBweightedDF) == "totalN"] <- "totalN_UKBB"
       
       UKBB_UKBBgeno=merge(UKBBweightedDF, UKBBweightedGenoDF, by="mergeID", all.y=T, suffixes=c("", ".y"))
       UKBB_UKBBgenosel=subset(UKBB_UKBBgeno, select=c(cat, level, weighted_estimate_UKBB, weighted_estimate_UKBB_geno, totalN_UKBB, totalN_UKBB_geno))
       
       saveRDS(UKBB_UKBBgenosel , paste0(HOME,"/results/rds/UKBB_UKBBgeno.rds"))
       uploadDropbox(file="UKBB_UKBBgeno.rds", folder="rds")
       
       print(paste0("Save genotype file "))
       library(BGData)
       print(paste0("Read in chromosomes "))
       chrIN <- lapply(seq(from=1, to=22, by=1), function(x) BEDMatrix(paste0("/data/sgg2/eleonora/public_data/UKBB_plink_files_for_PRS/chr", x)))
       
       wg <- ColumnLinkedMatrix(chrIN[[1]], 
                         chrIN[[2]], 
                         chrIN[[3]], 
                         chrIN[[4]],
                         chrIN[[5]],
                         chrIN[[6]],
                         chrIN[[7]],
                         chrIN[[8]],
                         chrIN[[9]],
                         chrIN[[10]],
                         chrIN[[11]],
                         chrIN[[12]],
                         chrIN[[13]],
                         chrIN[[14]],
                         chrIN[[15]],
                         chrIN[[16]],
                         chrIN[[17]],
                         chrIN[[18]],
                         chrIN[[19]],
                         chrIN[[20]],
                         chrIN[[21]],
                         chrIN[[22]]   )
                         
       print("Extract available SNPs")
       snpsAvailraw=colnames(wg)
       snpsAvail=do.call(rbind, strsplit(snpsAvailraw, split = "_"))[,1]
       snpsAvailDF=data.frame(snp=snpsAvail, avail="yes")
       print("Remove duplicates")
       snpsAvailDFclean=snpsAvailDF[!duplicated(snpsAvailDF$snp), ]
       saveRDS(snpsAvailDFclean, paste0(HOME, "/data/UKBB/snpAvail.rds"))
       print("Add phenotypes")
       bg <- as.BGData(wg, alternatePhenotypeFile = paste0(HOME, "/data/UKBB/phenoFile")) # phenotype file including phenotypes and ID [colnames(phenotype)=c("FID", "IID", "phenotype1", "phenotype2")]
       save(bg, file = paste0(HOME, "/data/UKBB/genoFile.RData"))
       
       print("Save files for LDAK")
       phenoFileGeno$batch=as.factor(as.numeric(phenoFileGeno$batch))
       covariatesUKBB=subset(phenoFileGeno, select=c(FID, IID, sex, PC1, PC2, PC3, PC4, PC5, batch))
       covariatesUKBB$sex=as.numeric(as.factor(covariatesUKBB$sex))
       
       write.table(covariatesUKBB,
              file= paste0(HOME, "/output/ldak/data/covarFile"),
              sep="\t",
              row.names = FALSE,
              col.names=FALSE,
              quote=F)
              
       print("Save quantitative and categorical separately for GCTA")
       covariatesUKBBquantitative=subset(phenoFileGeno, select=c(FID, IID, PC1, PC2, PC3, PC4, PC5))
       
       write.table(covariatesUKBBquantitative,
              file= paste0(HOME, "/output/ldak/data/covarFileQuant"),
              sep="\t",
              row.names = FALSE,
              col.names=FALSE,
              quote=F)
              
       covariatesUKBBdiscrete=subset(phenoFileGeno, select=c(FID, IID,sex, batch))
       write.table(covariatesUKBBdiscrete,
              file= paste0(HOME, "/output/ldak/data/covarFileDiscrete"),
              sep="\t",
              row.names = FALSE,
              col.names=FALSE,
              quote=F)
              
       print("remove sex from datafiles (for GWA on sex)")
       covariatesUKBBdiscreteSex=subset(phenoFileGeno, select=c(FID, IID, batch))
       write.table(covariatesUKBBdiscreteSex,
              file= paste0(HOME, "/output/ldak/data/covarFileDiscreteSex"),
              sep="\t",
              row.names = FALSE,
              col.names=FALSE,
              quote=F)
       
       covariatesUKBBsex=covariatesUKBB
       covariatesUKBBsex$sex=NULL
       
       # remove Sex for sex-gwas
       write.table(covariatesUKBBsex,
              file= paste0(HOME, "/output/ldak/data/covarFileSex"),
              sep="\t",
              row.names = FALSE,
              col.names=FALSE,
              quote=F)
       
       weightsUKBB=subset(phenoFileGeno, select=c(FID, IID, PS_w))
       write.table(weightsUKBB,
              file= paste0(HOME, "/output/ldak/data/weightsFile"),
              sep="\t",
              row.names = FALSE,
              col.names=FALSE,
              quote=F)
              
       print("save input files for LDAK")
       includeGWAmiss=subset(variableList, includeGWA=="yes")
       includeGWAPhenoMiss=includeGWAmiss$recodedName

       print(paste0("Number of variables selected for GWA: ", NROW(includeGWAPhenoMiss)))
       print(includeGWAPhenoMiss)

       print("Check missing data")
       missingDat=data.frame(variable=includeGWAPhenoMiss,
           missing=do.call(rbind, lapply(includeGWAPhenoMiss, function(x) sum(is.na(phenoFilenoMiss[[x]]))))) 
       
       print("Variables with too much missing data (>50,000")
       print(subset(missingDat, missing>50000))

       print("Remove variables with too much missing data")
       includeGWAPhenoInc=includeGWAPhenoMiss[includeGWAPhenoMiss %in% subset(missingDat, missing<=50000)$variable]

       includeGWA=subset(variableList, recodedName %in% includeGWAPhenoInc)
       includeGWAPheno=includeGWA$recodedName
       includeGWA$sample.prev=NA
       includeGWA$population.prev=NA


for ( i in 1:NROW(includeGWAPheno) ) {
     pheno=includeGWAPheno[i]
     if(includeGWA$binary[i]=="yes"){
          includeGWA$sample.prev[i]= prop.table(table(phenoFile[[pheno]]))[2] 
          weightedProp=weightedEstimates(df=phenoFile, var=pheno, wt="PS_w")
          includeGWA$population.prev[i]=min(weightedProp$outWeighted_raw)/100
          } else{
        includeGWA$sample.prev[i]=NA
        includeGWA$population.prev[i]=NA
        }
}

for ( i in 1:NROW(includeGWAPheno) ) {
       pheno=includeGWAPheno[i]
       UKBBphenoLDAK=subset(phenoFileGeno, select=c("FID", "IID" ,pheno))
       
       if(typeof(UKBBphenoLDAK[[pheno]])!="double"){
          UKBBphenoLDAK[[pheno]]=as.numeric(as.factor(UKBBphenoLDAK[[pheno]]))
       }
       print(head(UKBBphenoLDAK))
       
       write.table(UKBBphenoLDAK,
              file= paste0(HOME, "/output/ldak/data/phenoFile_", pheno),
              sep="\t",
              row.names = FALSE,
              col.names=FALSE,
              quote=F)
}


write.table(includeGWAPheno,
              file= paste0(HOME, "/data/UKBB/varListGWA"),
              sep="\t",
              row.names = FALSE,
              col.names=F,
              quote=F)


varListGWAldsc=subset(includeGWA, select=c(recodedName, sample.prev, population.prev))

print("Save file including prevalence rates")
write.table(varListGWAldsc,
              file= paste0(HOME, "/data/UKBB/varListGWAldsc"),
              sep="\t",
              row.names = FALSE,
              col.names=T,
              quote=F)          

print("")
print("")
print("")
print("")
print("")
print("")
print("")
print("Effective sample size included (before removeing related individuals)")
print((sum(phenoFile$PS_w,  na.rm=T)^2)/sum(phenoFile$PS_w^2, na.rm=T))


phenoGenoMiss=phenoFilenoMiss[complete.cases(phenoFilenoMiss$PC1),]
phenoGenoMiss$PS_w=normalizeWeights(phenoGenoMiss$PS_w) # normalize weights again


print("Effective sample size included (after removeing related individuals)")
print((sum(phenoGenoMiss$PS_w)^2)/sum(phenoGenoMiss$PS_w^2))
print("All output saved")
uploadLog(file=paste0("UKBB_HSE",validation, ".log"))

}

if(validation=="missingness"){

print("Check at which stage individuals were exlcuded")
sumExclusion1=readRDS(paste0(HOME,"/results/rds/sumExclusion1.rds"))

phenoFile=fread(paste0(HOME, "/data/UKBB/phenoFile"))
UKBBall_raw=readRDS(paste0(HOME, "/results/rds/UKBBsel.rds"))
UKBBall=readRDS(paste0(HOME,"/results/rds/UKBBall.rds"))
UKBBHSEPS=readRDS(paste0(HOME,"/results/rds/datHSEUKBB.rds"))
flowRemove=subset(UKBBHSEPS, select=c(eid, probs))
flowRemove$missingPS=ifelse(is.na(flowRemove$probs)==T, "missing", "available")
flowRemoveGeno=merge(flowRemove, UKBBall_raw, by.x="eid", by.y="eid", all.y=T, sort=F)


UKBB_availPS=subset(flowRemoveGeno, missingPS=="available")
UKBB_genoQC=subset(UKBB_availPS, includeGeno=="include")

exclusion5=data.frame(navail=NROW(UKBB_availPS), text=paste0(NROW(UKBBall)-NROW(UKBB_availPS), " exlcuded as missing PS data"))
exclusion6=data.frame(navail=NROW(UKBB_genoQC), text=paste0(NROW(UKBB_availPS)-NROW(UKBB_genoQC), " exlcuded after QC for genotyping (related individuals, non-european ancestry (based on genetic data), gender mismatch, removeHetmiss"))
exclusion7=data.frame(navail=NROW(UKBB_genoQC), text=paste0("Final sample included in GWA with weights: ", NROW(UKBB_genoQC)))

sumExclusion=rbind(sumExclusion1, exclusion5, exclusion6, exclusion7)

saveRDS(sumExclusion , paste0(HOME,"/results/rds/sumExclusion.rds"))
uploadDropbox(file="sumExclusion.rds", folder="rds")

uploadLog(file=paste0("UKBB_HSE",validation, ".log"))

}











print("")
print("")
print("")
print("")
print("")
print("")
print("")

print("All done!")