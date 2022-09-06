#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
HOME=args[1]
chunks=args[2]
array=args[3]
extractSNPs=args[4]

print("Load R packages and functions")
source(paste0(HOME, "/analysis/input.R"))
source(paste0(HOME, "/analysis/functions.R"))


con <- file(paste0(HOME, "/output/log/ldakSNPcor.", array, ".log"))
sink(con, append=TRUE, type="output")



phenoFile=fread(file= paste0(HOME, "/data/UKBB/phenoFile"))

if(extractSNPs=="extract"){

    print("Read in data")
    mrDat=fread(paste0(HOME, "/data/UKBB/varListGWA"), header=F)
    getMR=mrDat$V1

    gwaInW=lapply(getMR, function(x) fread(file=paste0(HOME, "/output/ldak/regressComb/", x, "_weighted")))
    gwaIn=lapply(getMR, function(x) fread(file=paste0(HOME, "/output/ldak/regressComb/", x)))
    names(gwaIn)=getMR
    names(gwaInW)=getMR


# get SNP effects
SNPeffectsList=list()
for ( i in 1:NROW(getMR) ) {
    exposure=getMR[i]
      print("Get weighted GWA sig hits")
      
      dfW_sig=subset(gwaInW[[exposure]], P_sw < 5e-8)
      df_sig=subset(gwaIn[[exposure]], P < 5e-8)

      if((NROW(dfW_sig) + NROW(df_sig))==0) next

    if(NROW(dfW_sig)==0) {
          print("No significant hits in weighted")
          dfWClumpInc=data.frame(SNPlabel=NA, SNP=NA, CHR=NA, BP=NA, A1=NA, A2=NA, BETA=NA, SE=NA, P=NA, N=NA, maf=NA, data="weighted")
    } else{
      dfWClumpInc=data.frame(SNPlabel= dfW_sig$SNPlabel, SNP=dfW_sig$SNP, CHR=dfW_sig$CHR, BP=dfW_sig$BP, A1=dfW_sig$A1, A2=dfW_sig$A2, BETA=dfW_sig$BETA_sw, SE=dfW_sig$SE_sw, P=dfW_sig$P_sw, N=dfW_sig$N, maf=dfW_sig$MAF, data="weighted")
    }

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

    gwaClumpW=merge(gwaInW[[exposure]], subset(dfClump, select=c(SNPlabel)),by="SNPlabel", all.y=T, sort=F)
    gwaClumpWInc=data.frame(SNPlabel= gwaClumpW$SNPlabel, BETA_wt=gwaClumpW$BETA_sw, SE_wt=gwaClumpW$SE_sw, P_wt=gwaClumpW$P_sw, N_wt=gwaClumpW$N)

    gwaClump=merge(gwaIn[[exposure]], subset(dfClump, select=c(SNPlabel)),by="SNPlabel", all.y=T, sort=F)
    gwaClumpInc=data.frame(SNPlabel= gwaClump$SNPlabel,SNP=gwaClump$SNP, CHR=gwaClump$CHR, BP=gwaClump$BP, A1=gwaClump$A1, A2=gwaClump$A2, maf=gwaClump$MAF, BETA=gwaClump$BETA, SE=gwaClump$SE, P=gwaClump$P, N=gwaClump$N)


     gwaClumpComb=merge(gwaClumpInc, gwaClumpWInc,by="SNPlabel", all=T, sort=F)
     gwaClumpClean=gwaClumpComb[!duplicated(gwaClumpComb$SNPlabel), ]
     
     gwaClumpClean$source=ifelse(gwaClumpClean$P < 5e-8 & gwaClumpClean$P_wt < 5e-8, "both", NA )
     gwaClumpClean$source=ifelse(gwaClumpClean$P >= 5e-8 & gwaClumpClean$P_wt < 5e-8, "weighted", gwaClumpClean$source )
     gwaClumpClean$source=ifelse(gwaClumpClean$P < 5e-8 & gwaClumpClean$P_wt >= 5e-8, "unweighted", gwaClumpClean$source )
     gwaClumpClean$pheno=exposure
     table(gwaClumpClean$source)
     SNPeffectsList[[i]]=gwaClumpClean
     
}
SNPeffectsCor=do.call(rbind, SNPeffectsList)
saveRDS(SNPeffectsCor, paste0(HOME, "/results/rds/SNPeffects.rds"))
gwaSNP=subset(SNPeffectsCor, select=c("SNPlabel", "CHR"))

system(paste0("rm ", HOME, "/output/ldak/mergeChr"))
 for ( i in 1:22 ) {
  chrDF=subset(gwaSNP, CHR==i, select=SNPlabel)
  write.table(chrDF, file= paste0(HOME, "/output/ldak/SNPselChr", i),row.names = FALSE,col.names=F,quote=F)
  system(paste0(HOME, "/programs/plink --bfile ", UKBBgeno,"/chr", i, "_clean --extract ", HOME, "/output/ldak/SNPselChr", i, " --make-bed --out ", HOME, "/output/ldak/SNPselChr",i ))
  system(paste0("echo ", HOME, "/output/ldak/SNPselChr", i, " >> ",HOME, "/output/ldak/mergeChr"))
 }

system(paste0("head ", HOME, "/output/ldak/mergeChr"))
system(paste0(HOME, "/programs/plink --merge-list ", HOME, "/output/ldak/mergeChr --make-bed --out ", HOME, "/output/ldak/SNPselMerged" ))

print("All done!!")
}



if(extractSNPs!="extract"){

print("Create chunks")
itNum=seq(from=1, to=100, by=1)
itNumChunk=split(itNum, sort(itNum%%as.numeric(chunks) ))

getSNPcor=function(iteration){ 
  setwd(paste0(HOME, "/output/ldak/"))
  set.seed(iteration)
  phenoFileSel=subset(phenoFile, select=c("FID", "IID"))
  phenoFileSel$random=as.numeric(rnorm(NROW(phenoFileSel), mean=0))
  write.table(phenoFileSel,
              file= paste0(HOME, "/output/ldak/data/randomIt_", iteration),
              sep="\t",
              row.names = FALSE,
              col.names=FALSE,
              quote=F)

  system(paste0(HOME, "/programs/ldak/ldak5.2.linux --linear random_", iteration, " --pheno ", HOME, "/output/ldak/data/randomIt_", iteration, " --bfile ", HOME, "/output/ldak/SNPselMerged --covar ", HOME, "/output/ldak/data/covarFile --bit-size 50"))
  ldakOut=fread(paste0(HOME, "/output/ldak/random_", iteration, ".assoc"), header=T)

  system(paste0(HOME, "/programs/ldak/ldak5.2.linux --linear random_weighted_", iteration, " --pheno ", HOME, "/output/ldak/data/randomIt_", iteration, " --bfile ", HOME, "/output/ldak/SNPselMerged --covar ", HOME, "/output/ldak/data/covarFile --bit-size 50 --sample-weights ", HOME, "/output/ldak/data/weightsFile"))
  ldakOutW=fread(paste0(HOME, "/output/ldak/random_weighted_", iteration, ".assoc"), header=T)
  
  ldakComb=merge(data.frame(SNP=ldakOut$Predictor,BETA=ldakOut$Effect), data.frame(SNP=ldakOutW$Predictor,BETA_wt=ldakOutW$Effect), by="SNP", all=T, sort=F)
  ldakComb$iteration=iteration
  
  return(ldakComb)
}
print("All done!!")
#array=1
ldakCorList=lapply(itNumChunk[[array]], function(x) getSNPcor(x))
saveRDS(ldakCorList, paste0(HOME, "/output/ldak/SNPcor_",array,".rds"))
}



if(extractSNPs=="process"){
print("Read in data")
mrDat=fread(paste0(HOME, "/data/UKBB/varListGWA"), header=F)
getMR=mrDat$V1

gwaInW=lapply(getMR, function(x) fread(file=paste0(HOME, "/output/ldak/regressComb/", x, "_weighted")))
gwaIn=lapply(getMR, function(x) fread(file=paste0(HOME, "/output/ldak/regressComb/", x)))
names(gwaIn)=getMR
names(gwaInW)=getMR

SNPeffectsDF=readRDS(paste0(HOME, "/results/rds/SNPeffects.rds"))
print(paste0("Number of processed phenotypes: ", NROW(levels(as.factor(SNPeffectsDF$pheno)))))


#chunks=10
print("Get correlations between weighted and unweighted estimates")
listSNPs_cor=list()
  for(j in 1:(as.numeric(chunks)-1) ) {
    dfIn=readRDS(paste0(HOME, "/output/ldak/SNPcor_",j,".rds"))
    listSNPs_cor[[j]]=do.call(rbind, dfIn)
  }
 DFSNPs_cor=do.call(rbind, listSNPs_cor)
 SNPcor= DFSNPs_cor %>%
  dplyr::group_by(as.factor(SNP)) %>%
  dplyr::summarize(COR=cor(BETA,BETA_wt))
 corData=data.frame(SNPlabel=SNPcor$`as.factor(SNP)`, cor=SNPcor$COR )


SNPeffectsDF$cor=NULL
SNPeffectsCor=merge(SNPeffectsDF, corData, by="SNPlabel", all.x=T, sort=F)

print(paste0("Missing correlation coefficients: "))
print(table(is.na(SNPeffectsCor$cor)))

saveRDS(SNPeffectsCor, paste0(HOME, "/results/rds/SNPeffectsCor.rds"))
uploadDropbox(file="SNPeffectsCor.rds", folder="rds")

print("All correlations extracted")

}


uploadLog(file=paste0("ldakSNPcor.", array, ".log"))