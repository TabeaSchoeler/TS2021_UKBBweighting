#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
HOME=args[1]
munge=args[2]
source(paste0(HOME, "/analysis/input.R"))
source(paste0(HOME, "/analysis/functions.R"))


con <- file(paste0(HOME, "/output/log/ldakLDSC.log"))
sink(con, append=TRUE, type="output")

gwaDat=fread(paste0(HOME, "/data/UKBB/varListGWAldsc"), header=T)
exclude=c("migraine", "cancer") # exclude as negative heritability estimates
gwaDat=subset(gwaDat, !recodedName %in% exclude)
gwaDatSel=subset(gwaDat, recodedName!="PS")
prevalenceUnweighted=gwaDat$sample.prev
prevalenceWeighted=gwaDat$population.prev

library(GenomicSEM)
library(ieugwasr)
library(TwoSampleMR)

if(munge=="yes"){

print("Remove previously generated rds files")
removeRDS=c("rgComb", "rgCombW")
lapply(removeRDS, function(x) file.remove(paste0(HOME,"/results/rds/",x,".rds")))


print("Prepare clean datasets for munging")
for ( i in 1:length(gwaDat$recodedName) ) {
    pheno=gwaDat$recodedName[i]

        print("Save weighted GWA")
        gwa_inW=fread(file= paste0(HOME, "/output/ldak/regressComb/", pheno, "_weighted"))
        gwa_selectW=data.frame(SNP=gwa_inW$SNP,CHR=gwa_inW$CHR,BP=gwa_inW$BP,A1=gwa_inW$A1,A2=gwa_inW$A2,BETA=gwa_inW$BETA_sw,SE=gwa_inW$SE_sw,P=gwa_inW$P_sw, N=gwa_inW$N  )
        
        print("Save output")
         fwrite(gwa_selectW, 
          paste0(HOME, "/output/ldsc/ldak_", pheno, "_weighted"),
          col.names = TRUE,
          row.names = FALSE, 
          quote = F, 
          sep = "\t")

        print("Save unweighted GWA")
        gwa_in=fread(file= paste0(HOME, "/output/ldak/regressComb/", pheno))
        gwa_select=data.frame(SNP=gwa_in$SNP,CHR=gwa_in$CHR,BP=gwa_in$BP,A1=gwa_in$A1,A2=gwa_in$A2,BETA=gwa_in$BETA,SE=gwa_in$SE,P=gwa_in$P, N=gwa_in$N  )

     print("Save output")
     fwrite(gwa_select, 
       paste0(HOME, "/output/ldsc/ldak_", pheno),
       col.names = TRUE,
       row.names = FALSE, 
       quote = F, 
       sep = "\t")

}

print("Munge files")
mungeFiles=c(paste0("ldak_",gwaDat$recodedName, "_weighted"),
            paste0("ldak_",gwaDat$recodedName))
lapply(mungeFiles, function(x) mungeData(x, clean=munge))
}

print("Munge files")
mungeFiles=c(paste0("ldak_",gwaDat$recodedName, "_weighted"),
            paste0("ldak_",gwaDat$recodedName))
lapply(mungeFiles, function(x) mungeData(x, clean="no"))


if(munge=="ldsc"){
print("Start ld score regression analysis")
rgWeighted=ldscLDAK(model=paste0("ldak_", gwaDatSel$recodedName, "_weighted"),est="w_100", prevDat=gwaDatSel )
rgUnweighted=ldscLDAK(model=paste0("ldak_", gwaDatSel$recodedName), prevDat=gwaDatSel)

saveRDS(rgWeighted, paste0(HOME, "/results/rds/rgCombW.rds"))
uploadDropbox(file="rgCombW.rds", folder="rds")

saveRDS(rgUnweighted, paste0(HOME, "/results/rds/rgComb.rds"))
uploadDropbox(file="rgComb.rds", folder="rds")
}



if(munge=="JK_filter"){
  for(i in 1:200){
       lapply(seq(1,22,1), function(x) filterScores(chr=x, folder=paste0(HOME,"/data/eur_w_ld_chr/"), iteration=i))
 }
}



if(munge=="JK"){
  iteration=args[3]
   con <- file(paste0(HOME, "/output/log/ldakLDSC_", iteration , ".log"))
  sink(con, append=TRUE, type="output")
  print(paste0("Perform JACKKNIFE for iteration ", iteration))
  
  rgUnweightedJK=ldscLDAK(model=paste0("ldak_", gwaDatSel$recodedName), prevDat=gwaDatSel, jkIt=iteration, est="jk_n" )
  rgWeightedJK=ldscLDAK(model=paste0("ldak_", gwaDatSel$recodedName, "_weighted"), prevDat=gwaDatSel, jkIt=iteration, est="jk_n" )


  print("Extract h2 estimates")
  h2unW=subset(rgUnweightedJK[["h2"]], scale=="observed")
  h2Cor=data.frame(pheno=h2unW$pheno, h2=h2unW$h2, h2_w=NA)
  h2W=subset(rgWeightedJK[["h2"]], scale=="observed")
  h2Cor$h2_w=h2W$h2
  saveRDS(h2Cor, paste0(HOME, "/output/ldsc/JK/h2Cor_", iteration, ".rds"))


library(reshape2)

print("Extract rg estimates")
  mRaw=rgUnweightedJK[["rg"]]
  rownames(mRaw)=colnames(mRaw)
  rgunW=melt(mRaw)
  rgunW$rgPair=paste0(rgunW$Var1, "_", rgunW$Var2)

  mW=rgWeightedJK[["rg"]]
  rownames(mW)=colnames(mW)
  rgW=melt(mW)
  rgComb=data.frame(P1=rgunW$Var1, P2=rgunW$Var2, rgPair=rgunW$rgPair, rgRaw=rgunW$value, rgW=rgW$value )
  
   saveRDS(rgComb, paste0(HOME, "/output/ldsc/JK/rgComb_", iteration, ".rds"))

uploadLog(file=paste0("ldakLDSC_", iteration , ".log"))


}


if(munge=="JK_process"){

it=seq(1,200,1)

print("Extract h2 estimates")
h2List=lapply(it, function(x) readRDS(paste0(HOME, "/output/ldsc/JK/h2Cor_", x, ".rds")))
h2DF=do.call(rbind, h2List)
phenoh2Cor=data.frame(pheno=levels(as.factor(h2DF$pheno)), cor=NA)


 for(i in 1:NROW(phenoh2Cor)){
  phenoCor=subset(h2DF, h2DF==phenoh2Cor$pheno[i])
  corOut=cor.test(phenoCor$h2, phenoCor$h2_w)
  phenoh2Cor$cor[i]=corOut$estimate
 }

  saveRDS(phenoh2Cor, paste0(HOME, "/results/rds/h2Cor.rds"))
  uploadDropbox(file="h2Cor.rds", folder="rds")

  print("Extract rg estimates")
rgList=lapply(it, function(x) readRDS(paste0(HOME, "/output/ldsc/JK/rgComb_", x, ".rds")))
rgDF=do.call(rbind, rgList)
rgLevel=levels(as.factor(rgDF$rgPair))
rgCor=data.frame(pheno=rgLevel, cor=NA)

 for(i in 1:length(rgLevel)){
  phenoCor=subset(rgDF, rgPair==rgLevel[i])
  corOut=cor.test(phenoCor$rgRaw, phenoCor$rgW)
  rgCor$cor[i]=corOut$estimate
 }

  saveRDS(rgCor, paste0(HOME, "/results/rds/rgCor.rds"))
  uploadDropbox(file="rgCor.rds", folder="rds")
  


}

print("All output saved")
uploadLog(file=paste0("ldakLDSC.log"))
uploadLog(file=paste0("ldakLDSC.out"))