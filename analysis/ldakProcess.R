#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
HOME=args[1]
source(paste0(HOME, "/analysis/input.R"))
source(paste0(HOME, "/analysis/functions.R"))
pheno=args[2]


con <- file(paste0(HOME, "/output/log/ldakProcess_", pheno, ".log"))
sink(con, append=TRUE, type="output")

print("read on phenodata (used to derive effective sample size)")
phenoFile=fread(file= paste0(HOME, "/data/UKBB/phenoFile"))

print("read in rsID names per chromosome")
snpNamesList=lapply(seq(1,22,1) , function(x) readInrsID(x) )

print("read in unweigthed")
gwaUnweightedList=lapply(seq(1,22,1) , function(x) readInSNPs(x, pheno=pheno))
gwaUnweightedDF=do.call(rbind, gwaUnweightedList)


print("read in weigthed")
gwaWeightedList=lapply(seq(1,22,1) , function(x) readInSNPs(x, weight="_weighted", pheno=pheno))
gwaWeightedDF=do.call(rbind, gwaWeightedList)


print("Save output")
write.table(gwaWeightedDF,
              file= paste0(HOME, "/output/ldak/regressComb/", pheno, "_weighted"),
              sep="\t",
              row.names = FALSE,
              col.names=TRUE,
              quote=F)


write.table(gwaUnweightedDF,
              file= paste0(HOME, "/output/ldak/regressComb/", pheno),
              sep="\t",
              row.names = FALSE,
              col.names=TRUE,
              quote=F)


print(paste0("Number of SNPs included in unweighted: ", NROW(gwaUnweightedDF)))
print(paste0("Number of SNPs included in weighted: ", NROW(gwaWeightedDF)))



drop_download(
  local_path = paste0(HOME,"/data/variableList.xlsx"),
  path = paste0(LOCAL,"/data/variableList.xlsx"), overwrite=TRUE)

variableList <- readxl::read_excel(paste0(HOME, "/data/variableList.xlsx"))
idSel=subset(variableList, recodedName==pheno)$ieugwasrID

library(ieugwasr)
origGWAall <- as.data.frame(tophits(id=idSel, pval = 5e-8, clump = 1, r2 = 0.1, kb = 250))

if(NROW(origGWAall)>0){
print("Check SNP effects from previous GWA")
corCheck=merge(gwaUnweightedDF, origGWAall, by.x="SNP", by.y="rsid", all.y=T)
corCheck$beta=ifelse(corCheck$A1==corCheck$ea, corCheck$beta, -1*corCheck$beta)
corOut=cor.test(corCheck$BETA, corCheck$beta)
corOutDF=data.frame(pheno=pheno, cor=round(corOut$estimate,2), lCI=round(corOut$conf.int[1], 2), uCI=round(corOut$conf.int[2], 2), nSNPsig=NROW(origGWAall))


write.table(corOutDF, paste0(HOME, "/output/log/corrCHECK_",pheno, ".log"), 
sep = "\t",
   quote = F, 
col.names = TRUE)

uploadLog(file=paste0("corrCHECK_",pheno, ".log"))
}




uploadLog(file=paste0("ldakProcess_",pheno, ".log"))




