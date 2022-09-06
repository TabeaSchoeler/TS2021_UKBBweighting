
#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
HOME=args[1]


source(paste0(HOME, "/analysis/input.R"))
source(paste0(HOME, "/analysis/functions.R"))
con <- file(paste0(HOME, "/output/log/sexSNP23andme.log"))


print("Load list of files to be included in GWA")
drop_auth(rdstoken = paste0(HOME, "/token.rds"))  # authentication for dropbox
drop_download(
  local_path = paste0(HOME,"/data/sexSNP23andme.xlsx"),
  path = paste0(LOCAL,"/data/sexSNP23andme.xlsx"), overwrite=TRUE)

library("readxl")
# sTable 2 from https://www.nature.com/articles/s41588-021-00846-7#Sec26
sex <- as.data.frame(readxl::read_excel(paste0(HOME, "/data/sexSNP23andme.xlsx")))
sex$maf=ifelse(sex$FREQ > 0.5, 1-sex$FREQ, sex$FREQ)


# Filter according to QC checks
#  49 out of 78 directly genotyped genome-wide significant signals remained. 
sexClean=subset(sex, flag_homo==0 & flag_AF==0 & flag_hwe==0  & flag_CR==0 )
print("Standardize estimates")
sexCleanSTD=standardBeta(df=sexClean, beta=sexClean$BETA, se=sexClean$SE, N=2462132)
sexClean=subset(sexCleanSTD, select=c(SNP, beta_std, se_std, P, A1, A2, maf))
names(sexClean)[names(sexClean) == "A1"] <- "A1_23"
names(sexClean)[names(sexClean) == "A2"] <- "A2_23"
names(sexClean)[names(sexClean) == "beta_std"] <- "BETA_23"
names(sexClean)[names(sexClean) == "se_std"] <- "SE_23"
names(sexClean)[names(sexClean) == "P"] <- "P_23"


print("Read in unweighted GWA")
sexGWA=fread(paste0(HOME, "/output/ldak/regressComb/sex"))
sexGWASig=merge(sexClean, sexGWA, by="SNP", all.x=T)
sexGWASig$maf=sexGWASig$MAF
print("Standardize estimates")
sexGWASigSTD=standardBeta(df=sexGWASig, beta=sexGWASig$BETA, se = sexGWASig$SE, N = sexGWASig$N) 
sexGWASig=subset(sexGWASigSTD, select=c(SNP, A1_23, A2_23, BETA_23, SE_23, P_23, beta_std, se_std, P, maf))
names(sexGWASig)[names(sexGWASig) == "beta_std"] <- "BETA"
names(sexGWASig)[names(sexGWASig) == "se_std"] <- "SE"

print("Read in weighted GWA")
sexGWA_w=fread(paste0(HOME, "/output/ldak/regressComb/sex_weighted"))
sexGWASig_w=merge(sexGWASig, subset(sexGWA_w, select=c(SNP, BETA_sw, SE_sw, P_sw, A1, A2,  N)), by="SNP", all.x=T)
print("Standardize estimates")
sexGWASigSTDw=standardBeta(df=sexGWASig_w, beta=sexGWASig_w$BETA_sw, se = sexGWASig_w$SE_sw, N = sexGWASig_w$N) 
names(sexGWASigSTDw)[names(sexGWASigSTDw) == "beta_std"] <- "BETA_sw"
names(sexGWASigSTDw)[names(sexGWASigSTDw) == "se_std"] <- "SE_sw"
sexGWA_wSig=subset(sexGWASigSTDw, select=c(SNP, BETA_23, SE_23, P_23, A1_23, A2_23, BETA, SE, P, BETA_sw, SE_sw, P_sw, A1, A2))

sexGWA_wSig$BETA_23= ifelse(sexGWA_wSig$A1_23==sexGWA_wSig$A1, sexGWA_wSig$BETA_23, -1*sexGWA_wSig$BETA_23)

print("Upload results")
saveRDS(sexGWA_wSig , paste0(HOME,"/results/rds/sexGWACheck.rds"))
uploadDropbox(file="sexGWACheck.rds", folder="rds")



uploadLog(file=paste0("sexSNP23andme.log"))