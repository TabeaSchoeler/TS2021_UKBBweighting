#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
HOME=args[1]
source(paste0(HOME, "/analysis/input.R"))
source(paste0(HOME, "/analysis/functions.R"))
pheno=args[2]
chr=args[3]
estmation=args[4]


con <- file(paste0(HOME, "/output/log/LDAK_wGWA.log"))
sink(con, append=TRUE, type="output")

print("Start LDAK")
start_time <- Sys.time()


setwd(paste0(HOME, "/output/ldak/regressRes"))

if(estmation=="unweighted"){
    print("No weights included")
    if(pheno!="sex"){
        print(pheno)
     system(paste0(HOME, "/programs/ldak/ldak5.2.linux --linear ", pheno, "_chr", chr, " --pheno ", HOME, "/output/ldak/data/phenoFile_", pheno, " --bfile ", UKBBgeno,"/genoID_clean/chr", chr, "_clean --covar ", HOME, "/output/ldak/data/covarFile --max-threads 8 --bit-size 6000"))
    }
    if(pheno=="sex"){
        print("Run GWA on sex (with sex removed from covariate file)")
         system(paste0(HOME, "/programs/ldak/ldak5.2.linux --linear ", pheno, "_chr", chr, " --pheno ", HOME, "/output/ldak/data/phenoFile_", pheno, " --bfile ", UKBBgeno,"/genoID_clean/chr", chr, "_clean --covar ", HOME, "/output/ldak/data/covarFileSex --max-threads 8 --bit-size 6000"))
    }

}



if(estmation=="weighted"){
    print("Weights included")
    if(pheno!="sex"){
    print(pheno)
    system(paste0(HOME, "/programs/ldak/ldak5.2.linux --linear ", pheno, "_chr", chr, "_weighted --pheno ", HOME, "/output/ldak/data/phenoFile_", pheno, " --bfile ", UKBBgeno,"/genoID_clean/chr", chr, "_clean --covar ", HOME, "/output/ldak/data/covarFile --bit-size 6000 --max-threads 8 --sample-weights ", HOME, "/output/ldak/data/weightsFile"))
    }
    if(pheno=="sex"){
     print("Run GWA on sex (with sex removed from covariate file)")
     system(paste0(HOME, "/programs/ldak/ldak5.2.linux --linear ", pheno, "_chr", chr, "_weighted --pheno ", HOME, "/output/ldak/data/phenoFile_", pheno, " --bfile ", UKBBgeno,"/genoID_clean/chr", chr, "_clean --covar ", HOME, "/output/ldak/data/covarFileSex --bit-size 6000 --max-threads 8 --sample-weights ", HOME, "/output/ldak/data/weightsFile"))
    }

}


end_time <- Sys.time()

print(paste0("Running time for ", chr))
print(end_time - start_time)

uploadLog(file=paste0("LDAK_wGWA.log"))
uploadLog(file=paste0("ldakGWA.", pheno,".", chr, ".", estmation, ".out"))



