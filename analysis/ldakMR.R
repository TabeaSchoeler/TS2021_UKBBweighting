#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
HOME=args[1]
task=args[2]


print("Load R packages and functions")
source(paste0(HOME, "/analysis/input.R"))
source(paste0(HOME, "/analysis/functions.R"))

con <- file(paste0(HOME, "/output/log/ldakMR.log"))
sink(con, append=TRUE, type="output")

print("Read in data")
mrDat=fread(paste0(HOME, "/data/UKBB/varListGWA"), header=F)
mrDat=subset(mrDat, V1!="sex" & V1!="PS")
getMR=mrDat$V1


if(task=="MR"){

print("Perform MR and JK")

gwaInW=lapply(getMR, function(x) fread(file=paste0(HOME, "/output/ldak/regressComb/", x, "_weighted")))
gwaIn=lapply(getMR, function(x) fread(file=paste0(HOME, "/output/ldak/regressComb/", x)))
names(gwaIn)=getMR
names(gwaInW)=getMR

weighted=funMR(estimation="weighted")
unweigthed=funMR(estimation="unweighted")
weightedJK=funMR(estimation="weighted", jk="yes")
unweightedJK=funMR(estimation="unweighted", jk="yes")

print("Finnished MR - save intermediate output")
saveRDS(weighted, paste0(HOME, "/results/rds/MRweighted.rds"))
saveRDS(unweigthed, paste0(HOME, "/results/rds/MRunweighted.rds"))
saveRDS(weightedJK, paste0(HOME, "/results/rds/MRweightedJK.rds"))
saveRDS(unweightedJK, paste0(HOME, "/results/rds/MRunweightedJK.rds"))

}


if(task=="MR_JKprocess"){

print("Process MR and JK output")
weighted=readRDS(paste0(HOME, "/results/rds/MRweighted.rds"))
unweigthed=readRDS(paste0(HOME, "/results/rds/MRunweighted.rds"))
weightedJK=readRDS(paste0(HOME, "/results/rds/MRweightedJK.rds"))
unweightedJK=readRDS(paste0(HOME, "/results/rds/MRunweightedJK.rds"))


listMRMerged=list()
listMROut=list()
for(i in 1:length(getMR)) {

    print(paste0("processs ", getMR[i]))
   if( NROW(unweigthed[[i]])==0) next

    JK1=unweightedJK[[i]]
    JK2=weightedJK[[i]]
    df1=unweigthed[[i]]
    df2=weighted[[i]]
    dfBind=rbind(df1, df2)
    corDF=data.frame(exposure=getMR[i], outcome=levels(as.factor(df1$outcome)), cor=NA)

    for(j in 1:length(levels(as.factor(df1$outcome)))) {
      outcomeVar=levels(as.factor(df1$outcome))[j]
      print(paste0("Outcome: ", outcomeVar))
      outcomeSel1=subset(JK1, outcome==outcomeVar)
      outcomeSel1$ID=seq(1,NROW(outcomeSel1), 1)

      outcomeSel2=subset(JK2, outcome==outcomeVar)
      outcomeSel2$ID=seq(1,NROW(outcomeSel2), 1)
      
      outcomeSelM=merge(outcomeSel1, outcomeSel2, by="ID", all=T)
      corDF$cor[j]=cor.test(outcomeSelM$beta.x, outcomeSelM$beta.y)$estimate
    }
    dfBind$exposure_outcome=paste0(dfBind$exposure, "_", dfBind$outcome)
    corDF$exposure_outcome=paste0(corDF$exposure, "_", corDF$outcome)
    corDF$exposure=NULL;corDF$outcome=NULL
    listMRMerged[[i]]=merge(dfBind, corDF, by="exposure_outcome", all.x=T, sort=F)
}

MRldak=do.call(rbind, listMRMerged)

saveRDS(MRldak, paste0(HOME, "/results/rds/MRldak.rds"))
uploadDropbox(file="MRldak.rds", folder="rds")


}


print("all done!")

uploadLog(file=paste0("ldakMR.log"))
uploadLog(file=paste0("ldakMR.log"))