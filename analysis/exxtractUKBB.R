#!/usr/bin/Rscript

args = commandArgs(trailingOnly=TRUE)
HOME=args[1]
source(paste0(HOME, "/analysis/input.R"))
source(paste0(HOME, "/analysis/functions.R"))

con <- file(paste0(HOME, "/output/log/extractUKBBlog.log"))
sink(con, append=TRUE, type="output")

print("Read in list with data to be read in")
redInRaw=list.files(paste0(UKBB, "/pheno/"), pattern="*csv", all.files=FALSE,
    full.names=FALSE)

print("Select most recenty uploaded phenotype files")
lsfiles <- lapply(redInRaw, function(x) file.info(paste0(UKBB, "/pheno/", x)))
lsfilesDF=do.call(rbind, lsfiles)
lsfilesDFordered=lsfilesDF[order(lsfilesDF$ctime,  decreasing = TRUE),]
redInFull=rownames(lsfilesDFordered)
redIn = redInFull %>% stringr::str_remove(paste0(UKBB, "/pheno/"))

print("Remove old file")
file.remove(paste0(HOME,"/data/UKBB/UKBBdf.rds"))

exclude=c( "w16389_20200204.csv", "exclusion_w16389_20180503.csv") # exclude
redIn <- redIn[!redIn %in% exclude]

print(paste0("Read in ", paste0(unique(redIn), collapse=", ")  ))
UKBBList=lapply(unique(redIn), function(x) fread(paste0(UKBB, "/pheno/", x)))
names(UKBBList)=c(redIn)


drop_auth(rdstoken = paste0(HOME, "/token.rds"))  # authentication for dropbox
drop_download(
  local_path = paste0(HOME,"/data/variableList.xlsx"),
  path = paste0(LOCAL,"/data/variableList.xlsx"), overwrite=TRUE)

varInc=subset(variableList, labelUKBB_raw!="NA")$labelUKBB_raw
weeklyFreq=c("1588", "1578", "1608", "5364", "1568", "1598") # add weekly alcohol use
varIncAlc=c(varInc, weeklyFreq)
varIncName=paste0(unique(varIncAlc), "-0.0")
listDFex=list()

for ( i in 1:length(UKBBList) ) {
    print(paste0("Read in ", redIn[i] ))
    df=UKBBList[[i]]
    dfOut=data.frame(eid=df$eid)

for ( j in 1:length(varIncName) ) {
    colIN=varIncName[j]
        if(colIN %in% colnames(df)==F) next
        
        if(colIN %in% colnames(df)==T){
            print(paste0(colIN, " available"))
             dfOut[colIN] = df[[colIN]]
             }
        }
      listDFex[[i]]=dfOut
}

str(listDFex)


print("Merge all dataframes")
UKBBdf=Reduce(function(x,y) full_join(x = x, y = y, by = "eid", suffix=c("", "") ),  listDFex)

print("Save selected data")
saveRDS(UKBBdf, paste0(HOME, "/data/UKBB/UKBBdf.rds"))

print("All output saved")
uploadLog(file=paste0("extractUKBBlog.log"))


print("All data saved on cluster")

