
##############################################################################
# ============================== UKBB weighting ==============================
##############################################################################

# ======= Define Home directory ==============
HOME="/data/sgg3/tabea/TS2021_UKBBweighting"
source $HOME/analysis/functions.sh
sCluster # in 10 cores
sCluster2
sClusterSGG

# Initiate R
alias R='$HOME/programs/R-4.1.2/bin/R'
cd $HOME
R
source("analysis/input.R")


# ======= Create R files loading libraries and pathnames
cat > $HOME/analysis/input.R <<EOF
HOME='$HOME'
UKBB='$UKBB'
GWA='$GWA'
UKBBgeno='$UKBBgeno'
LOCAL='$LOCAL'
R_libPaths=paste0('$HOME', "/programs/R")
.libPaths(R_libPaths)
load.lib=c('tidyverse', 'plyr', 'data.table', 'devtools', 'survey', 'foreign', 'gmodels', 'rdrop2', 'TwoSampleMR')
sapply(load.lib,require,character=TRUE)
drop_auth(rdstoken = paste0('$HOME', '/token.rds'))
drop_acc()
EOF


###########################################################################
## ======================= Generate Sampling Weights =================== ##
###########################################################################

# ======= Merge UKBB phenotype data ==============
sbatch --export=HOME=$HOME,sample="extractUKBB" --chdir=$HOME/output/log --job-name recodePheno.${sample} --partition=cluster2 $HOME/analysis/recodePheno.sh

# ======= Prepare UKBB phenotype data ==============
sbatch --export=HOME=$HOME,sample="UKBB" --chdir=$HOME/output/log --job-name recodePheno.${sample} --partition=cluster2 $HOME/analysis/recodePheno.sh

# ======= Prepare HSE phenotype data ==============
# Note: does not run as job (£ in income variable not recognized)
$HOME/programs/R-4.1.2/bin/R --no-save < $HOME/analysis/recodePhenoHSE.R --args $HOME > $HOME/output/log/recodePhenoHSE.log 2>&1 & disown

# ======= Prepare HSE/UKBB data for weighting ==============
sbatch --export=HOME=$HOME --chdir=$HOME/output/log --partition=cluster2 --time="0-02:00:00" --output=weighPrepUKBB_HSE.out $HOME/analysis/weighPrepUKBB_HSE.sh

# ======= Get weighted phenotype data ==============
# no cross-validation
validation="all"
sbatch --export=HOME=$HOME,validation="all" --chdir=$HOME/output/log --job-name weighUKBB_HSE.${validation} --error=weighUKBB_HSE.${validation}.err --partition=sgg --time="0-47:00:00" --output=weighUKBB_HSE.${validation}.out $HOME/analysis/weighUKBB_HSE.sh

# 5-times cross-validation
validation="validation"
sbatch --export=HOME=$HOME,validation="validation" --chdir=$HOME/output/log --job-name weighUKBB_HSE.${validation} --error=weighUKBB_HSE.${validation}.err --partition=sgg --time="0-47:00:00" --output=weighUKBB_HSE.${validation}.out $HOME/analysis/weighUKBB_HSE.sh

# get correlations
validation="getcor"
sbatch --export=HOME=$HOME,validation="getcor" --chdir=$HOME/output/log  --job-name weighUKBB_HSE.${validation} --error=weighUKBB_HSE.${validation}.err --partition=sgg --time="0-47:00:00" --output=weighUKBB_HSE.${validation}.out $HOME/analysis/weighUKBB_HSE.sh

# check Regresssion predicting sample status
validation="RegCheck"
sbatch --export=HOME=$HOME,validation="RegCheck" --chdir=$HOME/output/log  --job-name weighUKBB_HSE.${validation} --error=weighUKBB_HSE.${validation}.err --partition=sgg --time="0-47:00:00" --output=weighUKBB_HSE.${validation}.out $HOME/analysis/weighUKBB_HSE.sh

# prepare data for GWA
validation="prepGWA"
sbatch --export=HOME=$HOME,validation="prepGWA" --chdir=$HOME/output/log  --job-name weighUKBB_HSE.${validation} --error=weighUKBB_HSE.${validation}.err --partition=cluster2 --time="0-04:00:00" --output=weighUKBB_HSE.${validation}.out $HOME/analysis/weighUKBB_HSE.sh

# get info on excluded individuals
validation="missingness"
sbatch --export=HOME=$HOME,validation="matching" --chdir=$HOME/output/log  --job-name weighUKBB_HSE.${validation} --error=weighUKBB_HSE.${validation}.err --partition=sgg --time="0-44:00:00" --output=weighUKBB_HSE.${validation}.out $HOME/analysis/weighUKBB_HSE.sh


###########################################################################
## =======================  Match UK Census Data ======================= ##
###########################################################################
sbatch --export=HOME=$HOME --chdir=$HOME/output/log --partition=cluster2 --error=recodePhenoCensus.err --output=recodePhenoCensus.out $HOME/analysis/recodePhenoCensus.sh


###########################################################################
## ======================= Prepare genotype data ======================= ##
###########################################################################
task="QC" # Remove MAF > 0.01 & INFO> 0.9 & HWE
sbatch --export=HOME=$HOME,task=$task --job-name genoPrep.$task --partition=sgg --chdir=$HOME/output/log --output=genoPrep.$task.out --error=genoPrep.$task.err $HOME/analysis/recodeGenoUKBB.sh



###########################################################################
## ========================== LDAK GWA ================================= ##
###########################################################################

# ======= Prepare genotype data for LDAK ==============
task="LDAK" # Remove duplicates
sbatch --export=HOME=$HOME,task=$task --job-name genoPrep.$task --partition=sgg --chdir=$HOME/output/log --output=genoPrep.$task.out --error=genoPrep.$task.err $HOME/analysis/recodeGenoUKBB.sh

# ======= Run weighted GWA in LDAK ==============
for a in `seq 1 $(sed -n '$=' $HOME/data/UKBB/varListGWA)`
do
    line=$(sed "${a}q;d" $HOME/data/UKBB/varListGWA)
    pheno=$(echo "$line"| awk -F" "  '{print $1}')
    runningTime="0-5:00:00"
    echo $pheno

      for chr in {1..22}; do
        #rm $HOME/output/ldak/regressRes/${pheno}_chr${chr}_weighted.progress
        estimation="weighted"
        progressLDAK
        echo $exist
        if [ "$exist" = "noExist" ]; then
         echo "submit batch job for $pheno and chomosome $chr (weigthed)"
        sbatch --partition=sgg --time=$runningTime --job-name ldakGWA.$pheno.chr${chr}.$estimation --output=ldakGWA.$pheno.chr${chr}.$estimation.out --error=ldakGWA.$pheno.chr${chr}.$estimation.err --export=HOME=$HOME,pheno=$pheno,estimation=$estimation --array $chr --chdir=$HOME/output/log $HOME/analysis/ldakGWA.sh
        fi

        #rm $HOME/output/ldak/regressRes/${pheno}_chr${chr}.progress
        estimation="unweighted"
        progressLDAK
        echo $exist
        if [ "$exist" = "noExist" ]; then
        echo "submit batch job for $pheno and chomosome $chr (unweighted)"
        sbatch --partition=sgg --time=$runningTime --job-name ldakGWA.$pheno.chr${chr}.$estimation --output=ldakGWA.$pheno.chr${chr}.$estimation.out --error=ldakGWA.$pheno.chr${chr}.$estimation.err --export=HOME=$HOME,pheno=$pheno,estimation=$estimation --array $chr --chdir=$HOME/output/log $HOME/analysis/ldakGWA.sh
        fi
      done
done

# ======= Process LDAK output ==============
#rm $HOME/output/ldak/regressComb/*
for a in `seq 1 $(sed -n '$=' $HOME/data/UKBB/varListGWA)`
do
    line=$(sed "${a}q;d" $HOME/data/UKBB/varListGWA)
    pheno=$(echo "$line"| awk -F" "  '{print $1}')
        file=$HOME/output/ldak/regressComb/$pheno
       #rm $file
        if [ ! -f "$file" ]; then
        echo "$pheno not in folder"
        sbatch --export=HOME=$HOME,pheno=$pheno --chdir=$HOME/output/log --job-name ldakProcess_${pheno} --output=ldakProcess_${pheno}.out --partition=cluster2 --time="0-02:30:00" $HOME/analysis/ldakProcess.sh
        fi
done


# ======= Get SNP correlations ==============
# extrac SNPs in plink
chunks="1"
extractSNPs="extract"
sbatch --export=HOME=$HOME,chunks=$chunks,extractSNPs=$extractSNPs --time="0-01:30:00" --output=snpExtract.out --array 1-$chunks --chdir=$HOME/output/log --partition=sgg $HOME/analysis/ldakSNPcor.sh

# generate SNP correlations
chunks="10"
extractSNPs="noextract"
sbatch --export=HOME=$HOME,chunks=$chunks,extractSNPs=$extractSNPs --time="0-2:30:00" --output=snpCor.out --array 1-$chunks --chdir=$HOME/output/log --partition=sgg $HOME/analysis/ldakSNPcor.sh

# process SNP correlations
extractSNPs="process"
sbatch --export=HOME=$HOME,chunks=$chunks,extractSNPs=$extractSNPs --time="0-0:30:00" --output=snpProcess.out --array 1 --chdir=$HOME/output/log --partition=sgg $HOME/analysis/ldakSNPcor.sh



###########################################################################
## ========================== LD SCORE REGRESSION ====================== ##
###########################################################################

# ======= Munge data ==============
munge="yes" # should GWAS be newly muged?
sbatch --export=HOME=$HOME,munge=$munge,iterate="no" --chdir=$HOME/output/log --output=ldakLDSC.out --partition=cluster2 $HOME/analysis/ldakLDSC.sh

# ======= Perform LDSC on LDAK data ==============
munge="ldsc" # should GWAS be newly muged?
sbatch --export=HOME=$HOME,munge=$munge,iterate="no" --chdir=$HOME/output/log --output=ldakLDSC.out --partition=cluster2 $HOME/analysis/ldakLDSC.sh

# ======= JACKKNIFE ==============
munge="JK_filter" # filter LD scores (200 splits)
sbatch --export=HOME=$HOME,munge=$munge,iterate="no" --chdir=$HOME/output/log --error=sexCheck.err --output=ldakLDSC.out --partition=cluster2 $HOME/analysis/ldakLDSC.sh

# ======= JACKKNIFE ==============
munge="JK" # perform Jackknife
sbatch --export=HOME=$HOME,munge=$munge,iterate="yes" --array 1-200 --chdir=$HOME/output/log --output=ldakLDSC.out --partition=cluster2 $HOME/analysis/ldakLDSC.sh

# ======= JACKKNIFE (process) ==============
munge="JK_process" # perform Jackknife
sbatch --export=HOME=$HOME,munge=$munge,iterate="no" --chdir=$HOME/output/log --output=ldakLDSC.out --partition=cluster2 $HOME/analysis/ldakLDSC.sh


# ======= Perform LDSC on PS (weighted) ==============
# NOTE: needs to be done AFTER running LDSC on LDAK
munge="yes" 
sbatch --export=HOME=$HOME,munge=$munge --chdir=$HOME/output/log --output=ldscReg.out --partition=sgg $HOME/analysis/ldscReg.sh


###########################################################################
## ======================= 23andMe sex check =========================== ##
###########################################################################
sbatch --export=HOME=$HOME --chdir=$HOME/output/log --output=sexCheck.out --error=sexCheck.err --partition=sgg $HOME/analysis/sexSNPcheck.sh


###########################################################################
## ======================= MENDELIAN RANDOMIZATION ===================== ##
###########################################################################
# ======= Perform MR ==============
sbatch --export=HOME=$HOME,task="MR" --chdir=$HOME/output/log --output=ldakMR.out --error=ldakMR.err --partition=cluster $HOME/analysis/ldakMR.sh

# ======= Process MR results ==============
sbatch --export=HOME=$HOME,task="MR_JKprocess" --chdir=$HOME/output/log --output=ldakMR.out --error=ldakMR.err --partition=cluster $HOME/analysis/ldakMR.sh


###########################################################################
## =================== Upload all analytical scripts =================== ##
###########################################################################
$HOME/programs/R-4.1.2/bin/R --no-save < $HOME/analysis/uploadScripts.R --args $HOME > $HOME/output/log/uploadScripts.log  2>&1 & disown
