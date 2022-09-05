Correction for participation bias in the UK Biobank
================

# Overview

The following scripts guide through the main analytical steps of the
work, including

# Table of Contents

-   [Generation of the UKBB Sampling Weights](#genWeights)

    -   [Processing of UKBB phenotype data](#procPhenoUKBB)
    -   [Preparation of HSE phenotype data](#procPhenoHSE)
    -   [Assess performance of the sampling weights](#performanceW)
    -   [Compare with UK Census data](#performanceCensus)
    -   [Prepare genotype data](#prepareGeno)

-   [Weighted genome-wide association analyses](#wGWA)

    -   [Prepare genotype data for LDAK](#prepLDAK)
    -   [Perform weighted GWA in LDAK](#perfLDAK)
    -   [Process LDAK output](#procLDAK)
    -   [Get SNP correlations](#getCor)

-   [Downstream GWA analyses](#downstreamGWA)

    -   [LD Score regression and heritability estimates](#ldsc)
    -   [Autosomal GWA on sex](#sexGWA)
    -   [Mendelian Randomization analyses](#MR)

The complete analytical pipeline used to run the weighted genome-wide
association analyses is included in the script ‘xxx’. The scripts used
to process the results and prepare the plots and table for publication
are included in ‘xxx’.

# Generation of the UKBB Sampling Weights

#### Preparation of UKBB phenotype data

    # ======= Merge UKBB phenotype data ==============
    sbatch --export=HOME=$HOME,sample="extractUKBB" --chdir=$HOME/output/log --job-name recodePheno.${sample} --partition=cluster2 $HOME/analysis/recodePheno.sh

    # ======= Prepare UKBB phenotype data ==============
    sbatch --export=HOME=$HOME,sample="UKBB" --chdir=$HOME/output/log --job-name recodePheno.${sample} --partition=cluster2 $HOME/analysis/recodePheno.sh

#### Preparation of HSE phenotype data

    # ======= Prepare HSE phenotype data ==============
    # Note: does not run as job (£ in income variable not recognized)
    $HOME/programs/R-4.1.2/bin/R --no-save < $HOME/analysis/recodePhenoHSE.R --args $HOME > $HOME/output/log/recodePhenoHSE.log 2>&1 & disown

    # ======= Prepare HSE/UKBB data for weighting ==============
    sbatch --export=HOME=$HOME --chdir=$HOME/output/log --partition=cluster2 --time="0-02:00:00" --output=weighPrepUKBB_HSE.out $HOME/analysis/weighPrepUKBB_HSE.sh

#### Assess performance of the sampling weights

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

#### Compare with UK Census data

    sbatch --export=HOME=$HOME --chdir=$HOME/output/log --partition=cluster2 --error=recodePhenoCensus.err --output=recodePhenoCensus.out $HOME/analysis/recodePhenoCensus.sh

#### Prepare genotype data

    task="QC" # Remove MAF > 0.01 & INFO> 0.9 & HWE
    sbatch --export=HOME=$HOME,task=$task --job-name genoPrep.$task --partition=sgg --chdir=$HOME/output/log --output=genoPrep.$task.out --error=genoPrep.$task.err $HOME/analysis/recodeGenoUKBB.sh

# Weighted genome-wide association analyses

#### Prepare genotype data for LDAK

    task="LDAK" # Remove duplicates
    sbatch --export=HOME=$HOME,task=$task --job-name genoPrep.$task --partition=sgg --chdir=$HOME/output/log --output=genoPrep.$task.out --error=genoPrep.$task.err $HOME/analysis/recodeGenoUKBB.sh

#### Perform weighted GWA in LDAK

    for a in `seq 1 $(sed -n '$=' $HOME/data/UKBB/varListGWA)`
    do
        line=$(sed "${a}q;d" $HOME/data/UKBB/varListGWA)
        pheno=$(echo "$line"| awk -F" "  '{print $1}')
        runningTime="0-5:00:00"
        echo $pheno

          for chr in {1..22}; do
            estimation="weighted"
            progressLDAK
            echo $exist
            if [ "$exist" = "noExist" ]; then
             echo "submit batch job for $pheno and chomosome $chr (weigthed)"
            sbatch --partition=sgg --time=$runningTime --job-name ldakGWA.$pheno.chr${chr}.$estimation --output=ldakGWA.$pheno.chr${chr}.$estimation.out --error=ldakGWA.$pheno.chr${chr}.$estimation.err --export=HOME=$HOME,pheno=$pheno,estimation=$estimation --array $chr --chdir=$HOME/output/log $HOME/analysis/ldakGWA.sh
            fi

            estimation="unweighted"
            progressLDAK
            echo $exist
            if [ "$exist" = "noExist" ]; then
            echo "submit batch job for $pheno and chomosome $chr (unweighted)"
            sbatch --partition=sgg --time=$runningTime --job-name ldakGWA.$pheno.chr${chr}.$estimation --output=ldakGWA.$pheno.chr${chr}.$estimation.out --error=ldakGWA.$pheno.chr${chr}.$estimation.err --export=HOME=$HOME,pheno=$pheno,estimation=$estimation --array $chr --chdir=$HOME/output/log $HOME/analysis/ldakGWA.sh
            fi
          done
    done

#### Process LDAK output

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

#### Get SNP correlations

    # extract SNPs in plink
    chunks="1"
    extractSNPs="extract"
    sbatch --export=HOME=$HOME,chunks=$chunks,extractSNPs=$extractSNPs --time="0-01:30:00" --output=snpExtract.out --array 1-$chunks --chdir=$HOME/output/log --partition=sgg $HOME/analysis/ldakSNPcor.sh

    # obtain SNP correlations
    chunks="10"
    extractSNPs="noextract"
    sbatch --export=HOME=$HOME,chunks=$chunks,extractSNPs=$extractSNPs --time="0-2:30:00" --output=snpCor.out --array 1-$chunks --chdir=$HOME/output/log --partition=sgg $HOME/analysis/ldakSNPcor.sh

    # process SNP correlations
    extractSNPs="process"
    sbatch --export=HOME=$HOME,chunks=$chunks,extractSNPs=$extractSNPs --time="0-0:30:00" --output=snpProcess.out --array 1 --chdir=$HOME/output/log --partition=sgg $HOME/analysis/ldakSNPcor.sh

# Downstream GWA analyses

#### LD Score regression and heritability estimates

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
    munge="JK_process" # process Jackknife results
    sbatch --export=HOME=$HOME,munge=$munge,iterate="no" --chdir=$HOME/output/log --output=ldakLDSC.out --partition=cluster2 $HOME/analysis/ldakLDSC.sh

    # ======= Perform LDSC on PS (weighted) ==============
    # NOTE: needs to be done AFTER running LDSC on LDAK
    munge="yes" 
    sbatch --export=HOME=$HOME,munge=$munge --chdir=$HOME/output/log --output=ldscReg.out --partition=sgg $HOME/analysis/ldscReg.sh

#### Autosomal GWA on sex

    sbatch --export=HOME=$HOME --chdir=$HOME/output/log --output=sexCheck.out --error=sexCheck.err --partition=sgg $HOME/analysis/sexSNPcheck.sh

#### Mendelian Randomization analyses

    # ======= Perform MR ==============
    sbatch --export=HOME=$HOME --chdir=$HOME/output/log --output=ldakMR.out --error=ldakMR.err --partition=cluster $HOME/analysis/ldakMR.sh
