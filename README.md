Correction for participation bias in the UK Biobank
================

# Overview

The complete analytical pipeline used to run the weighted genome-wide
association analyses is included in the script
[UKBBweightingPipeline.sh](#https://github.com/TabeaSchoeler/TS2021_UKBBweighting/tree/main/analysis).
The scripts used to process the results and prepare the plots and table
for publication are included in the R script
[UKBBweighting.R](#https://github.com/TabeaSchoeler/TS2021_UKBBweighting/tree/main/analysis).

</br>

-   [Generation of the UKBB Sampling
    Weights](#generation-of-the-ukbb-sampling-weights)

    -   [Preparation of UKBB phenotype
        data](#preparation-of-ukbb-phenotype-data)
    -   [Preparation of HSE phenotype
        data](#processing-of-hse-phenotype-data)
    -   [Assess performance of the sampling
        weights](#assess-performance-of-the-sampling-weights)
    -   [Compare with UK Census data](#compare-with-uk-census-data)
    -   [Prepare genotype data](#prepare-genotype-data)

-   [Weighted genome-wide association
    analyses](#weighted-genome-wide-association-analyses)

    -   [Prepare genotype data for
        LDAK](#prepare-genotype-data-for-ldak)
    -   [Perform weighted GWA in LDAK](#perform-weighted-gwa-in-ldak)
    -   [Process LDAK output](#process-ldak-output)
    -   [Get SNP correlations](#get-snp-correlations)

-   [Downstream GWA analyses](#downstream-gwa-analyses)

    -   [LD Score regression and heritability
        estimates](#ls-score-regression-and-heritability-estimates)
    -   [Autosomal GWA on sex](#autosomal-gwa-on-sex)
    -   [Mendelian Randomization
        analyses](#mendelian-randomization-analyses)

</br></br>

# Generation of the UKBB Sampling Weights

</br>

#### Preparation of UKBB phenotype data

    # ======= Merge UKBB phenotype data ==============
    sbatch --export=HOME=$HOME,sample="extractUKBB" --chdir=$HOME/output/log --job-name recodePheno.${sample} --partition=cluster2 $HOME/analysis/recodePheno.sh

Executes the script
[exxtractUKBB.R](#https://github.com/TabeaSchoeler/TS2021_UKBBweighting/tree/main/analysis)

</br>

    # ======= Prepare UKBB phenotype data ==============
    sbatch --export=HOME=$HOME,sample="UKBB" --chdir=$HOME/output/log --job-name recodePheno.${sample} --partition=cluster2 $HOME/analysis/recodePheno.sh

Executes the script
[recodePhenoUKBB.R](#https://github.com/TabeaSchoeler/TS2021_UKBBweighting/tree/main/analysis)

</br>

#### Preparation of HSE phenotype data

    # ======= Prepare HSE phenotype data ==============
    $HOME/programs/R-4.1.2/bin/R --no-save < $HOME/analysis/recodePhenoHSE.R --args $HOME > $HOME/output/log/recodePhenoHSE.log 2>&1 & disown

Executes the script
[recodePhenoHSE.R](#https://github.com/TabeaSchoeler/TS2021_UKBBweighting/tree/main/analysis)

</br>

    # ======= Prepare HSE/UKBB data for weighting ==============
    sbatch --export=HOME=$HOME --chdir=$HOME/output/log --partition=cluster2 --time="0-02:00:00" --output=weighPrepUKBB_HSE.out $HOME/analysis/weighPrepUKBB_HSE.sh

Executes the script
[weighPrepUKBB_HSE.R](#https://github.com/TabeaSchoeler/TS2021_UKBBweighting/tree/main/analysis)

</br>

#### Assess performance of the sampling weights

Relies on the script
[weighUKBB_HSE.R](#https://github.com/TabeaSchoeler/TS2021_UKBBweighting/tree/main/analysis)
to perform LASSO regression in glmnet to predict UKBB participation,
conditional on all harmonized auxiliary variables.

</br>

    # no cross-validation
    validation="all"
    sbatch --export=HOME=$HOME,validation="all" --chdir=$HOME/output/log --job-name weighUKBB_HSE.${validation} --error=weighUKBB_HSE.${validation}.err --partition=sgg --time="0-47:00:00" --output=weighUKBB_HSE.${validation}.out $HOME/analysis/weighUKBB_HSE.sh

If `validation="all"`, the following LASSO is tested: a model including
the main effects of all auxiliary variables, where categorical and
binary variables are entered as dummy variables, indexing each possible
level of the variable. The model also includes all possible two-way
interaction terms among the dummy and continuous variables.

</br>

    # 5-times cross-validation
    validation="validation"
    sbatch --export=HOME=$HOME,validation="validation" --chdir=$HOME/output/log --job-name weighUKBB_HSE.${validation} --error=weighUKBB_HSE.${validation}.err --partition=sgg --time="0-47:00:00" --output=weighUKBB_HSE.${validation}.out $HOME/analysis/weighUKBB_HSE.sh

If `validation="validation"`, LASSO regression is performed in
train-test splits of the data (5-fold, with a split ratio of 80:20).

</br>

    # get correlations
    validation="getcor"
    sbatch --export=HOME=$HOME,validation="getcor" --chdir=$HOME/output/log  --job-name weighUKBB_HSE.${validation} --error=weighUKBB_HSE.${validation}.err --partition=sgg --time="0-47:00:00" --output=weighUKBB_HSE.${validation}.out $HOME/analysis/weighUKBB_HSE.sh

If `validation="getcor"`, the correlations among all auxiliary variables
within the UKBB and the HSE are estimated.

</br>

    # check Regresssion predicting sample status
    validation="RegCheck"
    sbatch --export=HOME=$HOME,validation="RegCheck" --chdir=$HOME/output/log  --job-name weighUKBB_HSE.${validation} --error=weighUKBB_HSE.${validation}.err --partition=sgg --time="0-47:00:00" --output=weighUKBB_HSE.${validation}.out $HOME/analysis/weighUKBB_HSE.sh

If `validation="RegCheck"`, the weighted means and proportions are
estimated. Also tested is a univariate logistic regression model
predicting UKBB participation, where UKBB individuals were given their
normalized weight and HSE participants were given a weight of 1.

</br>

    # prepare data for GWA
    validation="prepGWA"
    sbatch --export=HOME=$HOME,validation="prepGWA" --chdir=$HOME/output/log  --job-name weighUKBB_HSE.${validation} --error=weighUKBB_HSE.${validation}.err --partition=cluster2 --time="0-04:00:00" --output=weighUKBB_HSE.${validation}.out $HOME/analysis/weighUKBB_HSE.sh

</br>

    # get info on excluded individuals
    validation="missingness"
    sbatch --export=HOME=$HOME,validation="matching" --chdir=$HOME/output/log  --job-name weighUKBB_HSE.${validation} --error=weighUKBB_HSE.${validation}.err --partition=sgg --time="0-44:00:00" --output=weighUKBB_HSE.${validation}.out $HOME/analysis/weighUKBB_HSE.sh

</br>

#### Compare with UK Census data

    sbatch --export=HOME=$HOME --chdir=$HOME/output/log --partition=cluster2 --error=recodePhenoCensus.err --output=recodePhenoCensus.out $HOME/analysis/recodePhenoCensus.sh

Executes the script
[recodePhenoCensus.R](#https://github.com/TabeaSchoeler/TS2021_UKBBweighting/tree/main/analysis)
to prepare the UK Census data. The data used to assess the level of
representativeness of the HSE, by comparing the distributions and
associations between variables present in both the HSE and Census
sample.

</br>

#### Prepare genotype data

    task="QC" # Remove MAF > 0.01 & INFO> 0.9 & HWE
    sbatch --export=HOME=$HOME,task=$task --job-name genoPrep.$task --partition=sgg --chdir=$HOME/output/log --output=genoPrep.$task.out --error=genoPrep.$task.err $HOME/analysis/recodeGenoUKBB.sh

Application of QC filters for genome-wide analyses to select
participants (i.e., exclusion of related individuals, exclusion of
non-White British ancestry based on principal components, high missing
rate and high heterozygosity on autosomes) and genetic variants
(Hardy–Weinberg disequilibrium, minor allele frequency\>1% and call
rate\>90%).

</br></br>

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

</br>

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

</br></br>
