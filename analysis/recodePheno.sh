#!/bin/bash
#SBATCH --mail-type=NONE
#SBATCH --mail-user=t.schoeler@ucl.ac.uk
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=7500
#SBATCH --account=sgg
#SBATCH --time=03:00:00

export LC_ALL=C
unset LANGUAGE
unset LANG
tmpdir=/scratch/$USER/$SLURM_JOBID
resdir=$PWD
mkdir -p $tmpdir $resdir


if [ $sample == "extractUKBB" ]; then
echo "extract UKBB data"
$HOME/programs/R-4.1.2/bin/R --no-save < $HOME/analysis/exxtractUKBB.R --args $HOME > $HOME/output/log/extractUKBB.log  
fi



if [ $sample == "UKBB" ]; then

echo "recode UKBB data"
$HOME/programs/R-4.1.2/bin/R --no-save < $HOME/analysis/recodePhenoUKBB.R --args $HOME > $HOME/output/log/recodePhenoUKBB.log  
fi


if [ $sample == "HSE" ]; then

echo "recode HSE data"
$HOME/programs/R-4.1.2/bin/R --no-save < $HOME/analysis/recodePhenoHSE.R --args $HOME > $HOME/output/log/recodePhenoHSE.log  
fi


cp -a $tmpdir $resdir 
rm -rf $tmpdir