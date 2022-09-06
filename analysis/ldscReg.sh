#!/bin/bash
#SBATCH --mail-type=NONE
#SBATCH --mail-user=t.schoeler@ucl.ac.uk
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=7500
#SBATCH --account=sgg


export LC_ALL=C
unset LANGUAGE
unset LANG
tmpdir=/scratch/$USER/$SLURM_JOBID
resdir=$PWD
mkdir -p $tmpdir $resdir

$HOME/programs/R-4.1.2/bin/R --no-save < $HOME/analysis/ldscReg.R --args $HOME $munge > $HOME/output/log/ldscReg.log 


cp -a $tmpdir $resdir 
rm -rf $tmpdir