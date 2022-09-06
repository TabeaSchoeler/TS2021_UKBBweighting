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


export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK 
tmpdir=/scratch/$USER/$SLURM_JOBID
resdir=$PWD

$HOME/programs/R-4.1.2/bin/R --no-save < $HOME/analysis/ldakProcess.R --args $HOME $pheno > $HOME/output/log/ldakProcess_${pheno}.log

#> outputFile.log 2>&1
cp -a $tmpdir $resdir 
rm -rf $tmpdir

