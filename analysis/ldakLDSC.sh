#!/bin/bash
#SBATCH --mail-type=NONE
#SBATCH --mail-user=t.schoeler@ucl.ac.uk
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=7500
#SBATCH --account=sgg


export LC_ALL=C
unset LANGUAGE
unset LANG
tmpdir=/scratch/$USER/$SLURM_JOBID
resdir=$PWD
mkdir -p $tmpdir $resdir

if [ $iterate == 'no' ]; then
$HOME/programs/R-4.1.2/bin/R --no-save < $HOME/analysis/ldakLDSC.R --args $HOME $munge > $HOME/output/log/ldakLDSC.log 
fi


if [ $iterate == 'yes' ]; then
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK 
a=${SLURM_ARRAY_TASK_ID}

$HOME/programs/R-4.1.2/bin/R --no-save < $HOME/analysis/ldakLDSC.R --args $HOME $munge $a > $HOME/output/log/ldakLDSC.log 
fi




cp -a $tmpdir $resdir 
rm -rf $tmpdir