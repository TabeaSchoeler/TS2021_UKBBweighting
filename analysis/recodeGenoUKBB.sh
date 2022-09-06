#!/bin/bash

#SBATCH --mail-type=NONE
#SBATCH --mail-user=t.schoeler@ucl.ac.uk
#SBATCH --ntasks=1 # Run on a single core
#SBATCH --cpus-per-task=10
#SBATCH --mem-per-cpu=7500
#SBATCH --time=10:05:00


if [ $task == "QC" ]
then

echo "run QC filter on SNPs"
$HOME/programs/R-4.1.2/bin/R --no-save < $HOME/analysis/prepareGeno.R --args $HOME
fi 


if [ $task == "LDAK" ]
then
echo "prepare files for LDAK"
# copy over Eleonora's files
for chr in {1..22}; do
rm $UKBBgeno/genoID_clean/chr${chr}_clean.bed
ln -s $UKBBgeno/genoQC/chr${chr}.bed $UKBBgeno/genoID_clean/chr${chr}_clean.bed # creates only a link
cp $UKBBgeno/genoQC/chr${chr}.fam $UKBBgeno/genoID_clean/chr${chr}_clean.fam
done




# remove duplicates
# create a new bim file that contains generic names (chr:bp_a1_a2)
for chr in {1..22}; do
awk < $UKBBgeno/genoQC/chr${chr}.bim '{$2=$1":"$4"_"$5"_"$6;print $0}' > $UKBBgeno/genoID_clean/chr${chr}_clean.bim
done

fi


echo "upload log file"
log="$HOME/output/log/genoPrep.$task.out"
$HOME/programs/R-4.1.2/bin/R --no-save < $HOME/analysis/uploadLog.R --args $HOME $log
