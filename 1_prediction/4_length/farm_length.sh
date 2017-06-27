#!/bin/bash -l
#SBATCH -J MyJob

module load R
hostname -f
echo $SLURM_ARRAY_TASK_ID

#Use Rscript when you want debug info and print() to work
echo " Rscript ${1} ${2} "
Rscript --vanilla ${1} ${2} ${3}

#echo "R CMD BATCH --no-save --no-restore ${1} ${2}"
#R CMD BATCH --no-save --no-restore ${1} ${2}

