#!/bin/sh
#SBATCH --job-name=test
#SBATCH --account=notchpeak-shared-short
#SBATCH --partition=notchpeak-shared-short
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --time=00:05:00
#SBATCH --output=test.log

# Loading R
module load R/4.2.2

#  Running the script
Rscript --vanilla test.R
