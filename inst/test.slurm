#!/bin/sh
#SBATCH --job-name=test
#SBATCH --account=notchpeak-shared-short
#SBATCH --partition=notchpeak-shared-short
#SBATCH --nodes=2
#SBATCH --ntasks=20
#SBATCH --cpus-per-task=16
#SBATCH --time=00:05:00
#SBATCH --output=test.log
#SBATCH --array=1-2

# Loading R
module load R/4.2.2

#  Running the script
Rscript --vanilla test.R
