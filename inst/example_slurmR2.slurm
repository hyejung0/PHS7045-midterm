#!/bin/sh
#SBATCH --job-name=example_slurmR2
#SBATCH --time=00:10:00

module load R/4.2.2
Rscript --vanilla example_slurmR2.R
