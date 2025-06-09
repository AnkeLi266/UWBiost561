#!/bin/bash
#SBATCH --job-name=hw4sim
#SBATCH --output=HW4_simulation.out
#SBATCH --mem-per-cpu=10gb
#SBATCH --time=6:00:00
#SBATCH --mail-type=END,FAIL

module load R
Rscript HW4_simulation_execute.R
