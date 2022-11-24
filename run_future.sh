#!/bin/bash
#SBATCH --job-name=polysimIBD
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=nbrazeau@med.unc.edu
#SBATCH --ntasks=36
#SBATCH --mem=64G
#SBATCH --time=5-00:00:00
#SBATCH --output=polysim_%j.log

## Uncomment line to run each step of future
## R CMD BATCH analyses/_future_polySimIBD.R
## R CMD BATCH analyses/_future_searchgrid.R
## R CMD BATCH analyses/_future_discent.R

