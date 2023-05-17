#!/bin/bash
#SBATCH --job-name=polysimIBD
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=nbrazeau@med.unc.edu
#SBATCH --ntasks=36
#SBATCH --mem=32G
#SBATCH --time=3-00:00:00
#SBATCH --output=polysim_%j.log

## Uncomment line to run each step of future
## Rscript analyses/_future_polySimIBD.R 2> polysim.log
## Rscript BATCH analyses/_future_searchgrid.R 2> searchgrid.log
## Rscript BATCH analyses/_future_discent.R 2> discent.log

