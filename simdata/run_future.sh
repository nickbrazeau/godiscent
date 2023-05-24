#!/bin/bash
#SBATCH --job-name=polysimIBD
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=nbrazeau@med.unc.edu
#SBATCH --ntasks=1
#SBATCH --mem=96G
#SBATCH --time=3-00:00:00
#SBATCH --output=%j.log

## Uncomment line to run each step of future
## Rscript simdata/_future_polySimIBD.R 2> polysim.log
## Rscript simdata/_future_discent.R 2> discent.log

