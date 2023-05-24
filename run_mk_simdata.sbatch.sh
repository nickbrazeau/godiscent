#!/bin/bash
#SBATCH --job-name=goDISCent
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=nbrazeau@med.unc.edu
#SBATCH --ntasks=1
#SBATCH --mem=8G
#SBATCH --time=3-00:00:00
#SBATCH --output=%j.log

## Uncomment line to run each step of future
## Rscript simdata/future_polySimIBD.R 2> polysim.log
## Rscript simdata/future_discent.R 2> discent.log

