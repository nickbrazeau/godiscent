#! /bin/bash

ROOT=/proj/ideel/meshnick/users/NickB/Projects/godiscent # root directory for project (non-scratch)
WD=/work/users/n/f/nfb/Projects/godiscent # scratch directory for networks
NODES=1028 # max number of cluster nodes
WAIT=30 # number of seconds to wait for files to appear, absorbing some file system latency

snakemake \
	--snakefile $ROOT/simdata/run_discent.snake \
	--configfile $ROOT/simdata/config_godiscent_batch_LL.yaml \
	--printshellcmds \
	--directory $WD \
	--cluster $ROOT/simdata/launch.py \
	-j $NODES \
	--rerun-incomplete \
	--keep-going \
	--latency-wait $WAIT \
	--dryrun -p
