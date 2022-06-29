#!/bin/bash

### Sets the job’s name
#SBATCH --job-name=RunModel3
###Sets the job’s output file and path
# SBATCH --output=RunModel3.out. %j
### Sets the job’s error output file and path
# SBATCH --error=RunModel3.err. %j
### Requested number of nodes for this job. Can be a single number or a range
# -N 4
### Requested partition (group of nodes, I.e. compute, fat, gpu, etc.) for the resource allocation
# SBATCH -p math-alderman 

singularity exec /storage/singularity/mixtures.sif /home/coxeli/stewmap/RunModel3.sh
