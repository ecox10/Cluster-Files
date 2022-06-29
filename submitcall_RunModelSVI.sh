#!/bin/bash
### sets the job’s name
#SBATCH --job-name=RunSVIModel
###Sets the job’s output file and path
#SBATCH --output=RunSVIModel.out.%j
### Sets the job’s error output file and path
#SBATCH --error=RunSVIModel.err.%j
### Requested number of nodes for this job. Can be a single number or a range
#SBATCH -N 4
### Requested partition (group of nodes, I.e. compute, fat, gpu, etc.) for the resource allocation
#SBATCH -p math-alderaan

singularity exec /storage/singularity/mixtures.sif /home/coxeli/stewmap/RunModel3.sh

