{\rtf1\ansi\ansicpg1252\cocoartf2513
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\margl1440\margr1440\vieww10800\viewh8400\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 #!/bin/bash\
\
### Sets the job\'92s name\
#SBATCH --job-name=RunModel3\
###Sets the job\'92s output file and path\
# SBATCH --output=RunModel3.out. %j\
### Sets the job\'92s error output file and path\
# SBATCH --error=RunModel3.err. %j\
### Requested number of nodes for this job. Can be a single number or a range\
# -N 4\
### Requested partition (group of nodes, I.e. compute, fat, gpu, etc.) for the resource allocation\
# SBATCH -p math-alderman \
\
singularity exec /storage/singularity/mixtures.sif /home/coxeli/stewmap/RunModelSVI.sh}