#!/bin/bash
#SBATCH --account=epi
#SBATCH --qos=epi
#SBATCH --job-name=spades
#SBATCH --mail-type=ALL
#SBATCH --mail-user=ralcala@ufl.edu
#SBATCH --ntasks=12
#SBATCH --cpus-per-task=1
#SBATCH --mem=32gb
#SBATCH --time=80:00:00
#SBATCH --output=sassemb%j.out
#SBATCH --array=1-4%4
date; hostname; pwd

module load ufrc
module load spades

files=$(ls *.fastq.gz | cut -d_ -f1,2 | uniq | sed -n ${SLURM_ARRAY_TASK_ID}p )

echo "processing" $files

echo "Assembling reads with Spades" $(spades.py -v)

#while IFS= read -r line
#do

echo "assembling" $line "reads"  
spades.py --metaviral -1 "$files"_*1*.fastq.gz -2 "$files"_*2*.fastq.gz -t 12 --rnaviral -o "$files"_assemblies

#done < vectors.fof
