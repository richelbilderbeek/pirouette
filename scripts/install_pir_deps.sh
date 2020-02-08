#!/bin/bash
# Script to install pirouette's dependencies
# (excluding pirouette itself)
# on the Peregrine computer cluster
#
# Usage:
#
#   sbatch install_pir_deps.sh
#
#SBATCH --time=1:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --ntasks=1
#SBATCH --mem=1G
#SBATCH --job-name=install_pir_deps
#SBATCH --output=install_pir_deps.log
module load GCCcore/4.9.3
module load XZ/5.2.2-foss-2016a
module load R
module load ImageMagick

Rscript -e 'remotes::install_github("richelbilderbeek/mcbette")'
Rscript -e 'if (!beastier::is_beast2_installed()) beastier::install_beast2()'
Rscript -e 'if (!mauricer::is_beast2_ns_pkg_installed()) mauricer::install_beast2_pkg("NS")'

