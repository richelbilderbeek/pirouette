#!/bin/bash
# Script to install pirouette and its dependencies
# on the Peregrine computer cluster
#
# Usage:
#
#   sbatch install_pirouette.sh
#
#SBATCH --time=1:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --ntasks=1
#SBATCH --mem=1G
#SBATCH --job-name=install_pirouette
#SBATCH --output=install_pirouette.log
module load GCCcore/4.9.3
module load XZ/5.2.2-foss-2016a
module load R
module load ImageMagick

Rscript -e 'remotes::install_github("thijsjanzen/nLTT")'
Rscript -e 'remotes::install_github("richelbilderbeek/mcbette", dependencies = TRUE)'
Rscript -e 'if (!beastier::is_beast2_installed()) beastier::install_beast2()'
Rscript -e 'if (!mauricer::is_beast2_ns_pkg_installed()) mauricer::install_beast2_pkg("NS")'
