# DataProcessingEindOpdracht
Author: Gijs Bakker  
Data: 29th June 2022  
Version: 1  

This repo contains a snakemake pipeline for the genetic characterization of Genetically Engineered Mice (GEM) models 
based on NGS data. This snakemake pipeline is a translation of the already existing pipeline found in this 
[article](https://bmcgenomics.biomedcentral.com/articles/10.1186/s12864-019-5504-9).

# use
To run this pipeline make sure you have the packages installed.
> snakemake [path/to/Snakefile] --cores [cores]

# Prerequisites
In order to use this program the following packages are needed:
* python (version 3.9)
* freebayes
* Vcflib
* samtools (version 1.11)
* BEDtools (version 2.30.0)
* BamTools (version 2.5.1)
* bcftools (version 1.11)
* snakemake (version 7.8.2)
* R (version ≥ 3.3.0)
* dplyr
* gridExtra
* reshape2
* ggplot2
* DescTools
* faToTwoBit (version ≥ 2022-06-15)

# data
* The BAM files used in this pipeline can be found [here](https://usegalaxy.org/u/carlosfarkas/h/test-sall2-ko-rna-seq-gse123168-1).
* The reference genome can be found [here](http://hgdownload.cse.ucsc.edu/goldenpath/mm10/bigZips/mm10.2bit).