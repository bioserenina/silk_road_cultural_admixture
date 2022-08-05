//
  //  AMOVA_PNAS.script.h
//  
  //
  //  Created by Massimo Mezzavilla on 13/05/2022.
//
  
  #ifndef AMOVA_PNAS_script_h
  #define AMOVA_PNAS_script_h
  
  
  #endif /* AMOVA_PNAS_script_h */
  
  #AMOVA using poppr
  #prepare input files
  
plink --bfile Fst_PNAS_silk_pagani --geno 0.00 --maf 0.05 --recodeA --out CHR_ALL
plink --bfile Fst_PNAS_silk_pagani --geno 0.00 --maf 0.05 --make-just-bim --out CHR_ALL

#in R
library(adegenet)
#read id with genetic data
b=read.table("Fst_PNAS_silk_pagani.fam")
#subset id from a dataframe with pop information
a=read.table("DATA_Pagani_ERC.txt", h=T)
x=match(b$V2, a$id)
a1=a[x,]
#call the library
library(poppr)
v1=factor( a1$village)
p1=factor( a1$paese)
#import data as genlight object
datagen=read.PLINK("CHR_ALL.raw", map.file="CHR_ALL.bim")
POPULATION=p1
VILLAGE=v1
#add population and subpoulation information
strata(datagen) <- data.frame(POPULATION,VILLAGE)
#perform amova
amova.results=poppr.amova(datagen, ~POPULATION/VILLAGE)
