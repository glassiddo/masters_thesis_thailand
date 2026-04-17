#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    Around July 2025
#* Title:   Project Set Up
#* Note:    Run this file first. Adjust global data paths          
#*******************************************************************************
pacman::p_load(
  dplyr, haven, sandwich, plm, reshape2, data.table, here, tidyr,
  tidyverse, stargazer, ggplot2, purrr, magrittr, stringr, dtplyr,
  geodata, spData, sf, terra, maps, sp, raster, exactextractr, # spatial analysis
  rnaturalearth, rnaturalearthdata, # country/continent maps
  lmtest, fixest, # twfe + sun and abraham event study
  plotrix, cowplot, patchwork,
  kableExtra, ggpubr
)
rm(list = ls())
gc()
options(scipen = 999)
options(digits=3)
set.seed(10022026)
#*******************************************************************************

raw.dir <- "data/raw/"
build.dir <- "data/build/"
out.dir <- "data/final"
#*******************************************************************************
# functions used across files ----
'%!in%' <- function(x,y)!('%in%'(x,y))
select <- dplyr::select
lag <- dplyr::lag
year <- lubridate::year
month <- lubridate::month
week <- lubridate::week

setwd("C:/Users/iddo2/Dropbox/thesis/") # change as necessary

northern_regions_en <- c(
  "Lamphun", "Lampang", "Tak", "Chiang Rai", "Chiang Mai", 
  "Nan", "Mae Hong Son", "Phrae", "Phayao"
)

