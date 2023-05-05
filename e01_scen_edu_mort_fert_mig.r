
{#setup
  rm(list=ls())
  #checking for required packages and installing if missing
  required.packages <- c("tidyverse","readxl","mdpop","data.table","MortCast")
  new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  #these loads all required packages
  invisible(lapply(required.packages, library, character.only = TRUE))
  
  data("UNlocations",package= "wpp2022" )   
  
  SSP.name = "SSP2"
  iscen = "baseline"
  iscen_text = "edu_mort_fert_dom_0mig"
  iscen_fullname = paste("e01",iscen,SSP.name,iscen_text,sep="_")
  
  baseline = T #baseline T to generate efert and emort
  
  efert = F #estimate differential fertility OR not
  newfert = F#if T then some information here
  
  emort = F # estimate differential mortality OR not
  newmort = F#if T then some information here
  
  emig = F #estimate differential migration OR not
  newmig = F # if T then some information here
  
  edu = F
  newedu = F#if T then some information here
  
  edom = F#  
  newdom = F
}#setup

# r statespace
# 27 states*19 agegroups*2sex*6edu 
# 6,156 boxes * 10 timesteps [2010 to 2060]

#how many transitions?
# stochastic: 1 mort x (5 edu) x 2 internal_mig x 2 international_mig x Fertility
# deterministic: age_transition x no_sex_transition 
# New_members: births [27*7*1*6 = 1134]

source("statespace.r")#empty if baseline is F, then the statespace can already take the baseline value

source("fillstatespace.r")#

#transitions
source("main indproj V1.r")
# See "Report WIC3.Rmd"