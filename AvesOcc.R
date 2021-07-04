# Code written by Savannah Hartman 
# This code will compare occurrence records among birds for North and South America

# ctrlL will clear the console window, ctrlshiftR will create collapsible tab

# Introduction, Install Packages and Data ------------------------------------------
install.packages("tidyverse")
# library(tidyverse), package that provides a bunch of tools to help tidy up your messy datasets
install.packages("devtools")
install_github("iobis/robis") # install obis packages 
# You only need to install a package once, but you need to reload it every time you start a new session.


# Data Wrangling OBIS Data---------------------------------------------------------------
library(tidyverse) # packages for data visualization
library(dplyr)
library(readr)
library(devtools)
library(robis)
library(obistools)
library(ggplot2)

# data downloaded from OBIS already within lat long limits of the study
aves<- aves%>% 
  select(scientificName,class,eventDate,decimalLongitude,decimalLatitude,basisOfRecord,date_year,  
         individualCount, identifiedBy, datasetID, datasetName, dataset_id, institutionCode, 
         ownerInstitutionCode, collectionCode, catalogNumber, occurrenceStatus) %>%  
  filter(basisOfRecord == "HumanObservation", date_year >= 1940 & date_year < 2021)        #filtering for only human observations and data collection post 1960

gensp <- aves%>% 
  select(scientificName)            # Create data frame with only scientific names
freq <- as.data.frame(table(gensp)) # Create a data frame with species names and how often they appear 
num_gs <- count(freq)               # Counts the number of genus/genus species found in dataset "freq"

# Finding species present Americas: 1960-2020-------------------------------------------------
# (n = # of species), using a splitstring function

v1 <- gensp                      # vector with scientificNames
v2 <- 1:nrow(gensp)              # num of cells in scientificName and creating vector with the number of cells necessary for running splitstring fxn
species <- data.frame(v1,v2)
colnames(species) <- c("scientificName", "v2")

alpha <- function(species){        # Fxn to filter dataframe to include only rows with a space between two character strings (aka genus and species)
  booleans <- vector()
  i <- 1
  while (i <= nrow(species)){
    tmp <- strsplit(as.character(species$scientificName[i]),' ')[[1]]
    booleans[i] <- (length(tmp) == 2)
    i <- i + 1
    # print(booleans) prints what is true or false, remove when in full use
  } 
  return((booleans)) 
}

species <- alpha(species)          # Gives True/False whether scientifcName contains a space
species <- as.data.frame(species)  # Creating into a vector

# Trying to merge "species" with "div" to remove genus only names
df1 <- c(species,aves)              # Inputting "species" T/F into "div" dataframe
df2 <- as.data.frame(df1) %>% 
  filter(species == "TRUE")        # Making it readable as a dataframe and removing genus only

aves <- df2 %>% 
  select(-species)                 # Removing T/F column "div" dataset

num <- aves %>% 
  select(scientificName) 
freq1 <- as.data.frame(table(num)) 
avesSpecies <- freq1 %>% 
  filter(Freq != 0)                

# Remove likely duplicates 
# If scientificName are equal & decimalLongitude are equal & decimalLatitude are equal & eventDate are equal, remove
# one of the observations
# NOTE: This step will take some time
aves_nd <- invisible(unique(aves) %>%         # "invisible()" suppresses output
  arrange('scientificName'))
rm(aves,num,v1,species,num_gs,gensp,freq,freq1,df1,df2)

# Important!!!
# aves_nd (data without duplicates),avesSpecies (frequency of appearance in 
# dataset of genus species)

#-------Should remove all suspicious land and buffered records from original data ------------
# buffers commented out had a zero value, buffer of 1000 m

bufferA <- aves_nd %>% 
  filter(str_detect(scientificName, "^A")) # looking at all scientificNames starting with "a"
bufferA <- check_onland(bufferA, buffer = 1000)

bufferB <- aves_nd %>% 
  filter(str_detect(scientificName, "^B"))
bufferB <- check_onland(bufferB, buffer = 1000)

bufferC <- aves_nd %>% 
  filter(str_detect(scientificName, "^C"))
bufferC <- check_onland(bufferC, buffer = 1000)

bufferD <- aves_nd %>% 
  filter(str_detect(scientificName, "^D"))
bufferD <- check_onland(bufferD, buffer = 1000)

bufferE <- aves_nd %>% 
  filter(str_detect(scientificName, "^E"))
bufferE <- check_onland(bufferE, buffer = 1000)

bufferF <- aves_nd %>% 
  filter(str_detect(scientificName, "^F"))
bufferF <- check_onland(bufferF, buffer = 1000)

bufferG <- aves_nd %>% 
  filter(str_detect(scientificName, "^G"))
bufferG <- check_onland(bufferG, buffer = 1000)

bufferH <- aves_nd %>% 
  filter(str_detect(scientificName, "^H"))
bufferH <- check_onland(bufferH, buffer = 1000)

bufferI <- aves_nd %>%
  filter(str_detect(scientificName, "^I"))
bufferI <- check_onland(bufferI, buffer = 1000)

bufferJ <- aves_nd %>%
  filter(str_detect(scientificName, "^J"))
bufferJ <- check_onland(bufferJ, buffer = 1000)

bufferK <- aves_nd %>%
  filter(str_detect(scientificName, "^K"))
bufferK <- check_onland(bufferK, buffer = 1000)

bufferL <- aves_nd %>% 
  filter(str_detect(scientificName, "^L"))
bufferL <- check_onland(bufferL, buffer = 1000)

bufferM <- aves_nd %>% 
  filter(str_detect(scientificName, "^M"))
bufferM <- check_onland(bufferM, buffer = 1000)

bufferN <- aves_nd %>%
  filter(str_detect(scientificName, "^N"))
bufferN <- check_onland(bufferN, buffer = 1000)

bufferO <- aves_nd %>% 
  filter(str_detect(scientificName, "^O"))
bufferO <- check_onland(bufferO, buffer = 1000)

bufferP <- aves_nd %>% 
  filter(str_detect(scientificName, "^P"))
bufferP <- check_onland(bufferP, buffer = 1000)

bufferQ <- aves_nd %>%
  filter(str_detect(scientificName, "^Q"))
bufferQ <- check_onland(bufferQ, buffer = 1000)

bufferR <- aves_nd %>% 
  filter(str_detect(scientificName, "^R"))
bufferR <- check_onland(bufferR, buffer = 1000)

bufferS <- aves_nd %>% 
  filter(str_detect(scientificName, "^S"))
bufferS <- check_onland(bufferS, buffer = 1000)

bufferT <- aves_nd %>% 
  filter(str_detect(scientificName, "^T"))
bufferT <- check_onland(bufferT, buffer = 1000)

bufferU <- aves_nd %>% 
  filter(str_detect(scientificName, "^U"))
bufferU <- check_onland(bufferU, buffer = 1000)

bufferV <- aves_nd %>%
  filter(str_detect(scientificName, "^V"))
bufferV <- check_onland(bufferV, buffer = 1000)

bufferW <- aves_nd %>%
  filter(str_detect(scientificName, "^W"))
bufferW <- check_onland(bufferW, buffer = 1000)

bufferX <- aves_nd %>%
  filter(str_detect(scientificName, "^X"))
bufferX <- check_onland(bufferX, buffer = 1000)

bufferY <- aves_nd %>%
  filter(str_detect(scientificName, "^Y"))
bufferY <- check_onland(bufferY, buffer = 1000)

bufferZ <- aves_nd %>%
  filter(str_detect(scientificName, "^Z"))
bufferZ <- check_onland(bufferZ, buffer = 1000)

land_buffer <- rbind(bufferA,bufferB) %>% 
  rbind(bufferC) %>% 
  rbind(bufferD) %>% 
  rbind(bufferE) %>% 
  rbind(bufferF) %>% 
  rbind(bufferG) %>% 
  rbind(bufferH) %>% 
  rbind(bufferI) %>%
  rbind(bufferJ) %>%
  rbind(bufferK) %>%
  rbind(bufferL) %>% 
  rbind(bufferM) %>% 
  rbind(bufferN) %>% 
  rbind(bufferO) %>% 
  rbind(bufferP) %>% 
  rbind(bufferQ) %>% 
  rbind(bufferR) %>% 
  rbind(bufferS) %>% 
  rbind(bufferT) %>% 
  rbind(bufferU) %>%
  rbind(bufferV) %>% 
  rbind(bufferW) %>% 
  rbind(bufferX) %>% 
  rbind(bufferY) %>% 
  rbind(bufferZ)

aves_landbuffer <- land_buffer %>% 
  arrange(scientificName)

rm(bufferA,bufferB, bufferC, bufferD, bufferE, bufferF, bufferG, bufferH, bufferI,
   bufferJ, bufferK, bufferL, bufferM, bufferN, bufferO, bufferP, bufferQ, 
   bufferR, bufferS, bufferT, bufferU, bufferV, bufferW, bufferX, bufferY, bufferZ)

# remove land_buffer from aves_nd (there are additional 300 records removed 
# bc they had duplicate catalogNumbers - which isn't supposed to happen, human error putting info into OBIS)
Aves <- anti_join(aves_nd,aves_landbuffer, by = "catalogNumber") 
Aves <- Aves %>% 
  arrange(scientificName)

rm(aves_nd,aves_landbuffer)

# Aves does not have duplicates or iffy land data
write.csv(Aves, "./AvesOcc.csv")