#### Adversity X Brain Maturation ####
## Step 01: Calculation of SES during Childhood

rm(list=ls())

# Load Libraries
list.of.packages <- c("dplyr", "stringr", "haven")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# Set Working Directory
library(rstudioapi)
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))
print( getwd() )


#### Step 1. Read In Data ####
# Select Variables
t1_ses <- data.frame(read_sav("./behavioral_data/T1 edu_income_pc_ac.sav")) %>%
  dplyr::select(!c(w1_dad_demo17, twinid, famid, famidun))
t2_ses <- data.frame(read_sav("./behavioral_data/T2_ADI_Age_gender_ethn_income_edu.sav")) %>%
  dplyr::select(idtw, wav1_ADI_PERCENTILES)
t1_ses <- merge(t1_ses, t2_ses, by = "idtw", all.y = TRUE)
colnames(t1_ses) <- c("idtw", "wav1_mom_education", "wav1_famincome", "wav1_dad_education", "wav1_ADI_PERCENTILES")

# Rename ID's
msu_ids <- as.character(t1_ses$idtw)
fam_ids <- str_remove(substring(msu_ids, 3, 6), "^0+")  # use regex to remove leading 0's
twin_ids <- recode(substring(msu_ids, 7, 8), "00" = "t1", "01" = "t2")
umich_id <- paste0(fam_ids, twin_ids, sep = "")
t1_ses$sub <- paste0("sub-", umich_id)
ses_data <- t1_ses %>% dplyr::select(sub, wav1_famincome, wav1_mom_education, wav1_dad_education, wav1_ADI_PERCENTILES)

write.csv(ses_data, "./behavioral_data/ses1.csv", row.names = FALSE)


#### Step 2. Create SES Variables ####
ses_data <- data.frame(read.csv("./behavioral_data/ses1.csv", header = TRUE))
ses_data$wav1_famincome
ses_data$wav1_mom_education
ses_data$wav1_dad_education


#### Step 3. Create SES Composite ####
## 3a. Reverse score income and education so that higher scores represent higher disadvantage
ses_data$wav1_famincome_reverse <- ses_data$wav1_famincome %>% recode(
  '10' = 1, "9" = 2, "8" = 3, "7" = 4, "6" = 5, "5" = 6, "4" = 7, "3" = 8, "2" = 9, "1" = 10)

ses_data$wav1_dadEducation_reverse <- ses_data$wav1_dad_education %>% recode(
  '10' = 1, "9" = 2, "8" = 3, "7" = 4, "6" = 5, "5" = 6, "4" = 7, "3" = 8, "2" = 9, "1" = 10)

ses_data$wav1_momEducation_reverse <- ses_data$wav1_mom_education %>% recode(
  '10' = 1, "9" = 2, "8" = 3, "7" = 4, "6" = 5, "5" = 6, "4" = 7, "3" = 8, "2" = 9, "1" = 10)

## 3b. Calculate z-scores
ses_data$wav1_famincome_reverseZ <- as.numeric(scale(ses_data$wav1_famincome_reverse, center = TRUE, scale = TRUE))
ses_data$wav1_dadEducation_reverseZ <- as.numeric(scale(ses_data$wav1_dadEducation_reverse, center = TRUE, scale = TRUE))
ses_data$wav1_momEducation_reverseZ <- as.numeric(scale(ses_data$wav1_momEducation_reverse, center = TRUE, scale = TRUE))
ses_data$wav1_nbhadiZ <- as.numeric(scale(ses_data$wav1_ADI_PERCENTILES, center = TRUE, scale = TRUE))

## 3c. Calculate z-score sums
sesComposite <- ses_data %>% dplyr::select(sub, wav1_famincome_reverseZ, wav1_dadEducation_reverseZ,
                                           wav1_momEducation_reverseZ, wav1_nbhadiZ)
sesComposite$sesComposite <- rowSums(sesComposite[,2:5], na.rm = TRUE)
ses_data <- merge(ses_data, sesComposite, by = intersect(names(ses_data), names(sesComposite)))

## 3d. Calculate z-score means
ses_data$income_missing <- ifelse(is.na(ses_data$wav1_famincome_reverseZ), 0, 1)
ses_data$momEd_missing <- ifelse(is.na(ses_data$wav1_momEducation_reverseZ), 0, 1)
ses_data$dadEd_missing <- ifelse(is.na(ses_data$wav1_dadEducation_reverseZ), 0, 1)
ses_data$nbhadi_missing <- ifelse(is.na(ses_data$wav1_nbhadiZ), 0, 1)

ses_data$sesComposite_numvars <- ses_data$income_missing + ses_data$momEd_missing + 
                                 ses_data$dadEd_missing + ses_data$nbhadi_missing

ses_data$sesComposite_mean <- ses_data$sesComposite / ses_data$sesComposite_numvars

## 3e. Remove Composites with <50% of Data
table(ses_data$sesComposite_numvars)
ses_data$sesComposite_mean_final <- ifelse(ses_data$sesComposite_numvars < 2, NA, ses_data$sesComposite_mean)
hist(ses_data$sesComposite_mean_final)

ses_data <- ses_data %>% dplyr::select(sub, wav1_famincome_reverseZ, wav1_momEducation_reverseZ,
                                       wav1_dadEducation_reverseZ, wav1_nbhadiZ,
                                       sesComposite, sesComposite_numvars, sesComposite_mean,
                                       sesComposite_mean_final)

## 3f. Save Data
write.csv(ses_data, "./analyses/adversity/envXpace_compositeSES_childhood.csv", row.names = FALSE)
