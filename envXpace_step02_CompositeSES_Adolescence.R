#### Adversity X Brain Maturation ####
## Step 02: Calculation of SES during Adolescence

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
t2_ses <- data.frame(read_sav("./behavioral_data/T2_ADI_Age_gender_ethn_income_edu.sav")) %>%
  dplyr::select(idtw, T2_pc_moincome, T2_pc_education, T2_ac_education, wav2_ADI_PERCENTILES)
colnames(t2_ses) <- c("idtw", "wav2_famincome", "wav2_pc_education", "wav2_ac_education", "wav2_ADI_PERCENTILES")

# Rename ID's
msu_ids <- as.character(t2_ses$idtw)
fam_ids <- str_remove(substring(msu_ids, 3, 6), "^0+")  # use regex to remove leading 0's
twin_ids <- recode(substring(msu_ids, 7, 8), "00" = "t1", "01" = "t2")
umich_id <- paste0(fam_ids, twin_ids, sep = "")
t2_ses$sub <- paste0("sub-", umich_id)
ses_data <- t2_ses %>% dplyr::select(sub, wav2_famincome, wav2_pc_education, wav2_ac_education, wav2_ADI_PERCENTILES)

write.csv(ses_data, "./behavioral_data/ses2.csv", row.names = FALSE)


#### Step 2. Create SES Variables ####
ses_data <- data.frame(read.csv("./behavioral_data/ses2.csv", header = TRUE))
ses_data$wav2_famincome
ses_data$wav2_pc_education
ses_data$wav2_ac_education
ses_data[ses_data == -99] <- NA


#### Step 3. Create SES Composite ####
## 3a. Reverse score income and education so that higher scores represent higher disadvantage
table(ses_data$wav2_famincome)
ses_data$wav2_famincome_reverse <- ses_data$wav2_famincome %>% recode(
  '12' = 1, "11" = 2, "10" = 3, "9" = 4, "8" = 5, "7" = 6,
  "6" = 7, "5" = 8, "4" = 9, "3" = 10, "2" = 11, "1" = 12, "0" = 13)
cor.test(ses_data$wav2_famincome, ses_data$wav2_famincome_reverse)

table(ses_data$wav2_pc_education)
ses_data$wav2_pc_education_reverse <- ses_data$wav2_pc_education %>% recode(
  '8' = 1, "7" = 2, "6" = 3, "5" = 4, "4" = 5, "3" = 6, "2" = 7, "1" = 8)
cor.test(ses_data$wav2_pc_education, ses_data$wav2_pc_education_reverse)

table(ses_data$wav2_ac_education)
ses_data$wav2_ac_education_reverse <- ses_data$wav2_ac_education %>% recode(
  '8' = 1, "7" = 2, "6" = 3, "5" = 4, "4" = 5, "3" = 6, "2" = 7, "1" = 8)
cor.test(ses_data$wav2_ac_education, ses_data$wav2_ac_education_reverse)

## 3b. Calculate z-scores
ses_data$wav2_famincome_reverseZ <- as.numeric(scale(ses_data$wav2_famincome_reverse, center = TRUE, scale = TRUE))
ses_data$wav2_pc_education_reverseZ <- as.numeric(scale(ses_data$wav2_pc_education_reverse, center = TRUE, scale = TRUE))
ses_data$wav2_ac_education_reverseZ <- as.numeric(scale(ses_data$wav2_ac_education_reverse, center = TRUE, scale = TRUE))
ses_data$wav2_nbhadiZ <- as.numeric(scale(ses_data$wav2_ADI_PERCENTILES, center = TRUE, scale = TRUE))

## 3c. Calculate z-score sums
sesComposite <- ses_data %>% dplyr::select(sub, wav2_famincome_reverseZ, wav2_pc_education_reverseZ,
                                           wav2_ac_education_reverseZ, wav2_nbhadiZ)
sesComposite$sesComposite <- rowSums(sesComposite[,2:5], na.rm = TRUE)
ses_data <- merge(ses_data, sesComposite, by = intersect(names(ses_data), names(sesComposite)))

## 3d. Calculate z-score means
ses_data$income_missing <- ifelse(is.na(ses_data$wav2_famincome_reverseZ), 0, 1)
ses_data$pcEd_missing <- ifelse(is.na(ses_data$wav2_pc_education_reverseZ), 0, 1)
ses_data$acEd_missing <- ifelse(is.na(ses_data$wav2_ac_education_reverseZ), 0, 1)
ses_data$nbhadi_missing <- ifelse(is.na(ses_data$wav2_nbhadiZ), 0, 1)

ses_data$sesComposite_numvars <- ses_data$income_missing + ses_data$pcEd_missing + 
                                 ses_data$acEd_missing + ses_data$nbhadi_missing

ses_data$sesComposite_mean <- ses_data$sesComposite / ses_data$sesComposite_numvars

## 3e. Remove Composites with <50% of Data
table(ses_data$sesComposite_numvars)
ses_data$sesComposite_mean_final <- ifelse(ses_data$sesComposite_numvars < 2, NA, ses_data$sesComposite_mean)
hist(ses_data$sesComposite_mean_final)

ses_data <- ses_data %>% dplyr::select(sub, wav2_famincome_reverseZ, wav2_pc_education_reverseZ,
                                       wav2_ac_education_reverseZ, wav2_nbhadiZ,
                                       sesComposite, sesComposite_numvars, sesComposite_mean,
                                       sesComposite_mean_final)

## 3f. Save Data
write.csv(ses_data, "./analyses/adversity/envXpace_compositeSES_adolescence.csv", row.names = FALSE)
