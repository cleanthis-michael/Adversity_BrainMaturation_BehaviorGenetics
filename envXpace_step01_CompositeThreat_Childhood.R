#### Adversity X Brain Maturation ####
## Step 01: Calculation of Threat Exposure during Childhood

rm(list=ls())

# Load Libraries
list.of.packages <- c("dplyr", "stringr", "haven", "psych")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# Set Working Directory
library(rstudioapi)
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))
print( getwd() )


#### Step 1. Read In Phenotypic Data ####
pheno_data <- data.frame(read_sav("./behavioral_data/projBrainAge_phenoData_mtwins.sav"))
pheno_data_df <- data.frame(read.csv("./behavioral_data/projBrainAge_phenoData_mtwins.csv",
                                     sep = ",", header = TRUE))

#### Step 2. Score Children's Perception of Interparental Conflict (CPIC) Scale ####
## 2a. Identify Items for Reverse Scoring
pheno_data$cpic_01  # higher scores = more conflict
pheno_data$cpic_02  # higher scores = more conflict
pheno_data$cpic_03  # higher scores = less conflict, must rescore
pheno_data$cpic_04  # higher scores = less conflict, must rescore
pheno_data$cpic_05  # higher scores = more conflict
pheno_data$cpic_06  # higher scores = less conflict, must rescore
pheno_data$cpic_07  # higher scores = less conflict, must rescore
pheno_data$cpic_08  # higher scores = more conflict
pheno_data$cpic_09  # higher scores = less conflict, must rescore
pheno_data$cpic_10  # higher scores = less conflict, must rescore

pheno_data$cpic_11  # higher scores = less conflict, must rescore
pheno_data$cpic_12  # higher scores = more conflict
pheno_data$cpic_13  # higher scores = less conflict, must rescore
pheno_data$cpic_14  # higher scores = less conflict, must rescore
pheno_data$cpic_15  # higher scores = less conflict, must rescore
pheno_data$cpic_16  # higher scores = less conflict, must rescore
pheno_data$cpic_17  # higher scores = less conflict, must rescore
pheno_data$cpic_18  # higher scores = more conflict
pheno_data$cpic_19  # higher scores = less conflict, must rescore
pheno_data$cpic_20  # higher scores = more conflict

pheno_data$cpic_21  # higher scores = less conflict, must rescore
pheno_data$cpic_22  # higher scores = more conflict
pheno_data$cpic_23  # higher scores = less conflict, must rescore
pheno_data$cpic_24  # higher scores = less conflict, must rescore
pheno_data$cpic_25  # higher scores = less conflict, must rescore
pheno_data$cpic_26  # higher scores = more conflict
pheno_data$cpic_27  # higher scores = more conflict
pheno_data$cpic_28  # higher scores = less conflict, must rescore
pheno_data$cpic_29  # higher scores = less conflict, must rescore
pheno_data$cpic_30  # higher scores = less conflict, must rescore

pheno_data$cpic_31  # higher scores = less conflict, must rescore
pheno_data$cpic_32  # higher scores = less conflict, must rescore
pheno_data$cpic_33  # higher scores = less conflict, must rescore
pheno_data$cpic_34  # higher scores = less conflict, must rescore
pheno_data$cpic_35  # higher scores = more conflict
pheno_data$cpic_36  # higher scores = less conflict, must rescore
pheno_data$cpic_37  # higher scores = less conflict, must rescore
pheno_data$cpic_38  # higher scores = more conflict
pheno_data$cpic_39  # higher scores = less conflict, must rescore
pheno_data$cpic_40  # higher scores = less conflict, must rescore

pheno_data$cpic_41  # higher scores = less conflict, must rescore
pheno_data$cpic_42  # higher scores = less conflict, must rescore
pheno_data$cpic_43  # higher scores = less conflict, must rescore
pheno_data$cpic_44  # higher scores = less conflict, must rescore
pheno_data$cpic_45  # higher scores = less conflict, must rescore
pheno_data$cpic_46  # higher scores = less conflict, must rescore
pheno_data$cpic_47  # higher scores = more conflict
pheno_data$cpic_48  # higher scores = less conflict, must rescore

## 2b. Reverse Score Items
pheno_data_df$cpic_03R <- pheno_data_df$cpic_03 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_04R <- pheno_data_df$cpic_04 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_06R <- pheno_data_df$cpic_06 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_07R <- pheno_data_df$cpic_07 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_09R <- pheno_data_df$cpic_09 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_10R <- pheno_data_df$cpic_10 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)

pheno_data_df$cpic_11R <- pheno_data_df$cpic_11 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_13R <- pheno_data_df$cpic_13 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_14R <- pheno_data_df$cpic_14 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_15R <- pheno_data_df$cpic_15 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_16R <- pheno_data_df$cpic_16 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_17R <- pheno_data_df$cpic_17 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_19R <- pheno_data_df$cpic_19 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)

pheno_data_df$cpic_21R <- pheno_data_df$cpic_21 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_23R <- pheno_data_df$cpic_23 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_24R <- pheno_data_df$cpic_24 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_25R <- pheno_data_df$cpic_25 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)

pheno_data_df$cpic_28 <- ifelse(pheno_data_df$cpic_28 == 99899, NA, pheno_data_df$cpic_28)
pheno_data_df$cpic_28R <- pheno_data_df$cpic_28 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_29R <- pheno_data_df$cpic_29 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_30R <- pheno_data_df$cpic_30 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)

pheno_data_df$cpic_31R <- pheno_data_df$cpic_31 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_32R <- pheno_data_df$cpic_32 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_33R <- pheno_data_df$cpic_33 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_34R <- pheno_data_df$cpic_34 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_36R <- pheno_data_df$cpic_36 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_37R <- pheno_data_df$cpic_37 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_39R <- pheno_data_df$cpic_39 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_40R <- pheno_data_df$cpic_40 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)

pheno_data_df$cpic_41R <- pheno_data_df$cpic_41 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_42R <- pheno_data_df$cpic_42 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_43R <- pheno_data_df$cpic_43 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)

pheno_data_df$cpic_44 <- ifelse(pheno_data_df$cpic_44 == 4, NA, pheno_data_df$cpic_44)
pheno_data_df$cpic_44R <- pheno_data_df$cpic_44 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_45R <- pheno_data_df$cpic_45 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_46R <- pheno_data_df$cpic_46 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
pheno_data_df$cpic_48R <- pheno_data_df$cpic_48 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)

## 2c. Calculate CPIC Mean Z-Score
pheno_data_df$cpic_total <- rowSums(cbind(pheno_data_df$cpic_01, pheno_data_df$cpic_02, 
                                          pheno_data_df$cpic_03R, pheno_data_df$cpic_04R, 
                                          pheno_data_df$cpic_05, pheno_data_df$cpic_06R, 
                                          pheno_data_df$cpic_07R, pheno_data_df$cpic_08,
                                          pheno_data_df$cpic_09R, pheno_data_df$cpic_10R, 
                                          pheno_data_df$cpic_11R, pheno_data_df$cpic_12, 
                                          pheno_data_df$cpic_13R, pheno_data_df$cpic_14R, 
                                          pheno_data_df$cpic_15R, pheno_data_df$cpic_16R,
                                          pheno_data_df$cpic_17R, pheno_data_df$cpic_18, 
                                          pheno_data_df$cpic_19R, pheno_data_df$cpic_20, 
                                          pheno_data_df$cpic_21R, pheno_data_df$cpic_22, 
                                          pheno_data_df$cpic_23R, pheno_data_df$cpic_24R,
                                          pheno_data_df$cpic_25R, pheno_data_df$cpic_26, 
                                          pheno_data_df$cpic_27, pheno_data_df$cpic_28R, 
                                          pheno_data_df$cpic_29R, pheno_data_df$cpic_30R, 
                                          pheno_data_df$cpic_31R, pheno_data_df$cpic_32R,
                                          pheno_data_df$cpic_33R, pheno_data_df$cpic_34R, 
                                          pheno_data_df$cpic_35, pheno_data_df$cpic_36R, 
                                          pheno_data_df$cpic_37R, pheno_data_df$cpic_38, 
                                          pheno_data_df$cpic_39R, pheno_data_df$cpic_40R,
                                          pheno_data_df$cpic_41R, pheno_data_df$cpic_42R, 
                                          pheno_data_df$cpic_43R, pheno_data_df$cpic_44R, 
                                          pheno_data_df$cpic_45R, pheno_data_df$cpic_46R, 
                                          pheno_data_df$cpic_47, pheno_data_df$cpic_48R), na.rm = TRUE)

pheno_data_df$cpic_numvars <- rowSums(!is.na(cbind(pheno_data_df$cpic_01, pheno_data_df$cpic_02, 
                                                   pheno_data_df$cpic_03R, pheno_data_df$cpic_04R,
                                                   pheno_data_df$cpic_05, pheno_data_df$cpic_06R,
                                                   pheno_data_df$cpic_07R, pheno_data_df$cpic_08,
                                                   pheno_data_df$cpic_09R, pheno_data_df$cpic_10R, 
                                                   pheno_data_df$cpic_11R, pheno_data_df$cpic_12,
                                                   pheno_data_df$cpic_13R, pheno_data_df$cpic_14R, 
                                                   pheno_data_df$cpic_15R, pheno_data_df$cpic_16R,
                                                   pheno_data_df$cpic_17R, pheno_data_df$cpic_18, 
                                                   pheno_data_df$cpic_19R, pheno_data_df$cpic_20, 
                                                   pheno_data_df$cpic_21R, pheno_data_df$cpic_22, 
                                                   pheno_data_df$cpic_23R, pheno_data_df$cpic_24R,
                                                   pheno_data_df$cpic_25R, pheno_data_df$cpic_26, 
                                                   pheno_data_df$cpic_27, pheno_data_df$cpic_28R, 
                                                   pheno_data_df$cpic_29R, pheno_data_df$cpic_30R, 
                                                   pheno_data_df$cpic_31R, pheno_data_df$cpic_32R,
                                                   pheno_data_df$cpic_33R, pheno_data_df$cpic_34R, 
                                                   pheno_data_df$cpic_35, pheno_data_df$cpic_36R, 
                                                   pheno_data_df$cpic_37R, pheno_data_df$cpic_38, 
                                                   pheno_data_df$cpic_39R, pheno_data_df$cpic_40R,
                                                   pheno_data_df$cpic_41R, pheno_data_df$cpic_42R, 
                                                   pheno_data_df$cpic_43R, pheno_data_df$cpic_44R, 
                                                   pheno_data_df$cpic_45R, pheno_data_df$cpic_46R, 
                                                   pheno_data_df$cpic_47, pheno_data_df$cpic_48R)))

pheno_data_df$cpic_mean <- pheno_data_df$cpic_total / pheno_data_df$cpic_numvars
pheno_data_df$cpic_meanZ <- as.numeric(scale(pheno_data_df$cpic_mean, center = TRUE, scale = TRUE))
pheno_data_df$cpic_meanZ <- ifelse(pheno_data_df$cpic_numvars < 24, NA, pheno_data_df$cpic_meanZ)

sum(!is.na(pheno_data_df$cpic_meanZ))

## 2d. Internal Consistency
cpic_crA <- pheno_data_df %>% dplyr::filter(!is.na(pheno_data_df$cpic_meanZ))
cpic_crA <- cpic_crA %>% dplyr::select(all_of(c("cpic_01", "cpic_02",
                                                "cpic_03R", "cpic_04R",
                                                "cpic_05", "cpic_06R", 
                                                "cpic_07R", "cpic_08",
                                                "cpic_09R", "cpic_10R", 
                                                "cpic_11R", "cpic_12", 
                                                "cpic_13R", "cpic_14R", 
                                                "cpic_15R", "cpic_16R",
                                                "cpic_17R", "cpic_18", 
                                                "cpic_19R", "cpic_20", 
                                                "cpic_21R", "cpic_22", 
                                                "cpic_23R", "cpic_24R",
                                                "cpic_25R", "cpic_26", 
                                                "cpic_27", "cpic_28R", 
                                                "cpic_29R", "cpic_30R", 
                                                "cpic_31R", "cpic_32R",
                                                "cpic_33R", "cpic_34R", 
                                                "cpic_35", "cpic_36R", 
                                                "cpic_37R", "cpic_38", 
                                                "cpic_39R", "cpic_40R",
                                                "cpic_41R", "cpic_42R", 
                                                "cpic_43R", "cpic_44R", 
                                                "cpic_45R", "cpic_46R", 
                                                "cpic_47", "cpic_48R")))

alpha(cpic_crA)


#### Step 3. Score Parental Environment Questionnaire (PEQ) - Mother Report ####
## 3a. Identify Conflict Items
pheno_data$peq01mr
pheno_data$peq02mr
pheno_data$peq03mr  # conflict  (item 4: Often criticize son)
pheno_data$peq04mr
pheno_data$peq05mr  # conflict  (item 10: Often interrupt son)
pheno_data$peq06mr
pheno_data$peq07mr
pheno_data$peq08mr
pheno_data$peq09mr
pheno_data$peq10mr  # conflict  (item 7: Often irritate son)
pheno_data$peq11mr
pheno_data$peq12mr
pheno_data$peq13mr  # conflict  (item 2: Often have misunderstandings with son)
pheno_data$peq14mr
pheno_data$peq15mr
pheno_data$peq16mr
pheno_data$peq17mr
pheno_data$peq18mr
pheno_data$peq19mr  # conflict  (item 11: My child treats others with more respect than he/she treats me)
pheno_data$peq20mr  # conflict  (item 6: Often hurt son's feelings)
pheno_data$peq21mr
pheno_data$peq22mr
pheno_data$peq23mr
pheno_data$peq24mr
pheno_data$peq25mr
pheno_data$peq26mr
pheno_data$peq27mr  # conflict  (item 12: I do not trust my child to make his/her own decisions)
pheno_data$peq28mr  # conflict  (item 3: My child and I often get into arguments)
pheno_data$peq29mr
pheno_data$peq30mr  # conflict  (item 5: Son often angers or annoys me)
pheno_data$peq31mr  # conflict  (item 1: Often lose temper with son)
pheno_data$peq32mr  # conflict  (item 8: Sometimes hit son in anger)
pheno_data$peq33mr
pheno_data$peq34mr
pheno_data$peq35mr
pheno_data$peq36mr  # conflict  (item 9: Son has been really scared of me)
pheno_data$peq37mr
pheno_data$peq38mr
pheno_data$peq39mr
pheno_data$peq40mr
pheno_data$peq41mr
pheno_data$peq42mr
pheno_data$peq43mr
pheno_data$peq44mr
pheno_data$peq45mr
pheno_data$peq46mr
pheno_data$peq47mr
pheno_data$peq48mr
pheno_data$peq49mr
pheno_data$peq50mr

## 3b. Reverse Score Items
pheno_data_df$peq03mrR <- pheno_data_df$peq03mr %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
pheno_data_df$peq05mrR <- pheno_data_df$peq05mr %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
pheno_data_df$peq10mrR <- pheno_data_df$peq10mr %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
pheno_data_df$peq13mrR <- pheno_data_df$peq13mr %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
pheno_data_df$peq19mrR <- pheno_data_df$peq19mr %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
pheno_data_df$peq20mrR <- pheno_data_df$peq20mr %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
pheno_data_df$peq27mrR <- pheno_data_df$peq27mr %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
pheno_data_df$peq28mrR <- pheno_data_df$peq28mr %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
pheno_data_df$peq30mrR <- pheno_data_df$peq30mr %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
pheno_data_df$peq31mrR <- pheno_data_df$peq31mr %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
pheno_data_df$peq32mrR <- pheno_data_df$peq32mr %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
pheno_data_df$peq36mrR <- pheno_data_df$peq36mr %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)

## 3c. Extract Data
pheno_data_df$peqConflict_MomReport_total <- rowSums(cbind(pheno_data_df$peq03mrR, pheno_data_df$peq05mrR, 
                                                           pheno_data_df$peq10mrR, pheno_data_df$peq13mrR, 
                                                           pheno_data_df$peq19mrR, pheno_data_df$peq20mrR, 
                                                           pheno_data_df$peq27mrR, pheno_data_df$peq28mrR,
                                                           pheno_data_df$peq30mrR, pheno_data_df$peq31mrR, 
                                                           pheno_data_df$peq32mrR, pheno_data_df$peq36mrR), na.rm = TRUE)

pheno_data_df$peqConflict_MomReport_numvars <- rowSums(!is.na(cbind(pheno_data_df$peq03mrR, pheno_data_df$peq05mrR, 
                                                                    pheno_data_df$peq10mrR, pheno_data_df$peq13mrR, 
                                                                    pheno_data_df$peq19mrR, pheno_data_df$peq20mrR, 
                                                                    pheno_data_df$peq27mrR, pheno_data_df$peq28mrR,
                                                                    pheno_data_df$peq30mrR, pheno_data_df$peq31mrR, 
                                                                    pheno_data_df$peq32mrR, pheno_data_df$peq36mrR)))

pheno_data_df$peqConflict_MomReport_mean <- pheno_data_df$peqConflict_MomReport_total / pheno_data_df$peqConflict_MomReport_numvars
pheno_data_df$peqConflict_MomReport_meanZ <- as.numeric(scale(pheno_data_df$peqConflict_MomReport_mean, center = TRUE, scale = TRUE))
pheno_data_df$peqConflict_MomReport_meanZ <- ifelse(pheno_data_df$peqConflict_MomReport_numvars < 6, NA, pheno_data_df$peqConflict_MomReport_meanZ)


sum(!is.na(pheno_data_df$peqConflict_MomReport_meanZ))

## 3d. Internal Consistency
peqConflict_MomReport_crA <- pheno_data_df %>% dplyr::filter(!is.na(pheno_data_df$peqConflict_MomReport_meanZ))
peqConflict_MomReport_crA <- peqConflict_MomReport_crA %>% dplyr::select(all_of(c("peq03mrR", "peq05mrR", 
                                                                                  "peq10mrR", "peq13mrR", 
                                                                                  "peq19mrR", "peq20mrR", 
                                                                                  "peq27mrR", "peq28mrR",
                                                                                  "peq30mrR", "peq31mrR", 
                                                                                  "peq32mrR", "peq36mrR")))

alpha(peqConflict_MomReport_crA)


#### Step 4. Score Parental Environment Questionnaire (PEQ) - Twin Report on Mom ####
## 4a. Identify Conflict Items
pheno_data$peq_m03  # conflict  (item 4: Often criticize son)
pheno_data$peq_m05  # conflict  (item 10: Often interrupt son)
pheno_data$peq_m10  # conflict  (item 7: Often irritate son)
pheno_data$peq_m13  # conflict  (item 2: Often have misunderstandings with son)
pheno_data$peq_m19  # conflict  (item 11: My child treats others with more respect than he/she treats me)
pheno_data$peq_m20  # conflict  (item 6: Often hurt son's feelings)
pheno_data$peq_m27  # conflict  (item 12: I do not trust my child to make his/her own decisions)
pheno_data$peq_m28  # conflict  (item 3: My child and I often get into arguments)
pheno_data$peq_m30  # conflict  (item 5: Son often angers or annoys me)
pheno_data$peq_m31  # conflict  (item 1: Often lose temper with son)
pheno_data$peq_m32  # conflict  (item 8: Sometimes hit son in anger)
pheno_data$peq_m36  # conflict  (item 9: Son has been really scared of me)

## 4b. Reverse Score Items
pheno_data_df$peq_m03R <- pheno_data_df$peq_m03 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
pheno_data_df$peq_m05R <- pheno_data_df$peq_m05 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
pheno_data_df$peq_m10R <- pheno_data_df$peq_m10 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
pheno_data_df$peq_m13R <- pheno_data_df$peq_m13 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
pheno_data_df$peq_m19R <- pheno_data_df$peq_m19 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
pheno_data_df$peq_m20R <- pheno_data_df$peq_m20 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
pheno_data_df$peq_m27R <- pheno_data_df$peq_m27 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
pheno_data_df$peq_m28R <- pheno_data_df$peq_m28 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
pheno_data_df$peq_m30R <- pheno_data_df$peq_m30 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
pheno_data_df$peq_m31R <- pheno_data_df$peq_m31 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
pheno_data_df$peq_m32R <- pheno_data_df$peq_m32 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
pheno_data_df$peq_m36R <- pheno_data_df$peq_m36 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)

## 4c. Extract Data
pheno_data_df$peqConflict_TwinOnMom_total <- rowSums(cbind(pheno_data_df$peq_m03R, pheno_data_df$peq_m05R, 
                                                           pheno_data_df$peq_m10R, pheno_data_df$peq_m13R, 
                                                           pheno_data_df$peq_m19R, pheno_data_df$peq_m20R, 
                                                           pheno_data_df$peq_m27R, pheno_data_df$peq_m28R,
                                                           pheno_data_df$peq_m30R, pheno_data_df$peq_m31R, 
                                                           pheno_data_df$peq_m32R, pheno_data_df$peq_m36R), na.rm = TRUE)

pheno_data_df$peqConflict_TwinOnMom_numvars <- rowSums(!is.na(cbind(pheno_data_df$peq_m03R, pheno_data_df$peq_m05R, 
                                                                    pheno_data_df$peq_m10R, pheno_data_df$peq_m13R, 
                                                                    pheno_data_df$peq_m19R, pheno_data_df$peq_m20R, 
                                                                    pheno_data_df$peq_m27R, pheno_data_df$peq_m28R,
                                                                    pheno_data_df$peq_m30R, pheno_data_df$peq_m31R, 
                                                                    pheno_data_df$peq_m32R, pheno_data_df$peq_m36R)))

pheno_data_df$peqConflict_TwinOnMom_mean <- pheno_data_df$peqConflict_TwinOnMom_total / pheno_data_df$peqConflict_TwinOnMom_numvars
pheno_data_df$peqConflict_TwinOnMom_meanZ <- as.numeric(scale(pheno_data_df$peqConflict_TwinOnMom_mean, center = TRUE, scale = TRUE))
pheno_data_df$peqConflict_TwinOnMom_meanZ <- ifelse(pheno_data_df$peqConflict_TwinOnMom_numvars < 6, NA, pheno_data_df$peqConflict_TwinOnMom_meanZ)


sum(!is.na(pheno_data_df$peqConflict_TwinOnMom_meanZ))

## 4d. Internal Consistency
peqConflict_TwinOnMom_crA <- pheno_data_df %>% dplyr::filter(!is.na(pheno_data_df$peqConflict_TwinOnMom_meanZ))
peqConflict_TwinOnMom_crA <- peqConflict_TwinOnMom_crA %>% dplyr::select(all_of(c("peq_m03R", "peq_m05R", 
                                                                                  "peq_m10R", "peq_m13R", 
                                                                                  "peq_m19R", "peq_m20R", 
                                                                                  "peq_m27R", "peq_m28R",
                                                                                  "peq_m30R", "peq_m31R", 
                                                                                  "peq_m32R", "peq_m36R")))

alpha(peqConflict_TwinOnMom_crA)


#### Step 5. Score Exposure to Community Violence (Kid SAVE) - Twin Self-Report ####
## 5a. Explore Items
pheno_data$twin_cvc01a      # indirect violence
pheno_data$twin_cvchunt01   # indirect violence
pheno_data$twin_cvcrec01    # indirect violence

pheno_data$twin_cvc02a      # indirect violence
pheno_data$twin_cvc03a      # indirect violence
pheno_data$twin_cvc04a      # physical/verbal abuse
pheno_data$twin_cvc05a      # traumatic violence (YES)
pheno_data$twin_cvc06a      # physical/verbal abuse
pheno_data$twin_cvc07a      # traumatic violence (YES)
pheno_data$twin_cvc08a      # indirect violence
pheno_data$twin_cvc09a      # traumatic violence (YES)

pheno_data$twin_cvc10a      # indirect violence
pheno_data$twin_cvc11a      # indirect violence
pheno_data$twin_cvc12a      # indirect violence
pheno_data$twin_cvc13a      # indirect violence
pheno_data$twin_cvc14a      # indirect violence
pheno_data$twin_cvc15a      # traumatic violence (YES)
pheno_data$twin_cvc16a      # indirect violence
pheno_data$twin_cvc17a      # traumatic violence (YES)
pheno_data$twin_cvc18a      # physical/verbal abuse
pheno_data$twin_cvc19a      # indirect violence

pheno_data$twin_cvc20a      # traumatic violence (YES)
pheno_data$twin_cvc21a      # indirect violence
pheno_data$twin_cvc22a      # traumatic violence (YES)
pheno_data$twin_cvc23a      # indirect violence
pheno_data$twin_cvc24a      # traumatic violence (YES)
pheno_data$twin_cvc25a      # traumatic violence (YES)
pheno_data$twin_cvc26a      # traumatic violence (YES)
pheno_data$twin_cvc27a      # indirect violence

## Check Frequency of Endorsement
table(pheno_data$twin_cvc05a)
table(pheno_data$twin_cvc07a)
table(pheno_data$twin_cvc09a)
table(pheno_data$twin_cvc15a)
table(pheno_data$twin_cvc17a)
table(pheno_data$twin_cvc20a)
table(pheno_data$twin_cvc22a)
table(pheno_data$twin_cvc24a)
table(pheno_data$twin_cvc25a)
table(pheno_data$twin_cvc26a)

table(pheno_data$twin_cvc02a)      # indirect violence
table(pheno_data$twin_cvc03a)      # indirect violence
table(pheno_data$twin_cvc04a)      # physical/verbal abuse
table(pheno_data$twin_cvc05a)      # traumatic violence (YES)
table(pheno_data$twin_cvc06a)      # physical/verbal abuse
table(pheno_data$twin_cvc07a)      # traumatic violence (YES)
table(pheno_data$twin_cvc08a)      # indirect violence
table(pheno_data$twin_cvc09a)      # traumatic violence (YES)

table(pheno_data$twin_cvc10a)      # indirect violence
table(pheno_data$twin_cvc11a)      # indirect violence
table(pheno_data$twin_cvc12a)      # indirect violence
table(pheno_data$twin_cvc13a)      # indirect violence
table(pheno_data$twin_cvc14a)      # indirect violence
table(pheno_data$twin_cvc15a)      # traumatic violence (YES)
table(pheno_data$twin_cvc16a)      # indirect violence
table(pheno_data$twin_cvc17a)      # traumatic violence (YES)
table(pheno_data$twin_cvc18a)      # physical/verbal abuse
table(pheno_data$twin_cvc19a)      # indirect violence

table(pheno_data$twin_cvc20a)      # traumatic violence (YES)
table(pheno_data$twin_cvc21a)      # indirect violence
table(pheno_data$twin_cvc22a)      # traumatic violence (YES)
table(pheno_data$twin_cvc23a)      # indirect violence
table(pheno_data$twin_cvc24a)      # traumatic violence (YES)
table(pheno_data$twin_cvc25a)      # traumatic violence (YES)
table(pheno_data$twin_cvc26a)      # traumatic violence (YES)
table(pheno_data$twin_cvc27a)      # indirect violence


## 5b. Extract Data
pheno_data_df$cvc_total <- rowSums(cbind(pheno_data_df$twin_cvc04a,
                                         pheno_data_df$twin_cvc06a, pheno_data_df$twin_cvc11a,
                                         pheno_data_df$twin_cvc19a,
                                         pheno_data_df$twin_cvc27a), na.rm = TRUE)

pheno_data_df$cvc_numvars <- rowSums(!is.na(cbind(pheno_data_df$twin_cvc04a,
                                                  pheno_data_df$twin_cvc06a, pheno_data_df$twin_cvc11a,
                                                  pheno_data_df$twin_cvc19a,
                                                  pheno_data_df$twin_cvc27a)))

pheno_data_df$cvc_mean <- pheno_data_df$cvc_total / pheno_data_df$cvc_numvars
pheno_data_df$cvc_meanZ <- as.numeric(scale(pheno_data_df$cvc_mean, center = TRUE, scale = TRUE))
pheno_data_df$cvc_meanZ <- ifelse(pheno_data_df$cvc_numvars < 3, NA, pheno_data_df$cvc_meanZ)

sum(!is.na(pheno_data_df$cvc_meanZ))

## 5c. Internal Consistency
cvc_crA <- pheno_data_df %>% dplyr::filter(!is.na(pheno_data_df$cvc_meanZ))
cvc_crA <- cvc_crA %>% dplyr::select(all_of(c("twin_cvc04a", 
                                              "twin_cvc06a", "twin_cvc11a",
                                              "twin_cvc19a", 
                                              "twin_cvc27a")))

alpha(cvc_crA)


#### Step 6. Calculate Threat Composite ####
## 6a. Calculate an Average Harsh Parenting Score
pheno_data_df$peqConflict_sum <- rowSums(cbind(pheno_data_df$peqConflict_MomReport_meanZ,
                                               pheno_data_df$peqConflict_TwinOnMom_meanZ), na.rm = TRUE)

pheno_data_df$peqConflict_numvars <- rowSums(!is.na(cbind(pheno_data_df$peqConflict_MomReport_meanZ,
                                                          pheno_data_df$peqConflict_TwinOnMom_meanZ)))

pheno_data_df$peqConflict_mean <- pheno_data_df$peqConflict_sum / pheno_data_df$peqConflict_numvars
pheno_data_df$peqConflict_final <- ifelse(pheno_data_df$peqConflict_numvars < 0, NA, pheno_data_df$peqConflict_mean)


## 6b. Calculate a Threat Composite
pheno_data_df$threatComposite <- rowSums(cbind(pheno_data_df$cpic_meanZ,
                                               pheno_data_df$peqConflict_final,
                                               pheno_data_df$cvc_meanZ), na.rm = TRUE)

pheno_data_df$threat_numvars <- rowSums(!is.na(cbind(pheno_data_df$cpic_meanZ,
                                                     pheno_data_df$peqConflict_final,
                                                     pheno_data_df$cvc_meanZ)))

pheno_data_df$threatComposite_mean <- pheno_data_df$threatComposite / pheno_data_df$threat_numvars
pheno_data_df$threatComposite_final <- ifelse(pheno_data_df$threat_numvars < 1, NA, pheno_data_df$threatComposite_mean)


## 6c. Save Data
threat_data <- pheno_data_df %>% dplyr::select(idtw, cpic_meanZ, peqConflict_MomReport_meanZ,
                                               peqConflict_TwinOnMom_meanZ, cvc_meanZ,
                                               threatComposite, threat_numvars, threatComposite_mean,
                                               threatComposite_final)

msu_ids <- as.character(threat_data$idtw)
fam_ids <- str_remove(substring(msu_ids, 3, 6), "^0+")  # use regex to remove leading 0's
twin_ids <- recode(substring(msu_ids, 7, 8), "00" = "t1", "01" = "t2")
umich_id <- paste0(fam_ids, twin_ids, sep = "")
threat_data$sub <- paste0("sub-", umich_id)

threat_data$idtw <- NULL
threat_data <- threat_data %>% dplyr::select(sub, cpic_meanZ:threatComposite_final)

write.csv(threat_data, "./analyses/adversity/envXpace_compositeThreat_childhood.csv", row.names = FALSE)
