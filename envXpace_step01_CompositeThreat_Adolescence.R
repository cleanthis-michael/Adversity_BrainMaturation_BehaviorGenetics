#### Adversity X Brain Maturation ####
## Step 01: Calculation of Threat Exposure during Adolescence

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


#### Step 1. Score Children's Perception of Interparental Conflict (CPIC) Scale ####
cpic_data <- data.frame(read_sav("./behavioral_data/T2 CPIC raw and scored.sav"))
cpic_data_df <- data.frame(read.csv("./behavioral_data/cpic.csv",
                                       sep = ",", header = TRUE))

cpic_data_df$w2_tw_cpic_3R <- cpic_data_df$w2_tw_cpic_3 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_4R <- cpic_data_df$w2_tw_cpic_4 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_6R <- cpic_data_df$w2_tw_cpic_6 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_7R <- cpic_data_df$w2_tw_cpic_7 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_9R <- cpic_data_df$w2_tw_cpic_9 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_10R <- cpic_data_df$w2_tw_cpic_10 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)

cpic_data_df$w2_tw_cpic_11R <- cpic_data_df$w2_tw_cpic_11 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_13R <- cpic_data_df$w2_tw_cpic_13 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_14R <- cpic_data_df$w2_tw_cpic_14 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_15R <- cpic_data_df$w2_tw_cpic_15 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_16R <- cpic_data_df$w2_tw_cpic_16 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_17R <- cpic_data_df$w2_tw_cpic_17 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_19R <- cpic_data_df$w2_tw_cpic_19 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)

cpic_data_df$w2_tw_cpic_21R <- cpic_data_df$w2_tw_cpic_21 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_23R <- cpic_data_df$w2_tw_cpic_23 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_24R <- cpic_data_df$w2_tw_cpic_24 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_25R <- cpic_data_df$w2_tw_cpic_25 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)

cpic_data_df$w2_tw_cpic_28R <- cpic_data_df$w2_tw_cpic_28 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_29R <- cpic_data_df$w2_tw_cpic_29 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_30R <- cpic_data_df$w2_tw_cpic_30 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)

cpic_data_df$w2_tw_cpic_31R <- cpic_data_df$w2_tw_cpic_31 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_32R <- cpic_data_df$w2_tw_cpic_32 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_33R <- cpic_data_df$w2_tw_cpic_33 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_34R <- cpic_data_df$w2_tw_cpic_34 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_36R <- cpic_data_df$w2_tw_cpic_36 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_37R <- cpic_data_df$w2_tw_cpic_37 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_39R <- cpic_data_df$w2_tw_cpic_39 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_40R <- cpic_data_df$w2_tw_cpic_40 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)

cpic_data_df$w2_tw_cpic_41R <- cpic_data_df$w2_tw_cpic_41 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_42R <- cpic_data_df$w2_tw_cpic_42 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_43R <- cpic_data_df$w2_tw_cpic_43 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)

cpic_data_df$w2_tw_cpic_44R <- cpic_data_df$w2_tw_cpic_44 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_45R <- cpic_data_df$w2_tw_cpic_45 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_46R <- cpic_data_df$w2_tw_cpic_46 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)
cpic_data_df$w2_tw_cpic_48R <- cpic_data_df$w2_tw_cpic_48 %>% dplyr::recode('1' = 3, '2' = 2, '3' = 1)


## 1b. Calculate CPIC Mean Z-Score
cpic_data_df$cpic_total <- rowSums(cbind(cpic_data_df$w2_tw_cpic_1, cpic_data_df$w2_tw_cpic_2, 
                                          cpic_data_df$w2_tw_cpic_3R, cpic_data_df$w2_tw_cpic_4R, 
                                          cpic_data_df$w2_tw_cpic_5, cpic_data_df$w2_tw_cpic_6R, 
                                          cpic_data_df$w2_tw_cpic_7R, cpic_data_df$w2_tw_cpic_8,
                                          cpic_data_df$w2_tw_cpic_9R, cpic_data_df$w2_tw_cpic_10R, 
                                          cpic_data_df$w2_tw_cpic_11R, cpic_data_df$w2_tw_cpic_12, 
                                          cpic_data_df$w2_tw_cpic_13R, cpic_data_df$w2_tw_cpic_14R, 
                                          cpic_data_df$w2_tw_cpic_15R, cpic_data_df$w2_tw_cpic_16R,
                                          cpic_data_df$w2_tw_cpic_17R, cpic_data_df$w2_tw_cpic_18, 
                                          cpic_data_df$w2_tw_cpic_19R, cpic_data_df$w2_tw_cpic_20, 
                                          cpic_data_df$w2_tw_cpic_21R, cpic_data_df$w2_tw_cpic_22, 
                                          cpic_data_df$w2_tw_cpic_23R, cpic_data_df$w2_tw_cpic_24R,
                                          cpic_data_df$w2_tw_cpic_25R, cpic_data_df$w2_tw_cpic_26, 
                                          cpic_data_df$w2_tw_cpic_27, cpic_data_df$w2_tw_cpic_28R, 
                                          cpic_data_df$w2_tw_cpic_29R, cpic_data_df$w2_tw_cpic_30R, 
                                          cpic_data_df$w2_tw_cpic_31R, cpic_data_df$w2_tw_cpic_32R,
                                          cpic_data_df$w2_tw_cpic_33R, cpic_data_df$w2_tw_cpic_34R, 
                                          cpic_data_df$w2_tw_cpic_35, cpic_data_df$w2_tw_cpic_36R, 
                                          cpic_data_df$w2_tw_cpic_37R, cpic_data_df$w2_tw_cpic_38, 
                                          cpic_data_df$w2_tw_cpic_39R, cpic_data_df$w2_tw_cpic_40R,
                                          cpic_data_df$w2_tw_cpic_41R, cpic_data_df$w2_tw_cpic_42R, 
                                          cpic_data_df$w2_tw_cpic_43R, cpic_data_df$w2_tw_cpic_44R, 
                                          cpic_data_df$w2_tw_cpic_45R, cpic_data_df$w2_tw_cpic_46R, 
                                          cpic_data_df$w2_tw_cpic_47, cpic_data_df$w2_tw_cpic_48R), na.rm = TRUE)

cpic_data_df$cpic_numvars <- rowSums(!is.na(cbind(cpic_data_df$w2_tw_cpic_1, cpic_data_df$w2_tw_cpic_2, 
                                                   cpic_data_df$w2_tw_cpic_3R, cpic_data_df$w2_tw_cpic_4R, 
                                                   cpic_data_df$w2_tw_cpic_5, cpic_data_df$w2_tw_cpic_6R, 
                                                   cpic_data_df$w2_tw_cpic_7R, cpic_data_df$w2_tw_cpic_8,
                                                   cpic_data_df$w2_tw_cpic_9R, cpic_data_df$w2_tw_cpic_10R, 
                                                   cpic_data_df$w2_tw_cpic_11R, cpic_data_df$w2_tw_cpic_12, 
                                                   cpic_data_df$w2_tw_cpic_13R, cpic_data_df$w2_tw_cpic_14R, 
                                                   cpic_data_df$w2_tw_cpic_15R, cpic_data_df$w2_tw_cpic_16R,
                                                   cpic_data_df$w2_tw_cpic_17R, cpic_data_df$w2_tw_cpic_18, 
                                                   cpic_data_df$w2_tw_cpic_19R, cpic_data_df$w2_tw_cpic_20, 
                                                   cpic_data_df$w2_tw_cpic_21R, cpic_data_df$w2_tw_cpic_22, 
                                                   cpic_data_df$w2_tw_cpic_23R, cpic_data_df$w2_tw_cpic_24R,
                                                   cpic_data_df$w2_tw_cpic_25R, cpic_data_df$w2_tw_cpic_26, 
                                                   cpic_data_df$w2_tw_cpic_27, cpic_data_df$w2_tw_cpic_28R, 
                                                   cpic_data_df$w2_tw_cpic_29R, cpic_data_df$w2_tw_cpic_30R, 
                                                   cpic_data_df$w2_tw_cpic_31R, cpic_data_df$w2_tw_cpic_32R,
                                                   cpic_data_df$w2_tw_cpic_33R, cpic_data_df$w2_tw_cpic_34R, 
                                                   cpic_data_df$w2_tw_cpic_35, cpic_data_df$w2_tw_cpic_36R, 
                                                   cpic_data_df$w2_tw_cpic_37R, cpic_data_df$w2_tw_cpic_38, 
                                                   cpic_data_df$w2_tw_cpic_39R, cpic_data_df$w2_tw_cpic_40R,
                                                   cpic_data_df$w2_tw_cpic_41R, cpic_data_df$w2_tw_cpic_42R, 
                                                   cpic_data_df$w2_tw_cpic_43R, cpic_data_df$w2_tw_cpic_44R, 
                                                   cpic_data_df$w2_tw_cpic_45R, cpic_data_df$w2_tw_cpic_46R, 
                                                   cpic_data_df$w2_tw_cpic_47, cpic_data_df$w2_tw_cpic_48R)))

cpic_data_df$cpic_mean <- cpic_data_df$cpic_total / cpic_data_df$cpic_numvars
cpic_data_df$cpic_meanZ <- as.numeric(scale(cpic_data_df$cpic_mean, center = TRUE, scale = TRUE))
cpic_data_df$cpic_meanZ <- ifelse(cpic_data_df$cpic_numvars < 24, NA, cpic_data_df$cpic_meanZ)

sum(!is.na(cpic_data_df$cpic_meanZ))

## 1c. Internal Consistency
cpic_crA <- cpic_data_df %>% dplyr::filter(!is.na(cpic_data_df$cpic_meanZ))
cpic_crA <- cpic_crA %>% dplyr::select(all_of(c("w2_tw_cpic_1", "w2_tw_cpic_2",
                                                "w2_tw_cpic_3R", "w2_tw_cpic_4R",
                                                "w2_tw_cpic_5", "w2_tw_cpic_6R", 
                                                "w2_tw_cpic_7R", "w2_tw_cpic_8",
                                                "w2_tw_cpic_9R", "w2_tw_cpic_10R", 
                                                "w2_tw_cpic_11R", "w2_tw_cpic_12", 
                                                "w2_tw_cpic_13R", "w2_tw_cpic_14R", 
                                                "w2_tw_cpic_15R", "w2_tw_cpic_16R",
                                                "w2_tw_cpic_17R", "w2_tw_cpic_18", 
                                                "w2_tw_cpic_19R", "w2_tw_cpic_20", 
                                                "w2_tw_cpic_21R", "w2_tw_cpic_22", 
                                                "w2_tw_cpic_23R", "w2_tw_cpic_24R",
                                                "w2_tw_cpic_25R", "w2_tw_cpic_26", 
                                                "w2_tw_cpic_27", "w2_tw_cpic_28R", 
                                                "w2_tw_cpic_29R", "w2_tw_cpic_30R", 
                                                "w2_tw_cpic_31R", "w2_tw_cpic_32R",
                                                "w2_tw_cpic_33R", "w2_tw_cpic_34R", 
                                                "w2_tw_cpic_35", "w2_tw_cpic_36R", 
                                                "w2_tw_cpic_37R", "w2_tw_cpic_38", 
                                                "w2_tw_cpic_39R", "w2_tw_cpic_40R",
                                                "w2_tw_cpic_41R", "w2_tw_cpic_42R", 
                                                "w2_tw_cpic_43R", "w2_tw_cpic_44R", 
                                                "w2_tw_cpic_45R", "w2_tw_cpic_46R", 
                                                "w2_tw_cpic_47", "w2_tw_cpic_48R")))

alpha(cpic_crA)


#### Step 2. Score Parental Environment Questionnaire (PEQ) - PC Report ####
peq_data <- data.frame(read_sav("./behavioral_data/T2_PEQ_full.sav"))
peq_data_df <- data.frame(read.csv("./behavioral_data/peq.csv",
                                    sep = ",", header = TRUE))

## 2a. Identify Conflict Items
peq_data$w2_pcpeqtw_2   # conflict (Often criticize son)
peq_data$w2_pcpeqtw_4   # conflict (Often interrupt son)
peq_data$w2_pcpeqtw_7   # conflict (Often irritate son)
peq_data$w2_pcpeqtw_8   # conflict (Often have misunderstandings with son)
peq_data$w2_pcpeqtw_10  # conflict (My child treats others with more respect than he/she treats me)
peq_data$w2_pcpeqtw_11  # conflict (Often hurt son's feelings)
peq_data$w2_pcpeqtw_14  # conflict (I do not trust my child to make his/her own decisions)
peq_data$w2_pcpeqtw_15  # conflict (My child and I often get into arguments)
peq_data$w2_pcpeqtw_17  # conflict (My child often angers or ignores me)
peq_data$w2_pcpeqtw_18  # conflict (Often lose temper with son)
peq_data$w2_pcpeqtw_19  # conflict (I often hit my child in anger)
peq_data$w2_pcpeqtw_20  # conflict (Once in a while my child has been really scared of me)

## 2b. Reverse Score Items
peq_data_df$w2_pcpeqtw_2R <- peq_data_df$w2_pcpeqtw_2 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
peq_data_df$w2_pcpeqtw_4R <- peq_data_df$w2_pcpeqtw_4 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
peq_data_df$w2_pcpeqtw_7R <- peq_data_df$w2_pcpeqtw_7 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
peq_data_df$w2_pcpeqtw_8R <- peq_data_df$w2_pcpeqtw_8 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
peq_data_df$w2_pcpeqtw_10R <- peq_data_df$w2_pcpeqtw_10 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
peq_data_df$w2_pcpeqtw_11R <- peq_data_df$w2_pcpeqtw_11 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
peq_data_df$w2_pcpeqtw_14R <- peq_data_df$w2_pcpeqtw_14 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
peq_data_df$w2_pcpeqtw_15R <- peq_data_df$w2_pcpeqtw_15 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
peq_data_df$w2_pcpeqtw_17R <- peq_data_df$w2_pcpeqtw_17 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
peq_data_df$w2_pcpeqtw_18R <- peq_data_df$w2_pcpeqtw_18 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
peq_data_df$w2_pcpeqtw_19R <- peq_data_df$w2_pcpeqtw_19 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
peq_data_df$w2_pcpeqtw_20R <- peq_data_df$w2_pcpeqtw_20 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)

## 2c. Extract Data
peq_data_df$peqConflict_PC_total <- rowSums(cbind(peq_data_df$w2_pcpeqtw_2R, peq_data_df$w2_pcpeqtw_4R, 
                                                           peq_data_df$w2_pcpeqtw_7R, peq_data_df$w2_pcpeqtw_8R, 
                                                           peq_data_df$w2_pcpeqtw_10R, peq_data_df$w2_pcpeqtw_11R, 
                                                           peq_data_df$w2_pcpeqtw_14R, peq_data_df$w2_pcpeqtw_15R,
                                                           peq_data_df$w2_pcpeqtw_17R, peq_data_df$w2_pcpeqtw_18R, 
                                                           peq_data_df$w2_pcpeqtw_19R, peq_data_df$w2_pcpeqtw_20R),
                                            na.rm = TRUE)

peq_data_df$peqConflict_PC_numvars <- rowSums(!is.na(cbind(peq_data_df$w2_pcpeqtw_2R, peq_data_df$w2_pcpeqtw_4R, 
                                                           peq_data_df$w2_pcpeqtw_7R, peq_data_df$w2_pcpeqtw_8R, 
                                                           peq_data_df$w2_pcpeqtw_10R, peq_data_df$w2_pcpeqtw_11R, 
                                                           peq_data_df$w2_pcpeqtw_14R, peq_data_df$w2_pcpeqtw_15R,
                                                           peq_data_df$w2_pcpeqtw_17R, peq_data_df$w2_pcpeqtw_18R, 
                                                           peq_data_df$w2_pcpeqtw_19R, peq_data_df$w2_pcpeqtw_20R)))

peq_data_df$peqConflict_PC_mean <- peq_data_df$peqConflict_PC_total / peq_data_df$peqConflict_PC_numvars
peq_data_df$peqConflict_PC_meanZ <- as.numeric(scale(peq_data_df$peqConflict_PC_mean, center = TRUE, scale = TRUE))
peq_data_df$peqConflict_PC_meanZ <- ifelse(peq_data_df$peqConflict_PC_numvars < 6, NA, peq_data_df$peqConflict_PC_meanZ)

sum(!is.na(peq_data_df$peqConflict_PC_meanZ))

## 2d. Internal Consistency
peqConflict_PC_crA <- peq_data_df %>% dplyr::filter(!is.na(peq_data_df$peqConflict_PC_meanZ))
peqConflict_PC_crA <- peqConflict_PC_crA %>% dplyr::select(all_of(c("w2_pcpeqtw_2R", "w2_pcpeqtw_4R", 
                                                                    "w2_pcpeqtw_7R", "w2_pcpeqtw_8R", 
                                                                    "w2_pcpeqtw_10R", "w2_pcpeqtw_11R", 
                                                                    "w2_pcpeqtw_14R", "w2_pcpeqtw_15R",
                                                                    "w2_pcpeqtw_17R", "w2_pcpeqtw_18R", 
                                                                    "w2_pcpeqtw_19R", "w2_pcpeqtw_20R")))

alpha(peqConflict_PC_crA)


#### Step 3. Score Parental Environment Questionnaire (PEQ) - Twin Report on PC ####
## 3a. Identify Conflict Items
peq_data$w2_twpc_peq_2   # conflict (Often criticize son)
peq_data$w2_twpc_peq_4   # conflict (Often interrupt son)
peq_data$w2_twpc_peq_7   # conflict (Often irritate son)
peq_data$w2_twpc_peq_8   # conflict (Often have misunderstandings with son)
peq_data$w2_twpc_peq_10  # conflict (My child treats others with more respect than he/she treats me)
peq_data$w2_twpc_peq_11  # conflict (Often hurt son's feelings)
peq_data$w2_twpc_peq_14  # conflict (I do not trust my child to make his/her own decisions)
peq_data$w2_twpc_peq_15  # conflict (My child and I often get into arguments)
peq_data$w2_twpc_peq_17  # conflict (My child often angers or ignores me)
peq_data$w2_twpc_peq_18  # conflict (Often lose temper with son)
peq_data$w2_twpc_peq_19  # conflict (I often hit my child in anger)
peq_data$w2_twpc_peq_20  # conflict (Once in a while my child has been really scared of me)

## 3b. Reverse Score Items
peq_data_df$w2_twpc_peq_2R <- peq_data_df$w2_twpc_peq_2 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
peq_data_df$w2_twpc_peq_4R <- peq_data_df$w2_twpc_peq_4 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
peq_data_df$w2_twpc_peq_7R <- peq_data_df$w2_twpc_peq_7 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
peq_data_df$w2_twpc_peq_8R <- peq_data_df$w2_twpc_peq_8 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
peq_data_df$w2_twpc_peq_10R <- peq_data_df$w2_twpc_peq_10 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
peq_data_df$w2_twpc_peq_11R <- peq_data_df$w2_twpc_peq_11 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
peq_data_df$w2_twpc_peq_14R <- peq_data_df$w2_twpc_peq_14 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
peq_data_df$w2_twpc_peq_15R <- peq_data_df$w2_twpc_peq_15 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
peq_data_df$w2_twpc_peq_17R <- peq_data_df$w2_twpc_peq_17 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
peq_data_df$w2_twpc_peq_18R <- peq_data_df$w2_twpc_peq_18 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
peq_data_df$w2_twpc_peq_19R <- peq_data_df$w2_twpc_peq_19 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)
peq_data_df$w2_twpc_peq_20R <- peq_data_df$w2_twpc_peq_20 %>% dplyr::recode('1' = 4, '2' = 3, '3' = 2, '4' = 1)

## 3c. Extract Data
peq_data_df$peqConflict_twinOnPC_total <- rowSums(cbind(peq_data_df$w2_twpc_peq_2R, peq_data_df$w2_twpc_peq_4R, 
                                                  peq_data_df$w2_twpc_peq_7R, peq_data_df$w2_twpc_peq_8R, 
                                                  peq_data_df$w2_twpc_peq_10R, peq_data_df$w2_twpc_peq_11R, 
                                                  peq_data_df$w2_twpc_peq_14R, peq_data_df$w2_twpc_peq_15R,
                                                  peq_data_df$w2_twpc_peq_17R, peq_data_df$w2_twpc_peq_18R, 
                                                  peq_data_df$w2_twpc_peq_19R, peq_data_df$w2_twpc_peq_20R),
                                            na.rm = TRUE)

peq_data_df$peqConflict_twinOnPC_numvars <- rowSums(!is.na(cbind(peq_data_df$w2_twpc_peq_2R, peq_data_df$w2_twpc_peq_4R, 
                                                           peq_data_df$w2_twpc_peq_7R, peq_data_df$w2_twpc_peq_8R, 
                                                           peq_data_df$w2_twpc_peq_10R, peq_data_df$w2_twpc_peq_11R, 
                                                           peq_data_df$w2_twpc_peq_14R, peq_data_df$w2_twpc_peq_15R,
                                                           peq_data_df$w2_twpc_peq_17R, peq_data_df$w2_twpc_peq_18R, 
                                                           peq_data_df$w2_twpc_peq_19R, peq_data_df$w2_twpc_peq_20R)))

peq_data_df$peqConflict_twinOnPC_mean <- peq_data_df$peqConflict_twinOnPC_total / peq_data_df$peqConflict_twinOnPC_numvars
peq_data_df$peqConflict_twinOnPC_meanZ <- as.numeric(scale(peq_data_df$peqConflict_twinOnPC_mean, center = TRUE, scale = TRUE))
peq_data_df$peqConflict_twinOnPC_meanZ <- ifelse(peq_data_df$peqConflict_twinOnPC_numvars < 6, NA, peq_data_df$peqConflict_twinOnPC_meanZ)

sum(!is.na(peq_data_df$peqConflict_twinOnPC_meanZ))

## 3d. Internal Consistency
peqConflict_twinOnPC_crA <- peq_data_df %>% dplyr::filter(!is.na(peq_data_df$peqConflict_twinOnPC_meanZ))
peqConflict_twinOnPC_crA <- peqConflict_twinOnPC_crA %>% dplyr::select(all_of(c("w2_twpc_peq_2R", "w2_twpc_peq_4R", 
                                                                    "w2_twpc_peq_7R", "w2_twpc_peq_8R", 
                                                                    "w2_twpc_peq_10R", "w2_twpc_peq_11R", 
                                                                    "w2_twpc_peq_14R", "w2_twpc_peq_15R",
                                                                    "w2_twpc_peq_17R", "w2_twpc_peq_18R", 
                                                                    "w2_twpc_peq_19R", "w2_twpc_peq_20R")))

alpha(peqConflict_twinOnPC_crA)


#### Step 4. Score Exposure to Community Violence (Kid SAVE) - Twin Self-Report ####
cvc_data <- data.frame(read_sav("./behavioral_data/additional_data_threatPEQ_MTwiNS/ECV scored_T2.sav"))
cvc_data_df <- data.frame(read.csv("./behavioral_data/additional_data_threatPEQ_MTwiNS/cvc.csv",
                                    sep = ",", header = TRUE))

## 4a. Identify Items
cvc_data$w2tn_ECV_04a
cvc_data$w2tn_ECV_06a
cvc_data$w2tn_ECV_11a
cvc_data$w2tn_ECV_19a
cvc_data$w2tn_ECV_27a

## Check Frequency of Endorsement
table(cvc_data$w2tn_ECV_04a)
table(cvc_data$w2tn_ECV_06a)
table(cvc_data$w2tn_ECV_11a)
table(cvc_data$w2tn_ECV_19a)
table(cvc_data$w2tn_ECV_27a)


## 4b. Extract Data
cvc_data_df$cvc_total <- rowSums(cbind(cvc_data_df$w2tn_ECV_04a,
                                       cvc_data_df$w2tn_ECV_06a,
                                       cvc_data_df$w2tn_ECV_11a,
                                       cvc_data_df$w2tn_ECV_19a,
                                       cvc_data_df$w2tn_ECV_27a), na.rm = TRUE)

cvc_data_df$cvc_numvars <- rowSums(!is.na(cbind(cvc_data_df$w2tn_ECV_04a,
                                                cvc_data_df$w2tn_ECV_06a,
                                                cvc_data_df$w2tn_ECV_11a,
                                                cvc_data_df$w2tn_ECV_19a,
                                                cvc_data_df$w2tn_ECV_27a)))

cvc_data_df$cvc_mean <- cvc_data_df$cvc_total / cvc_data_df$cvc_numvars
cvc_data_df$cvc_meanZ <- as.numeric(scale(cvc_data_df$cvc_mean, center = TRUE, scale = TRUE))
cvc_data_df$cvc_meanZ <- ifelse(cvc_data_df$cvc_numvars < 3, NA, cvc_data_df$cvc_meanZ)

sum(!is.na(cvc_data_df$cvc_meanZ))

## 4c. Internal Consistency
cvc_crA <- cvc_data %>% dplyr::filter(!is.na(cvc_data_df$cvc_meanZ))
cvc_crA <- cvc_crA %>% dplyr::select(all_of(c("w2tn_ECV_04a", 
                                              "w2tn_ECV_06a", "w2tn_ECV_11a",
                                              "w2tn_ECV_19a", 
                                              "w2tn_ECV_27a")))

alpha(cvc_crA)


#### Step 5. Calculate Threat Composite ####
## 5a. Integrate Data
final_data <- merge(cpic_data_df, peq_data_df, by = intersect(names(cpic_data_df), names(peq_data_df)), all.x = TRUE)
final_data <- merge(final_data, cvc_data_df, by = intersect(names(final_data), names(cvc_data_df)), all.x = TRUE)


## 5b. Calculate an Average Harsh Parenting Score
final_data$peqConflict_sum <- rowSums(cbind(final_data$peqConflict_PC_meanZ, final_data$peqConflict_twinOnPC_meanZ),
                                      na.rm = TRUE)

final_data$peqConflict_numvars <- rowSums(!is.na(cbind(final_data$peqConflict_PC_meanZ,
                                                       final_data$peqConflict_twinOnPC_meanZ)))

final_data$peqConflict_mean <- final_data$peqConflict_sum / final_data$peqConflict_numvars
final_data$peqConflict_final <- ifelse(final_data$peqConflict_numvars < 0, NA, final_data$peqConflict_mean)


## 5c. Calculate a Threat Composite
final_data$threatComposite <- rowSums(cbind(final_data$cpic_meanZ,
                                               final_data$peqConflict_final,
                                               final_data$cvc_meanZ), na.rm = TRUE)

final_data$threat_numvars <- rowSums(!is.na(cbind(final_data$cpic_meanZ,
                                                     final_data$peqConflict_final,
                                                     final_data$cvc_meanZ)))

final_data$threatComposite_mean <- final_data$threatComposite / final_data$threat_numvars
final_data$threatComposite_final <- ifelse(final_data$threat_numvars < 1, NA, final_data$threatComposite_mean)


## 5d. Save Data
threat_data <- final_data %>% dplyr::select(idtw, cpic_meanZ, peqConflict_PC_meanZ,
                                               peqConflict_twinOnPC_meanZ, cvc_meanZ,
                                               threatComposite, threat_numvars, threatComposite_mean,
                                               threatComposite_final)

msu_ids <- as.character(threat_data$idtw)
fam_ids <- str_remove(substring(msu_ids, 3, 6), "^0+")  # use regex to remove leading 0's
twin_ids <- recode(substring(msu_ids, 7, 8), "00" = "t1", "01" = "t2")
umich_id <- paste0(fam_ids, twin_ids, sep = "")
threat_data$sub <- paste0("sub-", umich_id)

threat_data$idtw <- NULL
threat_data <- threat_data %>% dplyr::select(sub, cpic_meanZ:threatComposite_final)

write.csv(threat_data, "./analyses/adversity/envXpace_compositeThreat_adolescence.csv", row.names = FALSE)
