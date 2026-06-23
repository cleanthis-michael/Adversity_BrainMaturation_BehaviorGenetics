#### Adversity X Brain Maturation ####
## Step 03. Prepare Data for Analysis

rm(list=ls())

# Load Libraries
list.of.packages <- c("dplyr", "haven", "stringr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# Set Working Directory
library(rstudioapi)
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))
print( getwd() )


#### 1. Read Data ####
## 1a. Adversity Exposure
threat_child <- data.frame(read.csv("./analyses/adversity/envXpace_compositeThreat_childhood.csv",
                                    sep = ",", header = TRUE))
which(duplicated(threat_child$sub))
threat_child <- threat_child[-c(1255, 1257, 1317, 1319, 1477, 1479, 1683, 1684, 2007, 2009),]
colnames(threat_child)[2:9] <- paste0(colnames(threat_child)[2:9], "_child")

threat_adol <- data.frame(read.csv("./analyses/adversity/envXpace_compositeThreat_adolescence.csv",
                                   sep = ",", header = TRUE))
colnames(threat_adol)[2:9] <- paste0(colnames(threat_adol)[2:9], "_adol")

ses_child <- data.frame(read.csv("./analyses/adversity/envXpace_compositeSES_childhood.csv",
                                 sep = ",", header = TRUE))
colnames(ses_child)[2:9] <- paste0(colnames(ses_child)[2:9], "_child")

ses_adol <- data.frame(read.csv("./analyses/adversity/envXpace_compositeSES_adolescence.csv",
                                sep = ",", header = TRUE))
colnames(ses_adol)[2:9] <- paste0(colnames(ses_adol)[2:9], "_adol")


## 1b. Demographics
demo_data <- data.frame(read_sav("./behavioral_data/T2_ADI_Age_gender_ethn_income_edu.sav"))

demo_data <- demo_data %>% dplyr::select(idtw, famid, zygosity, T2_AgeatVisit, ethnicity, gender)
demo_data$race <- ifelse(demo_data$ethnicity == 6, 1, 0)

msu_ids <- as.character(demo_data$idtw)
fam_ids <- str_remove(substring(msu_ids, 3, 6), "^0+")  # use regex to remove leading 0's
twin_ids <- recode(substring(msu_ids, 7, 8), "00" = "t1", "01" = "t2")
umich_id <- paste0(fam_ids, twin_ids, sep = "")
demo_data$sub <- paste0("sub-", umich_id)
demo_data <- demo_data %>% dplyr::select(sub, famid, zygosity, T2_AgeatVisit, ethnicity, gender, race)
colnames(demo_data) <- c("sub", "famid", "zygosity", "wav2_twin_age", "ethnicity", "gender", "race")

write.csv(demo_data, "./behavioral_data/demo_data.csv", row.names = FALSE)
demo_data <- data.frame(read.csv("./behavioral_data/demo_data.csv", sep = ",", header = TRUE))

## 1c. Acquisition Details
acq_data <- data.frame(read.csv("./behavioral_data/MTwiNS_acq_w2.csv",
                                sep = ",", header = TRUE))


## 1d. Euler Numbers (QC)
euler_data <- data.frame(read.csv("./behavioral_data/eulerheader_brainage.csv",
                                  sep = ",", header = TRUE))
colnames(euler_data)[colnames(euler_data) == "SubjID"] <- "sub"


## 1e. Brain Age
drobinin <- data.frame(read.csv("./analyses/brain_age/brainAge_Drobinin_mtwins.csv", sep = ",")) %>%
  filter(!sub %in% c("sub-762t1","sub-6220t2","sub-6663t1","sub-702t1","sub-6681t2","sub-731t1", "sub-6433t2"))
drobinin$truth_age <- NULL

pyment <- data.frame(read.csv("./analyses/brain_age/brainAge_Pyment_mtwins.csv", sep = ",")) %>%
  filter(!sub %in% c("sub-762t1","sub-6220t2","sub-6663t1","sub-702t1","sub-6681t2","sub-731t1", "sub-6433t2"))
pyment$brainAgeGap_Pyment_mtwins <- pyment$brain_age_Pyment - pyment$wav2_twin_age
pyment$wav2_twin_age <- NULL

centile <- data.frame(read.csv("./analyses/brain_age/CentileBrainAGE_OutputsAll_mtwins.csv", sep = ",")) %>%
  filter(!sub %in% c("sub-762t1","sub-6220t2","sub-6663t1","sub-702t1","sub-6681t2","sub-731t1", "sub-6433t2"))
centile <- merge(centile, demo_data, by = "sub", all.x = TRUE)
centile$centilebrain_BAG <- centile$brain_age_Centile - centile$wav2_twin_age
centile <- centile %>% dplyr::select(sub, brain_age_Centile, centilebrain_BAG)

## 1j. Merge Data Frames
all_data <- merge(drobinin, pyment, by = "sub", all.x = TRUE)
all_data <- merge(all_data, centile, by = "sub", all.x =  TRUE)
all_data <- merge(all_data, demo_data, by = "sub", all.x = TRUE)
all_data <- merge(all_data, acq_data, by = "sub", all.x = TRUE)
all_data <- merge(all_data, euler_data, by = "sub", all.x = TRUE)
all_data <- merge(all_data, threat_child, by = "sub", all.x = TRUE)
all_data <- merge(all_data, threat_adol, by = "sub", all.x = TRUE)
all_data <- merge(all_data, ses_child, by = "sub", all.x = TRUE)
all_data <- merge(all_data, ses_adol, by = "sub", all.x = TRUE)

which(duplicated(all_data$sub))
all_data <- all_data[-c(273:275, 277:279, 529, 531, 588:610, 612:632, 633:634),]

#write.csv(all_data, file = "./analyses/envXpace_AllData.csv", row.names = FALSE)

rachel_data <- all_data %>% dplyr::select(sub, famid, zygosity, threatComposite_final_child, cpic_meanZ_child,
                                          peqConflict_TwinOnMom_meanZ_child, brainAgeGap_Pyment_mtwins,
                                          wav2_twin_age, gender, race, acq, totalSurfaceHoles)
#write.csv(rachel_data, "./analyses/envXpace_BivariateData.csv", row.names = FALSE)







#### 3. Prepare Data for Mplus ####
mplus_data <- all_data %>%
  dplyr::select(c(sub, famid, wav2_twin_age, gender, race, acq, totalSurfaceHoles,
                  brainAgeGap_Drobinin_mtwins, brainAgeGap_Pyment_mtwins, centilebrain_BAG,
                  sesComposite_mean_final_child, sesComposite_mean_final_adol,
                  threatComposite_final_child, threatComposite_final_adol,
                  wav1_famincome_reverseZ_child, wav1_momEducation_reverseZ_child, wav1_dadEducation_reverseZ_child,
                  wav1_nbhadiZ_child, wav2_famincome_reverseZ_adol, wav2_pc_education_reverseZ_adol,
                  wav2_ac_education_reverseZ_adol, wav2_nbhadiZ_adol,
                  cpic_meanZ_child, peqConflict_MomReport_meanZ_child, peqConflict_TwinOnMom_meanZ_child,
                  cvc_meanZ_child, cpic_meanZ_adol, peqConflict_PC_meanZ_adol, peqConflict_twinOnPC_meanZ_adol, cvc_meanZ_adol,
                  w2pc_cbcl_anxious_r, w2pc_cbcl_withdrawn_r, w2pc_cbcl_attention_r, w2pc_cbcl_rulebreak_r, w2pc_cbcl_aggressive_r,
                  w2pc_cbcl_internal_r, w2pc_cbcl_external_r, w2pc_cbcl_total_p_r,
                  w2pc_cbcl_depress_r, w2pc_cbcl_anxiety_r, w2pc_cbcl_adhd_r, w2pc_cbcl_odd_r, w2pc_cbcl_conduct_r,
                  w3pc_cbcl_anxious_r, w3pc_cbcl_withdrawn_r, w3pc_cbcl_attention_r, w3pc_cbcl_rulebreak_r, w3pc_cbcl_aggressive_r,
                  w3pc_cbcl_internal_r, w3pc_cbcl_external_r, w3pc_cbcl_total_p_r,
                  w3pc_cbcl_depress_r, w3pc_cbcl_anxiety_r, w3pc_cbcl_adhd_r, w3pc_cbcl_odd_r, w3pc_cbcl_conduct_r,
                  wav2_kkanxdr, wav2_kkwithr, wav2_kkattnr, wav2_kkdelir, wav2_kkaggrr,
                  wav2_kkintr, wav2_kkextr, wav2_kktotr,
                  wav3_kkanxdr, wav3_kkwithr, wav3_kkattnr, wav3_kkdelir, wav3_kkaggrr,
                  wav3_kkintr, wav3_kkextr, wav3_kktotr, wav2_tr_eatq_SUPER_EFFFIN, eea_1, eea_2,
                  Academic.Resilience, Psych.Resilience, Social.Resilience,
                  L_bankssts_thickavg_centilezscore:WB_centilezmean)) %>%
  mutate(across(everything(), as.vector)) %>%
  clean_names()

mplus_data[is.na(mplus_data)] <- -999
write.table(mplus_data, file = "./analyses/mplus/envXpace_AllData.csv", sep = ",", col.names = FALSE, row.names = FALSE)
write.table(mplus_data, file = "./analyses/mplus/step1a_envXpace/envXpace_AllData.csv", sep = ",", col.names = FALSE, row.names = FALSE)
write.table(mplus_data, file = "./analyses/mplus/step1b_envXpace_Demo/envXpace_AllData.csv", sep = ",", col.names = FALSE, row.names = FALSE)
write.table(mplus_data, file = "./analyses/mplus/step1c_envXpace_allCovars/envXpace_AllData.csv", sep = ",", col.names = FALSE, row.names = FALSE)


#### 4. Mplus Automation - Aim 1a ####
template <- "
TITLE: Environment X Pace of Brain Development;

DATA:
	FILE IS envXpace_AllData.csv;
	FORMAT IS free;
	TYPE IS individual;

VARIABLE:
  NAMES ARE
  sub
  famid
  age
  sex
  race
  acq
  euler
  drob
  pym
  cent
  ses1
  ses2
  thr1
  thr2
  inc1
  momed1
  daded1
  adi1
  inc2
  pced2
  aced2
  adi2
  cpic1
  peqm1
  peqt1
  cvc1
  cpic2
  peqpc2
  peqac2
  cvc2
  anx_pc2
  wth_pc2
  att_pc2
  rb_pc2
  agg_pc2
  int_pc2
  ext_pc2
  tot_pc2
  depd_pc2
  anxd_pc2
  adhd_pc2
  odd_pc2
  cd_pc2
  anx_pc3
  wth_pc3
  att_pc3
  rb_pc3
  agg_pc3
  int_pc3
  ext_pc3
  tot_pc3
  depd_pc3
  anxd_pc3
  adhd_pc3
  odd_pc3
  cd_pc3
  anx2
  wth2
  att2
  rb2
  agg2
  int2
  ext2
  tot2
  anx3
  wth3
  att3
  rb3
  agg3
  int3
  ext3
  tot3
  eatq_ef
  eea1
  ee2
  res_a
  res_p
  res_s
  roi1-roi154;
  
  USEVARIABLES ARE pym [[predictor]];
  
  CLUSTER = famid;
  
  MISSING ARE ALL (-999);
  
ANALYSIS:
  TYPE = COMPLEX;
  ESTIMATOR = MLR;
  ITERATIONS = 10000;
	CONVERGENCE = 0.00005;

MODEL:
  pym ON [[predictor]];
  
  [pym [[predictor]]];
  
OUTPUT:
  sampstat cinterval standardized modindices TECH1 TECH3;
"

predictors <- c("ses1", "ses2", "thr1", "thr2",
                "inc1", "momed1", "daded1", "adi1",
                "inc2", "pced2", "aced2", "adi2",
                "cpic1", "peqm1", "peqt1", "cvc1",
                "cpic2", "peqpc2", "peqac2", "cvc2")

for (pred in predictors) {
  model_string <- gsub("\\[\\[predictor\\]\\]", pred, template)
  
  file_name <- paste0("./analyses/mplus/step1a_envXpace/envXpace_", pred, ".inp")
  writeLines(model_string, con = file_name)
}

runModels(target = "./analyses/mplus/step1a_envXpace", log = NULL)
model_results <- readModels(target = "./analyses/mplus/step1a_envXpace/")

summary_df_unstd <- data.frame()
summary_df_std <- data.frame()
summary_df_ci <- data.frame()

for (i in seq_along(model_results)) {
  
  model_output <- model_results[[i]]
  
  unstd <- subset(model_output$parameters$unstandardized, grepl("ON", paramHeader))
  stdyx <- subset(model_output$parameters$stdyx.standardized, grepl("ON", paramHeader))
  stdyx_ci <- subset(model_output$parameters$ci.stdyx.standardized, grepl("ON", paramHeader))
  
  # Add to summary data frame
  summary_df_unstd <- rbind(summary_df_unstd, unstd)
  summary_df_std <- rbind(summary_df_std, stdyx)
  summary_df_ci <- rbind(summary_df_ci, stdyx_ci)
  
}

write.csv(summary_df_unstd, file = "./analyses/mplus/step1a_envXpace_unstd.csv", row.names = FALSE)
write.csv(summary_df_std, file = "./analyses/mplus/step1a_envXpace_std.csv", row.names = FALSE)
write.csv(summary_df_ci, file = "./analyses/mplus/step1a_envXpace_ci.csv", row.names = FALSE)


#### 5. Mplus Automation - Aim 1b ####
template <- "
TITLE: Environment X Pace of Brain Development;

DATA:
	FILE IS envXpace_AllData.csv;
	FORMAT IS free;
	TYPE IS individual;

VARIABLE:
  NAMES ARE
  sub
  famid
  age
  sex
  race
  acq
  euler
  drob
  pym
  cent
  ses1
  ses2
  thr1
  thr2
  inc1
  momed1
  daded1
  adi1
  inc2
  pced2
  aced2
  adi2
  cpic1
  peqm1
  peqt1
  cvc1
  cpic2
  peqpc2
  peqac2
  cvc2
  anx_pc2
  wth_pc2
  att_pc2
  rb_pc2
  agg_pc2
  int_pc2
  ext_pc2
  tot_pc2
  depd_pc2
  anxd_pc2
  adhd_pc2
  odd_pc2
  cd_pc2
  anx_pc3
  wth_pc3
  att_pc3
  rb_pc3
  agg_pc3
  int_pc3
  ext_pc3
  tot_pc3
  depd_pc3
  anxd_pc3
  adhd_pc3
  odd_pc3
  cd_pc3
  anx2
  wth2
  att2
  rb2
  agg2
  int2
  ext2
  tot2
  anx3
  wth3
  att3
  rb3
  agg3
  int3
  ext3
  tot3
  eatq_ef
  eea1
  ee2
  res_a
  res_p
  res_s
  roi1-roi154;
  
  USEVARIABLES ARE pym [[predictor]]
  age sex race;
  
  CLUSTER = famid;
  
  MISSING ARE ALL (-999);
  
ANALYSIS:
  TYPE = COMPLEX;
  ESTIMATOR = MLR;
  ITERATIONS = 10000;
	CONVERGENCE = 0.00005;

MODEL:
  pym ON [[predictor]]
  age sex race;
  
  [pym [[predictor]]
  age sex race];
  
OUTPUT:
  sampstat cinterval standardized modindices TECH1 TECH3;
"

predictors <- c("ses1", "ses2", "thr1", "thr2",
                "inc1", "momed1", "daded1", "adi1",
                "inc2", "pced2", "aced2", "adi2",
                "cpic1", "peqm1", "peqt1", "cvc1",
                "cpic2", "peqpc2", "peqac2", "cvc2")

for (pred in predictors) {
  model_string <- gsub("\\[\\[predictor\\]\\]", pred, template)
  
  file_name <- paste0("./analyses/mplus/step1b_envXpace_Demo/envXpace_", pred, ".inp")
  writeLines(model_string, con = file_name)
}

runModels(target = "./analyses/mplus/step1b_envXpace_Demo", log = NULL)
model_results <- readModels(target = "./analyses/mplus/step1b_envXpace_Demo/")

summary_df_unstd <- data.frame()
summary_df_std <- data.frame()
summary_df_ci <- data.frame()

for (i in seq_along(model_results)) {
  
  model_output <- model_results[[i]]
  
  unstd <- subset(model_output$parameters$unstandardized, grepl("ON", paramHeader))
  stdyx <- subset(model_output$parameters$stdyx.standardized, grepl("ON", paramHeader))
  stdyx_ci <- subset(model_output$parameters$ci.stdyx.standardized, grepl("ON", paramHeader))
  
  # Add to summary data frame
  summary_df_unstd <- rbind(summary_df_unstd, unstd)
  summary_df_std <- rbind(summary_df_std, stdyx)
  summary_df_ci <- rbind(summary_df_ci, stdyx_ci)
  
}

write.csv(summary_df_unstd, file = "./analyses/mplus/step1b_envXpace_Demo_unstd.csv", row.names = FALSE)
write.csv(summary_df_std, file = "./analyses/mplus/step1b_envXpace_Demo_std.csv", row.names = FALSE)
write.csv(summary_df_ci, file = "./analyses/mplus/step1b_envXpace_Demo_ci.csv", row.names = FALSE)


#### 6. Mplus Automation - Aim 1c ####
template <- "
TITLE: Environment X Pace of Brain Development;

DATA:
	FILE IS envXpace_AllData.csv;
	FORMAT IS free;
	TYPE IS individual;

VARIABLE:
  NAMES ARE
  sub
  famid
  age
  sex
  race
  acq
  euler
  drob
  pym
  cent
  ses1
  ses2
  thr1
  thr2
  inc1
  momed1
  daded1
  adi1
  inc2
  pced2
  aced2
  adi2
  cpic1
  peqm1
  peqt1
  cvc1
  cpic2
  peqpc2
  peqac2
  cvc2
  anx_pc2
  wth_pc2
  att_pc2
  rb_pc2
  agg_pc2
  int_pc2
  ext_pc2
  tot_pc2
  depd_pc2
  anxd_pc2
  adhd_pc2
  odd_pc2
  cd_pc2
  anx_pc3
  wth_pc3
  att_pc3
  rb_pc3
  agg_pc3
  int_pc3
  ext_pc3
  tot_pc3
  depd_pc3
  anxd_pc3
  adhd_pc3
  odd_pc3
  cd_pc3
  anx2
  wth2
  att2
  rb2
  agg2
  int2
  ext2
  tot2
  anx3
  wth3
  att3
  rb3
  agg3
  int3
  ext3
  tot3
  eatq_ef
  eea1
  ee2
  res_a
  res_p
  res_s
  roi1-roi154;
  
  USEVARIABLES ARE pym [[predictor]]
  age sex race acq euler;
  
  CLUSTER = famid;
  
  MISSING ARE ALL (-999);
  
ANALYSIS:
  TYPE = COMPLEX;
  ESTIMATOR = MLR;
  ITERATIONS = 10000;
	CONVERGENCE = 0.00005;
	
DEFINE:
  euler = euler / 1000;

MODEL:
  pym ON [[predictor]]
  age sex race acq euler;
  
  [pym [[predictor]]
  age sex race acq euler];
  
OUTPUT:
  sampstat cinterval standardized modindices TECH1 TECH3;
"

predictors <- c("ses1", "ses2", "thr1", "thr2",
                "inc1", "momed1", "daded1", "adi1",
                "inc2", "pced2", "aced2", "adi2",
                "cpic1", "peqm1", "peqt1", "cvc1",
                "cpic2", "peqpc2", "peqac2", "cvc2")

for (pred in predictors) {
  model_string <- gsub("\\[\\[predictor\\]\\]", pred, template)
  
  file_name <- paste0("./analyses/mplus/step1c_envXpace_allCovars/envXpace_", pred, ".inp")
  writeLines(model_string, con = file_name)
}

runModels(target = "./analyses/mplus/step1c_envXpace_allCovars", log = NULL)
model_results <- readModels(target = "./analyses/mplus/step1c_envXpace_allCovars/")

summary_df_unstd <- data.frame()
summary_df_std <- data.frame()
summary_df_ci <- data.frame()

for (i in seq_along(model_results)) {
  
  model_output <- model_results[[i]]
  
  unstd <- subset(model_output$parameters$unstandardized, grepl("ON", paramHeader))
  stdyx <- subset(model_output$parameters$stdyx.standardized, grepl("ON", paramHeader))
  stdyx_ci <- subset(model_output$parameters$ci.stdyx.standardized, grepl("ON", paramHeader))
  
  # Add to summary data frame
  summary_df_unstd <- rbind(summary_df_unstd, unstd)
  summary_df_std <- rbind(summary_df_std, stdyx)
  summary_df_ci <- rbind(summary_df_ci, stdyx_ci)
  
}

write.csv(summary_df_unstd, file = "./analyses/mplus/step1c_envXpace_allCovars_unstd.csv", row.names = FALSE)
write.csv(summary_df_std, file = "./analyses/mplus/step1c_envXpace_allCovars_std.csv", row.names = FALSE)
write.csv(summary_df_ci, file = "./analyses/mplus/step1c_envXpace_allCovars_ci.csv", row.names = FALSE)


#### 10. Mplus - Compare Childhood versus Adolescence ####
library(tidyverse)

mplus_data <- all_data %>%
  dplyr::select(sub, famid, brainAgeGap_Pyment_mtwins, threatComposite_final_child, threatComposite_final_adol) %>%
  mutate(across(everything(), as.vector)) %>%
  clean_names()

mplus_data_long <- mplus_data %>%
  pivot_longer(
    cols = c("threat_composite_final_child", "threat_composite_final_adol"),
    names_to = "period",
    values_to = "threat") %>%
  mutate(period = ifelse(period == "threat_composite_final_child", 0, 1))

mplus_data_long[is.na(mplus_data_long)] <- -999
write.table(mplus_data_long, file = "./analyses/mplus/step1d_childVadol/envXpace_AllData.csv",
            sep = ",", col.names = FALSE, row.names = FALSE)

df_wide <- mplus_data_long %>%
  pivot_wider(
    id_cols = c(sub, famid, brain_age_gap_pyment_mtwins),
    names_from = period,
    values_from = threat,
    names_prefix = "thr"
  )
write.table(df_wide, file = "./analyses/mplus/step1d_childVadol/envXpace_AllData_Wide.csv",
            sep = ",", col.names = FALSE, row.names = FALSE)


template <- "
TITLE: Environment X Pace of Brain Development;

DATA:
	FILE IS envXpace_AllData_Wide.csv;
	FORMAT IS free;
	TYPE IS individual;

VARIABLE:
  NAMES ARE
  sub
  famid
  pym
  thr1
  thr2;
  
  USEVARIABLES ARE pym thr1 thr2;
  
  CLUSTER = famid;
  
  MISSING ARE ALL (-999);
  
ANALYSIS:
  TYPE = COMPLEX;
  ESTIMATOR = MLR;
  ITERATIONS = 10000;
	CONVERGENCE = 0.00005;

MODEL:
  pym ON thr1 thr2;
  
  [pym thr1 thr2];
  
OUTPUT:
  sampstat cinterval standardized modindices TECH1 TECH3;
"

writeLines(template, con = "./analyses/mplus/step1d_childVadol/envXpace_childVadol_Wide.inp")
runModels(target = "./analyses/mplus/step1d_childVadol/", log = NULL)


template <- "
TITLE: Environment X Pace of Brain Development;

DATA:
	FILE IS envXpace_AllData.csv;
	FORMAT IS free;
	TYPE IS individual;

VARIABLE:
  NAMES ARE
  sub
  famid
  pym
  dev
  thr;
  
  USEVARIABLES ARE pym thr;
  
  GROUPING = dev (0 = child, 1 = adol);
  
  CLUSTER = famid;
  
  MISSING ARE ALL (-999);
  
ANALYSIS:
  TYPE = COMPLEX;
  ESTIMATOR = MLR;
  ITERATIONS = 10000;
	CONVERGENCE = 0.00005;

MODEL:
  pym ON thr;
  
  MODEL child:
  pym ON thr (a);
  
  MODEL adol:
  pym ON thr (b);
  
  MODEL TEST:
  0 = a - b;
  
OUTPUT:
  sampstat cinterval standardized modindices TECH1 TECH3;
"

writeLines(template, con = "./analyses/mplus/step1d_childVadol/envXpace_childVadol.inp")
runModels(target = "./analyses/mplus/step1d_childVadol/", log = NULL)



#### 12. Structured Life-Course Modeling Approach ####
source("./slcma R functions v0.5.R")

slcma_data <- all_data %>% dplyr::select(sub, famid, wav2_twin_age, gender, race, acq, totalSurfaceHoles,
                                         brainAgeGap_Drobinin_mtwins, brainAgeGap_Pyment_mtwins, centilebrain_BAG,
                                         sesComposite_mean_final_child, sesComposite_mean_final_adol,
                                         threatComposite_final_child, threatComposite_final_adol,
                                         wav1_famincome_reverseZ_child, wav1_momEducation_reverseZ_child, 
                                         wav1_dadEducation_reverseZ_child,
                                         wav1_nbhadiZ_child, wav2_famincome_reverseZ_adol, wav2_pc_education_reverseZ_adol,
                                         wav2_ac_education_reverseZ_adol, wav2_nbhadiZ_adol,
                                         cpic_meanZ_child, peqConflict_MomReport_meanZ_child, peqConflict_TwinOnMom_meanZ_child,
                                         cvc_meanZ_child, cpic_meanZ_adol, peqConflict_PC_meanZ_adol, 
                                         peqConflict_twinOnPC_meanZ_adol,
                                         cvc_meanZ_adol, wav2_kkintr, wav2_kkextr, wav2_kktotr,
                                         eea_1, eea_2, gng_go_sdrt_1, gng_go_sdrt_2,
                                         Academic.Resilience, Psych.Resilience, Social.Resilience)

all_data$gender <- factor(all_data$gender, levels = c(0,1))
all_data$race <- factor(all_data$race, levels = c(0,1))
all_data$acq <- factor(all_data$acq, levels = c(0,1))

threat_data <- all_data %>% dplyr::select(brainAgeGap_Pyment_mtwins, threatComposite_final_child, threatComposite_final_adol,
                                          wav2_twin_age, gender, race, acq, totalSurfaceHoles)
threat_imp <- mice(threat_data, m = 20, maxit = 25, seed = 150)

threat_slcma <- slcma(brainAgeGap_Pyment_mtwins ~ threatComposite_final_child + threatComposite_final_adol +
                        Accumulation(threatComposite_final_child, threatComposite_final_adol), data = threat_imp)

summary.slcma(threat_slcma)
plot.slcma(threat_slcma, labels = c("Childhood", "Adolescence", "Accumulation"),
           cex.main = 0.9, cex.lab = 0.9, cex.axis = 0.9)

slcmaInfer(threat_slcma, 1, method = "selectiveInference")
slcmaInfer(threat_slcma, 1, method = "relaxed")


## Standardize
all_data$threatComposite_final_child <- as.numeric(scale(all_data$threatComposite_final_child, center = TRUE, scale = TRUE))
all_data$threatComposite_final_adol <- as.numeric(scale(all_data$threatComposite_final_adol, center = TRUE, scale = TRUE))
all_data$brainAgeGap_Pyment_mtwins <- as.numeric(scale(all_data$brainAgeGap_Pyment_mtwins, center = TRUE, scale = TRUE))
all_data$wav2_twin_age <- as.numeric(scale(all_data$wav2_twin_age, center = TRUE, scale = TRUE))
all_data$totalSurfaceHoles <- as.numeric(scale(all_data$totalSurfaceHoles, center = TRUE, scale = TRUE))



#### 4. Prepare Data for SPSS (Co-Twin Control) ####
# 1 = MZ, 2 = DZ
all_data$twinid <- ifelse(endsWith(all_data$sub, "1"), 0, 1)

spss_data <- all_data %>%
  dplyr::select(c(sub, twinid, famid, zygosity, wav2_twin_age, gender, race, acq.x, totalSurfaceHoles,
                  brainAgeGap_Drobinin_mtwins, brainAgeGap_Pyment_mtwins,
                  threatComposite_final, cpic_meanZ, peqConflict_MomReport_meanZ, peqConflict_DadReport_meanZ,
                  peqConflict_TwinOnMom_meanZ, peqConflict_TwinOnDad_meanZ, cvc_meanZ,
                  wav2_kkintr, wav2_kkextr, wav3_kkintr, wav3_kkextr,
                  int, ext, p, eea_1, eea_2,
                  Academic.Resilience, Psych.Resilience, Social.Resilience)) %>%
  mutate(across(everything(), as.vector)) %>%
  clean_names()

colnames(spss_data)[colnames(spss_data) == "famid"] <- "famidun"
colnames(spss_data)[colnames(spss_data) == "zygosity"] <- "zygos"
colnames(spss_data)[colnames(spss_data) == "brain_age_gap_pyment_mtwins"] <- "bag"
colnames(spss_data)[colnames(spss_data) == "threat_composite_final"] <- "threat"
spss_data[is.na(spss_data)] <- -999

write.csv(spss_data, file = "./analyses/cotwin/envXpace_threatXbrainage.csv", row.names = FALSE)

