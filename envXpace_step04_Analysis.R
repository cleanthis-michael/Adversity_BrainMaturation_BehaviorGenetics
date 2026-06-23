#### Adversity X Brain Maturation ####
## Step 04. Data Analysis

rm(list=ls())

# Load Libraries
list.of.packages <- c("dplyr", "haven", "stringr", "janitor", "MplusAutomation")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# Set Working Directory
library(rstudioapi)
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))
print( getwd() )


#### 1. Prepare Data ####
all_data <- data.frame(read.csv("./analyses/envXpace_AllData.csv"), sep = ",", header = TRUE)

mplus_data <- all_data %>% dplyr::select(
  sub, famid, wav2_twin_age, gender, race, acq, totalSurfaceHoles,
  brainAgeGap_Drobinin, brainAgeGap_Pyment_mtwins, centilebrain_BAG,
  sesComposite_mean_final_child, sesComposite_mean_final_adol,
  threatComposite_final_child, threatComposite_final_adol,
  wav1_famincome_reverseZ_child, wav1_momEducation_reverseZ_child, wav1_dadEducation_reverseZ_child,
  wav1_nbhadiZ_child, wav2_famincome_reverseZ_adol, wav2_pc_education_reverseZ_adol,
  wav2_ac_education_reverseZ_adol, wav2_nbhadiZ_adol,
  cpic_meanZ_child, peqConflict_MomReport_meanZ_child, peqConflict_TwinOnMom_meanZ_child,
  cvc_meanZ_child, cpic_meanZ_adol, peqConflict_PC_meanZ_adol, peqConflict_twinOnPC_meanZ_adol, cvc_meanZ_adol
)

mplus_data <- mplus_data %>% mutate(across(everything(), as.vector)) %>% clean_names()
mplus_data[is.na(mplus_data)] <- -999

write.table(mplus_data, file = "./analyses/mplus/envXpace_AllData.csv", sep = ",", col.names = FALSE, row.names = FALSE)
write.table(mplus_data, file = "./analyses/mplus/step1a_envXpace/envXpace_AllData.csv", sep = ",", col.names = FALSE, row.names = FALSE)
write.table(mplus_data, file = "./analyses/mplus/step1b_envXpace_Demo/envXpace_AllData.csv", sep = ",", col.names = FALSE, row.names = FALSE)
write.table(mplus_data, file = "./analyses/mplus/step1c_envXpace_allCovars/envXpace_AllData.csv", sep = ",", col.names = FALSE, row.names = FALSE)


#### 2. Baseline Models - No Covariates ####
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
  cvc2;
  
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


#### 3. Demographic Covariates ####
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
  cvc2;
  
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


#### 4. Neuroimaging Covariates ####
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
  cvc2;
  
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


#### 5. Childhood versus Adolescence ####
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


#### 6. Structured Life-Course Modeling Approach ####
source("./slcma R functions v0.5.R")

slcma_data <- all_data %>% dplyr::select(sub, famid, wav2_twin_age, gender, race, acq, totalSurfaceHoles,
                                         brainAgeGap_Drobinin, brainAgeGap_Pyment_mtwins, centilebrain_BAG,
                                         sesComposite_mean_final_child, sesComposite_mean_final_adol,
                                         threatComposite_final_child, threatComposite_final_adol,
                                         wav1_famincome_reverseZ_child, wav1_momEducation_reverseZ_child, 
                                         wav1_dadEducation_reverseZ_child,
                                         wav1_nbhadiZ_child, wav2_famincome_reverseZ_adol, wav2_pc_education_reverseZ_adol,
                                         wav2_ac_education_reverseZ_adol, wav2_nbhadiZ_adol,
                                         cpic_meanZ_child, peqConflict_MomReport_meanZ_child, peqConflict_TwinOnMom_meanZ_child,
                                         cvc_meanZ_child, cpic_meanZ_adol, peqConflict_PC_meanZ_adol, 
                                         peqConflict_twinOnPC_meanZ_adol,
                                         cvc_meanZ_adol)

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


#### 7. Prepare Data for SPSS (Behavior Genetics) ####
# 1 = MZ, 2 = DZ
all_data$twinid <- ifelse(endsWith(all_data$sub, "1"), 0, 1)

spss_data <- all_data %>%
  dplyr::select(c(sub, twinid, famid, zygosity, wav2_twin_age, gender, race, acq, totalSurfaceHoles,
                  brainAgeGap_Pyment_mtwins, threatComposite_final_child, cvc_meanZ_child)) %>%
  mutate(across(everything(), as.vector)) %>%
  clean_names()

colnames(spss_data)[colnames(spss_data) == "famid"] <- "famidun"
colnames(spss_data)[colnames(spss_data) == "zygosity"] <- "zygos"
colnames(spss_data)[colnames(spss_data) == "brain_age_gap_pyment_mtwins"] <- "bag"
colnames(spss_data)[colnames(spss_data) == "threat_composite_final"] <- "threat"
spss_data[is.na(spss_data)] <- -999

write.csv(spss_data, file = "./analyses/cotwin/envXpace_threatXbrainage.csv", row.names = FALSE)
