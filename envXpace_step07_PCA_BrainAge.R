#### Adversity X Brain Maturation ####
## Data-Driven Brain Age

rm(list=ls())

# Load Libraries
list.of.packages <- c("dplyr", "janitor", "ggplot2", "cowplot", "MplusAutomation", "GPArotation", "parameters", "EFAtools")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# Set Working Directory
library(rstudioapi)
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))
print( getwd() )


#### 1. Read Data ####
all_data <- data.frame(read.csv("./analyses/envXpace_alldata.csv", sep = ",", header = TRUE))

#### 2. Principal Component Analysis ####
brain_age <- all_data %>% dplyr::select(brainAgeGap_Drobinin, brainAgeGap_Pyment_mtwins, centilebrain_BAG)
KMO(brain_age)  # .60
pc3 <- principal(brain_age, nfactors = 3, rotate = "none"); pc3
qplot(c(1:3), pc3$values) +
  geom_line() +
  #geom_vline(xintercept = 2.5, linetype = "dashed", color = "black", size = 0.9) +
  scale_x_continuous(breaks = c(1:3)) +
  xlab("Principal Component") + ylab("Eigenvalue") + theme_bw() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 17))

n_principalComponents <- n_components(brain_age, type = "PCA", rotation = "none")
n_principalComponents  # n = 1

pc1 <- principal(brain_age, nfactors = 1, rotate = "oblimin",
                 normalize = TRUE, scores = TRUE)
pc1

#print.psych(pc2, cut = .3, sort = TRUE) #view pattern matrix, suppress at |.30| and sort
pca_scores <- data.frame(pc1$scores)
all_data$PCbag <- pca_scores$PC1

cor.test(all_data$PCbag, all_data$brainAgeGap_Drobinin)        # .64
cor.test(all_data$PCbag, all_data$brainAgeGap_Pyment_mtwins)   # .77
cor.test(all_data$PCbag, all_data$centilebrain_BAG)            # .78

#### 3. Adversity and Brain Maturation ####
## 3a. Threat X Brain Age
threat_brainage <- ggplot(aes(x = threatComposite_final_child, y = PCbag), data = all_data) +
  geom_point(size = 2.1, color = "#C9821E", alpha = 0.2) +
  geom_smooth(method = "lm", se = TRUE, color = "#C9821E") +
  theme_classic() +
  xlab("Threat Exposure\n(Childhood)") + ylab("Brain Age Gap (years)\nAdolescence") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 12))

xdens <- axis_canvas(threat_brainage, axis = "x") +
  geom_density(data = all_data, aes(x = threatComposite_final_child),
               color = "#D98C2B", fill = "#D98C2B", alpha = .5, size = 0.75)

ydens <- axis_canvas(threat_brainage, axis = "y", coord_flip = TRUE) +
  geom_density(data = all_data, aes(x = PCbag),
               color = "black", fill = "gray", alpha = .8, size = .75) + coord_flip()

p1_glob <- insert_xaxis_grob(threat_brainage, xdens, grid::unit(.2, "null"), position = "top")
p2_glob <- insert_yaxis_grob(p1_glob, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(p2_glob)


## 3b. SES X Brain Age
ses_brainage <- ggplot(aes(x = sesComposite_mean_final_child, y = PCbag), data = all_data) +
  geom_point(size = 1.7, color = "#4C9A6A", alpha = 0.25) +
  geom_smooth(method = "lm", se = TRUE, color = "#4C9A6A") +
  theme_classic() +
  xlab("Socioeconomic Disadvantage\n(Childhood)") + ylab("Brain Age Gap (years)\nAdolescence") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 12))

xdens <- axis_canvas(ses_brainage, axis = "x") +
  geom_density(data = all_data, aes(x = sesComposite_mean_final_child),
               color = "#4C9A6A", fill = "#4C9A6A", alpha = .5, size = 0.75)

ydens <- axis_canvas(ses_brainage, axis = "y", coord_flip = TRUE) +
  geom_density(data = all_data, aes(x = PCbag),
               color = "black", fill = "gray", alpha = .8, size = .75) + coord_flip()

p1_glob <- insert_xaxis_grob(ses_brainage, xdens, grid::unit(.2, "null"), position = "top")
p2_glob <- insert_yaxis_grob(p1_glob, ydens, grid::unit(.2, "null"), position = "right")
ggdraw(p2_glob)


#### 4. Prepare Data for Mplus ####
mplus_data <- all_data %>%
  dplyr::select(c(sub, famid, wav2_twin_age, gender, race, acq, totalSurfaceHoles,
                  PCbag,
                  sesComposite_mean_final_child, sesComposite_mean_final_adol,
                  threatComposite_final_child, threatComposite_final_adol,
                  wav1_famincome_reverseZ_child, wav1_momEducation_reverseZ_child, wav1_dadEducation_reverseZ_child,
                  wav1_nbhadiZ_child, wav2_famincome_reverseZ_adol, wav2_pc_education_reverseZ_adol,
                  wav2_ac_education_reverseZ_adol, wav2_nbhadiZ_adol,
                  cpic_meanZ_child, peqConflict_MomReport_meanZ_child, peqConflict_TwinOnMom_meanZ_child,
                  cvc_meanZ_child, cpic_meanZ_adol, peqConflict_PC_meanZ_adol, peqConflict_twinOnPC_meanZ_adol, cvc_meanZ_adol)) %>%
  mutate(across(everything(), as.vector)) %>%
  clean_names()

mplus_data[is.na(mplus_data)] <- -999
write.table(mplus_data, file = "./analyses/mplus_pcBAG/envXpace_AllData.csv", sep = ",", col.names = FALSE, row.names = FALSE)
write.table(mplus_data, file = "./analyses/mplus_pcBAG/step1a_envXpace/envXpace_AllData.csv", sep = ",", col.names = FALSE, row.names = FALSE)
write.table(mplus_data, file = "./analyses/mplus_pcBAG/step1b_envXpace_Demo/envXpace_AllData.csv", sep = ",", col.names = FALSE, row.names = FALSE)
write.table(mplus_data, file = "./analyses/mplus_pcBAG/step1c_envXpace_allCovars/envXpace_AllData.csv", sep = ",", col.names = FALSE, row.names = FALSE)


#### 5. Baseline Models - No Covariates ####
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
  bag
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
  
  USEVARIABLES ARE bag [[predictor]];
  
  CLUSTER = famid;
  
  MISSING ARE ALL (-999);
  
ANALYSIS:
  TYPE = COMPLEX;
  ESTIMATOR = MLR;
  ITERATIONS = 10000;
	CONVERGENCE = 0.00005;

MODEL:
  bag ON [[predictor]];
  
  [bag [[predictor]]];
  
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
  
  file_name <- paste0("./analyses/mplus_pcBAG/step1a_envXpace/envXpace_", pred, ".inp")
  writeLines(model_string, con = file_name)
}

runModels(target = "./analyses/mplus_pcBAG/step1a_envXpace", log = NULL)
model_results <- readModels(target = "./analyses/mplus_pcBAG/step1a_envXpace/")

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

write.csv(summary_df_unstd, file = "./analyses/mplus_pcBAG/step1a_envXpace_unstd.csv", row.names = FALSE)
write.csv(summary_df_std, file = "./analyses/mplus_pcBAG/step1a_envXpace_std.csv", row.names = FALSE)
write.csv(summary_df_ci, file = "./analyses/mplus_pcBAG/step1a_envXpace_ci.csv", row.names = FALSE)


#### 6. Demographic Covariates ####
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
  bag
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
  
  USEVARIABLES ARE bag [[predictor]]
  age sex race;
  
  CLUSTER = famid;
  
  MISSING ARE ALL (-999);
  
ANALYSIS:
  TYPE = COMPLEX;
  ESTIMATOR = MLR;
  ITERATIONS = 10000;
	CONVERGENCE = 0.00005;

MODEL:
  bag ON [[predictor]]
  age sex race;
  
  [bag [[predictor]]
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
  
  file_name <- paste0("./analyses/mplus_pcBAG/step1b_envXpace_Demo/envXpace_", pred, ".inp")
  writeLines(model_string, con = file_name)
}

runModels(target = "./analyses/mplus_pcBAG/step1b_envXpace_Demo", log = NULL)
model_results <- readModels(target = "./analyses/mplus_pcBAG/step1b_envXpace_Demo/")

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

write.csv(summary_df_unstd, file = "./analyses/mplus_pcBAG/step1b_envXpace_Demo_unstd.csv", row.names = FALSE)
write.csv(summary_df_std, file = "./analyses/mplus_pcBAG/step1b_envXpace_Demo_std.csv", row.names = FALSE)
write.csv(summary_df_ci, file = "./analyses/mplus_pcBAG/step1b_envXpace_Demo_ci.csv", row.names = FALSE)


#### 7. Neuroimaging Covariates ####
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
  bag
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
  
  USEVARIABLES ARE bag [[predictor]]
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
  bag ON [[predictor]]
  age sex race acq euler;
  
  [bag [[predictor]]
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
  
  file_name <- paste0("./analyses/mplus_pcBAG/step1c_envXpace_allCovars/envXpace_", pred, ".inp")
  writeLines(model_string, con = file_name)
}

runModels(target = "./analyses/mplus_pcBAG/step1c_envXpace_allCovars", log = NULL)
model_results <- readModels(target = "./analyses/mplus_pcBAG/step1c_envXpace_allCovars/")

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

write.csv(summary_df_unstd, file = "./analyses/mplus_pcBAG/step1c_envXpace_allCovars_unstd.csv", row.names = FALSE)
write.csv(summary_df_std, file = "./analyses/mplus_pcBAG/step1c_envXpace_allCovars_std.csv", row.names = FALSE)
write.csv(summary_df_ci, file = "./analyses/mplus_pcBAG/step1c_envXpace_allCovars_ci.csv", row.names = FALSE)
