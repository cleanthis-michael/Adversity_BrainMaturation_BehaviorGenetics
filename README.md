# Adversity_BrainMaturation_BehaviorGenetics
Delineating the etiology of associations between adversity exposure and brain maturation
This study uses the Michigan Twin Neurogenetics Study (MTwiNS), a population-based cohort of twins recruited from lower-income neighborhoods in Southeast Michigan. We characterize how adversity exposure during childhood and adolescence is associated with brain age gaps (a potential proxy for brain maturation) during adolescence, and implement behavior genetic analyses to identify whether observed associations reflect environmental versus genetic pathways. Primary analyses use the Pyment algorithm to generate brain age and derive brain age gaps (Leonardsen, E. H., Peng, H., Kaufmann, T., Agartz, I., Andreassen, O. A., Celius, E. G., ... & Wang, Y. (2022). Deep neural networks learn general and clinically relevant representations of the ageing brain. NeuroImage, 256, 119210).

## Steps 1 & 2. Computation of Adversity Composites
Code snippets for the calculation of exposure to threat and socioeconomic disadvantage during childhood and adolescence.

## Step 3. Data Preparation and Analyses
Code snippet for preparation of data (e.g., integrating data frames) and Mplus analyses using MplusAutomation.

## Step 4. Generation of Plots
Code snippet for creation of figures.

## Step 5. Sensitivity Analyses - Motion Correction
Code snippet for preparation and analysis of data after exclusion of participants with lower image quality.

## Step 6. Sensitivity Analyses - Brain Age
Code snippet for preparation and analysis of data after creating a composite index of brain age. Sensitivity analyses integrate the Pyment algorithm with the Drobinin algorithm (Drobinin, V., Van Gestel, H., Helmick, C. A., Schmidt, M. H., Bowen, C. V., & Uher, R. (2022). The developmental brain age is associated with adversity, depression, and functional outcomes among adolescents. Biological Psychiatry: Cognitive Neuroscience and Neuroimaging, 7(4), 406-414) and Centile algorithm (Yu, Y., Cui, H. Q., Haas, S. S., New, F., Sanford, N., Yu, K., ... & ENIGMA‐Lifespan Working Group. (2024). Brain‐age prediction: Systematic evaluation of site effects, and sample age range and size. Human Brain Mapping, 45(10), e26768).

## Step 7. Sensitivity Analyses - Adversity Exposure
Code snippet for preparation and analysis of data after creating adversity composites using a data-driven, rather than theory-driven, approach.

## Step 8. Behavior Genetics
SPSS code implemented to conduct twin difference and co-twin control analyses characterizing the etiology of observed associations between adversity exposure and brain maturation.
