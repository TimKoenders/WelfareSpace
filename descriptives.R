#### Clean-up and packages ----------------------------------------------------------------
rm(list = ls())

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr",
              "plyr",                          
              "readr",
              "readxl",
              "tidyverse",
              "ggplot2",
              "stringr",
              "car",
              "lmtest",
              "lme4",
              "lmerTest",
              "MuMIn",
              "apaTables",
              "report",
              "lmerTest",
              "lme4",
              "lmtest",
              "apaTables",
              "MuMIn",
              "readxl",
              "influence.ME",
              "rstatix",
              "haven",
              "ggpubr",
              "GenderInfer",
              "foreign",
              "extrafont",
              "tidyr",
              "reshape2",
              "knitr",
              "Gmisch",
              "grid",
              "survey",
              "psych",
              "dplyr",
              "performance",
              "nnet",
              "sandwich",
              "clubSandwich",
              "mlogit",
              "dfidx",
              "clusterSEs",
              "ggrepel"
)
ipak(packages)


list.files()







#### Load data ----------------------------------------------------------
df_ess <- read_dta("ees_2019.dta")
df_ess <- df_ess[df_ess$hCountry %in% c(7, 8, 14, 15, 20, 26, 27, 28), ]




#### Organizing --------------------------------------------------------------
# Reset country variable
df_ess <- df_ess %>%
  filter(hCountry %in% c(7, 8, 14, 15, 20, 26, 27, 28)) %>%
  mutate(hCountry = case_when(
    hCountry == 7  ~ "Denmark",
    hCountry == 8  ~ "Germany",
    hCountry == 14 ~ "Ireland",
    hCountry == 15 ~ "Italy",
    hCountry == 20 ~ "Netherlands",
    hCountry == 26 ~ "Spain",
    hCountry == 27 ~ "Sweden",
    hCountry == 28 ~ "UK"
  ))
# Subset netherlands
df_nl <- df_ess %>% filter(hCountry == "Netherlands")
# Rename pvt-variables
names(df_nl)[names(df_nl) == "q10_1"] <- "p_vvd"
names(df_nl)[names(df_nl) == "q10_2"] <- "p_pvv"
names(df_nl)[names(df_nl) == "q10_3"] <- "p_cda"
names(df_nl)[names(df_nl) == "q10_4"] <- "p_d66"
names(df_nl)[names(df_nl) == "q10_5"] <- "p_gl"
names(df_nl)[names(df_nl) == "q10_6"] <- "p_sp"
names(df_nl)[names(df_nl) == "q10_7"] <- "p_pvda"
names(df_nl)[names(df_nl) == "q10_8"] <- "p_cu"
names(df_nl)[names(df_nl) == "q10_9"] <- "p_fvd"
# Set 98 naar NA
vars <- c("p_vvd", "p_pvv", "p_cda", "p_d66", "p_gl", "p_sp", "p_pvda", "p_cu", "p_fvd")
for (v in vars) {
  df_nl[[v]][df_nl[[v]] == 98] <- NA
}










#### Summary statistics ------------------------------------------------------
## Pvt-VVD
summary(df_nl$p_vvd)
# Pvt-PVV
summary(df_nl$p_pvv)
# Pvt-CDA
summary(df_nl$p_cda)
# Pvt-D66
summary(df_nl$p_d66)
# Pvt-GL
summary(df_nl$p_gl)
# Pvt-SP
summary(df_nl$p_sp)
# Pvt-PVDA
summary(df_nl$p_pvda)
# Pvt-CU
summary(df_nl$p_cu)
# Pvt-FVD
summary(df_nl$p_fvd)
