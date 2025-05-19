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

#### Three voter types ---------------------------------------------------------
# Define threshold
tau <- 6

# Create matrix of PTV scores
ptv_matrix <- as.matrix(df_nl[, c("p_vvd", "p_pvv", "p_cda", "p_d66", 
                                  "p_gl", "p_sp", "p_pvda", "p_cu", "p_fvd")])

# Count number of parties considered (PTV >= tau)
df_nl$cs_size <- rowSums(ptv_matrix >= tau, na.rm = TRUE)

# Define group indicators
df_nl$cs_empty    <- df_nl$cs_size == 0    # No party meets threshold
df_nl$cs_single   <- df_nl$cs_size == 1    # Exactly one party
df_nl$cs_multiple <- df_nl$cs_size >= 2    # Two or more parties

# Summary statistics
summary(df_nl$cs_size)                     # Summary of set sizes
table(df_nl$cs_size)                       # Frequency of exact sizes

# Group sizes
n_empty    <- sum(df_nl$cs_empty, na.rm = TRUE)
n_single   <- sum(df_nl$cs_single, na.rm = TRUE)
n_multiple <- sum(df_nl$cs_multiple, na.rm = TRUE)
n_total    <- n_empty + n_single + n_multiple

# Proportions
p_empty    <- n_empty / n_total
p_single   <- n_single / n_total
p_multiple <- n_multiple / n_total

# Print results
cat("Empty set   :", n_empty, "(", round(100 * p_empty, 1), "%)\n")
cat("Single party:", n_single, "(", round(100 * p_single, 1), "%)\n")
cat("Multiple    :", n_multiple, "(", round(100 * p_multiple, 1), "%)\n")




#### Bi-partitions: Calculation of separation scores ----------------------------------------
# 1. Filter to respondents with at least two parties in CS
df_multi <- df_nl[df_nl$cs_size >= 2, ]
ptv_vars <- c("p_vvd", "p_pvv", "p_cda", "p_d66", 
              "p_gl", "p_sp", "p_pvda", "p_cu", "p_fvd")
party_labels <- ptv_vars
J <- length(party_labels)

# 2. Define all unique, unordered, non-trivial bipartitions
library(gtools)
all_partitions <- list()

for (k in 1:(J - 1)) {
  combos <- combinations(J, k)
  for (r in 1:nrow(combos)) {
    group1 <- combos[r, ]
    group2 <- setdiff(1:J, group1)
    # Enforce uniqueness by keeping only one of each unordered pair
    if (min(group1) < min(group2)) {
      all_partitions[[length(all_partitions) + 1]] <- list(group1 = group1, group2 = group2)
    }
  }
}

C <- length(all_partitions)  # should equal 255

# 3. Compute normalized separation scores for each individual and cleavage
results <- matrix(NA, nrow = nrow(df_multi), ncol = C)

for (i in 1:nrow(df_multi)) {
  v_i <- as.numeric(df_multi[i, ptv_vars])
  
  # Distance matrix using absolute deviations
  D_i <- abs(outer(v_i, v_i, "-"))
  
  for (c in 1:C) {
    g1 <- all_partitions[[c]]$group1
    g2 <- all_partitions[[c]]$group2
    
    # Between-cluster distances
    D_btw <- D_i[g1, g2]
    bar_D_btw <- mean(D_btw, na.rm = TRUE)
    
    # Within-cluster distances
    D_in_1 <- D_i[g1, g1]
    D_in_2 <- D_i[g2, g2]
    
    bar_D_in_1 <- mean(D_in_1[lower.tri(D_in_1)], na.rm = TRUE)
    bar_D_in_2 <- mean(D_in_2[lower.tri(D_in_2)], na.rm = TRUE)
    bar_D_in <- 0.5 * (bar_D_in_1 + bar_D_in_2)
    
    # Normalized separation score
    denom <- max(bar_D_btw, bar_D_in)
    if (!is.na(denom) && denom > 0) {
      results[i, c] <- (bar_D_btw - bar_D_in) / denom
    } else {
      results[i, c] <- NA
    }
  }
}



#### Bi-partitions: Assigning meaning to partitions -----------------------------------------
# Create cleavage metadata
cleavage_meta <- data.frame(
  cleavage_id = 1:length(all_partitions),
  group1 = sapply(all_partitions, function(x) paste(ptv_vars[x$group1], collapse = "|")),
  group2 = sapply(all_partitions, function(x) paste(ptv_vars[x$group2], collapse = "|")),
  stringsAsFactors = FALSE
)
df_long <- melt(results, varnames = c("respondent_id", "cleavage_id"), value.name = "separation_score")
df_long$cleavage_id <- as.integer(gsub("c_", "", df_long$cleavage_id))
df_long <- merge(df_long, cleavage_meta, by = "cleavage_id", all.x = TRUE)


#### Bi-partitions: Aggregate separation scores -----------------------------------------------------------------------

## Index 1: Aggregate separation scores by cleavage
agg_scores <- aggregate(separation_score ~ cleavage_id + group1 + group2, 
                        data = df_long, 
                        FUN = mean, 
                        na.rm = TRUE)

# Order by average separation score (descending)
agg_scores <- agg_scores[order(-agg_scores$separation_score), ]

# Select top 5 cleavages
top5_cleavages <- head(agg_scores, 5)

# Print result
print(top5_cleavages)

## Index 2: Identify the cleavage with the highest separation score
library(data.table)
dt <- as.data.table(df_long)

# Remove rows with NA scores to avoid issues in max selection
dt_nonmissing <- dt[!is.na(separation_score)]

# For each respondent, keep only the cleavage(s) with the max score
dt_best <- dt_nonmissing[dt_nonmissing[, .I[separation_score == max(separation_score)], by = respondent_id]$V1]

# Use aggregate to count how many times each cleavage is selected
cleavage_counts <- aggregate(respondent_id ~ cleavage_id + group1 + group2, 
                             data = dt_best, 
                             FUN = length)

# Compute share by dividing by total number of respondents
total_N <- length(unique(df_long$respondent_id))
cleavage_counts$share <- cleavage_counts$respondent_id / total_N

# Order by share descending
cleavage_counts <- cleavage_counts[order(-cleavage_counts$share), ]

# Rrename for clarity
names(cleavage_counts)[names(cleavage_counts) == "respondent_id"] <- "count"

# View top 5
print(head(cleavage_counts, 5))



#### Tri-partitions: Calculation of separation scores --------------------------------------------------------------------

## Generate all unique tri-partitions (unordered)
generate_tri_partitions <- function(J) {
  canonicalize_tri_partition <- function(partition) {
    g1 <- sort(partition$group1)
    g2 <- sort(partition$group2)
    g3 <- sort(partition$group3)
    group_strings <- sort(sapply(list(g1, g2, g3), paste, collapse = "-"))
    paste(group_strings, collapse = " || ")
  }
  
  all_tri_partitions <- list()
  seen_partitions <- character()
  idx <- 1
  parties <- 1:J
  
  for (k1 in 1:(J - 2)) {
    group1_combos <- combn(parties, k1, simplify = FALSE)
    for (g1 in group1_combos) {
      remaining1 <- setdiff(parties, g1)
      for (k2 in 1:(length(remaining1) - 1)) {
        group2_combos <- combn(remaining1, k2, simplify = FALSE)
        for (g2 in group2_combos) {
          g3 <- setdiff(remaining1, g2)
          if (length(g3) >= 1) {
            part <- list(group1 = g1, group2 = g2, group3 = g3)
            key <- canonicalize_tri_partition(part)
            if (!(key %in% seen_partitions)) {
              all_tri_partitions[[idx]] <- part
              seen_partitions <- c(seen_partitions, key)
              idx <- idx + 1
            }
          }
        }
      }
    }
  }
  
  return(all_tri_partitions)
}

# Generate unique tri-partitions
all_tri_partitions <- generate_tri_partitions(J = 9)
T_len <- length(all_tri_partitions)

## Compute separation scores for each tri-partition and respondent
results_tri <- matrix(NA, nrow = nrow(df_multi), ncol = T_len)

for (i in 1:nrow(df_multi)) {
  v_i <- as.numeric(df_multi[i, ptv_vars])
  D_i <- abs(outer(v_i, v_i, "-"))
  
  for (t in 1:T_len) {
    g1 <- all_tri_partitions[[t]]$group1
    g2 <- all_tri_partitions[[t]]$group2
    g3 <- all_tri_partitions[[t]]$group3
    
    D_btw <- c(D_i[g1, g2], D_i[g1, g3], D_i[g2, g3])
    bar_D_btw <- mean(D_btw, na.rm = TRUE)
    
    D_in_1 <- D_i[g1, g1]
    D_in_2 <- D_i[g2, g2]
    D_in_3 <- D_i[g3, g3]
    
    bar_D_in_1 <- mean(D_in_1[lower.tri(D_in_1)], na.rm = TRUE)
    bar_D_in_2 <- mean(D_in_2[lower.tri(D_in_2)], na.rm = TRUE)
    bar_D_in_3 <- mean(D_in_3[lower.tri(D_in_3)], na.rm = TRUE)
    
    bar_D_in <- mean(c(bar_D_in_1, bar_D_in_2, bar_D_in_3), na.rm = TRUE)
    
    denom <- max(bar_D_btw, bar_D_in)
    if (!is.na(denom) && denom > 0) {
      results_tri[i, t] <- (bar_D_btw - bar_D_in) / denom
    }
  }
}


#### Tri-partitions: Assigning meaning to partitions ---------------------

# Create tri-partition metadata
tri_meta <- data.frame(
  tri_id = 1:length(all_tri_partitions),
  group1 = sapply(all_tri_partitions, function(x) paste(ptv_vars[x$group1], collapse = "|")),
  group2 = sapply(all_tri_partitions, function(x) paste(ptv_vars[x$group2], collapse = "|")),
  group3 = sapply(all_tri_partitions, function(x) paste(ptv_vars[x$group3], collapse = "|")),
  stringsAsFactors = FALSE
)

# Reshape results_tri to long format
library(reshape2)
colnames(results_tri) <- paste0("t_", 1:ncol(results_tri))
results_tri_df <- data.frame(respondent_id = 1:nrow(results_tri), results_tri)
df_tri_long <- melt(results_tri_df, id.vars = "respondent_id", 
                    variable.name = "tri_id_str", value.name = "separation_score")
df_tri_long$tri_id <- as.integer(gsub("t_", "", df_tri_long$tri_id_str))

# Merge with tri-partition metadata
df_tri_long <- merge(df_tri_long, tri_meta, by.x = "tri_id", by.y = "tri_id", all.x = TRUE)



#### Aggregate separation scores--------------------------------------------------------------------

# Index 1: Average separation score across all individuals
tri_scores_avg <- aggregate(separation_score ~ tri_id + group1 + group2 + group3,
                            data = df_tri_long,
                            FUN = mean, na.rm = TRUE)

# Order by highest average separation score
tri_scores_avg <- tri_scores_avg[order(-tri_scores_avg$separation_score), ]

# View top 5 tri-partitions by average score
print(head(tri_scores_avg, 5))


# Index 2: Modal assignment — which tri-partition is best-fitting for each respondent
library(data.table)
dt_tri <- as.data.table(df_tri_long)

# Remove missing values
dt_tri_nonmissing <- dt_tri[!is.na(separation_score)]

# For each respondent, select tri-partition(s) with max score
dt_tri_best <- dt_tri_nonmissing[
  dt_tri_nonmissing[, .I[separation_score == max(separation_score)], by = respondent_id]$V1
]

# Count frequency of each best-fitting tri-partition
tri_counts <- aggregate(respondent_id ~ tri_id + group1 + group2 + group3,
                        data = dt_tri_best,
                        FUN = length)

# Compute share of each tri-partition
total_respondents <- length(unique(df_tri_long$respondent_id))
tri_counts$share <- tri_counts$respondent_id / total_respondents

# Rename column
names(tri_counts)[names(tri_counts) == "respondent_id"] <- "count"

# Order by share
tri_counts <- tri_counts[order(-tri_counts$share), ]

# View top 5 tri-partitions by frequency
print(head(tri_counts, 5))


