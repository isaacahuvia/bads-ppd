############################
##  BADS-PPD Interaction  ##
############################

#### Setup ####
## Load packages
library(tidyverse)
library(here)
library(RVAideMemoire)
library(scales)

## Load data
dat2 <- readRDS(here("data", "T2T Data for Pinder et al.rds"))

####  Prepare Data  ####
## Select relevant variables
names(dat2)
dat2$yb_cdi_25
dat2 %>%
  mutate(cdi_total)
dat1 %>%
  count(race_eth) %>%
  mutate(pct = percent(n / sum(n)))







dat1 <- dat1 %>%
  
  # Select relevant variables
  select(pb_childethnicity, pb_childsex, pb_childgender, pb_childage, pb_income, 
         yb_permanence, yb_cause_brain, yb_cause_env,
         yb_bads_3, yb_bads_4, yb_bads_5, yb_bads_7, yb_bads_8, yb_bads_9, 
         yb_bads_10,yb_bads_11, yb_bads_12, yb_bads_13, yb_bads_14, yb_bads_15, 
         yb_bads_23, yb_bads_24, yb_bads_25) %>%
  
  # Rename variables
  rename(race_eth = pb_childethnicity,
         sex = pb_childsex,
         gender = pb_childgender,
         age = pb_childage,
         income = pb_income,
         ppd_perm = yb_permanence,
         ppd_brain = yb_cause_brain,
         ppd_env = yb_cause_env)

## Inspect NAs
sapply(dat1, function(x) sum(is.na(x)))
# No missing demographic info
# Look at NAs in analysis variables
names(dat1)
dat1[6:10]
# 2 subjects NA for all 3 ppd vars - drop now (analyses impossible)
dat1 <- subset(dat1, dat1$ppd_perm != "NA")
dat1$ppd_perm
# 4 others missing some vars, exclude from analyses pairwise

## Create composite BADS subscale variables
dat1$bads_act <- dat1$yb_bads_3 + dat1$yb_bads_4 + dat1$yb_bads_5 +
  dat1$yb_bads_7 + dat1$yb_bads_11 + dat1$yb_bads_12 + 
  dat1$yb_bads_23

dat1$bads_avr <- dat1$yb_bads_8 + dat1$yb_bads_9 + dat1$yb_bads_10 +
  dat1$yb_bads_13 + dat1$yb_bads_14 + dat1$yb_bads_15 + 
  dat1$yb_bads_24 + dat1$yb_bads_25
# Cases with a missing item value get a missing subscale value
# Remove individual BADS item variables
dat1 <- dat1 %>%
  select(-matches("yb_bads_[0-9]"))

## Get demographic details
# Write function for tables
freq_tab_with_perc <- function(x) {
  tab <- as.data.frame(table(x)) %>%
    mutate(Perc = (Freq/sum(Freq))*100) %>%
    mutate_if(is.numeric, round, 2)
  return(tab)
}
# Generate tables
(race_tab <- freq_tab_with_perc(dat1$race_eth))
(sex_tab <- freq_tab_with_perc(dat1$sex))
(gender_tab <- freq_tab_with_perc(dat1$gender))
(income_tab <- freq_tab_with_perc(dat1$income))
(age_tab <- freq_tab_with_perc(dat1$age))
mean(dat1$age)
sd(dat1$age)

# Another way
dat1 %>%
  count(race_eth) %>%
  mutate(pct = percent(n / sum(n)))

## Recode variables
# Recode race/ethnicity
dat1 <- dat1 %>% 
  mutate(race_eth = recode(
    race_eth,
    "American Indian and/or Alaska Native" = "AI/AN",
    "Black or African American" = "Black",
    "More than one race" = "Other/Mult",
    "White, non-Hispanic (includes Middle Eastern)" = "White",
    "Asian (including Asian Desi and Pacific Islander)" = "AA/PI",
    "Hispanic or Latino/a" = "Hisp/Lat",
    "Other, please specify" = "Other/Mult"))
# Recode gender variable (cross-ref w/sex)
table(dat1$gender)
table(dat1$sex)
dat1 = dat1 %>% mutate(gender = case_when(
  sex == "female" & gender == "Woman" ~ "Cis Woman",
  sex == "male" & gender == "Man" ~ "Cis Man",
  TRUE ~ "Other"))
freq_tab_with_perc(dat1$gender)
# Remove original sex variable & reorder columns
dat1 <- subset(dat1, select = c("ppd_perm", "ppd_brain", "ppd_env",
                                "bads_act", "bads_avr",
                                "race_eth", "gender", "age", "income"))
#Check data
View(dat1)

## Median split age and income
# Function to determine whether median should be included in upper or lower
median_in_lower <- function(x) {
  x <- as.integer(as.character(x))
  var_median <- median(x)
  print(var_median)
  even_split <- sum(table(x))/2
  var_freq <- as.data.frame(table(x))
  var_freq$x <- as.integer(as.character((var_freq$x)))
  med_in_lower = var_freq[var_freq$x <= var_median,]
  med_in_upper = var_freq[var_freq$x < var_median,]
  return(abs(even_split - sum(med_in_lower$Freq)) < abs(even_split - sum(med_in_upper$Freq)))
}
# Function to split variable at median per previous
median_split_variable <- function(x, median_in_lower) {
  x <- as.integer(as.character(x))
  var_median <- median(x)
  ifelse(median_in_lower == TRUE,
         new_var <- ifelse(x <= var_median, "Below Median", "Above Median"),
         new_var <- ifelse(x < var_median, "Below Median", "Above Median"))
  return(new_var)
}
names(dat1)
# Split age at median
(med_in_lower <- median_in_lower(dat1$age))
dat1$age_split <- median_split_variable(dat1$age, med_in_lower)
# Check 
count(dat1, age, age_split)

# Clean up income variable
table(dat1$income)
dat1$income <- dat1$income %>%
  gsub("\\-.*$", "", .) %>% #See https://regex101.com/
  gsub("[^0-9]", "", .) %>%
  as.numeric()

# Split income at median
table(dat1$income)
(med_in_lower <- median_in_lower(dat1$income))
dat1$income_split <- median_split_variable(dat1$income, med_in_lower)
count(dat1, income, income_split)
dat1 <- subset(dat1, select = -c(income))

## Look at numeric variables
# Get means and sds
(means <- sapply(dat1[1:5], mean, na.rm = TRUE))
(sds <- sapply(dat1[1:5], sd, na.rm = TRUE))
(means_and_sds <- round(data.frame(means, sds), digits = 2))

# Another way
dat1 %>%
  pivot_longer(where(is.numeric)) %>%
  group_by(name) %>%
  summarize(mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T))

## Determine which demographic vars have 2 adequately sized subgroups
table(dat1$race_eth)
# No - drop from analyses
table(dat1$gender)
# Compare cis man & cis woman only
table(dat1$age_split)
# Yes
table(dat1$income)
# Yes
# Secondary analyses will compare cis man/woman, younger/older, & higher/lower income

####  Main Analysis  ####
# ppd_perm & bads_act
cor.test(dat1$ppd_perm, dat1$bads_act, method = "spearman", exact = FALSE)
spearman.ci(dat1$ppd_perm, dat1$bads_act)
# ppd_perm & bads_avr
cor.test(dat1$ppd_perm, dat1$bads_avr, method = "spearman", exact = FALSE)
spearman.ci(dat1$ppd_perm, dat1$bads_avr)

# ppd_brain & bads_act
(p1 <- cor.test(dat1$ppd_brain, dat1$bads_act, method = "spearman", exact = FALSE))
spearman.ci(dat1$ppd_brain, dat1$bads_act)
# ppd_brain & bads_avr
(p2 <- cor.test(dat1$ppd_brain, dat1$bads_avr, method = "spearman", exact = FALSE))
spearman.ci(dat1$ppd_brain, dat1$bads_avr)
# ppd_env & bads_act
(p3 <- cor.test(dat1$ppd_env, dat1$bads_act, method = "spearman", exact = FALSE))
spearman.ci(dat1$ppd_env, dat1$bads_act)
# ppd_env & bads_avr
(p4 <- cor.test(dat1$ppd_env, dat1$bads_avr, method = "spearman", exact = FALSE))
spearman.ci(dat1$ppd_env, dat1$bads_avr)

## Adjust p-values
ps <- c(p1$p.value, p2$p.value, p3$p.value, p4$p.value)
round(p.adjust(ps, "fdr"), 3)