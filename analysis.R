#### Setup ####
# Load packages
library(tidyverse)
library(assertr)
library(here)

# Load data
dat1 <- readRDS(here("data", "T2T Data for Pinder et al.rds"))

####  Prepare Data  ####
# Select relevant variables
dat1 <- subset(dat1, select = c("pb_childethnicity", "pb_childsex",
                                "pb_childgender","pb_childage", "pb_income",
                                "yb_permanence","yb_cause_brain", "yb_cause_env",
                                "yb_bads_3", "yb_bads_4", "yb_bads_5",
                                "yb_bads_7", "yb_bads_8","yb_bads_9",
                                "yb_bads_10","yb_bads_11", "yb_bads_12",
                                "yb_bads_13", "yb_bads_14", "yb_bads_15",
                                "yb_bads_23", "yb_bads_24", "yb_bads_25"
))
table(dat1$pb_childethnicity)
table(dat1$pb_childsex)
table(dat1$pb_childgender)
table(dat1$pb_childgender)
table(dat1$pb_childgender)
table(dat1$pb_childage)
table(dat1$pb_income)
sum(table(dat1$yb_permanence))

sum(is.na(dat1$yb_permanence))

sum(table(dat1$pb_childgender))

sapply(dat1, function(x) sum(is.na(x)))

dat2 <- select(dat1, c("yb_permanence", "yb_cause_brain", "yb_cause_env",
                       "yb_bads_4", "yb_bads_7"))
dat2

# Create composite BADS subscale variables
dat1$bads_act <- dat1$yb_bads_3 + dat1$yb_bads_4 + dat1$yb_bads_5 +
                dat1$yb_bads_7 + dat1$yb_bads_11 + dat1$yb_bads_12 + 
                dat1$yb_bads_23

dat1$bads_avr <- dat1$yb_bads_8 + dat1$yb_bads_9 + dat1$yb_bads_10 +
                dat1$yb_bads_13 + dat1$yb_bads_14 + dat1$yb_bads_15 + 
                dat1$yb_bads_24 + dat1$yb_bads_25

# Remove individual BADS item variables
dat1 <- subset(dat1, select = c("pb_childethnicity", "pb_childsex",
                                 "pb_childgender", "pb_childage",
                                 "pb_income", "yb_permanence",
                                 "yb_cause_brain", "yb_cause_env",
                                 "bads_act", "bads_avr"
))

  
# Rename variables
dat1 <- dat1 %>% rename(race_eth = pb_childethnicity,
                        sex = pb_childsex,
                        gender = pb_childgender,
                        age = pb_childage,
                        income = pb_income,
                        ppd_perm = yb_permanence,
                        ppd_brain = yb_cause_brain,
                        ppd_env = yb_cause_env)

# Recode race/ethnicity variable
dat1 = dat1 %>% mutate(race_eth = recode(race_eth,
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
table(dat1$gender)

# Remove original sex variable & reorder columns
dat1 <- subset(dat1, select = c("ppd_perm", "ppd_brain", "ppd_env",
                                "bads_act", "bads_avr",
                                "race_eth", "gender", "age", "income"))

# Functions for median splits
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

median_split_variable <- function(x, median_in_lower) {
  x <- as.integer(as.character(x))
  var_median <- median(x)
  ifelse(median_in_lower == TRUE,
         new_var <- ifelse(x <= var_median, "Below Median", "Above Median"),
         new_var <- ifelse(x < var_median, "Below Median", "Above Median"))
  return(new_var)
}

# Split age at median
(med_in_lower <- median_in_lower(dat1$age))
dat1$age_split <- median_split_variable(dat1$age, med_in_lower)

dat1 <- subset(dat1, select = -c(age))
dat1

# Median split income
table(dat1$income)

incl_age_med = age_freq[age_freq$Var1 <= age_median,]
excl_age_med = age_freq[age_freq$Var1 < age_median,]
bool_incl_lower <- abs(eq_split - sum(incl_age_med$Freq)) < abs(eq_split - sum(excl_age_med$Freq))









dat1$age <- as.integer(dat1$age)
age_median <- median(dat1$age)
age_freq <- as.data.frame(table(dat1$age))
age_freq$Var1 <- as.integer(as.character((age_freq$Var1)))
eq_split <- sum(age_freq$Freq)/2

# Assess including median values in lower or upper split
incl_age_med = age_freq[age_freq$Var1 <= age_median,]
excl_age_med = age_freq[age_freq$Var1 < age_median,]
bool_incl_lower <- abs(eq_split - sum(incl_age_med$Freq)) < abs(eq_split - sum(excl_age_med$Freq))
# Include median in lower half







dat1$age_split[dat1$age < 15] <- "<=14"
dat1$age_split[dat1$age > 14] <- ">14"
table(dat1$age_split)
print(dat1, max = 100)


dat1 = dat1 %>% mutate(income_split = recode(income,
                                             "$0-$19,000" = "Lower",
                                             "$20,000-$39,000" = "Lower",
                                             "$40,000-$59,000" = "Lower",
                                             "$60,000-$79,000" = "Lower",
                                             "$80,000 - $99,000" = "Higher",
                                             "$100,000 - $119,000" = "Higher",
                                             "$120,000-$140,000" = "Higher",
                                             "$140,000+" = "Higher"))





dat1$ppd_perm <- as.numeric(dat1$ppd_perm)
dat1$ppd_brain <- as.numeric(dat1$ppd_brain)
dat1$ppd_env <- as.numeric(dat1$ppd_env)
dat1$bads_act <- as.numeric(dat1$bads_act)
dat1$bads_avr <- as.numeric(dat1$bads_avr)

hist(dat1$bads_act)
hist(dat1$bads_avr)
hist(dat1$ppd_perm)
hist(dat1$ppd_brain)
hist(dat1$ppd_env)



# Create median splits


dat1 <- subset(dat1, select = c("ppd_perm", "ppd_brain", "ppd_env",
                                "bads_act", "bads_avr",
                                "race_eth", "gender", "age_split", "income_split"
))


head(dat1)  




# Determine which demographic vars have adequately sized groups
table(dat1$race_eth)
#nope - DROP FROM ANALYSES
table(dat1$gender)
#man & woman only
table(dat1$age)
#yep
table(dat1$income)
#yep

# Secondary analyses will compare man/woman, younger/older, & higher/lower income








####  Analysis  ####
# Correlations
cor.test(dat1$ppd_perm, dat1$bads_act, method = "spearman")
#sig
cor.test(dat1$ppd_perm, dat1$bads_avr, method = "spearman")
#sig

cor.test(dat1$ppd_brain, dat1$bads_act, method = "spearman")
#ns
cor.test(dat1$ppd_brain, dat1$bads_avr, method = "spearman")
#ns

cor.test(dat1$ppd_env, dat1$bads_act, method = "spearman")
#ns
cor.test(dat1$ppd_env, dat1$bads_avr, method = "spearman")
#sig



## Demographic analyses
# Gender
dat1_women <- dat1[which(dat1$gender=="Woman"),]
dat1_men <- dat1[which(dat1$gender=="Man"),] 

cor.test(dat1_women$ppd_perm, dat1_women$bads_act, method = "spearman")
#sig
cor.test(dat1_men$ppd_perm, dat1_men$bads_act, method = "spearman")
#ns
cor.test(dat1_women$ppd_perm, dat1_women$bads_avr, method = "spearman")
#ns (marg)
cor.test(dat1_men$ppd_perm, dat1_men$bads_avr, method = "spearman")
#sig

cor.test(dat1_women$ppd_brain, dat1_women$bads_act, method = "spearman")
#ns
cor.test(dat1_men$ppd_brain, dat1_men$bads_act, method = "spearman")
#ns
cor.test(dat1_women$ppd_brain, dat1_women$bads_avr, method = "spearman")
#ns
cor.test(dat1_men$ppd_brain, dat1_men$bads_avr, method = "spearman")
#ns

cor.test(dat1_women$ppd_env, dat1_women$bads_act, method = "spearman")
#sig
cor.test(dat1_men$ppd_env, dat1_men$bads_act, method = "spearman")
#sig
cor.test(dat1_women$ppd_env, dat1_women$bads_avr, method = "spearman")
#ns
cor.test(dat1_men$ppd_env, dat1_men$bads_avr, method = "spearman")
#sig


# Age (median split)
dat1_younger <- dat1[which(dat1$age_split=="<=14"),]
count(dat1_younger)

dat1_older <- dat1[which(dat1$age_split==">14"),]
count(dat1_older)

cor.test(dat1_younger$ppd_perm, dat1_younger$bads_act, method = "spearman")
#ns (marg)
cor.test(dat1_older$ppd_perm, dat1_older$bads_act, method = "spearman")
#sig
cor.test(dat1_younger$ppd_perm, dat1_younger$bads_avr, method = "spearman")
#sig
cor.test(dat1_older$ppd_perm, dat1_older$bads_avr, method = "spearman")
#ns (marg)

cor.test(dat1_younger$ppd_brain, dat1_younger$bads_act, method = "spearman")
#ns
cor.test(dat1_older$ppd_brain, dat1_older$bads_act, method = "spearman")
#ns
cor.test(dat1_younger$ppd_brain, dat1_younger$bads_avr, method = "spearman")
#ns
cor.test(dat1_older$ppd_brain, dat1_older$bads_avr, method = "spearman")
#ns

cor.test(dat1_younger$ppd_env, dat1_younger$bads_act, method = "spearman")
#ns
cor.test(dat1_older$ppd_env, dat1_older$bads_act, method = "spearman")
#ns
cor.test(dat1_younger$ppd_env, dat1_younger$bads_avr, method = "spearman")
#ns
cor.test(dat1_older$ppd_env, dat1_older$bads_avr, method = "spearman")
#sig

# Income (median split)
dat1_higher <- dat1[which(dat1$income_split=="Higher"),]
dat1_higher
count(dat1_higher)

dat1_lower <- dat1[which(dat1$income_split=="Lower"),]
count(dat1_lower)

cor.test(dat1_higher$ppd_perm, dat1_higher$bads_act, method = "spearman")
#ns
cor.test(dat1_lower$ppd_perm, dat1_lower$bads_act, method = "spearman")
#sig
cor.test(dat1_higher$ppd_perm, dat1_higher$bads_avr, method = "spearman")
#ns (marg)
cor.test(dat1_lower$ppd_perm, dat1_lower$bads_avr, method = "spearman")
#sig

cor.test(dat1_higher$ppd_brain, dat1_higher$bads_act, method = "spearman")
#ns
cor.test(dat1_lower$ppd_brain, dat1_lower$bads_act, method = "spearman")
#ns
cor.test(dat1_higher$ppd_brain, dat1_higher$bads_avr, method = "spearman")
#ns
cor.test(dat1_lower$ppd_brain, dat1_lower$bads_avr, method = "spearman")
#ns (marg)

cor.test(dat1_higher$ppd_env, dat1_higher$bads_act, method = "spearman")
#ns
cor.test(dat1_lower$ppd_env, dat1_lower$bads_act, method = "spearman")
#ns
cor.test(dat1_higher$ppd_env, dat1_higher$bads_avr, method = "spearman")
#sig
cor.test(dat1_lower$ppd_env, dat1_lower$bads_avr, method = "spearman")
#sig





## Plots
#df %>%
  #ggplot() +
  #geom_histogram(aes(yb_bads_1))



#plot(dat1$bads_act, dat1$ppd_perm)
