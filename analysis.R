#### Setup ####
# Load packages
library(tidyverse)
library(assertr)
library(here)

#  Load data
dat1 <- readRDS(here("data", "T2T Data for Pinder et al.rds"))


####  Prepare Data  ####
dat2 <- subset(dat1, select = c("yb_lsmh_id", "pb_childethnicity", "pb_childgender",
                                "pb_childage", "pb_income", "yb_permanence",
                                "yb_cause_brain", "yb_cause_env", "yb_bads_3",
                                "yb_bads_4", "yb_bads_5", "yb_bads_7",
                                "yb_bads_8","yb_bads_9", "yb_bads_10",
                                "yb_bads_11", "yb_bads_12", "yb_bads_13",
                                "yb_bads_14", "yb_bads_15", "yb_bads_23",
                                "yb_bads_24", "yb_bads_25"
))

dat2$bads_act <- dat2$yb_bads_3 + dat2$yb_bads_4 + dat2$yb_bads_5 +
                dat2$yb_bads_7 + dat2$yb_bads_11 + dat2$yb_bads_12 + 
                dat2$yb_bads_23

dat2$bads_avr <- dat2$yb_bads_8 + dat2$yb_bads_9 + dat2$yb_bads_10 +
                dat2$yb_bads_13 + dat2$yb_bads_14 + dat2$yb_bads_15 + 
                dat2$yb_bads_24 + dat2$yb_bads_25

dat3 <- subset (dat2, select = c("pb_childethnicity", "pb_childgender",
                                 "pb_childage", "pb_income", "yb_permanence",
                                 "yb_cause_brain", "yb_cause_env",
                                 "bads_act", "bads_avr"
))

table(dat3$pb_childethnicity)
table(dat3$pb_childgender)
table(dat3$pb_income)
table(dat3$pb_childage)

  
# Rename variables
dat4 <- dat3 %>% rename(race_eth = pb_childethnicity,
                        gender = pb_childgender,
                        age = pb_childage,
                        income = pb_income,
                        ppd_dep_perm = yb_permanence,
                        ppd_cause_brain = yb_cause_brain,
                        ppd_cause_env = yb_cause_env)
head(dat4)
names(dat4)
table(dat4$race_eth)
  
# Create new variables
dat5 = dat4 %>% mutate(race_eth = recode(race_eth,
                "American Indian and/or Alaska Native" = "AI/AN",
               "Black or African American" = "Black",
               "More than one race" = "Other/Multiple",
               "White, non-Hispanic (includes Middle Eastern)" = "White",
               "Asian (including Asian Desi and Pacific Islander)" = "AA/PI",
               "Hispanic or Latino/a" = "Hisp/Lat",
               "Other, please specify" = "Other/Multiple"))
head(dat5)
dat5$race_eth
names(dat5)
                

dat5$age <- as.numeric(dat5$age)
hist(dat5$age)

dat5$ppd_dep_perm <- as.numeric(dat5$ppd_dep_perm)
dat5$ppd_cause_brain <- as.numeric(dat5$ppd_cause_brain)
dat5$ppd_cause_env <- as.numeric(dat5$ppd_cause_env)
dat5$bads_act <- as.numeric(dat5$bads_act)
dat5$bads_avr <- as.numeric(dat5$bads_avr)

hist(dat5$bads_act)
hist(dat5$bads_avr)
hist(dat5$ppd_dep_perm)
hist(dat5$ppd_cause_brain)
hist(dat5$ppd_cause_env)



# Create median splits
table(dat5$age)
median(dat5$age)
4 + 12 + 24 + 20
27 + 19

dat5$age_split[dat5$age < 15] <- "<=14"
dat5$age_split[dat5$age > 14] <- ">14"
table(dat5$age_split)

print(dat5, max = 100)

table(dat5$income)
9 + 14 + 8 + 16
17 + 6 + 8 + 28

dat5 = dat5 %>% mutate(income_split = recode(income,
                                             "$0-$19,000" = "Lower",
                                             "$20,000-$39,000" = "Lower",
                                             "$40,000-$59,000" = "Lower",
                                             "$60,000-$79,000" = "Lower",
                                             "$80,000 - $99,000" = "Higher",
                                             "$100,000 - $119,000" = "Higher",
                                             "$120,000-$140,000" = "Higher",
                                             "$140,000+" = "Higher"))

dat5 <- subset(dat5, select = c("ppd_dep_perm", "ppd_cause_brain", "ppd_cause_env",
                                "bads_act", "bads_avr",
                                "race_eth", "gender", "age_split", "income_split"
))


head(dat5)  




# Determine which demographic vars have adequately sized groups
table(dat5$race_eth)
#nope - DROP FROM ANALYSES
table(dat5$gender)
#man & woman only
table(dat5$age)
#yep
table(dat5$income)
#yep

# Secondary analyses will compare man/woman, younger/older, & higher/lower income








####  Analysis  ####
# Correlations
cor.test(dat5$ppd_dep_perm, dat5$bads_act, method = "spearman")
#sig
cor.test(dat5$ppd_dep_perm, dat5$bads_avr, method = "spearman")
#sig

cor.test(dat5$ppd_cause_brain, dat5$bads_act, method = "spearman")
#ns
cor.test(dat5$ppd_cause_brain, dat5$bads_avr, method = "spearman")
#ns

cor.test(dat5$ppd_cause_env, dat5$bads_act, method = "spearman")
#ns
cor.test(dat5$ppd_cause_env, dat5$bads_avr, method = "spearman")
#sig



## Demographic analyses
# Gender
dat5_women <- dat5[which(dat5$gender=="Woman"),]
dat5_men <- dat5[which(dat5$gender=="Man"),] 

cor.test(dat5_women$ppd_dep_perm, dat5_women$bads_act, method = "spearman")
#sig
cor.test(dat5_men$ppd_dep_perm, dat5_men$bads_act, method = "spearman")
#ns
cor.test(dat5_women$ppd_dep_perm, dat5_women$bads_avr, method = "spearman")
#ns (marg)
cor.test(dat5_men$ppd_dep_perm, dat5_men$bads_avr, method = "spearman")
#sig

cor.test(dat5_women$ppd_cause_brain, dat5_women$bads_act, method = "spearman")
#ns
cor.test(dat5_men$ppd_cause_brain, dat5_men$bads_act, method = "spearman")
#ns
cor.test(dat5_women$ppd_cause_brain, dat5_women$bads_avr, method = "spearman")
#ns
cor.test(dat5_men$ppd_cause_brain, dat5_men$bads_avr, method = "spearman")
#ns

cor.test(dat5_women$ppd_cause_env, dat5_women$bads_act, method = "spearman")
#sig
cor.test(dat5_men$ppd_cause_env, dat5_men$bads_act, method = "spearman")
#sig
cor.test(dat5_women$ppd_cause_env, dat5_women$bads_avr, method = "spearman")
#ns
cor.test(dat5_men$ppd_cause_env, dat5_men$bads_avr, method = "spearman")
#sig


# Age (median split)
dat5_younger <- dat5[which(dat5$age_split=="<=14"),]
count(dat5_younger)

dat5_older <- dat5[which(dat5$age_split==">14"),]
count(dat5_older)

cor.test(dat5_younger$ppd_dep_perm, dat5_younger$bads_act, method = "spearman")
#ns (marg)
cor.test(dat5_older$ppd_dep_perm, dat5_older$bads_act, method = "spearman")
#sig
cor.test(dat5_younger$ppd_dep_perm, dat5_younger$bads_avr, method = "spearman")
#sig
cor.test(dat5_older$ppd_dep_perm, dat5_older$bads_avr, method = "spearman")
#ns (marg)

cor.test(dat5_younger$ppd_cause_brain, dat5_younger$bads_act, method = "spearman")
#ns
cor.test(dat5_older$ppd_cause_brain, dat5_older$bads_act, method = "spearman")
#ns
cor.test(dat5_younger$ppd_cause_brain, dat5_younger$bads_avr, method = "spearman")
#ns
cor.test(dat5_older$ppd_cause_brain, dat5_older$bads_avr, method = "spearman")
#ns

cor.test(dat5_younger$ppd_cause_env, dat5_younger$bads_act, method = "spearman")
#ns
cor.test(dat5_older$ppd_cause_env, dat5_older$bads_act, method = "spearman")
#ns
cor.test(dat5_younger$ppd_cause_env, dat5_younger$bads_avr, method = "spearman")
#ns
cor.test(dat5_older$ppd_cause_env, dat5_older$bads_avr, method = "spearman")
#sig

# Income (median split)
dat5_higher <- dat5[which(dat5$income_split=="Higher"),]
dat5_higher
count(dat5_higher)

dat5_lower <- dat5[which(dat5$income_split=="Lower"),]
count(dat5_lower)

cor.test(dat5_higher$ppd_dep_perm, dat5_higher$bads_act, method = "spearman")
#ns
cor.test(dat5_lower$ppd_dep_perm, dat5_lower$bads_act, method = "spearman")
#sig
cor.test(dat5_higher$ppd_dep_perm, dat5_higher$bads_avr, method = "spearman")
#ns (marg)
cor.test(dat5_lower$ppd_dep_perm, dat5_lower$bads_avr, method = "spearman")
#sig

cor.test(dat5_higher$ppd_cause_brain, dat5_higher$bads_act, method = "spearman")
#ns
cor.test(dat5_lower$ppd_cause_brain, dat5_lower$bads_act, method = "spearman")
#ns
cor.test(dat5_higher$ppd_cause_brain, dat5_higher$bads_avr, method = "spearman")
#ns
cor.test(dat5_lower$ppd_cause_brain, dat5_lower$bads_avr, method = "spearman")
#ns (marg)

cor.test(dat5_higher$ppd_cause_env, dat5_higher$bads_act, method = "spearman")
#ns
cor.test(dat5_lower$ppd_cause_env, dat5_lower$bads_act, method = "spearman")
#ns
cor.test(dat5_higher$ppd_cause_env, dat5_higher$bads_avr, method = "spearman")
#sig
cor.test(dat5_lower$ppd_cause_env, dat5_lower$bads_avr, method = "spearman")
#sig





## Plots
df %>%
  ggplot() +
  geom_histogram(aes(yb_bads_1))



plot(dat5$bads_act, dat5$ppd_dep_perm)
