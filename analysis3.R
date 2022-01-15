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
dat1 <- readRDS(here("data", "T2T Data for Pinder et al.rds"))
view(dat1)
####  Prepare Data  ####
## Select relevant variables
names(dat1)
dat1 <- dat1 %>%
  mutate(cdi_mean = round(rowMeans(select(., contains("cdi")), na.rm = TRUE), 2)) %>%

  # Select relevant variables
  select(pb_childethnicity, pb_childsex, pb_childgender, pb_childage, pb_income, 
         yb_permanence, yb_cause_brain, yb_cause_env,
         yb_bads_3, yb_bads_4, yb_bads_5, yb_bads_7, yb_bads_8, yb_bads_9, 
         yb_bads_10,yb_bads_11, yb_bads_12, yb_bads_13, yb_bads_14, yb_bads_15, 
         yb_bads_23, yb_bads_24, yb_bads_25, cdi_mean) %>%
  
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
head(dat1)

#Check data
View(dat1)
names(dat1)
## Look at numeric variables
# Get means and sds
(means <- sapply(dat1[6:11], mean, na.rm = TRUE))
(sds <- sapply(dat1[6:11], sd, na.rm = TRUE))
(means_and_sds <- round(data.frame(means, sds), digits = 2))

# Another way
dat1 %>%
  pivot_longer(where(is.numeric)) %>%
  group_by(name) %>%
  summarize(mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T))


####  Regressions  ####
# ppd_perm & bads_act
lm1 <- lm(bads_act ~ ppd_perm, data = dat1)
summary(lm1)
lm2 <- lm(bads_act ~ ppd_perm + cdi_mean, data = dat1)
summary(lm2)

# ppd_perm & bads_avr
lm3 <- lm(bads_avr ~ ppd_perm, data = dat1)
summary(lm3)
lm4 <- lm(bads_avr ~ ppd_perm + cdi_mean, data = dat1)
summary(lm4)

# ppd_brain & bads_act
lm5 <- lm(bads_act ~ ppd_brain, data = dat1)
summary(lm5)
lm6 <- lm(bads_act ~ ppd_brain + cdi_mean, data = dat1)
summary(lm6)

# ppd_brain & bads_avr
lm7 <- lm(bads_avr ~ ppd_brain, data = dat1)
summary(lm7)
lm8 <- lm(bads_avr ~ ppd_brain + cdi_mean, data = dat1)
summary(lm8)

# ppd_env & bads_act
lm9 <- lm(bads_act ~ ppd_env, data = dat1)
summary(lm9)
lm10 <- lm(bads_act ~ ppd_env + cdi_mean, data = dat1)
summary(lm10)

# ppd_env & bads_avr
lm11 <- lm(bads_avr ~ ppd_env, data = dat1)
summary(lm11)
lm12 <- lm(bads_avr ~ ppd_env + cdi_mean, data = dat1)
summary(lm12)

# get ps
ps_lm6 <- (summary(lm6)$coefficients[,4])
(p_act_brain <- ps_lm6[2])

ps_lm8 <- (summary(lm8)$coefficients[,4])
(p_avr_brain <- ps_lm8[2])

ps_lm10 <- (summary(lm10)$coefficients[,4])
(p_act_env <- ps_lm10[2])

ps_lm12 <- (summary(lm12)$coefficients[,4])
(p_avr_env <- ps_lm12[2])
    
## Adjust p-values
(ps <- c(as.numeric(p_act_brain), as.numeric(p_act_env), as.numeric(p_avr_brain), as.numeric(p_avr_env)))
round(p.adjust(ps, "fdr"), 3)
