#
# DISP 2020: Lines, Temporal
# 

# Import packages
library(ez)
library(tidyverse)

# Load data
setwd('/Users/Brett/Dalhousie/Klein/DISP/2020/Analysis/')
load('line_time.RData')
dat <- line.time

# Check RT distribution
hist(dat$rt, freq=FALSE, br=30)
hist(log(dat$rt), freq=FALSE, br=30)

# Bin RTs
dat$rt_bin <- fct_explicit_na(cut_width(dat$rt, 50), "NA")
dat <- filter(dat, rt != 'NA')

# Binwise summary stats
# Determine upper/lower bin w/ sufficient n(rts) for analysis
dat %>% 
  group_by(rt_bin) %>% 
  summarize(
    nResponses = n(), 
    mean.RT = mean(rt), 
    mean.Err = mean(error)
  ) %>% View()

# 
# General performance
# ===============================
dat %>%
  group_by(
    participant,
    target_distractor, distractor_distractor, 
    present_absent
  ) %>%
  summarise(
    meanRT = mean(rt),
    meanErr = mean(error)
  ) -> SubjMeans

SubjMeans %>%
  group_by(
    target_distractor, distractor_distractor, 
    present_absent
  ) %>%
  summarise(
    meanRT = mean(meanRT),
    meanErr = mean(meanErr)
  )

#
# Error scores
# ===============================

dat$err <- as.numeric(dat$error)

ezStats(
  data = dat,
  dv = err,
  wid = participant,
  within = .(present_absent, target_distractor, distractor_distractor)
)

ezANOVA(
  data = dat,
  dv = err,
  wid = participant,
  within = .(present_absent, target_distractor, distractor_distractor)
)

ezPlot(
  data = dat,
  dv = err,
  wid = participant,
  within = .(present_absent, target_distractor, distractor_distractor),
  x = target_distractor,
  split = present_absent,
  col = distractor_distractor
)




