#
# DISP 2020: Lines, Spatial
# 

# Import packages
library(ez)
library(tidyverse)

# Load data
setwd('/Users/Brett/Dalhousie/Klein/DISP/2020/Analysis/')
load('line_space.RData')
dat <- line.space

# Check RT distribution
hist(dat$rt, freq=FALSE, br=30)
hist(log(dat$rt), freq=FALSE, br=30)

# Bin RTs
dat$rt_bin <- fct_explicit_na(cut_width(dat$rt, 50), "NA")

# Binwise summary stats
# Determine upper/lower bin w/ sufficient n(rts) for analysis
dat %>% 
  group_by(rt_bin) %>% 
  summarize(
    nResponses = n(), 
    mean.RT = mean(rt), 
    mean.Err = mean(error)
  ) %>% View()

# Appropriate cutoff?
hist(dat$rt, freq=FALSE, br=50)
abline(v=c(325, 5000))

# Apply trim
dat <- filter(dat, between(rt,200, 5000))


# 
# General performance
# ===============================
dat %>%
  group_by(
    participant, set_size, 
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


# Reaction Times 
# ===============================

rts <- filter(dat, error == 0) # (erroneous trials excluded)


ezStats(
  dat=rts,
  dv=rt, wid=participant,
  within = .(present_absent, target_distractor, distractor_distractor),
  within_full = .(set_size, present_absent, 
                  target_distractor, distractor_distractor)
)

# Take log of RTs for testing purposes
rts$lrt <- log(rts$rt)

ezANOVA(
  dat=rts,
  dv=lrt, wid=participant,
  within = .(set_size, target_distractor, distractor_distractor),
  within_full = .(present_absent, set_size, target_distractor, distractor_distractor)
)

ezPlot(
  rts,
  dv=rt, wid=participant,
  within=.(target_distractor, distractor_distractor, present_absent, set_size),
  x=set_size,
  split=present_absent,
  row=target_distractor,
  col=distractor_distractor
) 
# ===============================


# Seach Slope
# ===============================

slopes <- rts %>%
  group_by(participant,target_distractor, distractor_distractor, present_absent) %>%
  do({
    mod = lm(rt ~ as.numeric(set_size), data = .)
    data.frame(Intercept = coef(mod)[1], Slope = coef(mod)[2])
  }) 

ezStats(
  slopes, 
  dv=Slope, wid=participant, 
  within = .(target_distractor, distractor_distractor, present_absent)
)

ezPlot(
  slopes,
  dv=Slope, wid=participant,
  within=.(present_absent, target_distractor, distractor_distractor),
  x = present_absent,
  row = distractor_distractor,
  col = target_distractor
)
