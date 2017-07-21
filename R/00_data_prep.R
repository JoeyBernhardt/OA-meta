
# Figuring out how to do a factorial meta-analyis -------------------------
## Code written by JB, last updated July 20 2017

library(tidyverse)
library(janitor)

# Under the random effects model we need to take account of two levels of
# sampling, and two source of error. First, the true effect sizes θ are distributed
# about μ with a variance τ2
# that reflects the actual distribution of the true effects
# about their mean. Second, the observed effect T for any given θ will be
# distributed about that θ with a variance σ2
# that depends primarily on the sample
# size for that study. Therefore, in assigning weights to estimate μ, we need to
# deal with both sources of sampling error – within studies (e), and between
# studies (ε).



## What is Q?

## Q is sum(w_i * (T_i)^2) - (sum(w_i * T_i))^2 / sum(w_i)
# w_i = v*i is the within-study variance for study (i) plus the between-studies variance, tau-squared.


# interactive effects -----------------------------------------------------

calc_ES_raw <- read_csv("data-processed/calcification_interaction_effect_sizes.csv")

calc <- calc_ES_raw %>% 
  select(author, lnRR_interaction, sampling_variance) 

## ok let's get Q
## Q is sum(ES^2 *w_i) - (sum(ES*w_i)^2)/sum(w_i)


calc1 <- calc %>% 
  mutate(term1 = (1/sampling_variance) * (lnRR_interaction^2)) %>% 
  mutate(term2 = (1/sampling_variance) * lnRR_interaction) %>% 
  mutate(term3 = 1/sampling_variance)

calc2 <- calc1 %>% 
  summarise(Q = sum(term1) - ((sum(term2))^2)/ sum(term3))

Q <- calc2[[1]]

## Tau_sq is = 0 if Q < df (which is number of studies - 1), so here Tau_sq is 0.

calc4 <- calc1 %>% 
  mutate(Q = Q, 
         tau_sq = 0)

calc5 <- calc4 %>% 
  mutate(w = (1/(sampling_variance + tau_sq))) %>% 
  mutate(termT1 = w*lnRR_interaction) %>% 
  mutate(termT2 = w)

T_weighted <- calc5 %>% 
  summarise(T_weighted = sum(termT1)/sum(termT2))


variance <- calc5 %>% 
  summarise(1/sum(w))

lower_limit <- T_weighted[[1]] - 1.96*(variance[[1]]^1/2)
upper_limit <- T_weighted[[1]] + 1.96*(variance[[1]]^1/2)

conf_intervals <- data.frame(lower_limit, upper_limit, T_weighted) %>% 
  mutate(lnRR_type = "interaction")


## plot the weighted mean and 95% confidence intervals
conf_intervals %>% 
  ggplot(aes(x = lnRR_type, y = T_weighted)) + geom_point() +
  geom_errorbar(aes(ymin = lower_limit, ymax = upper_limit), width = 0.1)



# overall effects ---------------------------------------------------------

lnRR_all <- read_csv("data-processed/lnRR_all.csv")
