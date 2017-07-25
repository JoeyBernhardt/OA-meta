
# Figuring out how to do a factorial meta-analyis -------------------------
## Code written by JB, last updated July 20 2017

library(tidyverse)
library(janitor)
library(purrr)
library(broom)
library(dplyr)
library(tidyr)
library(modelr)
library(readr)

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


calcCO2 <- lnRR_all %>% 
  select(author, ends_with("CO2")) 



calcCO2_1 <- calcCO2 %>% 
  mutate(term1 = (1/sampling_variance_overall_CO2) * (lnRR_overall_CO2^2)) %>% 
  mutate(term2 = (1/sampling_variance_overall_CO2) * lnRR_overall_CO2) %>% 
  mutate(term3 = 1/sampling_variance_overall_CO2)

calcCO2_2 <- calcCO2_1 %>% 
  summarise(Q = sum(term1) - ((sum(term2))^2)/ sum(term3))

Q_CO2 <- calcCO2_2[[1]]

## Tau_sq is = 0 if Q < df (which is number of studies - 1), otherwise Tau_sq = (Q - df)/C, where C = sum(w) - sum(w^2)/sum(w)

Tau_sq_CO2 <- (Q_CO2 - 9)/(sum(calcCO2_1$term3) - (sum(calcCO2_1$term3 ^ 2)/sum(calcCO2_1$term3)))

calcCO2_3 <- calcCO2_1 %>% 
  mutate(Q = Q_CO2,
         tau_sq = Tau_sq_CO2)

calcCO2_4 <- calcCO2_3 %>% 
  mutate(w = (1/(sampling_variance_overall_CO2 + tau_sq))) %>% 
  mutate(termT1 = w*lnRR_overall_CO2) %>% 
  mutate(termT2 = w)

T_CO2_weighted <- calcCO2_4 %>% 
  summarise(T_weighted = sum(termT1)/sum(termT2))


variance_CO2 <- calcCO2_4 %>% 
  summarise(1/sum(w))

lower_limit_CO2 <- T_CO2_weighted[[1]] - 1.96*(variance_CO2[[1]]^1/2)
upper_limit_CO2 <- T_CO2_weighted[[1]] + 1.96*(variance_CO2[[1]]^1/2)

conf_intervals_CO2 <- data.frame(lower_limit_CO2, upper_limit_CO2, T_CO2_weighted) %>% 
  mutate(lnRR_type = "overall_CO2")

conf_intervals_CO2_2 <- conf_intervals_CO2 %>% 
  rename(lower_limit = lower_limit_CO2,
         upper_limit = upper_limit_CO2,
         T_weighted = T_weighted)



# now food overall effect -------------------------------------------------

calcfood <- lnRR_all %>% 
  select(author, ends_with("food")) 



calcfood_1 <- calcfood %>% 
  mutate(term1 = (1/sampling_variance_overall_food) * (lnRR_overall_food^2)) %>% 
  mutate(term2 = (1/sampling_variance_overall_food) * lnRR_overall_food) %>% 
  mutate(term3 = 1/sampling_variance_overall_food)

calcfood_2 <- calcfood_1 %>% 
  summarise(Q = sum(term1) - ((sum(term2))^2)/ sum(term3))

Q_food <- calcfood_2[[1]]

## Tau_sq is = 0 if Q < df (which is number of studies - 1), otherwise Tau_sq = (Q - df)/C, where C = sum(w) - sum(w^2)/sum(w)

Tau_sq_food <- (Q_food - 9)/(sum(calcfood_1$term3) - (sum(calcfood_1$term3 ^ 2)/sum(calcfood_1$term3)))

calcfood_3 <- calcfood_1 %>% 
  mutate(Q = Q_food,
         tau_sq = Tau_sq_food)

calcfood_4 <- calcfood_3 %>% 
  mutate(w = (1/(sampling_variance_overall_food + tau_sq))) %>% 
  mutate(termT1 = w*lnRR_overall_food) %>% 
  mutate(termT2 = w)

T_food_weighted <- calcfood_4 %>% 
  summarise(T_weighted = sum(termT1)/sum(termT2))


variance_food <- calcfood_4 %>% 
  summarise(1/sum(w))

lower_limit_food <- T_food_weighted[[1]] - 1.96*(variance_food[[1]]^1/2)
upper_limit_food <- T_food_weighted[[1]] + 1.96*(variance_food[[1]]^1/2)

conf_intervals_food <- data.frame(lower_limit_food, upper_limit_food, T_food_weighted) %>% 
  mutate(lnRR_type = "overall_food")

conf_intervals_food_2 <- conf_intervals_food %>% 
  rename(lower_limit = lower_limit_food,
         upper_limit = upper_limit_food,
         T_weighted = T_weighted)


overall_interaction <- bind_rows(conf_intervals, conf_intervals_CO2_2, conf_intervals_food_2)

overall_interaction %>% 
  ggplot(aes(x = lnRR_type, y = T_weighted)) + geom_point() +
  geom_errorbar(aes(ymin = lower_limit, ymax = upper_limit), width = 0.1) +
  geom_hline(yintercept = 0) + theme_bw() + ylab("weighted mean lnRR") + xlab("lnRR type")

ggsave("figures/calcification_weighted_lnRR.pdf")


# now bootstrap! ----------------------------------------------------------


times <- rep(10, 1000)

 bootfunction <- function(n) {
  thing <- sample_n(calc_ES_raw, size = n, replace= TRUE)
 }
 
 bootrapped_data_set <- times %>% 
   map_df(bootfunction) 
   
 
 calc <- bootrapped_data_set %>% 
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
 
 ## with broom
 
 calc_ES_raw %>% 
   bootstrap(100) %>% View
 

 