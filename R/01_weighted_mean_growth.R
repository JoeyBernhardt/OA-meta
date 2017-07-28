library(tidyverse)
library(janitor)
library(purrr)
library(broom)
library(dplyr)
library(tidyr)
library(modelr)
library(readr)
library(ggplot2)

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

calc_ES_raw <- read_csv("data-processed/growth_lnRR_all.csv")

calc <- calc_ES_raw %>% 
  select(author, lnRR_interaction, sampling_variance_interaction) 

## ok let's get Q
## Q is sum(ES^2 *w_i) - (sum(ES*w_i)^2)/sum(w_i)


calc1 <- calc %>% 
  mutate(term1 = (1/sampling_variance_interaction) * (lnRR_interaction^2)) %>% 
  mutate(term2 = (1/sampling_variance_interaction) * lnRR_interaction) %>% 
  mutate(term3 = 1/sampling_variance_interaction)

calc2 <- calc1 %>% 
  summarise(Q = sum(term1) - ((sum(term2))^2)/ sum(term3),
            C_term1 = sum(1/sampling_variance_interaction),
            C_term2 = sum((1/sampling_variance_interaction)^2)) 

df <- 12 - 1
C <- calc2[["C_term1"]] - (calc2[["C_term1"]]/calc2[["C_term1"]])
Q <- calc2[["Q"]]
tau_sq <- (Q - df)/C


calc4 <- calc1 %>% 
  mutate(Q = Q, 
         tau_sq = tau_sq)

calc5 <- calc4 %>% 
  mutate(w = (1/(sampling_variance_interaction + tau_sq))) %>% 
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

lnRR_all <- read_csv("data-processed/growth_lnRR_all.csv")


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

Tau_sq_CO2 <- (Q_CO2 - 12)/(sum(calcCO2_1$term3) - (sum(calcCO2_1$term3 ^ 2)/sum(calcCO2_1$term3)))

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

Tau_sq_food <- (Q_food - 12)/(sum(calcfood_1$term3) - (sum(calcfood_1$term3 ^ 2)/sum(calcfood_1$term3)))

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


# now onto eq 7 in Hedges (SE small sample correction) ----------------------------------------------------------

## SE = ( (1/SWstar) * ( 1 + 4*sum( (1./dfi) .* ((Wstar./W).^2) .* (Wstar.*(SWstar-Wstar)/(SWstar^2) ) ) ) )^.5;
## ok so Wstar is our term w, their W is our Term1 term, which in the within study sampling variance
## ok so let's just start with the food effect.

## first term 1 here, which is 1/sum(w*), start with calc_food4

eq7_T1 <- calcfood_4 %>% 
  summarise(eq7_T1 = 1/sum(w)) 

## ok now we need the degrees of freedom in the individual studies!

sample_sizes <- lnRR_all %>% 
  select(starts_with("n"), author) %>% 
  mutate(df = n_B + n_C - 2)

calcfood_5 <- left_join(calcfood_4, sample_sizes)

sum_wj <- calcfood_5 %>% 
  summarise(sum(w))

eq7_T2 <- calcfood_5 %>% 
  mutate(T2.1 = 1/df) %>% 
  mutate(T2.2 = (w/(1/sampling_variance_overall_food))^2)  %>% 
  mutate(T3.1 = w*(sum_wj[[1]] - w)) %>% 
  mutate(T3.2 = sum_wj[[1]]^2) %>% 
  mutate(T3 = T3.1/T3.2)

eq7_B <- eq7_T2 %>% 
  summarise(TermB = sum(T2.1 *T2.2 * T3)*4) %>% 
  mutate(termb = 1 + TermB)


se_food <- (eq7_T1[[1]] * eq7_B[[1]])^0.5

overall_interaction %>% 
  ggplot(aes(x = lnRR_type, y = T_weighted)) + geom_point() +
  geom_errorbar(aes(ymin = T_weighted - 1.96*se_food[[1]], ymax = T_weighted + 1.96*se_food[[1]]), width = 0.1) +
  geom_hline(yintercept = 0) + theme_bw() + ylab("weighted mean lnRR") + xlab("lnRR type")


# se for small sample, interaction effect ---------------------------------

eq7_T1_interaction <- calc5 %>% 
  summarise(eq7_T1 = 1/sum(w)) 

## ok now we need the degrees of freedom in the individual studies!

sample_sizes_interaction <- lnRR_all %>% 
  select(starts_with("n"), author) %>% 
  mutate(df = n_B + n_C - 2)

calc6 <- left_join(calc5, sample_sizes_interaction)

sum_wj_interaction <- calc6 %>% 
  summarise(sum(w))

eq7_T2_interaction <- calc6 %>% 
  mutate(T2.1 = 1/df) %>% 
  mutate(T2.2 = (w/(1/sampling_variance_interaction))^2)  %>% 
  mutate(T3.1 = w*(sum_wj_interaction[[1]] - w)) %>% 
  mutate(T3.2 = sum_wj_interaction[[1]]^2) %>% 
  mutate(T3 = T3.1/T3.2)

eq7_B_interaction <- eq7_T2_interaction %>% 
  summarise(TermB = sum(T2.1 *T2.2 * T3)*4) %>% 
  mutate(termb = 1 + TermB)


se_interaction <- (eq7_T1_interaction[[1]] * eq7_B_interaction[[1]])^0.5


# small sample size correction for CO2 overall ----------------------------

eq7_T1_CO2 <- calcCO2_4 %>% 
  summarise(eq7_T1 = 1/sum(w)) 

## ok now we need the degrees of freedom in the individual studies!

sample_sizes_CO2 <- lnRR_all %>% 
  select(starts_with("n"), author) %>% 
  mutate(df = n_B + n_C - 2)

calc6_CO2 <- left_join(calcCO2_4, sample_sizes_CO2)

sum_wj_CO2 <- calc6_CO2 %>% 
  summarise(sum(w))

eq7_T2_CO2 <- calc6_CO2 %>% 
  mutate(T2.1 = 1/df) %>% 
  mutate(T2.2 = (w/(1/sampling_variance_overall_CO2))^2)  %>% 
  mutate(T3.1 = w*(sum_wj_CO2[[1]] - w)) %>% 
  mutate(T3.2 = sum_wj_CO2[[1]]^2) %>% 
  mutate(T3 = T3.1/T3.2)

eq7_B_CO2 <- eq7_T2_CO2 %>% 
  summarise(TermB = sum(T2.1 *T2.2 * T3)*4) %>% 
  mutate(termb = 1 + TermB)


se_CO2 <- (eq7_T1_CO2[[1]] * eq7_B_CO2[[1]])^0.5


overall_interaction2 <- overall_interaction %>% 
  mutate(se_small_sample = NA) %>% 
  mutate(se_small_sample = ifelse(lnRR_type == "interaction", se_interaction[[1]], se_small_sample)) %>%
  mutate(se_small_sample = ifelse(lnRR_type == "overall_food", se_food[[1]], se_small_sample)) %>% 
  mutate(se_small_sample = ifelse(lnRR_type == "overall_CO2", se_CO2[[1]], se_small_sample)) 

write_csv(overall_interaction2, "data-processed/weighted_mean_growth.csv")


overall_interaction2 %>% 
  ggplot(aes(x = lnRR_type, y = T_weighted)) + geom_point() +
  geom_errorbar(aes(ymin = T_weighted - 1.96*se_small_sample, ymax = T_weighted + 1.96*se_small_sample), width = 0.1) +
  geom_hline(yintercept = 0) + theme_bw() + ylab("weighted mean lnRR") + xlab("lnRR type")
ggsave("figures/weighted_mean_growth.pdf")
