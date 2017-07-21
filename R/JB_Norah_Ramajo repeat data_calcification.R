library(metafor)
library(ggplot2)
# library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)



calcification <- read_csv("data-raw/calcification.csv")

## now onto interactive effects

cal <- calcification %>% 
  clean_names() 


ambienthigh <- cal %>% 
  filter(treatment == "AmbientHigh") %>% 
  rename(mean_ambienthigh = mean) %>% 
  rename(sd_ambienthigh = sd) %>% 
  rename(n_ambienthigh = n) %>% 
  dplyr::select(author, study, treatment, 23, 25, 27) 

ambientlow <- cal %>% 
  filter(treatment == "AmbientLow") %>% 
  rename(mean_ambientlow = mean) %>% 
  rename(sd_ambientlow = sd) %>% 
  rename(n_ambientlow = n) %>% 
  dplyr::select(author, study, treatment, 23, 25, 27) 

elevatedlow <- cal %>% 
  filter(treatment == "ElevatedLow") %>% 
  rename(mean_elevatedlow = mean) %>% 
  rename(sd_elevatedlow = sd) %>% 
  rename(n_elevatedlow = n)  %>% 
  dplyr::select(author, study, treatment, 23, 25, 27) 

elevatedhigh <- cal %>% 
  filter(treatment == "ElevatedHigh") %>% 
  rename(mean_elevatedhigh = mean) %>% 
  rename(sd_elevatedhigh = sd) %>% 
  rename(n_elevatedhigh = n)  %>% 
  dplyr::select(author, study, treatment, 23, 25, 27) 


all1 <- dplyr::left_join(ambienthigh, ambientlow, by = "author")
all2 <- left_join(elevatedhigh, elevatedlow, by = "author")


all3 <- left_join(all1, all2, by = "author")

## ok now we have the right dataset, yay!!!
wide_data <- all3 %>% 
  dplyr::select(-starts_with("treatment"), -starts_with("study")) 

### ok here's how we get the interaction effect:
## For each study: L_interaction = log(AB) - log(A) - log(B) + log(C)
## sampling variance: s^2 = (s_a^2/A^2 * N_A) + (s_b^2/B^2 * N_B) + (s_ab^2/AB^2 * N_AB) + (S_c^2/C^2 * N_C)
## so here log(AB) is Elevated Low, log(A) is Elevated High, log(B) is Ambient Low, log(C) is Ambient High

wide2 <- wide_data %>% 
  mutate(log_AB = log(mean_elevatedlow)) %>% 
  mutate(log_A = log(mean_elevatedhigh)) %>% 
  mutate(log_B = log(mean_ambientlow)) %>% 
  mutate(log_C = log(mean_ambienthigh)) %>% 
  mutate(lnRR_interaction = log_AB - log_A - log_B + log_C)


## ok now let's get sampling variances for each study


lnRR_interaction <- wide2 %>% 
  mutate(sampling_variance_interaction = ((sd_elevatedhigh^2)/((mean_elevatedhigh^2)*(n_elevatedhigh))) +
           (sd_ambientlow^2)/((mean_ambientlow^2)*(n_ambientlow)) +
           (sd_elevatedlow^2)/((mean_elevatedlow^2)*(n_elevatedlow)) +
           (sd_ambienthigh^2)/((mean_ambienthigh^2)*(n_ambienthigh)))

write_csv(lnRR_interaction,  "data-processed/calcification_interaction_effect_sizes.csv")


## at this point, do we need to consider the positive and negative effects thing???


### now let's get the overall effects (i.e. multiplicative, not interactive)

## start with overall effect of A, which is CO2 effect

## L_A = log(A + AB) - log(C + B)
## sampling variance for A = ((1/A+B)^2) * ((s_A^2/N_A) + (s_AB^2/N_AB)) + ((1/C+B)^2) * ((s_c^2/N_C) + (s_B^2/N_B))

## ok now let's make this easier by renaming the variables in A/B/C format :)

wide3 <- lnRR_interaction %>% 
  rename(AB = mean_elevatedlow, 
         A = mean_elevatedhigh,
         B = mean_ambientlow,
         C = mean_ambienthigh,
         s_A = sd_elevatedhigh,
         s_B = sd_ambientlow,
         s_C = sd_ambienthigh,
         s_AB = sd_elevatedlow,
         n_A = n_elevatedhigh,
         n_B = n_ambientlow,
         n_C = n_ambienthigh,
         n_AB = n_elevatedlow)


wide4 <- wide3 %>% 
  mutate(lnRR_overall_CO2 = log(A + AB) - log(C + B))

lnRR_all <- wide4 %>% 
  mutate(sampling_variance_overall_CO2 = (((1/((A + B))^2) * ((s_A^2/n_A) + (s_AB^2/n_AB))) + (((1/(C + B))^2) * ((s_C^2/n_C) + (s_B^2)/n_B)))) 


write_csv(lnRR_all, "data-processed/lnRR_all.csv")
