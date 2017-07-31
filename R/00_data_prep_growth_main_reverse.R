library(dplyr)
library(tidyr)
library(readr)
library(janitor)


growth <- read_csv("data-raw/growth_main.csv")

## now onto interactive effects

gro1 <- growth %>% 
  clean_names() %>%
  group_by(author_2, level, food_supply, co2_level) %>% 
  summarise_each(funs(mean), mean_ambient, sd_ambient, n_ambient, mean_elevated, sd_elevated, n_elevated) 

high <- gro1 %>%
  ungroup() %>% 
  filter(food_supply == "High") %>% 
  rename(mean_ambienthigh = mean_ambient) %>% 
  rename(sd_ambienthigh = sd_ambient) %>% 
  rename(n_ambienthigh = n_ambient) %>% 
  rename(mean_elevatedhigh = mean_elevated) %>% 
  rename(sd_elevatedhigh = sd_elevated) %>% 
  rename(n_elevatedhigh = n_elevated) %>% 
  dplyr::select(author_2, 5:10, co2_level)

low <- gro1 %>% 
  ungroup() %>% 
  filter(food_supply == "Low") %>% 
  rename(mean_ambientlow = mean_ambient) %>% 
  rename(sd_ambientlow = sd_ambient) %>% 
  rename(n_ambientlow = n_ambient) %>% 
  rename(mean_elevatedlow = mean_elevated) %>% 
  rename(sd_elevatedlow = sd_elevated) %>% 
  rename(n_elevatedlow = n_elevated) %>% 
  dplyr::select(author_2, 5:10, co2_level)

all <- left_join(high, low, by = c("author_2", "co2_level")) %>% 
  distinct(author_2, co2_level, .keep_all = TRUE)

intersect(low$author_2, high$author_2)



## ok now we have the right dataset, yay!!!
wide_data <- all %>% 
  dplyr::select(-starts_with("treatment"), -starts_with("study")) 


### ok here's how we get the interaction effect:
## For each study: L_interaction = log(AB) - log(A) - log(B) + log(C)
## sampling variance: s^2 = (s_a^2/A^2 * N_A) + (s_b^2/B^2 * N_B) + (s_ab^2/AB^2 * N_AB) + (S_c^2/C^2 * N_C)
## so here log(AB) is Elevated Low, log(A) is Elevated High, log(B) is Ambient Low, log(C) is Ambient High

### ok here's how we get the interaction effect:
## For each study: L_interaction = log(AB) - log(A) - log(B) + log(C)
## sampling variance: s^2 = (s_a^2/A^2 * N_A) + (s_b^2/B^2 * N_B) + (s_ab^2/AB^2 * N_AB) + (S_c^2/C^2 * N_C)
## so here log(AB) is Elevated Low, log(A) is Elevated High, log(B) is Ambient Low, log(C) is Ambient High

wide2 <- wide_data %>% 
  mutate(log_AB = log(mean_elevatedhigh)) %>% 
  mutate(log_A = log(mean_elevatedlow)) %>% 
  mutate(log_B = log(mean_ambienthigh)) %>% 
  mutate(log_C = log(mean_ambientlow)) %>% 
  mutate(lnRR_interaction = log_AB - log_A - log_B + log_C)


## ok now let's get sampling variances for each study

wide3 <- wide2 %>% 
  rename(AB = mean_elevatedhigh, 
         A = mean_elevatedlow,
         B = mean_ambienthigh,
         C = mean_ambientlow,
         s_A = sd_elevatedlow,
         s_B = sd_ambienthigh,
         s_C = sd_ambientlow,
         s_AB = sd_elevatedhigh,
         n_A = n_elevatedlow,
         n_B = n_ambienthigh,
         n_C = n_ambientlow,
         n_AB = n_elevatedhigh)


wide4 <- wide3 %>% 
  mutate(sampling_variance_interaction = ((s_A^2)/((A^2)*(n_A))) +
           (s_B^2)/((B^2)*(n_B)) +
           (s_AB^2)/((AB^2)*(n_AB)) +
           (s_C^2)/((C^2)*(n_C)))



lnRR_all <- wide4 %>% 
  mutate(lnRR_overall_CO2 = log(A + AB) - log(C + B)) %>% 
  mutate(sampling_variance_overall_CO2 = (((1/((A + AB))^2) * ((s_A^2/n_A) +
                                                                 (s_AB^2/n_AB))) + (((1/(C + B))^2) *
                                                                                      ((s_C^2/n_C) + (s_B^2)/n_B)))) %>% 
  mutate(lnRR_overall_food = log(B + AB) - log(C + A)) %>% 
  mutate(sampling_variance_overall_food = (((1/((B + AB))^2) * ((s_B^2/n_B) +
                                                                  (s_AB^2/n_AB))) + (((1/(C + A))^2) *
                                                                                       ((s_C^2/n_C) + (s_A^2)/n_A))))

write_csv(lnRR_all, "data-processed/growth_main_lnRR_all_reverse.csv")
