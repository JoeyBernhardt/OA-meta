library(plotrix)
library(ggplot2)

weighted_means <- function(sample_size) {

calc <- ES %>% 
    sample_n(size = sample_size, replace = TRUE)

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

return(conf_intervals)

}


sample_size <- rep(10, 10000)

bs_lnRR <- sample_size %>% 
  map_df(weighted_means, .id = "id_number") 

bs_lnRR %>% 
  group_by(lnRR_type) %>% 
  summarise_each(funs(mean, std.error), T_weighted) %>%
  ggplot(aes(x = lnRR_type, y = mean)) + geom_point() +
  geom_errorbar(aes(ymin = mean - 1.96*std.error, ymax = mean + 1.96*std.error), width = 0.1) +
  geom_hline(yintercept = 0)

hist(bs_lnRR$T_weighted)
abline(v = -0.07235218)
