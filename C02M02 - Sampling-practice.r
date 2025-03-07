library(statsr)
library(dplyr)
library(shiny)
library(ggplot2)

data(ames)
names(ames)

ggplot(data=ames, aes(x=area)) + geom_histogram(binwidth = 250)

ames %>%
  summarise(mu = mean(area), pop_med = median(area),
  sigma = sd(area), pop_iqr = IQR(area),
  pop_min = min(area), pop_max = max(area),
  pop_q1 = quantile(area, 0.25), pop_q3 = quantile(area, 0.75))

samp1 <- ames %>%
  sample_n(size = 50)

samp1 %>%
  summarise(mus = mean(area), pop_meds = median(area),
  sigmas = sd(area), pop_iqrs = IQR(area),
  pop_mins = min(area), pop_maxs = max(area),
  pop_q1s = quantile(area, 0.25), pop_q3s = quantile(area, 0.75))

samp1 %>%
  summarise(x_bar = mean(area))

ames %>%
  sample_n(size = 1000) %>%
  summarise(x_bar = mean(area))

sample_means50 <- ames %>%
                rep_sample_n(size = 50, reps = 15000, replace = TRUE) %>%
                summarise(x_bar = mean(area))

ggplot(data = sample_means50, aes(x = x_bar)) +
  geom_histogram(binwidth = 20)

dim(sample_means50)

ames %>%
  sample_n(size = 50) %>%
  summarise(x_bar = mean(area))

sample_means_small <- ames %>%
                  rep_sample_n(size = 10, reps = 25, replace = TRUE) %>%
                  summarise(x_bar = mean(area))

ggplot(data = sample_means_small, aes(x = x_bar)) +
  geom_histogram(binwidth = 20)

dim(sample_means_small)

ggplot(data = sample_means50, aes(x = x_bar)) +
  geom_histogram(binwidth = 20)

sample_means50 <- ames %>%
                  rep_sample_n(size = 50, reps = 5000, replace = TRUE) %>%
                  summarise(x_bar = mean(price))

ggplot(data = sample_means50, aes(x = x_bar)) +
  geom_histogram(binwidth = 20)

sample_means150 <- ames %>%
                  rep_sample_n(size = 150, reps = 5000, replace = TRUE) %>%
                  summarise(x_bar = mean(price))

ggplot(data = sample_means150, aes(x = x_bar)) +
  geom_histogram(binwidth = 20)



ames %>%
  sample_n(size = 15) %>%
  summarise(x_bar = mean(price))

sample_means_15 <- ames %>%
                  rep_sample_n(size = 15, reps = 2000, replace = TRUE) %>%
                  summarise(x_bar = mean(price))

ggplot(data = sample_means_15, aes(x = x_bar)) +
  geom_histogram(binwidth = 20)

ames %>%
  summarise(pop_med_pp= median(price))
