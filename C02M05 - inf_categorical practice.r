library(statsr)
library(dplyr)
library(ggplot2)

data(atheism)

dim(atheism)
summary(atheism)

us12 <- atheism %>%
  filter(nationality == "United States", year == 2012)

us12 %>%
  group_by(response) %>%
  summarise(count = n())

inference(y = response, data = us12, statistic = "proportion",
    type = "ci", method = "theoretical", success = "atheist")

d <- data.frame(p <- seq(0, 1, 0.01))
n <- 1000
d <- d %>%
  mutate(me = 1.96*sqrt(p*(1 - p)/n))
ggplot(d, aes(x = p, y = me)) +
  geom_line()

spdata <- atheism %>%
  filter(nationality == "Spain")

summary(spdata)

spdata %>%
  inference(y = response, x=year, data= ., statistic = "proportion",
    type = "ht", method = "theoretical", success = "atheist",
    alternative = "twosided")

usdata <- atheism %>%
  filter(nationality == "United States")

usdata %>%
  inference(y = response, x=year, data= ., statistic = "proportion",
    type = "ht", method = "theoretical", success = "atheist",
    alternative = "twosided")

# 0.01 = 1.96 * sqrt(0.5*(1-0.5)/n)
#calculate n
n = (0.5*0.5)/(0.01/1.96)^2
n
