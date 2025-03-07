library(statsr)
library(dplyr)
library(ggplot2)

data(mlb11)

ggplot(data = mlb11, aes(x=at_bats, y=runs)) + geom_point()

mlb11 %>%
  summarise(cor(runs, at_bats))

plot_ss(x=at_bats, y=runs, data=mlb11, showSquares = TRUE)

m1 <- lm(runs ~ at_bats, data=mlb11)
summary(m1)

m2 <- lm(runs ~ homeruns, data=mlb11)
summary(m2)

ggplot(data = mlb11, aes(x = at_bats, y = runs)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

mlb11 %>%
  filter(at_bats == 5579) %>%
  select(runs)

fp <- (-2789.2429+0.6305*5579)
xp <- 713-fp
xp

ggplot(data=m1, aes(x=.fitted, y=.resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

ggplot(data=m1, aes(x=.resid)) +
  geom_histogram(binwidth = 25) +
  xlab("Residuals")

ggplot(data=m1, aes(sample=.resid)) +
  stat_qq()


# Using hits to explain runs -> There seems to be a linear relationship

m3 <- lm(runs ~ hits, data=mlb11)
summary (m3) # 0.6419

ggplot(data = mlb11, aes(x = hits, y = runs)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

ggplot(data=m3, aes(x=.fitted, y=.resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

ggplot(data=m3, aes(x=.resid)) +
  geom_histogram(binwidth = 25) +
  xlab("Residuals")

ggplot(data=m3, aes(sample=.resid)) +
  stat_qq()

# Question 09 -> To beat: 0.6292 (hits)
m4 <- lm(runs ~ wins, data=mlb11)
summary (m4) # 0.361

m5 <- lm(runs ~ bat_avg, data=mlb11)
summary (m3) # 0.6419

names(mlb11)

# Question 10 -> To beat: 0.6292 (hits)
m6 <- lm(runs ~ new_obs, data=mlb11)
summary (m6) # 0.9349

m7 <- lm(runs ~ new_slug, data=mlb11)
summary (m7) # 0.8969

m8 <- lm(runs ~ new_onbase, data=mlb11)
summary (m8) # 0.8491


ggplot(data = mlb11, aes(x = new_obs, y = runs)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

ggplot(data=m6, aes(x=.fitted, y=.resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

ggplot(data=m6, aes(x=.resid)) +
  geom_histogram(binwidth = 25) +
  xlab("Residuals")

ggplot(data=m6, aes(sample=.resid)) +
  stat_qq()
