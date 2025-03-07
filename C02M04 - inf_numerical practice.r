library(statsr)
library(dplyr)
library(ggplot2)

data(nc)
names(nc)
dim(nc)

unique(nc$habit)
str(nc)

summary(nc$gained)

ggplot(nc, aes(x=habit, y=weight)) +
  geom_boxplot() +
  labs(title="Side-by-side Boxplots of Habit and Weight",
       x="Habit",
       y="Weight") +
  theme_minimal()

nc %>%
  group_by(habit) %>%
  summarise(mean_weight = mean(weight))

nc %>%
  group_by(habit) %>%
  summarise(q = n())

inference(y=weight, x=habit, data=nc, statistic="mean", type="ht", null=0,
         alternative="twosided", method="theoretical")

inference(y=weight, x=habit, data=nc, statistic="mean", type="ci",
         method="theoretical", conf_level=0.95)

inference(y=weight, x=habit, data=nc, statistic="mean", type="ci",
         method="theoretical", order=c("smoker","nonsmoker"), conf_level=0.95)

inference(y=weeks, data=nc, statistic="mean", type="ci",
         method="theoretical", conf_level=0.99)

inference(y=weeks, data=nc, statistic="mean", type="ci",
         method="theoretical", conf_level=0.90)

inference(y=gained, x=mature, data=nc, statistic="mean", type="ht",
         null=0, alternative="twosided", method="theoretical")

nc %>%
  group_by(mature) %>%
  summarise(min = min(mage), max = max(mage))

# Is there a difference between on the weight between white and non-white moms?
inference(y=weight, x=whitemom, data=nc, statistic="mean", type="ht",
         null=0, alternative="twosided", method="theoretical")
# Because p-value is too low, we reject H0, HA is true, there is a difference
inference(y=weight, x=whitemom, data=nc, statistic="mean", type="ci",
         , method="theoretical", conf_level = 0.9, order=c("white","non-white"))
# We are 90% confident that white babies are on average 0.3492 to 0.7126 pounds
# heavier at birth than non-white babies