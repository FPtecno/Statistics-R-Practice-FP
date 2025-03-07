library(statsr)
library(dplyr)
library(ggplot2)

library(DAAG)
data(allbacks)

# fit model
book_mlr = lm(weight ~ volume + cover, data = allbacks)

names(allbacks)
summary(book_mlr)

vol = seq(0, 1200, 1)
weight_hc = 197.96284 + 0.7195 * vol
weight_pb = 197.96284 + 0.7195 * vol - 184.04727 * 1
data <- data.frame(vol, weight_hc, weight_pb)

# plot results
ggplot(data=data, aes(x = vol)) +
  geom_line(aes(y=weight_hc, color="red"), size=1) +
  geom_line(aes(y=weight_pb, color="blue"), size=1) +
  labs(title="Predicted Weights vs Volume", x="Volume", y="Weights")



# Another example
states = read.csv("http://d396qusza40orc.cloudfront.net/statistics/lec_resources/states.csv")
summary(states)

pov_slr = lm(poverty ~ female_house, data = states)
summary(pov_slr)

ggplot(data=pov_slr, aes(sample=.resid)) +
  stat_qq()

ggplot(data = states, aes(x = female_house, y = poverty)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

anova_model <- aov(poverty ~ female_house, data = states)
anova_summary <- summary(anova_model)

ss_model <- anova_summary[[1]]$`Sum Sq`[1]
ss_residual <- anova_summary[[1]]$`Sum Sq`[2]

r_squared_anova = ss_model/(ss_model+ss_residual)
r_squared_anova


pov_mlr = lm(poverty ~ female_house + white, data = states)
summary(pov_mlr)
anova(pov_mlr)


# Another example

cognitive = read.csv("http://bit.ly/dasi_cognitive")

names(cognitive)
cog_full = lm(kid_score ~ mom_hs + mom_iq +
        mom_work + mom_age, data = cognitive)
summary(cog_full)

# After selecting the best variables fot the model
cog_final = lm(kid_score ~ mom_hs + mom_iq + mom_work, data = cognitive)
summary(cog_final)

# Check linear relationship
plot(cog_final$residuals ~ cognitive$mom_iq)

# Check normality of residuals
hist(cog_final$residuals)
qqnorm(cog_final$residuals)
qqline(cog_final$residuals)

# Check homoscedasticity
plot(cog_final$residuals ~ cog_final$fitted)
plot(abs(cog_final$residuals) ~ cog_final$fitted)

# Check independence of residuals
plot(cog_final$residuals)

(cognitive$kid_score)
