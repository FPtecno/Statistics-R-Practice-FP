library(statsr)
library(dplyr)
library(ggplot2)
library(GGally)

data(evals)
summary(evals)
summary(evals$score)
hist(evals$score)

# Data cleaning
evtest <- evals %>%
  filter(score <= 3) %>%
  count(score)
evtest

ggplot(data=evals, aes(x=bty_avg, y=score)) +
  geom_point()

ggplot(data=evals, aes(x=bty_avg, y=score)) +
  geom_jitter()

m_bty = lm(score ~ bty_avg, data=evals)
summary(m_bty)

ggplot(data=evals, aes(x=bty_avg, y=score)) +
  geom_jitter() +
  geom_smooth(method="lm", se=FALSE)

# Check linear relationship
plot(m_bty$residuals ~ evals$bty_avg)

# Check normality of residuals
hist(m_bty$residuals)
qqnorm(m_bty$residuals)
qqline(m_bty$residuals)

# Check homoscedasticity
plot(m_bty$residuals ~ m_bty$fitted)
plot(abs(m_bty$residuals) ~ m_bty$fitted)

# Check independence of residuals
plot(m_bty$residuals)


# Multiple Linear Regression
ggplot(data=evals, aes(x=bty_f1lower, y=bty_avg)) +
  geom_jitter()

evals %>%
  summarise(cor(bty_avg, bty_f1lower))

ggpairs(evals, columns = 13:19)

m_bty_gen <- lm(score ~ bty_avg + gender, data=evals)
summary(m_bty_gen)

m_bty_rank = lm(score ~ bty_avg + rank, data=evals)
summary(m_bty_rank)


# Prediction
newprof <- data.frame(gender="male", bty_avg = 3)
predict(m_bty_gen, newprof) # Predict value
predict(m_bty_gen, newprof, interval="prediction", level=0.95) #IC


# Search for the best model
m_full <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval 
             + cls_students + cls_level + cls_profs + cls_credits + bty_avg 
             + pic_outfit + pic_color, data = evals)
summary(m_full)

m_full_2 <- lm(score ~ rank + gender + language + age + cls_perc_eval 
             + cls_students + cls_level + cls_profs + cls_credits + bty_avg 
             + pic_outfit + pic_color, data = evals)
summary(m_full_2)

m1 <- lm(score ~ ethnicity + gender + language + age + cls_perc_eval 
             + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m1)$adj.r.squared

m2 = lm(score ~ ethnicity + rank + gender + language + age + cls_perc_eval + 
    cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m2)$adj.r.squared


# TEST of each one of the explanatory variables (adjusted R2)
# List of explanatory variables
variables <- c("ethnicity", "rank", "gender", "language", "age", 
               "cls_perc_eval", "cls_students", "cls_level", "cls_profs", 
               "cls_credits", "bty_avg")

# Initialize a data frame to store results
results <- data.frame(Variable = character(), AdjR2 = numeric(), stringsAsFactors = FALSE)

# Loop through each variable
for (var in variables) {
  # Create a formula excluding the current variable
  formula <- as.formula(paste("score ~", paste(setdiff(variables, var), collapse = " + ")))
  
  # Fit the model
  model <- lm(formula, data = evals)
  
  # Get the adjusted R-squared
  adj_r2 <- summary(model)$adj.r.squared
  
  # Store the results
  results <- rbind(results, data.frame(Variable = var, AdjR2 = adj_r2))
}

# Print the results
print(results)
