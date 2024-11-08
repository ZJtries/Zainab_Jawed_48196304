**Question no 01:**

Is there a linear relationship between height and weight?

```{r}
library(ggplot2)
# Load the data
data <- read.csv("project.csv")
# Assumption Checks
# 1. Check for linearity
ggplot(data, aes(x = height, y = weight)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Scatterplot of Height vs Weight", x = "Height", y = "Weight")
# 2. Check for normality of residuals (after fitting the model)
model <- lm(weight ~ height, data = data)
residuals <- model$residuals
qqnorm(residuals)
qqline(residuals)
# 3. Check for homoscedasticity
plot(model$fitted.values, residuals, main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")
# Perform Linear Regression
summary(model)
```
**Question no 02:**

#1.Check for normality within each group

```{r}
male_weight <- data$weight[data$gender == "Male"]
female_weight <- data$weight[data$gender == "Female"]
qqnorm(male_weight); qqline(male_weight, col = "blue", main = "Male Weight Q-Q Plot")
qqnorm(female_weight); qqline(female_weight, col = "red", main = "Female Weight Q-Q Plot")

#2.Check for equal variances (Levene's Test)

```{r}
library(car)
leveneTest(weight ~ gender, data = data)

# Perform Two-Sample T-Test
t.test(weight ~ gender, data = data, var.equal = TRUE) # Use var.equal = FALSE if variances are unequal

Create a contingency table:

```{r}
phys_gender_table <- table(data$phys, data$gender)
phys_gender_table

```{r}
addmargins(phys_gender_table) # To inspect counts in each cell
```

Perform Chi-Square Test

```{r}
chisq.test(phys_gender_table)


