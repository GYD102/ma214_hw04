# MA214 - Problem Set 4
library(ggplot2)

# 2) F-test: variance ratios
s <- 0.4^2 / 0.3^2
s
# Lower Bound
s / qf(0.95, df1 = 405, df2 = 229)
# Upper Bound
s * qf(0.95, df1 = 229, df2 = 405) # Calculation method in textbook
s / qf(0.05, df1 = 405, df2 = 229) # Calculation method online

pf(1 / qf(0.95, df1 = 229, df2 = 405) , df1 = 405, df2 = 229)
qf(0.95, df1 = 405, df2 = 229)
qf(0.95, df1 = 229, df2 = 405)

# 1 / qf(0.95, df1 = 405, df2 = 229) + qf(0.95, df1 = 229, df2 = 405)
# 1 / qf(0.95, df1 = 305, df2 = 229) + qf(0.95, df1 = 229, df2 = 305)
# 1 / qf(0.95, df1 = 305, df2 = 129) + qf(0.95, df1 = 129, df2 = 305)
# 1 / qf(0.95, df1 = 305, df2 = 329) + qf(0.95, df1 = 329, df2 = 305)
# 1 / qf(0.95, df1 = 1305, df2 = 1329) + qf(0.95, df1 = 1329, df2 = 1305)

# 3) 
data <- read.csv("HANDSHKtxt.csv")
# Sample variances
var(data$HShake)
var(data$FBump)
mean(data$HShake)
mean(data$FBump)

# Test statistic 
s = var(data$HShake) / var(data$FBump)
s

qf(0.975, df1 = 4, df2 = 4)

# 5)
data <- read.csv("FCATtxt.csv")
xbar <- mean(data$POVERTY)
ybar <- mean(data$MATH)
SSXY <- sum((data$POVERTY - xbar) * (data$MATH - ybar))
SSXX <- sum((data$POVERTY - xbar)^2)

b_1 = SSXY / SSXX
b_0 = ybar - b_1 * xbar

c(b_0, b_1)

lm(data$MATH ~ data$POVERTY)

ggplot(data, aes(x = data$POVERTY, y = data$MATH)) +
  geom_point() +
  geom_abline(slope = b_1, intercept = b_0, color = "red") +
  xlab("Percentage of Students Below Poverty Level") +
  ylab("FCAT Math Score Average") +
  ggtitle("FCAT Math Scores vs. Student Poverty")


xbar <- mean(data$POVERTY)
ybar <- mean(data$READING)
SSXY <- sum((data$POVERTY - xbar) * (data$READING - ybar))
SSXX <- sum((data$POVERTY - xbar)^2)

b_1 = SSXY / SSXX
b_0 = ybar - b_1 * xbar

c(b_0, b_1)

lm(data$READING ~ data$POVERTY)

ggplot(data, aes(x = data$POVERTY, y = data$READING)) +
  geom_point() +
  geom_abline(slope = b_1, intercept = b_0, color = "red") +
  xlab("Percentage of Students Below Poverty Level") +
  ylab("FCAT READING Score Average") +
  ggtitle("FCAT READING Scores vs. Student Poverty")
