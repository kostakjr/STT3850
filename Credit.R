library(ISLR)
library(dplyr)
library(ggplot2)

str(Credit)

#see if theres a difference between mean balance based on gender
Credit %>%
    group_by(Gender) %>%
      summarize(Mean = mean(Balance))

mod <- lm(Balance~Gender, data = Credit)
summary(mod)

contrasts(Credit$Gender)
X <- model.matrix(mod)
head(X)

y <- Credit$Balance
betahat <- solve(t(X) %*% X) %*% t(X) %*% y
betahat

#both genders on same plot
Credit %>%
    ggplot(aes(x = Rating, y = Balance, color = Gender)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE)

#put genders on two seperate plots
Credit %>%
  ggplot(aes(x = Rating, y = Balance, color = Gender)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
    facet_wrap(~Gender)


par(mfrow = c(2,2))
plot(mod)
par(mfrow = c(1,1))




#my own thing with balance and student

mod2 <- lm(Rating ~ ., data = Credit)
summary(mod2)


