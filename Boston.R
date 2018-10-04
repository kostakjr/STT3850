library(MASS)
library(ggplot2)
?Boston

#make scatter plot
ggplot(Boston, aes(x = rm, y = medv))+ 
  geom_point() +
  geom_smooth(method = "lm") 
#lm means linear model, this will put a line of best fit in the plot

#pearsons correlation coeficient
cor(Boston$rm, Boston$medv)

#regress medv onto rm
#this is why we need the MASS package
mod <-lm(medv~rm, data = Boston)
summary(mod)

1 - pnorm(115, 100, 15)

#params in order: value, mean, standard deviation
pnorm(120, 100, 15) - pnorm(90, 100, 15)

#times 2 because it's symetric
pt(-13.08, 504) * 2
(1 - pt(21.72, 504)) * 2

#new model
mod2 <- lm(medv ~ rm + lstat, data = Boston)
summary(mod2)

y <- Boston$medv
X <- model.matrix(mod2)
head(X)
#transpose of X times X inversed
solve(t(X) %*% X) %*% t(X) %*% y
