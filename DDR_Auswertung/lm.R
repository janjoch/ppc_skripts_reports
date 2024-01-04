# linear model lm()

x <- c(2,4,6,8,10)
y <- c(2.4, 2.7, 4.5, 5.0, 6.6)

model <- lm(y ~ x)

print(summary(model))

model <- lm(y ~ 1 + x)  # für y = a * 1 + b * x. Dasselbe wie oben

#model <- lm(y ~ 0 + x)  # für y = a * 0 + b * x => durch Ursprung!

#model <- lm(y ~ I(x) + I(x^2))  # Parabel: y = a + b*x + c*x^2

print(summary(model))

yc = predict(model)

plot(x, y)
abline(model, col="lightblue")
points(x, yc, pch=16, col="red")