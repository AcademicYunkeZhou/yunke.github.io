## unweighted
#d = data set with columns Elong, size, method
#want to investigate effect of method on Elong while accounting for size

mod <- lm(d$Elong~d$method)
summary(mod)
plot(mod)
plot(d$size,mod$residuals, xlab = "Sample Size", ylab= "Residuals")

## weighted
w.mod <- lm(d$Elong~d$method, weights=d$size)
summary(w.mod)
plot(w.mod)
plot(d$size,w.mod$residuals, xlab = "Sample Size", ylab= "Residuals")