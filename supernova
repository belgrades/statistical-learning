x = load("supernova.Rdata")
head(supernova)



# Prepare the data --------------------------------------------------------

y = supernova$Magnitude
X = as.matrix(supernova[, -c(1, 12)])

# LASSO/CV ----------------------------------------------------------------

require(glmnet)
install.packages("glmnet")

fit0 = glmnet(X, y)

names(fit0)

plot(fit0, xvar="lambda", label = T)


# Take a look at the df vs lambda -----------------------------------------

plot(log(fit0$lambda), fit0$df, type = "h") 

plot(fit0$lambda, fit0$df, type = "h") 


# Pick lambda by CV -------------------------------------------------------

?cv.glmnet

fit.cv = cv.glmnet(X,y)
plot(fit.cv)

names(fit.cv)
c(min= fit.cv$lambda.min, onese = fit.cv$lambda.1se)


# Prediction --------------------------------------------------------------

?predict.cv.glmnet

yhat = predict(fit.cv, X)

plot(yhat, y, pch = 21, bg = "purple")
coef(fit.cv)


# LASSO/Cp ----------------------------------------------------------------

plot(fit0)

# For each fit0$lambda we need to build its Cp risk estimate --------------
length(fit0$lambda)

yhat = predict(fit0, X)

dim(yhat)

n = nrow(X)

RSS = colSums((y-yhat)^2)
Rhat = (1/n)*RSS
plot(fit0$lambda, Rhat)
plot(log(fit0$lambda), Rhat)

s2 = RSS/(n-fit0$df-1)

Cp = Rhat + (2*s2*fit0$df)/n

plot(log(fit0$lambda), Cp, type="l")

lam.cp = fit0$lambda[which.min(Cp)]
log(lam.cp)

fit0$df[which.min(Cp)]

plot(fit0, xvar = "lambda", label= T)
abline(v = log(lam.cp), lty = 3)
