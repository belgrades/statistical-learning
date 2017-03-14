meat = read.csv("data.csv")

summary(meat)
mode(meat)

# Basic stats -------------------------------------------------------------

# More about it
# fat will be our response var
# the other 100 vars will be our feature vector
# Predict the content of fat in each meat sample
# based on its spectrum (sampled in 100 freq's)


# Take a look at the spectra ----------------------------------------------

?matplot
matplot(t(meat[, -101]), type = "l", )
#functional linear model > feature are 

# Correlation analysis ----------------------------------------------------

# Study the linear relationship between features/covariates
# The more correlated our covariances are, the more we are in statistical 
# problems (OLS)


# Correlation -------------------------------------------------------------

cspec = cor(meat[,-101])
round(spec, 2)
# Difficult to understand
# Summary
mean(cspec[upper.tri(cspec)]) # multicolinearity


# Visualization -----------------------------------------------------------

# library(corplot)

# make an image of the correlation structure
#mage, you can plot with this
image(cspec)


# Simple linear regression ------------------------------------------------

# 1 wavelength as feature and predict

# To check empiricallt the performance of our predictor 
# we split our dataset in two parts (sample splitting)
# Training set: on which we build/estimate/learn the model
# Test set: on which we check its predictive performance
# (future spoiler: really useful to make reliable inference in high-dim setup)

tr.idx = 1:150
tr.data = meat[tr.idx, ]
te.data = meat[-tr.idx, ]

# consider just V10 as a feature

mod1 = lm(fat ~ V10, data = tr.data)
mod1
summary(mod1)
plot(tr.data$V10, tr.data$fat, pch = 19, col = "blue")
abline(mod1, lwd = 3, col="green")
mean((tr.data$fat - fitted(mod1))^2)

# Test error
fat.test = predict(mod1, te.data)
mean((tr.data$fat - fat.test)^2)


# ALL IN ------------------------------------------------------------------

mod.all = lm(fat ~ ., data = tr.data)
summary(mod.all)

# 1. Unstable OLS
# 2. Overfitting (tracking the noise) R^2: 0.9979
# Too close to the training set to generalize well (99% is too much, probably)
# In other words, your model is too cpmplex for the data at hand

# Train error

mean((tr.data$fat-fitted(mod.all))^2)
fat.te = predict(mod.all, te.data)
mean((tr.data$fat - fat.te)^2)
tr.data$fat - fat.te

# t should be 12.41006

# Totally on a different scale 
# How badly optimistic  is the training error (empirical risk) as a predictive 
# perf measure/estimator
