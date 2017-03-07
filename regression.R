data(cars)

install.packages("RCurl")
require(RCurl)

str(cars)

URL <- "https://elearning2.uniroma1.it/pluginfile.php/473385/mod_label/intro/meatspec.txt"
x <- getURL(URL)
prueba <- read.csv(textConnection(x))

download.file(url=URL,
              destfile='localfile.csv', method='curl')

# first linear model
?lm
?formula

plot(cars)

firstlm = lm(dist ~ speed, data = cars)
abline(a = firstlm$coefficients[1], b = firstlm$coefficients[2])

secondlm = lm(dist~0+speed, data = cars)
abline(a = 0, b = secondlm$coefficients[1])


# summary -----------------------------------------------------------------

t value
additional assumption that the error term is also N distributed
e ~ N(0, sigma_e In)

summary(firstlm)

cor(cars$dist, cars$speed)^2

plot(firstlm)

par(mfrow = c(2,2))
plot(firstlm)


# Add regression line -----------------------------------------------------

plot(cars, pch  = 19, col = "red")
abline(firstlm , lwd = 4, col ="yellow")


# Get stuff out an object -------------------------------------------------

residuals(firstlm)

# RSS - noise variance
sqrt(sum(residuals(firstlm)^2)/48)

# Fitted values
fitted(firstlm)

# design matrix
model.matrix(firstlm)

# predictions -------------------------------------------------------------

speed = c(30)

df.s = data.frame(speed)
df.s

predict.lm(object = firstlm, df.s)

