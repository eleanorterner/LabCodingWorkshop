# Put a comment here with purpose, author, and date
# Example code structure. Savannah Rogers. April 2026

# Libraries and script sourcing
library(tidyverse) 
library(ggplot2)
source("demo_functions.R") # this runs a different script where I defined functs

#---------------------- User arguments -----------------------------------------
# This is where you can set arguments that you want to be able to easily change
# before rerunning the whole script. Sometimes you won't know what they will be
# until you've been working with a script for a while.

set.seed(42) # ALWAYS set a seed if you do any random generation
err_sigma <- 45 # the true spread of the error
true_m <- -5 # the true linear relationship
true_b <- 500 # the tru intercept
n_data <- 75 # how many data points to simulate

#---------------------- Functions ----------------------------------------------
# Define your functions here. Again, sometimes you won't know what you want to
# turn into a function until you've been working for a while, so its ok if there
# is nothing here to begin with.


#---------------------- Do some things and save objects ------------------------

# Simulate an independent variable (since this is all made up anyway, though 
# normally you'd read this data in from a file)
xs <- runif(n_data, 0, 100)

# Simulate dependent variable 
ys <- gen_lin_norm_dat(true_m, xs, true_b, err_sigma)

thedat <- data.frame(x = xs, 
                     y = ys)

# Fit linear model
fit <- glm(y ~ x, data = thedat, family = gaussian)

# Get estimator y hat
mod_b <- fit$coefficients[1]
mod_m <- fit$coefficients[2]
y_hat <- mod_m * xs + mod_b

# Get uncertainty
X <- model.matrix(fit)
vcov <- vcov(fit)
sqrt(vcov[2,2]) # standard error of the linear term
yhat_se <- sqrt(diag(X %*% vcov %*% t(X))) # standard error of each predicted y^

thedat$yhat <- y_hat
thedat$yhatlow <- y_hat - 1.96 * yhat_se
thedat$yhatupp <- y_hat + 1.96 * yhat_se

# save if you want to
saveRDS(thedat, "ex_data.RDS")

#---------------------- Visualize them -----------------------------------------

ggplot() +
  geom_point(thedat, mapping = aes(x = x,
                                   y = y)) +
  geom_line(thedat, mapping = aes(x = x, 
                                  y = yhat),
            color = "green3") +
  geom_ribbon(thedat, mapping = aes(x = x,
                                    ymin = yhatlow,
                                    ymax = yhatupp),
              alpha = .5, fill = "green3") +
  theme_bw()

#can save visualizations here if you want, too

# what if I want to do all of this again for 200 data points instead of 75 to 
# see if this changes the standard error of my prediction?

# what if I want to do that 1000 times to know what the average se is?

# what if I want to do it 100 times each for different values of n_data?




