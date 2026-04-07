# Functions for data generating. Savannah Rogers. April 2026

# You can create comments like these below. These work with roxygen to 
# automatically create documentation for functions if you turn this into a 
# package later. It's also just helpful to see. Another option is to just
# comment in line within the function. I did both here.

#' Generate data from a linear model with normal error
#'
#' @param m Numeric, the slope
#' @param x Numeric (can be vector), the independent variable
#' @param b Numeric, the intercept
#' @param sigma Numeric, spread of error
#'
#' @return A numeric vector of simulated response values \code{y}.
gen_lin_norm_dat <- function(m, # the slope
                             x, # the independent variable, can be array
                             b, # the intercept
                             sigma # the spread of the error
                             ){
  y <- m * x + b
  n <- length(x)
  e <- rnorm(n, 0, sd = sigma)
  out <- y + e
  
  return(out) # returns ???
}
