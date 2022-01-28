#' Calculate a censored mean
#'
#' @param x A numeric vector
#' @param lower,upper The lower and upper bounds for censoring \code{x}
#' @param include_equal,include_equal_lower,include_equal_upper Should the
#'   bounds consider \code{x <= lower} and \code{x >= upper} (if \code{TRUE}) or
#'   \code{x < lower} and \code{x > upper} (if \code{FALSE}) to be censored?
#'   (\code{include_equal} is overridden by setting \code{include_equal_lower}
#'   or \code{include_equal_upper}.)
#' @param distribution What distribution (Student "t" or "normal") should be
#'   used.  If "t", the degrees of freedom are the number of non-\code{NA}
#'   values in \code{x}.
#' @param na.rm A logical value indicating whether \code{NA} values should be
#'   stripped from \code{x} before the computation proceeds.
#' @inheritDotParams stats::optim
#' @return A 'mean_tobit' object
#' @family Censored Descriptive Statistics
#' @export
mean_tobit <- function(x,
                       lower=-Inf, upper=Inf,
                       include_equal=TRUE, include_equal_lower=include_equal, include_equal_upper=include_equal,
                       distribution,
                       na.rm=TRUE, ...) {
  x_lower_and_upper <-
    mean_tobit_check_x_lower_upper(
      x=x,
      lower=lower,
      upper=upper,
      na.rm=na.rm
    )
  x <- x_lower_and_upper$x
  if (inherits(x, what="mean_tobit")) {
    # na.rm or all NA values triggered a result without more math required
    return(x)
  }
  lower <- x_lower_and_upper$lower
  upper <- x_lower_and_upper$upper
  mask_upper <-
    if (include_equal_upper) {
      x >= upper
    } else {
      x > upper
    }
  mask_lower <-
    if (include_equal_lower) {
      x <= lower
    } else {
      x < lower
    }
  mask_between <- !(mask_upper | mask_lower)
  if (all(mask_upper)) {
    ret <- as_mean_tobit(mu=NA_real_, sd=NA_real_, message="all values above upper")
  } else if (all(mask_lower)) {
    ret <- as_mean_tobit(mu=NA_real_, sd=NA_real_, message="all values below lower")
  } else if (all(x == x[1])) {
    ret <- as_mean_tobit(mu=x[1], sd=0, message="single unique value")
  } else {
    lgamma <- log(1/sd(x))
    delta <- mean(x)*sd(x)
    ret_prep <-
      optim(
        par=c(lgamma, delta),
        fn=negLogLik_tobit,
        method="L-BFGS-B",
        x=x,
        lower_limit=lower,
        upper_limit=upper,
        mask_lower=mask_lower,
        mask_between=mask_between,
        mask_upper=mask_upper,
        distribution=distribution,
        ...
      )
    if (ret_prep$convergence == 0) {
      # Successful convergence
      sd_est <- exp(1/ret_prep$par[1])
      mu_est <- ret_prep$par[2]/exp(ret_prep$par[1])
      ret <-
        as_mean_tobit(
          mu=mu_est,
          sd=sd_est,
          message=
            paste(
              "Successful convergence.  message from optim():",
              ret_prep$message
            )
        )
    } else {
      # Unsuccessful convergence
      ret <-
        as_mean_tobit(
          mu=NA_real_,
          sd=NA_real_,
          message=
            paste(
              "Failed convergence.  message from optim():",
              ret_prep$message
            )
        )
    }
  }
  ret
}

#' Calculate the censored standard deviation
#'
#' Since the standard deviation is almost always desired with the mean, it is
#' more efficient to use \code{as_sd_tobit()} on the mean object instead of
#' calculating the standard deviation separately.
#'
#' @inheritDotParams mean_tobit
#' @return A "sd_tobit" class object
#' @family Censored Descriptive Statistics
#' @export
sd_tobit <- function(...) {
  as_sd_tobit(mean_tobit(...))
}

#' Calculate the censored geometric mean
#'
#' @inheritParams mean_tobit
#' @inheritDotParams mean_tobit
#' @return A "sd_tobit" class object
#' @family Censored Descriptive Statistics
#' @export
geomean_tobit <- function(x, lower=0, upper=Inf, na.rm=TRUE, ...) {
  x_lower_and_upper <-
    mean_tobit_check_x_lower_upper(
      x=x,
      lower=lower,
      upper=upper,
      na.rm=na.rm
    )
  x <- x_lower_and_upper$x
  if (inherits(x, what="mean_tobit")) {
    # na.rm or all NA values triggered a result without more math required
    return(as_geomean_tobit(x))
  }
  lower <- x_lower_and_upper$lower
  upper <- x_lower_and_upper$upper
  mask_lower_x_problem <- x <= 0 & lower <= 0
  if (any(mask_lower_x_problem)) {
    stop("'x' and 'lower' cannot both be <= 0 at the same time")
  }
  mask_negative_x <- x < 0
  if (any(mask_negative_x)) {
    warning("some 'x' values were below zero, setting to zero")
    x[mask_negative_x] <- 0
  }
  mask_negative_lower <- lower < 0
  if (any(mask_negative)) {
    warning("some 'lower' values were below zero, setting to zero")
    lower[mask_negative] <- 0
  }
  if (any(upper <= 0)) {
    stop("some 'upper' values were at or below zero")
  }
  ret_prep <- mean_tobit(x=log(x), lower=log(lower), upper=log(upper), na.rm=na.rm, ...)
  as_geomean_tobit(
    mu=exp(ret_prep),
    sd=attr(ret_prep, "sd", exact=TRUE),
    message=attr(ret_prep, "message", exact=TRUE)
  )
}
