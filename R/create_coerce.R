# Object coercion and casting ####

# Standardize input checking
as_tobit_input_check <- function(mu, sd, message) {
  stopifnot("'mu' must be a scalar"=length(mu) == 1)
  stopifnot("'sd' must be a scalar"=length(sd) == 1)
  # Coerce NA to be numeric, regardless of the input class
  if (is.na(mu)) {
    mu <- NA_real_
  }
  if (is.na(sd)) {
    sd <- NA_real_
  }
  stopifnot("'mu' must be numeric"=is.numeric(mu))
  stopifnot("'sd' must be numeric"=is.numeric(sd))
  stopifnot("'mu' and 'sd' must either both or neither be NA"=!xor(is.na(mu), is.na(sd)))
  list(mu=mu, sd=sd, message=message)
}

as_tobit_input_check_conversion <- function(mu, sd, message) {
  # Check that the input object is valid
  if (!missing(mu)) {
    tmp <-
      as_tobit_input_check(
        mu=as.numeric(mu),
        sd=attr(mu, which="sd", exact=TRUE),
        message=attr(mu, which="message", exact=TRUE)
      )
    ret <- list(mu=tmp$mu, sd=tmp$sd, message=tmp$message)
  } else {
    ret <- NULL
  }
  if (!missing(sd)) {
    warning("'sd' is ignored for object conversion (sd is taken from the object)")
  }
  if (!is.null(message)) {
    warning("'message' is ignored for object conversion (message is taken from the object)")
  }
  ret
}

#' Create or cast to a mean_tobit object
#'
#' @param mu The arithmetic mean
#' @param sd The arithmetic standard deviation
#' @param message Additional information about the mean
#' @return A 'mean_tobit' class object which is a number with attributes for
#'   'sd' and 'message'.
#' @family Class creation and coercion
#' @examples
#' as_mean_tobit(mu=10, sd=2, message="This is just an example")
#' @export
as_mean_tobit <- function(mu, sd, message=NULL) {
  UseMethod("as_mean_tobit")
}

#' @rdname as_mean_tobit
#' @export
as_mean_tobit.default <- function(mu, sd, message=NULL) {
  tmp <- as_tobit_input_check(mu=mu, sd=sd, message=message)
  structure(
    tmp$mu,
    sd=tmp$sd,
    message=tmp$message,
    class="mean_tobit"
  )
}

#' Create or cast to a geomean_tobit object
#'
#' @param mu The geometric mean
#' @param sd The geometric standard deviation
#' @inheritParams as_mean_tobit
#' @return A 'geomean_tobit' class object which is a number with attributes for
#'   'sd' and 'message'.
#' @family Class creation and coercion
#' @examples
#' as_geomean_tobit(mu=10, sd=2, message="This is just an example")
#' @export
as_geomean_tobit <- function(mu, sd, message=NULL) {
  UseMethod("as_geomean_tobit")
}

#' @rdname as_geomean_tobit
#' @export
as_geomean_tobit.default <- function(mu, sd, message=NULL) {
  tmp <- as_tobit_input_check(mu=mu, sd=sd, message=message)
  structure(
    tmp$mu,
    sd=tmp$sd,
    message=tmp$message,
    class=c("geomean_tobit", "mean_tobit")
  )
}

#' @rdname as_geomean_tobit
#' @export
as_geomean_tobit.mean_tobit <- function(mu, sd, message=NULL) {
  stopifnot("mean_tobit can only be converted to a geomean_tobit if it is NA"=is.na(mu))
  as_tobit_input_check_conversion(mu=mu, sd=sd, message=message)
  structure(
    mu,
    class=c("geomean_tobit", "mean_tobit")
  )
}

#' Calculate the percent coefficient of variation (CV)
#'
#' @param x The object to use for CV calculation
#' @return The percent coefficient of variation as a numeric scalar
#' @family Class creation and coercion
#' @export
as_cv_tobit <- function(x) {
  UseMethod("as_cv_tobit")
}

#' @describeIn as_cv_tobit CV for the arithmetic mean: \code{100*sd/mu}
#' @export
as_cv_tobit.mean_tobit <- function(x) {
  100*attr(x, "sd", exact=TRUE)/as.numeric(x)
}

#' @describeIn as_cv_tobit CV for the geometric mean:
#'   \code{100*sqrt(exp(sd^2)-1)}
#' @export
as_cv_tobit.geomean_tobit <- function(x) {
  100*sqrt(exp(attr(x, "sd", exact=TRUE)^2) - 1)
}

#' Create or extract the standard deviation from a mean_tobit object
#' @inheritParams as_mean_tobit
#' @return A 'sd_tobit' class object which is a number with attributes of 'mu'
#'   (the mean) and 'message'.
#' @family Class creation and coercion
#' @export
as_sd_tobit <- function(mu, sd, message=NULL) {
  UseMethod("as_sd_tobit")
}

#' @rdname as_sd_tobit
#' @export
as_sd_tobit.default <- function(mu, sd, message=NULL) {
  as_tobit_input_check(mu=mu, sd=sd, message=message)
  structure(
    sd,
    mu=mu,
    message=message,
    class="sd_tobit"
  )
}

as_sd_tobit_convert <- function(mu, sd, message) {
  as_tobit_input_check_conversion(sd=sd, message=message)
  structure(
    as_sd_tobit.default(
      mu=as.numeric(mu),
      sd=attr(mu, "sd", exact=TRUE),
      message=attr(mu, "message", exact=TRUE)
    ),
    class=unique(c("sd_tobit", class(mu)))
  )
}

#' @describeIn as_sd_tobit Convert a 'mean_tobit' object to a 'sd_tobit' object
#' @export
as_sd_tobit.mean_tobit <- function(mu, sd, message=NULL) {
  as_sd_tobit_convert(mu, sd, message=NULL)
}

#' @describeIn as_sd_tobit Convert a 'mean_tobit' object to a 'sd_tobit' object
#' @export
as_sd_tobit.geomean_tobit <- function(mu, sd, message=NULL) {
  as_sd_tobit_convert(mu, sd, message=NULL)
}
