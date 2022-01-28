test_that("as_mean_tobit", {
  expect_equal(
    as_mean_tobit(1, 2, "foo"),
    structure(
      1,
      sd=2,
      message="foo",
      class="mean_tobit"
    )
  )
  expect_error(
    as_mean_tobit(1:2, 2, "foo"),
    regexp="'mu' must be a scalar"
  )
  expect_error(
    as_mean_tobit(2, 1:2, "foo"),
    regexp="'sd' must be a scalar"
  )
  expect_equal(
    as_mean_tobit(NA, NA_character_, "foo"),
    as_mean_tobit(NA_real_, NA_real_, "foo")
  )
  expect_error(
  )
  expect_error(
    as_mean_tobit("A", 2, "foo"),
    regexp="'mu' must be numeric"
  )
  expect_error(
    as_mean_tobit(2, "A", "foo"),
    regexp="'sd' must be numeric"
  )
  expect_error(
    as_mean_tobit(2, NA, "foo"),
    regexp="'mu' and 'sd' must either both or neither be NA"
  )
  expect_error(
    as_mean_tobit(NA, 2, "foo"),
    regexp="'mu' and 'sd' must either both or neither be NA"
  )
})

test_that("as_geomean_tobit", {
  expect_equal(
    as_geomean_tobit(1, 2, "foo"),
    structure(
      1,
      sd=2,
      message="foo",
      class=c("geomean_tobit", "mean_tobit")
    )
  )
  expect_error(
    as_geomean_tobit(as_mean_tobit(1, 2, "foo")),
    regexp="mean_tobit can only be converted to a geomean_tobit if it is NA"
  )
  expect_equal(
    as_geomean_tobit(as_mean_tobit(NA_real_, NA, "foo")),
    as_geomean_tobit(NA_real_, NA, "foo")
  )
  expect_warning(
    result <- as_geomean_tobit(as_mean_tobit(NA, NA, "foo"), sd=1),
    regexp="'sd' is ignored"
  )
  expect_equal(
    result,
    as_geomean_tobit(NA, NA, "foo")
  )
  expect_warning(
    result <- as_geomean_tobit(as_mean_tobit(NA, NA, "foo"), message="foo"),
    regexp="'message' is ignored"
  )
  expect_equal(
    result,
    as_geomean_tobit(NA, NA, "foo")
  )
})

test_that("as_cv_tobit", {
  expect_equal(
    as_cv_tobit(as_mean_tobit(1, 2)),
    200
  )
  expect_equal(
    as_cv_tobit(as_geomean_tobit(1, sd(log(1:2)))),
    100*sqrt(exp(sd(log(1:2))^2)-1)
  )
})

test_that("as_sd_tobit", {
  expect_equal(
    as_sd_tobit(1, 2, "foo"),
    structure(
      2,
      mu=1,
      message="foo",
      class="sd_tobit"
    )
  )
  expect_equal(
    as_sd_tobit(as_mean_tobit(1, 2, "foo")),
    structure(
      2,
      mu=1,
      message="foo",
      class=c("sd_tobit", "mean_tobit")
    )
  )
  expect_equal(
    as_sd_tobit(as_geomean_tobit(1, 2, "foo")),
    structure(
      2,
      mu=1,
      message="foo",
      class=c("sd_tobit", "geomean_tobit", "mean_tobit")
    )
  )
})
