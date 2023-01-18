# The tests are run via the main code to confirm that the internals are working
# as expected when there is a simple, direct test for the internal function from
# the main code.

test_that("negLogLik_tobit", {
  expect_error(
    negLogLik_tobit(distribution="foo")
  )
  expect_error(
    negLogLik_tobit(x=1:2, lower_limit=1:3),
    regexp="length(x) == length(lower_limit) is not TRUE",
    fixed=TRUE
  )
  expect_error(
    negLogLik_tobit(x=1:2, lower_limit=1:2, upper_limit=3:5),
    regexp="length(x) == length(upper_limit) is not TRUE",
    fixed=TRUE
  )
  expect_error(
    negLogLik_tobit(x=1:2, lower_limit=1:2, upper_limit=3:4, mask_lower=c(TRUE, FALSE, FALSE)),
    regexp="length(x) == length(mask_lower) is not TRUE",
    fixed=TRUE
  )
  expect_error(
    negLogLik_tobit(x=1:2, lower_limit=1:2, upper_limit=3:4, mask_lower=c(TRUE, FALSE), mask_between=c(FALSE, FALSE, FALSE)),
    regexp="length(x) == length(mask_between) is not TRUE",
    fixed=TRUE
  )
  expect_error(
    negLogLik_tobit(x=1:2, lower_limit=1:2, upper_limit=3:4, mask_lower=c(TRUE, FALSE), mask_between=c(FALSE, FALSE), mask_upper=c(FALSE, FALSE, FALSE)),
    regexp="length(x) == length(mask_upper) is not TRUE",
    fixed=TRUE
  )
  expect_error(
    negLogLik_tobit(
      x=1:2, lower_limit=1:2, upper_limit=3:4,
      mask_lower=c(FALSE, TRUE), mask_between=c(FALSE, TRUE), mask_upper=c(FALSE, TRUE)
    ),
    regexp="mask_upper, mask_between, and mask_upper may not overlap"
  )
  expect_error(
    negLogLik_tobit(
      x=1:2, lower_limit=1:2, upper_limit=3:4,
      mask_lower=c(TRUE, TRUE), mask_between=c(FALSE, TRUE), mask_upper=c(FALSE, TRUE)
    ),
    regexp="mask_upper, mask_between, and mask_upper may not overlap"
  )
  expect_error(
    negLogLik_tobit(
      param=c(1, log(2), 1),
      x=1:3,
      lower_limit=rep(1.5, 3), upper_limit=rep(4, 3),
      mask_lower=c(TRUE, FALSE, FALSE),
      mask_between=c(FALSE, TRUE, FALSE),
      mask_upper=c(FALSE, FALSE, TRUE)
    ),
    regexp="param must have exactly 2 values"
  )
  expect_equal(
    negLogLik_tobit(
      param=c(2, 3),
      x=1:3,
      lower_limit=rep(1.5, 3), upper_limit=rep(4, 3),
      mask_lower=c(TRUE, FALSE, FALSE),
      mask_between=c(FALSE, TRUE, FALSE),
      mask_upper=c(FALSE, FALSE, TRUE),
      distribution="t"
    ),
    -sum(
      c(
        log(2) + dt(exp(2)*2 - 3, df=3, log=TRUE),
        pt(exp(2)*1.5 - 3, df=3, log.p=TRUE),
        pt(exp(2)*4-3, df=3, log.p=TRUE)
      )
    )
  )
  expect_equal(
    negLogLik_tobit(
      param=c(1, log(2)),
      x=1:3,
      lower_limit=rep(1.5, 3), upper_limit=rep(4, 3),
      mask_lower=c(TRUE, FALSE, FALSE),
      mask_between=c(FALSE, TRUE, FALSE),
      mask_upper=c(FALSE, FALSE, TRUE)
    ),
    -sum(
      c(
        dt((2 - 1)/2, df=3, log=TRUE),
        pt((1.5 - 1)/2, df=3, log.p=TRUE),
        pt((1 - 4)/2, df=3, log.p=TRUE)
      )
    )
  )
})
