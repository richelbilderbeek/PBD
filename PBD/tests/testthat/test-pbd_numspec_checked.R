context("pbd_numspec_checked")

test_that("pbd_numspec_mean_checked use", {

  expected <- pbd_numspec_mean_checked(
      erg = 0.12,
      eri = 0.23,
      scr = 0.34,
      sir = 0.45,
      crown_age = 0.56
  )
  expect_equal(expected, 1.046121595)

})

test_that("pbd_numspec_mean_checked abuse", {

  expect_silent(
    pbd_numspec_mean_checked(
      erg = 1.0,
      eri = 1.0,
      scr = 1.0,
      sir = 1.0,
      crown_age = 1.0
    )
  )

  expect_error(
    pbd_numspec_mean_checked(
      erg = -123.456,
      eri = 1.0,
      scr = 1.0,
      sir = 1.0,
      crown_age = 1.0
    ),
    "'erg' must be positive"
  )

  expect_error(
    pbd_numspec_mean_checked(
      erg = 1.0,
      eri = -123.456,
      scr = 1.0,
      sir = 1.0,
      crown_age = 1.0
    ),
    "'eri' must be positive"
  )

  expect_error(
    pbd_numspec_mean_checked(
      erg = 1.0,
      eri = 1.0,
      scr = -123.456,
      sir = 1.0,
      crown_age = 1.0
    ),
    "'scr' must be positive"
  )

  expect_error(
    pbd_numspec_mean_checked(
      erg = 1.0,
      eri = 1.0,
      scr = 1.0,
      sir = -123.456,
      crown_age = 1.0
    ),
    "'sir' must be positive"
  )

  expect_error(
    pbd_numspec_mean_checked(
      erg = 1.0,
      eri = 1.0,
      scr = 1.0,
      sir = 1.0,
      crown_age = -123.456
    ),
    "'crown_age' must be positive"
  )
})

test_that("pbd_numspec_median_checked use", {

  expected <- pbd_numspec_median_checked(
    erg = 1.2,
    eri = 2.3,
    scr = 3.4,
    sir = 4.5,
    crown_age = 6.7
  )
  expect_equal(expected, 99408712)

})

test_that("pbd_numspec_median_checked abuse", {

  expect_silent(
    pbd_numspec_median_checked(
      erg = 1.0,
      eri = 1.0,
      scr = 1.0,
      sir = 1.0,
      crown_age = 1.0
    )
  )

  expect_error(
    pbd_numspec_median_checked(
      erg = -123.456,
      eri = 1.0,
      scr = 1.0,
      sir = 1.0,
      crown_age = 1.0
    ),
    "'erg' must be positive"
  )

  expect_error(
    pbd_numspec_median_checked(
      erg = 1.0,
      eri = -123.456,
      scr = 1.0,
      sir = 1.0,
      crown_age = 1.0
    ),
    "'eri' must be positive"
  )

  expect_error(
    pbd_numspec_median_checked(
      erg = 1.0,
      eri = 1.0,
      scr = -123.456,
      sir = 1.0,
      crown_age = 1.0
    ),
    "'scr' must be positive"
  )

  expect_error(
    pbd_numspec_median_checked(
      erg = 1.0,
      eri = 1.0,
      scr = 1.0,
      sir = -123.456,
      crown_age = 1.0
    ),
    "'sir' must be positive"
  )

  expect_error(
    pbd_numspec_median_checked(
      erg = 1.0,
      eri = 1.0,
      scr = 1.0,
      sir = 1.0,
      crown_age = -123.456
    ),
    "'crown_age' must be positive"
  )
})
