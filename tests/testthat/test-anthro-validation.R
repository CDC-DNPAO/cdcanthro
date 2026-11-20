# Agreement with the WHO 'anthro' package (anthro::anthro_zscores()).
#
# fixtures/who_anthro.csv holds the inputs (sex, agedays, wt, lenhei, bmi,
# headc) and anthro's z-scores. lenhei is recumbent length through 730 days
# and standing height after that, and bmi is wt / (lenhei/100)^2, so no
# length/height adjustment by anthro is involved.
#
#   anthro    who_z()
#   zlen      who_lhaz     length/height-for-age
#   zwei      who_waz      weight-for-age
#   zwfl      who_wflz     weight-for-length
#   zbmi      who_bmiz     BMI-for-age
#   zhc       who_headcz   head circumference-for-age
#
# anthro reports z-scores to 2 decimal places, so who_z() output is rounded
# the same way before comparison. Two calculations that differ only in the
# last floating-point digits could round to opposite sides of a boundary, so
# the tolerance is one unit in the second decimal place.
#
# anthro gives weight-for-height for children over 730 days; who_z() gives
# weight-for-length only, for ages <= 730 days, so zwfl is compared for those
# ages only. Both packages give NA after 1826 days (60 months).

test_that("who_z() agrees with the WHO anthro package", {
  f <- test_path("fixtures", "who_anthro.csv")
  skip_if_not(file.exists(f), "fixtures/who_anthro.csv not present")

  ref <- utils::read.csv(f)
  out <- who_z(ref[c("sex", "agedays", "wt", "lenhei", "bmi", "headc")])

  pairs <- c(zlen = "who_lhaz", zwei = "who_waz", zwfl = "who_wflz",
             zbmi = "who_bmiz", zhc = "who_headcz")

  for (a in names(pairs)) {
    w <- pairs[[a]]
    v <- out[[w]]
    expect_false(is.null(v), label = paste(w, "is not a who_z() output column"))
    if (is.null(v)) next

    rows <- if (a == "zwfl") ref$agedays <= 730 else rep(TRUE, nrow(ref))
    r   <- v[rows]
    tol <- 0.005 + 1e-8 # the unrounded who_z() value must be within 0.005 of the anthro value
    s <- ref[[a]][rows]

    expect_gt(sum(!is.na(s)), 0)
    expect_identical(is.na(r), is.na(s),
                     label = paste("NA pattern of", w, "vs anthro", a))
    expect_lte(max(abs(r - s), na.rm = TRUE), tol,
               label = paste("max |who_z - anthro| for", w))
  }
})
