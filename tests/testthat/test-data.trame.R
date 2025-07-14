test_that("data.trame() produces a valid data.trame object", {
  dtrm <- data.trame(a = 1:3, b = letters[1:3], c = factor(LETTERS[1:3]),
    .key = c('a', 'b'), .rows = 3)

  expect_true(is.data.trame(dtrm))
  expect_equal(nrow(dtrm), 3L)
  expect_equal(ncol(dtrm), 3L)
  expect_s3_class(dtrm, "data.frame")
  expect_true("a" %in% key(dtrm))
  expect_true("b" %in% key(dtrm))
})
