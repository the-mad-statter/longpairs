test_that("formula_terms() works", {
  expect_equal(formula_terms(mtcars, "mpg ~ vs"), c("mpg", "vs"))
})
