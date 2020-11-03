library(testthat)
library(checkmate)

context("col_means")

test_that("basically works", {
  # check structure:
  expect_data_frame(col_means(mtcars),
                    min.rows = 1, max.rows = 1,
                    min.cols = ncol(mtcars), max.cols = ncol(mtcars))
  #check content:
  expect_equivalent(col_means(mtcars),
                    colMeans(as.matrix(mtcars)))
  #check details:
  expect_equal(colnames(col_means(mtcars)),
               colnames(mtcars))
})

test_that("works for simple vectors", {
  expect_equivalent(col_means(1:10),
                    5.5)
})


test_that("behaves identically for equivalent inputs", {
  expect_equal(col_means(as.matrix(mtcars)),
               col_means(mtcars))
  expect_equal(col_means(as.list(mtcars)),
               col_means(mtcars))
  expect_equal(col_means(mtcars[, "mpg", drop = FALSE]),
               col_means(mtcars)[, "mpg", drop = FALSE])
})

test_that("behaves sensibly for empty inputs", {
  expect_warning(col_means(mtcars[, 0]))
  expect_warning(col_means(mtcars[0, ]))
  expect_data_frame(col_means(mtcars[, 0]),
                    max.rows = 0, max.cols = 0)
  expect_data_frame(col_means(mtcars[0, ]),
                    max.rows = 0, max.cols = 0)
  expect_equal(col_means(mtcars[, 0]),
               col_means(mtcars[0, ]))
})

test_that("deals with missings", {
  mtcars_NA <- mtcars
  mtcars_NA[1:10, "mpg"] <- NA
  expect_equal(col_means(mtcars_NA)[, "mpg"],
               NA_real_)
  expect_equal(col_means(mtcars_NA)[, -1],
               col_means(mtcars)[, -1])
  expect_equal(col_means(mtcars_NA, na.rm = TRUE)[, "mpg"],
               col_means(mtcars[-(1:10), ])[, "mpg"])
  expect_equal(col_means(mtcars_NA, na.rm = TRUE)[, -1],
               col_means(mtcars)[, -1])
})

test_that("deals with factors, characters", {
  mtcars_chr <- mtcars
  mtcars_chr[-1] <- lapply(mtcars_chr[-1], as.character)
  expect_data_frame(col_means(mtcars_chr),
                    min.rows = 1, max.rows = 1,
                    min.cols = 1, max.cols = 1)
  expect_equal(col_means(mtcars_chr),
               col_means(mtcars)[, "mpg", drop = FALSE])

  mtcars_fct <- mtcars
  mtcars_fct$carb <- ordered(mtcars_fct$carb)
  expect_equal(col_means(mtcars)[, -11],
               col_means(mtcars_fct))
  expect_warning(col_means(mtcars_fct[, "carb", drop = FALSE]))
  expect_equal(col_means(mtcars[, 0]),
               col_means(mtcars_fct[, "carb", drop = FALSE]))
})

#extra bonus:
if (FALSE) {
 #deal with Date-variables as well
    mtcars_date <- mtcars
    mtcars_date$date <- as.Date(seq_len(nrow(mtcars)), origin = Sys.Date())
    expect_equal(col_means(mtcars_date),
                 cbind(col_means(mtcars), date = mean(mtcars_date$date)))

  #deal with matrix-columns
    mtcars_matrix <- mtcars
    mtcars_matrix$matrix_column <- as.matrix(mtcars[, 3:4])
    # expect_what?

  #deal with list-columns
    mtcars_list <- mtcars
    mtcars_list$list_column <- list(mpg = mtcars$mpg, hp = mtcars$hp)
    # expect_what?
}
