context("get_pset")

test_that("in a fresh directory", {
  tmp <- tempdir()
  setwd(tmp)
  if (dir.exists("gay-marriage")) # clean start
    unlink("gay-marriage", recursive = TRUE)

  get_pset("gay-marriage")
  expect_true(any(grepl("gay-marriage", list.dirs()))) # folder unpacks
  rmd <- file.path(tmp, "gay-marriage", "gay-marriage.Rmd")
  expect_true(file.exists(rmd)) # with contents

  unlink("gay-marriage", recursive = TRUE) # remove
})

test_that("in a fresh directory, setting the newname", {
  tmp <- tempdir()
  setwd(tmp)
  if (dir.exists("gay-marriage")) # clean start
    unlink("gay-marriage", recursive = TRUE)

  get_pset("gay-marriage", newname = "gmm")
  expect_true(file.exists("gmm")) # we moved
  expect_true("gay-marriage.Rmd" %in% list.files("gmm")) # we're next to the rmd

  setwd(tmp)
  unlink("gmm", recursive = TRUE) # remove
})

test_that("in a directory with a pset already unpacked", {
  tmp <- tempdir()
  setwd(tmp)
  if (dir.exists("gay-marriage")) # clean start
    unlink("gay-marriage", recursive = TRUE)

  get_pset("gay-marriage")
  expect_error(get_pset("gay-marriage"), "exists here already")
})
