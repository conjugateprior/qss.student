## ----setup, include = FALSE----------------------------------------------
library(knitr)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)
knit_hooks$set(wrapper = function(before, options, envir) {
  if (before) {
    sprintf('    ```{r %s}\n', options$params.src)
  } else '    ```\n'
})

# Ascii tree from Jenny Bryan (adjusted to use a unicode arrow)
# https://gist.github.com/jennybc/2bf1dbe6eb1f261dfe60
twee <- function(path = getwd(), level = Inf) {
  fad <- list.files(path = path, recursive = TRUE, 
                    no.. = TRUE, include.dirs = TRUE)
  fad_split_up <- strsplit(fad, "/")
  too_deep <- lapply(fad_split_up, length) > level
  fad_split_up[too_deep] <- NULL
  jfun <- function(x) {
    n <- length(x)
    if (n > 1)
      x[n - 1] <- "\U21b3 "
    if (n > 2)
      x[1:(n - 2)] <- "   "
    x <- if (n == 1) c("- ", x) else c("  ", x)
    x
  }
  fad_subbed_out <- lapply(fad_split_up, jfun)
  cat(unlist(lapply(fad_subbed_out, paste, collapse = "")), sep = "\n")
}

## ---- eval = TRUE--------------------------------------------------------
library(qss.student)

## ------------------------------------------------------------------------
#  get_pset("bias-in-turnout")

## ------------------------------------------------------------------------
#  setwd("bias-in-turnout")

## ------------------------------------------------------------------------
#  file.edit("bias-in-turnout.Rmd")

## ------------------------------------------------------------------------
#  get_pset("bias-in-turnout")

## ------------------------------------------------------------------------
#  setwd("..")

## ------------------------------------------------------------------------
#  get_pset("bias-in-turnout", newname = "bias-in-turnout-the-sequel")

## ---- eval = TRUE--------------------------------------------------------
list_psets()

## ------------------------------------------------------------------------
#  preview_pset("constitutions")

