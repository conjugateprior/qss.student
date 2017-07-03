#' fsi: POL 245 problem sets at FSI
#'
#' This package contains problem sets from the qss-inst repository
#' of exercises in Imai 2016 An Introduction to Quantitative Social Science.
#'
#' The instructor assigns students the name of a problem set, e.g. 'gay-marriage'.
#' Each student loads the package and calls
#'
#' \code{get_pset('gay-marriage')}
#'
#' This function unzips the problem set into the student's current working directory,
#' and switches their current working directory to the top level of the problem set.
#'
#' Students open the Rmd file and add their own text and code blocks to answer the
#' questions.  They then submit their compiled document as homework.
#'
#' A problem set from the qss-inst repository always contains
#' \itemize{
#'  \item{an Rmd file describing the data set and listing several question.}
#'  \item{an pdf version of the Rmd file for readability}
#'  \item{a data folder containing the data set}
#' }
#'
#'
#' @docType package
#' @name fsi
NULL


#' Locate a problem set by name and unpack it
#'
#' This function \itemize{
#'   \item{Finds a problem set by name},
#'   \item{Unpacks it to wherever you are working},
#'   \item{Moves your current working directory to the problem set folder (unless you set \code{setwd} to FALSE).}
#'   \item{Reminds you that you probably want to make the Files view match your new working directory, and tells you how.}
#' }
#'
#' @param pname Name of a problem set
#' @param setwd Whether to change working directory to where the files were unpacked. Default: TRUE
#' @return None
#' @export
#'
#' @examples
#' \dontrun{ get_pset("test") }
#'
get_pset <- function(pname, setwd=TRUE){
  f <- system.file(file.path("extdata", paste0(pname, ".zip")),
                   package="fsi")
  if (f == "")
    stop("Could not find a problem set called '", pname, "'")

  if (pname %in% dir("."))
    stop(paste0("It looks like you've already unpacked '", pname, "' here"))

  message(paste0("Unpacking the '", pname, "' problem set..."))
  unzip(f)

  if (setwd){
    setwd(pname)
    message(paste0("\nSetting the working directory to '", getwd(),
                   "'\n(To update RStudio's Files view, press the arrow next to the",
                  " grey file path above your Console)"))
  }
}

##
## Functions for creating the inst/extdata folder contents from inside qss-inst
##

strip_answers_and_overwrite <- function(f){
  lines <- readLines(f)
  n <- length(lines)
  rec = rep(TRUE, n)
  keep <- TRUE
  for (i in 1:n){
    if (grepl("## Question ", lines[i]))
      keep <- TRUE
    if (grepl("## Answer ", lines[i]))
      keep = FALSE
    rec[i] <- keep
  }
  writeLines(lines[rec], f)
}

# function to be run in the top directory of qss-inst
make_zips <- function(..., student=FALSE,
                         output = c("html_document", "pdf_document")){

  chapters <- c("CAUSALITY", "DISCOVERY", "INTRO", "MEASUREMENT", "PREDICTION")
                ## "PROBABILITY", "UNCERTAINTY")
  chs <- c(...)
  if (!is.null(chs)) # of they specified any in particular, override the list
    chapters <- chs

  bundletype <- ifelse(student, "student", "instructor")

  psets <- unlist(lapply(chapters, list.dirs, recursive=FALSE))
  lapply(psets, file.copy, to=".", recursive = TRUE) # drag to top level
  psets <- lapply(psets, basename) # the new paths
  rmds <- lapply(psets, list.files, recursive=TRUE, pattern=".Rmd$", full.name=TRUE)
  print(rmds)
  process_rmd <- function(x){
    message("Processing ", x)
    if (student)
      strip_answers_and_overwrite(x)
    rmarkdown::render(x, output)
  }
  lapply(rmds, process_rmd)

  stopfilenames <- ifelse(student, # files to leave out of the zips
                          ".DS_Store|(README.md)|(lesson-plan.md)|(pics)",
                          ".DS_Store|README.md")
  remove_stopfiles <- function(x){
    fs <- list.files(x, recursive=TRUE)
    togo <- fs[grepl(stopfilenames, fs)]
    if (length(togo) > 0)
      file.remove(file.path(x, togo))
  }
  lapply(psets, remove_stopfiles)
  lapply(psets, function(x){ zip(paste0(x, ".zip"), x) })
  lapply(psets, unlink, recursive=TRUE)
}
