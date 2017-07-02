

#' Locate a problem set by name and unpack it
#'
#' Finds a problem set by name and unpacks it in your file system.
#' By default the problem set will unpack in your home directory.
#' and set that to your working directory.
#'
#' @param pname Name of the problem set
#' @param setwd Whether to change working directory to where the files were unpacked. Default: TRUE
#' @param ... Extra arguments to pass to \code{unzip}
#' @return None
#' @export
#'
unpack_pset <- function(pname, setwd=TRUE, ...){
  f <- system.file(file.path("extdata", paste0(pname, ".zip")), package="fsi")
  unzip(f, ...)
  if (setwd)
    setwd(pname)
}

## Functions for creating the inst/extdata folder contents from inside qss-inst

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

make_bundles <- function(..., student=FALSE,
                         output = c("html_document", "pdf_document")){

  chapters <- c("CAUSALITY", "DISCOVERY", "INTRO", "MEASUREMENT", "PREDICTION",
                "PROBABILITY", "UNCERTAINTY")
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
