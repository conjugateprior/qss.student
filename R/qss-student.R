#' fsi: POL 245 problem sets at FSI
#'
#' This package contains problem sets from the qss-inst repository
#' of exercises in Imai 2016 An Introduction to Quantitative Social Science.
#'
#' The instructor assigns students the name of a problem set, e.g.
#' 'gay-marriage'.  Each student loads the package and calls
#'
#' \code{get_pset('gay-marriage')}
#'
#' This function unzips the problem set into the student's current working
#' directory, and switches their current working directory to the top level of
#' the problem set.
#'
#' Students open the Rmd file and add their own text and code blocks to answer
#' the questions.  They then submit their compiled document as homework.
#'
#' A problem set from the qss-inst repository always contains
#' \itemize{
#'  \item{an Rmd file describing the data set and listing several question.}
#'  \item{an pdf version of the Rmd file for readability}
#'  \item{a data folder containing the data set}
#' }
#' @docType package
#' @name qss-student
NULL


get_pset_by_name <- function(pname){
  system.file(file.path("extdata", paste0(pname, ".zip")),
              package="qss-student")
}



#' Unzips problem set in working directory
#'
#' @param fname Problem set name
#' @param setwd Whether to set the working directory to the unpacked directory
#' @param overwrite Whether to overwrite any existing versions of the same
#'                  problem set.
#'
#' @return Nothing
unzip_pset_in_wd <- function(fname, setwd, overwrite){
  if (!overwrite && (fname %in% list.dirs(".")))
    stop(paste0(fname,
                " is already unpacked here. To overwrite, set overwrite=TRUE\n",
                "in get_pset."))
  utils::unzip(fname, overwrite = overwrite)
  if (setwd){
    setwd(fname)
    message(paste0("You might now want to update the files pane to look at the new problem set.",
      "In RStudio, press the grey right-bending arrow in the Console heading",
      collapse = "\n"))
  }
}

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
#' @param overwrite Whether to overwrite any previously unpacked problem sets.
#'                  (Default: FALSE)
#' @param setwd Whether to change working directory to where the files were
#' unpacked. Default: TRUE
#' @export
get_pset <- function(pname, overwrite = FALSE, setwd = FALSE){
  f <- get_pset_by_name(pname)
  if (f == ""){
    message("There is no problem set called '", pname, "'")
    psets <- list_psets()
    pmat <- agrep(pname, psets, max.distance = 3,
                  value = TRUE, ignore.case = TRUE)
    if (length(pmat) > 0 && length(pmat) < 6){
      v <- utils::menu(c(psets, "No, it's not one of these"),
           "Did you mean one of these?")
      if (!(v %in% c(0, length(psets) + 1))){
        f <- file.path(system.file(file.path("extdata"), package = "fsi"),
                       paste0(psets[v], ".zip"))
        unzip_pset_in_wd(f, setwd, overwrite)
      }
    }
    return(invisible(NULL)) # bailed out of menu or didn't identify pset name
  } else
    unzip_pset_in_wd(f, setwd, overwrite)
}

#   if (!(pname %in% dir("."))){
#     if (setwd){
#       setwd(pname)
#       message(paste0("\nSetting the working directory to '", getwd(),
#                      "'\n(To update RStudio's Files view, press the arrow next to the",
#                      " grey file path above your Console)"))
#     }
#   } else {
#     v <- utils::menu(
#       c("Set your working directory to the old unpacked problem",
#         "Rename the old unpacked problem set and unpack a fresh one",
#         "Stop"),
#         "It looks like that problem set has already been unpacked here.\nDo you want to")
#
#     if (v == 2){
#
#     } else if (v == 1)
#       setwd(pname)
#     else
#       return(invisible(NULL))
#   }
#   message(paste0("Unpacking the '", pname, "' problem set..."))
#   unzip(f)
#
#
# }



#' List available problem sets
#'
#' Call this function to get the names of all problem sets bundled
#' in the package.  \code{get_pset} will try to guess which problem set
#' you meant, but it will not always succeed.  This is the complete set.
#'
#' @return a vector of names of bundled problem sets
#' @export
list_psets <- function(){
  psets <- list.files("extdata", pattern = "*.zip")
  sort(tools::file_path_sans_ext(psets))
}
#
# ##
# ## Functions for creating the inst/extdata folder contents from inside qss-inst
# ##
#
# strip_answers_and_overwrite <- function(f){
#   lines <- readLines(f)
#   n <- length(lines)
#   rec = rep(TRUE, n)
#   keep <- TRUE
#   for (i in 1:n){
#     if (grepl("## Question ", lines[i]))
#       keep <- TRUE
#     if (grepl("## Answer ", lines[i]))
#       keep = FALSE
#     rec[i] <- keep
#   }
#   writeLines(lines[rec], f)
# }
#
# # function to be run in the top directory of qss-inst
# make_zips <- function(..., student=FALSE,
#                          output = c("html_document", "pdf_document")){
#
#   chapters <- c("CAUSALITY", "DISCOVERY", "INTRO", "MEASUREMENT", "PREDICTION")
#                 ## "PROBABILITY", "UNCERTAINTY")
#   chs <- c(...)
#   if (!is.null(chs)) # of they specified any in particular, override the list
#     chapters <- chs
#
#   bundletype <- ifelse(student, "student", "instructor")
#
#   psets <- unlist(lapply(chapters, list.dirs, recursive=FALSE))
#   lapply(psets, file.copy, to=".", recursive = TRUE) # drag to top level
#   psets <- lapply(psets, basename) # the new paths
#   rmds <- lapply(psets, list.files, recursive=TRUE, pattern=".Rmd$", full.name=TRUE)
#   print(rmds)
#   process_rmd <- function(x){
#     message("Processing ", x)
#     if (student)
#       strip_answers_and_overwrite(x)
#     rmarkdown::render(x, output)
#   }
#   lapply(rmds, process_rmd)
#
#   stopfilenames <- ifelse(student, # files to leave out of the zips
#                           ".DS_Store|(README.md)|(lesson-plan.md)|(pics)",
#                           ".DS_Store|README.md")
#   remove_stopfiles <- function(x){
#     fs <- list.files(x, recursive=TRUE)
#     togo <- fs[grepl(stopfilenames, fs)]
#     if (length(togo) > 0)
#       file.remove(file.path(x, togo))
#   }
#   lapply(psets, remove_stopfiles)
#   lapply(psets, function(x){ zip(paste0(x, ".zip"), x) })
#   lapply(psets, unlink, recursive=TRUE)
# }
