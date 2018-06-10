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
#'  \item{a pdf version of the Rmd file for readability}
#'  \item{a data folder containing the data set}
#' }
#' @docType package
#' @name qss-student
NULL

get_pset_by_name <- function(pname){
  system.file(file.path("extdata", paste0(pname, ".zip")),
              package = "qss.student")
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
#' @param newname What you'd like the unpacked problem set folder to be called
#'                (Defaults to the value of \code{pname})
#'
#' @export
get_pset <- function(pname, newname = NULL){
  f <- get_pset_by_name(pname)
  if (f == "") {
    message("There is no problem set called '", pname, "'")
    psets <- list_psets()
    v <- utils::menu(c(psets), "Did you mean one of these?")
    if (v != 0) {
      pname <- psets[v]
      f <- file.path(system.file(file.path("extdata"), package = "qss.student"),
                     paste0(psets[v], ".zip"))
    } else
      return()
  }
  # f is the zip file
  temp <- tempfile()
  utils::unzip(f, exdir = temp)
  if (!is.null(newname)) { # they've assigned a new name
    if (!file.exists(newname)) {
      file.rename(file.path(temp, pname), newname)
      dname <- newname
    } else
      stop(paste0('"', newname, '" exists here already. ",
                  "Choose a different newname or delete the "', newname,
                  '" folder first.'))
  } else {
    # they want it to use its original name
    if (!file.exists(pname)) {
      file.rename(file.path(temp, pname), pname)
      dname <- pname
    } else
      stop(paste0('"', pname, '" exists here already. ",
                  "Assign a newname or delete the "', pname,
                  '" folder first.'))
  }

  cli::cat_line("Hint: To start working on this pset", col = "darkgray")
  cli::cat_line("")
  cli::cat_bullet('setwd("', dname, '")',
                  col = "darkgray", bullet_col = "grey", bullet = "pointer")
  cli::cat_bullet('file.edit("', paste0(dname, '.Rmd'), '")',
                  bullet_col = "gray", col = "darkgray", bullet = "pointer")
  cli::cat_line("")

  cli::cat_line('To view the questions in compiled form, click on ',
                paste0(dname, '.pdf'), "in the Files tab",
                col = "darkgray", bullet_col = "grey", bullet = "pointer")
  if (identical(.Platform$GUI, "RStudio")) {
    cli::cat_line("")
    cli::cat_line("It's also good idea to update the files pane ",
                  "to show your current working directory", col = "darkgrey")
    cli::cat_line("To do that, click on the grey right-turning arrow ",
                  "in this header of this Console", col = "darkgrey")
  }
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
  fs <- system.file("extdata", package = "qss.student")
  psets <- list.files(fs, pattern = "*.zip")
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
