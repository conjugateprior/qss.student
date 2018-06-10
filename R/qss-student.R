#' qss.student
#'
#' This package contains problem sets from
#' K. Imai (2016) 'An Introduction to Quantitative Social
#' Science' Princeton University Press.
#'
#' @section Unpacking a Problem Set:
#'
#' Typically, your instructor will assign you the name of a problem set, e.g.
#' 'gay-marriage'.  You can unpack the problem set into your local file
#' system using \code{\link{get_pset}}, like this:
#'
#' \code{get_pset('gay-marriage')}
#'
#' This will unpack the problem set materials into a folder called
#' 'gay-marriage' in your current working directory.
#' (Type \code{\link{getwd}} if you're
#' not sure where that is.  You can change it using \code{\link{setwd}} or
#' through the graphical user interface).
#'
#' Don't worry about typing the name wrong. If it's not recognized you'll get a
#' menu of problem sets to choose from.
#'
#' @section Starting Again:
#' If you want to start again on the same problem set, you'll can unpack the
#' problem set again under a different name (\code{\link{get_pset}} won't
#' overwrite an existing folder.)  To do this use the
#' \code{newname} argument.  If you want your new copy to be called
#' 'gay-marriage-take2', then use
#'
#' \code{get_pset('gay-marriage', newname = "gay-marriage-take2")}
#'
#' Provided there's no 'gay-marriage-take2' folder already in your
#' current working directory, you'll get a fresh problem set unpacked there.
#'
#' @section Lists and Previews:
#' To see the complete list of available problem sets, use
#' \code{\link{list_psets}}.
#'
#' If you want to preview the questions in a problem set without
#' unpacking it into your file system, use \code{\link{preview_pset}}.
#'
#' @docType package
#' @name qss.student
NULL

get_pset_by_name <- function(pname){
  system.file(file.path("extdata", paste0(pname, ".zip")),
              package = "qss.student")
}

#' Unpack a problem set by name
#'
#' This function finds a problem set by its name,
#' unpacks it into your current working directory, and
#' provides some hints to get going.
#'
#' By default the problem set will unpack into a folder of the same name.
#' If you'd prefer to unpack the problem set under a different name, perhaps
#' because you've decided to start fresh, or because for whatever reason there's
#' already a folder with that name where you want to unpack it, use the
#' \code{newname} argument to asign a new one.  (Note that this only renames the
#' top folder. The contents are keep their original names.)
#'
#' @param pname Name of a problem set
#' @param newname Your preferred name for the unpacked problem set.
#'                Default: NULL (use \code{pname} as the folder name)
#' @seealso \code{\link{preview_pset}} to see problem set questions without
#'          unpacking anything, and \code{\link{list_psets}} to see the complete
#'          set of problem sets.
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

#' List available problem sets
#'
#' Call this function to get the names of all problem sets
#' in the package.
#'
#' @return a vector of names of bundled problem sets
#' @export
list_psets <- function(){
  fs <- system.file("extdata", package = "qss.student")
  psets <- list.files(fs, pattern = "*.zip")
  sort(tools::file_path_sans_ext(psets))
}

#' Preview a problem set
#'
#' Launches the pdf viewer to show the questions in a problem set. If the
#' problem set name is not recognized a menu of options is provided.
#'
#' @param pname Name of a problem set
#'
#' @return Nothing.
#' @export
#'
preview_pset <- function(pname){
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
  tmp <- tempdir()
  utils::unzip(f, exdir = tmp)
  pdf <- file.path(tmp, pname, paste0(pname, ".pdf"))
  if (file.exists(pdf))
    system2("open", args = list(pdf))
  else
    message("Sorry. There doesn't seem to be a preview available for that pset")
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
