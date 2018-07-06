# qss.student 0.2.2
 
## Package 

* The package now requires swirl, on the assumption 
  students might also be using the qss-swirl course
 
## Functions

* get_pset now makes its own local temporary directory and
  deletes it after use to get around file.rename failing due
  to an "Invalid cross-device link" errors
* Fixed typos in the get_pset advice and made it a bit more
  verbose

# qss.student 0.2.1

## Package 
 
* Fixed reference and link to textbook in documentation
* Added travis CI

# qss.student 0.2.0

## Data

* updated data from commit 9870a33 of `kosukeimai/qss-inst`

## Functions

* `get_pset` no longer `setwd`'s to the new problem set folder
* `get_pset` allows unpacking under a new name
* `preview_pset` opens a compiled pdf problem set without unpacking it locally
* man pages rewritten assuming a student package user

## Package

* Vignette explaining intended usage
* New `pkgdown` website 

# qss.student 0.1.0

* First cut

