#!/bin/sh

# Install qss-inst (Assumed to be in ~/wip/qss-inst)
# From R call:
#   make_bundles("CAUSALITY", "DISCOVERY", "INTRO", "PREDICTION", "MEASUREMENT",
#                student = TRUE, output = "pdf_document", pset_zips = TRUE)
# This fills qss-student/ with zipped psets (without solutions)
#
QSSINST="~/wip/qss-inst-forked"
QSSSTUDENT="~/wip/qss.student"

rm $QSSSTUDENT/extdata/*.zip
cp $QSSINST/qss-student/*.zip $QSSSTUDENT/extdata/
