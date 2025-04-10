% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{convert_bvdr_marp}
\alias{convert_bvdr_marp}
\title{Convert .bvdr files to .marp files}
\usage{
convert_bvdr_marp(path_bvdr, verbose = FALSE)
}
\arguments{
\item{path_bvdr}{Character string. The full path of the .bvdr file you want to convert. For example, path_bvdr <- system.file("exdata/convert_bvdr_marp/20220811-00Za.bvdr", package = "GAPsurvey")}

\item{verbose}{Logical. Default = FALSE. If you would like a readout of what the file looks like in the console, set to TRUE.}
}
\description{
If you mistakenly delete the marport data for a haul, you can retrieve that data through this converter.
Before using this script,
1. Open the .bvdr file in Notepad ++ or a similar text editor.
2. Find the uninterpretable character symbol. Often, depending on the editor, this will look like a box or the highlighted letters "SUB". Find and delete (via replace) these characters for the whole document. An error will appear and only part of the file will be read (stopping at the line before where this unsupported symbol is) if you do not edit the data ahead of time.
3. Save the .bvdr file with these changes and use the link to that file below for path_bvdr
For an example of what a proper .marp file looks like, refer to system.file("exdata/convert_bvdr_marp/HAUL0001.marp", package = "GAPsurvey")
}
\examples{
# readLines(system.file("exdata/convert_bvdr_marp/20220811-00Za.bvdr",
#   package = "GAPsurvey"))[1:5] # input file
# head(convert_bvdr_marp(
#   path_bvdr = system.file("exdata/convert_bvdr_marp/20220811-00Za.bvdr",
#                                   package = "GAPsurvey"),
#           verbose = TRUE), 20)
# convert_bvdr_marp(
#   path_bvdr = system.file("exdata/convert_bvdr_marp/20220811-00Za.bvdr",
#                                   package = "GAPsurvey"))
# readLines(system.file("exdata/convert_bvdr_marp/20220811-00Za.marp",
#   package = "GAPsurvey")) # output file
}
