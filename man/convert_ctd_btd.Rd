% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hex_to_cnv.R
\name{convert_ctd_btd}
\alias{convert_ctd_btd}
\title{Convert CTD data from .hex to BTD and BTH}
\usage{
convert_ctd_btd(
  filepath_hex = NULL,
  dirpath_output = NULL,
  filepath_xmlcon = NULL,
  latitude = NA,
  VESSEL = NA,
  CRUISE = NA,
  HAUL = NA,
  MODEL_NUMBER = NA,
  VERSION_NUMBER = NA,
  SERIAL_NUMBER = NA,
  instrument_timezone = "America/Anchorage",
  filename_add = "new"
)
}
\arguments{
\item{filepath_hex}{Required. Filepath to the SBE19plus hexadecimal (.hex) file from a single cast as a character vector (e.g. "C:/CTD/202301_162_L1/sbe19plus01908091_05_04_0001.hex")/.}

\item{dirpath_output}{Optional. The default is the local working directory but may be specified with a string.}

\item{filepath_xmlcon}{Required. Filepath to the instrument configuration file (.xmlcon) for the CTD (e.g. serial number 8091 would use the file with 8091 in the name ("C:/CTD/xmlcon configuration files/SBE19plusV2_8091.xmlcon").}

\item{latitude}{Required. Latitude in decimal degrees.}

\item{VESSEL}{Required. The vessel number (e.g., 94).}

\item{CRUISE}{Required. The cruise number (e.g., 201901).}

\item{HAUL}{Required. The haul number (e.g. 150).}

\item{MODEL_NUMBER}{Optional. The model number of the CTD .}

\item{VERSION_NUMBER}{Optional. The version number of the CTD.}

\item{SERIAL_NUMBER}{Optional. The serial number of the CTD.}

\item{instrument_timezone}{Time zone for the instrument data. Do not change unless the instrument is not setup for Alaska Time}

\item{filename_add}{Optional. Default = "new". This string will be added to BTD and BTH outputs. This can help prevent accidentally overwriting BTH and BTD files.}
}
\value{
.BTH and .BTD files to the dirpath_output directory.
}
\description{
This function converts a CTD  hexadecimal (.hex) file to bathythermic data (.btd) and bathythermic header files (.bth). If you are unable to convert your file, please contact sean.rohan@noaa.gov.
}
\examples{
\dontrun{
# CTD without auxiliary sensors

# Run without passing arguments as code

convert_ctd_btd()

convert_ctd_btd(
 filepath_hex = system.file(paste0("exdata/convert_ctd_btd/",
   "2021_06_13_0003.hex"), package = "GAPsurvey"),
 filepath_xmlcon = system.file(paste0("exdata/convert_ctd_btd/",
   "19-8102_Deploy2021.xmlcon"), package = "GAPsurvey"),
 latitude = 55,
 VESSEL = 94,
 CRUISE = 202101,
 HAUL = 107,
 SERIAL_NUMBER = 8105)

}
}
