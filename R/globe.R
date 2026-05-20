#' Recover position data from Globe .log file
#'
#' In the event that the MARPORT server GPS fails or is incomplete, "convert_log_gps()" converts GLOBE LOG files into a format that can be uploaded into WHEELHOUSE.
#' To get a .log file that is usable in this function,
#' 1) Go the C:\ globe\ logs\ 2018\ directory and choose GLG file with proper date
#' 2) Use GLOBE Files>Logs> to convert .GLG (binary) to a .LOG (.csv) file
#' 3) convert_log_gps()will prompt you for Vessel code, Cruise no., Haul no. and Date
#' 4) The final prompt will ask for the location of the GLOBE LOG file
#' 5) convert_log_gps()will create csv file in the R directory with filename "new.gps"
#' 6) Rename "new.gps" to HAULXXXX.GPS where XXXX is the haul number
#' 7) Upload HAULXXXX.GPS into WHEELHOUSE
#' 8) NOTE: The raw GLOBE log data are in GMT time (-8 hrs or 4PM AKDT prior day to 4PM current day. Hence if haul with missing GPS spans the 4PM hour (e.g.,3:45-4:30 PM),YOU WILL HAVE TO CONVERT TWO GLG files (current day and next day)and run convert_log_gps()twice & manually combine the two GPS files
#' 9) ALSO NOTE: You may have to shut down GLOBE or wait until after 4pm on following day before all the incoming NMEA data are written to the GLG file.
#'
#' Now that you have a .log file, you can RUN the function by putting your cursor on the "convert_log_gps()" line below & press CTRL+R.
#'
#' @param VESSEL Optional. Default = NA. The vessel number (e.g., 94). If NA or not called in the function, a prompt will appear asking for this data.
#' @param CRUISE Optional. Default = NA. The cruise number, which is usually the year date (e.g., 201901). If NA or not called in the function, a prompt will appear asking for this data.
#' @param HAUL Optional. Default = NA. The haul number, aka the iterative number of this haul (e.g., 3). If NA or not called in the function, a prompt will appear asking for this data.
#' @param DATE Optional. Default = NA. The date in MM/DD/YYYY format (e.g., "06/02/2019"). If NA or not called in the function, a prompt will appear asking for this data.
#' @param path_in Optional. Default = "./., or the local working directory but any path (as a string) may be entered.
#' @param path_out Optional. The default is the local working directory but may be specified with a string.
#' @param filename_add Optional. Default = "new". This string will be added to the name of the outputted file. Here, you can additional information that may make this file helpful to find later.
#'
#' @return A .GPS file to the path_out directory with DATE/TIME in AKDT.
#' @noRd
#'
#' @examples
#' readLines(system.file("exdata/convert_log_gps/06062017.log",
#'   package = "GAPsurvey"))[1:5] # input file
#' convert_log_gps(
#'     VESSEL = 94,
#'     CRUISE = 201901,
#'     HAUL = 3,
#'     DATE = "06/06/2017",
#'     path_in = system.file("exdata/convert_log_gps/06062017.log",
#'         package = "GAPsurvey"),
#'     path_out = getwd(),
#'     filename_add = "newlog")
#' readLines(system.file("exdata/convert_log_gps/HAUL0003_newlog.gps",
#'   package = "GAPsurvey"))[1:5] # output file
convert_log_gps <- function(
    VESSEL = NA,
    CRUISE = NA,
    HAUL = NA,
    DATE = NA,
    path_in,
    path_out = "./",
    filename_add = "") {

  if (is.na(VESSEL)) {
    VESSEL <- readline("Type vessel code:  ")
  }
  if (is.na(CRUISE)) {
    CRUISE <- readline("Type cruise number:  ")
  }
  if (is.na(HAUL)) {
    HAUL <- readline("Type haul number:  ")
  }
  if (is.na(DATE)) {
    DATE <- readline("Type date of haul (MM/DD/YYYY):  ")
  }

  path_in <- fix_path(path_in)
  file.name <- path_in

  # make sure path_in comes in with correct format
  path_out <- fix_path(path_out)

  HAUL <- as.numeric(HAUL)
  shaul <- numbers0(x = HAUL, number_places = 4)

  log.file <- utils::read.csv(file.name, header = F, sep = ",")

  only.GPRMC <- log.file[log.file$V1 == "$GPRMC", ]

  only.GPRMC <- only.GPRMC[, c(2, 4, 5, 6, 7)]

  info <- cbind(VESSEL, CRUISE, HAUL, DATE)
  infoselect <- cbind(info, only.GPRMC)
  colnames(infoselect) <- c("VESSEL", "CRUISE", "HAUL", "DATE", "TIME", "LAT1", "LAT2", "LONG1", "LONG2")

  tstamp <- round(as.numeric(infoselect$TIME)) # sometimes this reads as chr and sometimes as num so force to num. Sometimes a decimal timestamp will break it if you don't round.
  tstamp <- sprintf("%06d", tstamp) # add leading zeroes
  hh <- as.numeric(substr(tstamp, start = 1, stop = 2))
  hh <- ifelse(hh < 8, hh + 24, hh) - 8 # convert to AKDT
  mm <- substr(tstamp, start = 3, stop = 4)
  ss <- substr(tstamp, start = 5, stop = 6)
  DATE_TIME <- paste(infoselect$"DATE", paste(hh, mm, ss, sep = ":"))


  lat1 <- as.numeric(as.character(infoselect$LAT1))
  LAT <- ifelse(infoselect$"LAT2" == "N", lat1, -lat1)
  LAT <- formatC(x = LAT, digits = 4, format = "f")

  long1 <- as.numeric(as.character(infoselect$LONG1))
  LONG <- ifelse(infoselect$"LONG2" == "E", long1, -long1)
  LONG <- formatC(x = LONG, digits = 4, format = "f")

  new_gps <- cbind.data.frame(VESSEL, CRUISE, HAUL, DATE_TIME, LAT, LONG)


  filename <- paste0(
    path_out, "HAUL", shaul,
    ifelse(is.na(filename_add) | filename_add == "",
           "", paste0("_", filename_add)
    ),
    ".gps"
  )

  new_gps1 <- new_gps
  new_gps1 <- as.matrix(new_gps1)

  utils::write.table(
    x = new_gps1,
    file = filename,
    quote = FALSE,
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    eol = "\n"
  )

  message(paste0("Your new .gps files are saved to ", filename))
}


