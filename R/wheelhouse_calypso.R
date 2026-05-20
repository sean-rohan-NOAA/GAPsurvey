#' BVDR Conversion to Create BTD data
#'
#' Converts Marport BVDR data (.ted and .tet files from Marport headrope sensor) to .BTD format.  You must first run the BVDR converter program (BVDRReader.exe) to convert the Marport .bvdr files into .ted and .tet files that can be pulled into R. The BVDR program and instructions can be found in the RACE Survey App (NEW BVDR README.txt).  You will have to create your own .SGT file using the example in the BVDR instruction file with start and end time (be sure to include a carriage return after your (second and) final row of data!), because this is not a file that our current systems creates.  Once you have used the BVDR converter to output the .ted and .tet files you are ready to use the convert_ted_btd() function here!
#' @param VESSEL Optional. Default = NA. The vessel number (e.g., 162 for AK Knight, 94 for Vesteraalen). If NA or not called in the function, a prompt will appear asking for this data.
#' @param CRUISE Optional. Default = NA. The cruise number, which is usually the year + sequential two digit cruise (e.g., 202101). If NA or not called in the function, a prompt will appear asking for this data.
#' @param HAUL Optional. Default = NA. The haul number that you are trying to convert data for (e.g., 3). If NA or not called in the function, a prompt will appear asking for this data.
#' @param MODEL_NUMBER Optional. Default = NA. The model number of the Marport sensor (e.g., 123 or 999, you can put in NA or a dummy number here instead of the actual model number without any negative repercussions).
#' @param VERSION_NUMBER Optional. Default = NA. The version number of the Marport sensor (e.g., 123 or 999, you can put in NA or a dummy number here instead of the actual version number without any negative repercussions).
#' @param SERIAL_NUMBER Optional. Default = NA. The serial number of the Marport sensor (e.g., 123 or 999, you can put in NA or a dummy number here instead of the actual serial number without any negative repercussions).
#' @param path_in Optional. The default is the location on the catch computer ("C:/Program Files/Marport Server/Logs/") but any path can be entered.
#' @param path_out Optional. The default is the local working directory but can be specified with a string.
#' @param filename_add Optional. Default = "new". This string will be added to the name of the outputed file. Here, you can additional information that may make this file helpful to find later.
#'
#' @return .BTH and .BTD files to the path_out directory.
#' @noRd
#'
#' @examples
#' # input files
#' readLines(system.file("exdata/convert_bvdr_btd/201901_94_0003.ted",
#'   package = "GAPsurvey"))[1:5]
#' readLines(system.file("exdata/convert_bvdr_btd/201901_94_0003.tet",
#'   package = "GAPsurvey"))[1:5]
#' readLines(system.file("exdata/convert_bvdr_btd/201901_94_0003.teh",
#'   package = "GAPsurvey"))[1:5]
#' #' run function
#' convert_ted_btd(
#'    VESSEL = 94,
#'    CRUISE = 201901,
#'    HAUL = 3,
#'    MODEL_NUMBER = 123,
#'    VERSION_NUMBER = 456,
#'    SERIAL_NUMBER = 789,
#'    path_in = system.file("exdata/convert_bvdr_btd/", package = "GAPsurvey"),
#'    path_out = getwd(),
#'    filename_add = "newted")
#' # output files
#' readLines(system.file("exdata/convert_bvdr_btd/HAUL0003_newted.BTD",
#'   package = "GAPsurvey"))[1:5]
#' readLines(system.file("exdata/convert_bvdr_btd/HAUL0003_newted.BTH",
#'   package = "GAPsurvey"))[1:5]
convert_ted_btd <- function(
    VESSEL = NA,
    CRUISE = NA,
    HAUL = NA,
    MODEL_NUMBER = NA,
    VERSION_NUMBER = NA,
    SERIAL_NUMBER = NA,
    path_in = "C:/Program Files/Marport Server/Logs/",
    path_out = "./",
    filename_add = "new"){

  format_date <- function(x, ...) {
    tmp <- format(x, ...)
    tmp <- sub("^[0]+", "", tmp)
    tmp <- sub('/0', "/", tmp)
    return(tmp)
  }

  if (is.na(VESSEL)){ VESSEL <- readline("Type vessel code:  ") }
  if (is.na(CRUISE)){ CRUISE <- readline("Type cruise number:  ") }
  if (is.na(HAUL)){ HAUL <- readline("Type haul number:  ") }
  if (is.na(MODEL_NUMBER)){ MODEL_NUMBER <- readline("Type model number:  ") }
  if (is.na(VERSION_NUMBER)){ VERSION_NUMBER <- readline("Type version number:  ") }
  if (is.na(SERIAL_NUMBER)){ SERIAL_NUMBER <- readline("Type serial number of Marport height sensor:  ") }

  # make sure path_in comes in with correct format
  path_in <- fix_path(path_in)
  path_out <- fix_path(path_out)

  HAUL <- as.numeric(HAUL)
  shaul <- numbers0(x = HAUL, number_places = 4)

  file.name.ted <- paste(path_in,
                         CRUISE,"_",VESSEL,"_",shaul,".ted",sep="")
  file.name.tet <- paste(path_in,
                         CRUISE,"_",VESSEL,"_",shaul,".tet",sep="")

  ted.file=utils::read.csv(file.name.ted,header=F)
  tet.file=utils::read.csv(file.name.tet,header=F)

  ted.file$V4=strptime(ted.file[,4], format = "%m/%d/%Y %H:%M:%S")
  tet.file$V4=strptime(tet.file[,4], format = "%m/%d/%Y %H:%M:%S")
  ted.file=ted.file[,c(4,6)]
  tet.file=tet.file[,c(4,6)]
  colnames(ted.file)=c("date","depth")
  colnames(tet.file)=c("date","temp")

  merged<-base::merge(ted.file,tet.file,all=T)


  xx=merged$date
  DATE_TIME <- format(xx, format = "%m/%d/%Y %H:%M:%S")

  DATE_TIME_btd <- format(as.POSIXct(DATE_TIME, format = "%m/%d/%Y %H:%M:%S"),
                          format = "%m/%d/%Y %H:%M:%S")

  DATE_TIME_btd <- format_date(DATE_TIME_btd)
  DATE_TIME <- format(xx, format = "%m/%d/%y %H:%M:%S")

  HOST_TIME=max(DATE_TIME)
  LOGGER_TIME=max(DATE_TIME)
  LOGGING_START=min(DATE_TIME)
  LOGGING_END=max(DATE_TIME)
  TEMPERATURE=merged$temp
  DEPTH=merged$depth
  SAMPLE_PERIOD=3
  NUMBER_CHANNELS=2
  NUMBER_SAMPLES=0
  MODE=2

  # Write BTD file
  DATE_TIME <- DATE_TIME_btd
  new.BTD=cbind(VESSEL,CRUISE,HAUL,SERIAL_NUMBER,DATE_TIME,TEMPERATURE,DEPTH)
  new.BTD[which(is.na(new.BTD))]=""
  new.BTD <- data.frame(new.BTD)

  # Write BTH file
  new.BTH=cbind(VESSEL,CRUISE,HAUL,MODEL_NUMBER,VERSION_NUMBER,SERIAL_NUMBER,
                HOST_TIME,LOGGER_TIME,LOGGING_START,LOGGING_END,
                SAMPLE_PERIOD,NUMBER_CHANNELS,
                NUMBER_SAMPLES,MODE)
  new.BTH <- data.frame(new.BTH)

  new.BTD=new.BTD[new.BTD$DEPTH!="2000",]

  filename <- paste0(path_out, "HAUL",shaul,
                     ifelse(is.na(filename_add) | filename_add == "",
                            "", paste0("_", filename_add)))
  utils::write.csv(x = new.BTD,
                   file = paste0(filename, ".BTD"),
                   quote=F,
                   row.names=F,
                   eol=",\n"
  )

  utils::write.csv(x = new.BTH,
                   file = paste0(filename, ".BTH"),
                   quote=F,
                   row.names=F)

  message(paste0("Your new ", filename, " .BTD and .BTH files are saved."))

}

#' Convert .bvdr files to .marp files
#' @description
#' If you mistakenly delete the marport data for a haul, you can retrieve that data through this converter.
#' Before using this script,
#' 1. Open the .bvdr file in Notepad ++ or a similar text editor.
#' 2. Find the uninterpretable character symbol. Often, depending on the editor, this will look like a box or the highlighted letters "SUB". Find and delete (via replace) these characters for the whole document. An error will appear and only part of the file will be read (stopping at the line before where this unsupported symbol is) if you do not edit the data ahead of time.
#' 3. Save the .bvdr file with these changes and use the link to that file below for path_bvdr
#' For an example of what a proper .marp file looks like, refer to system.file("exdata/convert_bvdr_marp/HAUL0001.marp", package = "GAPsurvey")
#' @param path_bvdr Character string. The full path of the .bvdr file you want to convert. For example, path_bvdr <- system.file("exdata/convert_bvdr_marp/20220811-00Za.bvdr", package = "GAPsurvey")
#' @param make_btd_bth Logical. Should a .btd and bth file be generated?
#' @param sort_by_path Logical. Should .bvdr files be read in alphabetical order. Note: Keep this TRUE when the original file names are used to ensure NMEA strings are read in chronological order.
#' @param verbose Logical. Default = FALSE. If you would like a readout of what the file looks like in the console, set to TRUE.
#' @param ... Optional additional arguments passed to convert_nmea_btd().
#' @importFrom utils choose.files
#' @noRd
#' @examples
#' # readLines(system.file("exdata/convert_bvdr_marp/20220811-00Za.bvdr",
#' #   package = "GAPsurvey"))[1:5] # input file
#' # head(convert_bvdr_marp(
#' #   path_bvdr = system.file("exdata/convert_bvdr_marp/20220811-00Za.bvdr",
#' #                                   package = "GAPsurvey"),
#' #           verbose = TRUE), 20)
#' # convert_bvdr_marp(
#' #   path_bvdr = system.file("exdata/convert_bvdr_marp/20220811-00Za.bvdr",
#' #                                   package = "GAPsurvey"))
#' # readLines(system.file("exdata/convert_bvdr_marp/20220811-00Za.marp",
#' #   package = "GAPsurvey")) # output file
convert_bvdr_marp <- function(path_bvdr = NULL,
                              make_btd_bth = TRUE,
                              sort_by_path = TRUE,
                              verbose = TRUE,
                              ...) {

  if(is.null(path_bvdr)) {
    path_bvdr <-
      choose.files(
        default = "*.bvdr",
        caption = "Select .bvdr file(s)",
        multi = TRUE,
        filters = matrix(c("Binary Voyage Data Recorder (.bvdr)", "*.bvdr"),
                         ncol = 2)
      )
  }

  # Sort entries by filename to ensure proper date/time order
  if(sort_by_path) {
    path_bvdr <- sort(path_bvdr)
  }

  # Read binary files and remove lines that are empty or missing valid start characters
  dat <- unlist(
    lapply(
      path_bvdr,
      function(x) {
        lines <- readBin(x, what = "rb", n = 1e8)
        lines <- iconv(lines, from = "latin1", to = "UTF-8")
        lines <- lines[nchar(lines) > 0]
        lines <-
          lines[any(
            c(grepl(lines, pattern = "\\$GPZDA"),
              grepl(lines, pattern = "\\$GPGLL"),
              grepl(lines, pattern = "\\$GPRMC"),
              grepl(lines, pattern = "\\$GPVTG"),
              grepl(lines, pattern = "\\$GPGGA"),
              grepl(lines, pattern = "\\$01TE"),
              grepl(lines, pattern = "\\:::m"),
              grepl(lines, pattern = "\\$01DST"))
          )]
      }
    ),
    use.names = FALSE)

  dat1 <- strsplit(x = dat, split = "\\$G", useBytes = TRUE)
  dat2 <- strsplit(x = dat, split = "\\:::m", useBytes = TRUE)
  dat3 <- strsplit(x = dat, split = "\\$01TE", useBytes = TRUE)
  dat4 <- strsplit(x = dat, split = "\\$01DST", useBytes = TRUE)

  dat1 <- lapply(
    seq_along(dat1), function(i) {
      if (length(dat4[[i]]) > 1) {
        dat <- dat4[[i]]
        dat[2] <- paste0("$01DST", dat[2])
      } else if (length(dat3[[i]]) > 1) {
        dat <- dat3[[i]]
        dat[2] <- paste0("$01TE", dat[2])
      } else if (length(dat2[[i]]) > 1) {
        dat <- dat2[[i]]
        dat[2] <- paste0(":::m", dat[2])
      } else if (length(dat1[[i]]) > 1) {
        dat <- dat1[[i]]
        dat[2] <- paste0("$G", dat[2])
      } else {
        dat <- dat1[[i]]
      }
      dat
    }
  )

  dat <- sapply(X = dat1, "[", 2)
  dat <- dat[!is.na(dat)]

  # Remove unnecessary carriage returns
  dat <- gsub(pattern = "\\r", replacement = "", x = dat)
  dat <- gsub(pattern = "\\n", replacement = "", x = dat)
  dat <- dat[!grepl(pattern = "\\$Gf", x = dat, useBytes = TRUE)]
  file_name_out <- gsub(pattern = ".bvdr", replacement = ".marp", x = path_bvdr[1], fixed = TRUE)

  writeLines(text = dat, con = file_name_out)

  # stopifnot("convert_bvdr_marp: .marp file was not successfully generated." = check.exists(file_name_out))

  cat(paste0("convert_bvdr_marp: ", length(dat), " lines written to ", file_name_out, "\n"))

  output <- list(marp = dat)

  # Create BTD and BTH files from NMEA strings
  if(make_btd_bth) {

    btd_bth_output <- convert_nmea_btd(nmea_strings = dat, ...)

    output <- c(output, btd_bth_output)

  }

  if(verbose) {
    return(output)
  }
}



#' Extract depth and temperature from NMEA strings
#'
#' Convert Marport Trawl Explorer NMEA strings to BTD and BTH files. Called internally by convert_bvdr_marp().
#'
#' @param nmea_strings Character vector of NMEA strings (e.g., from a .bvdr file) or a path to a .marp file.
#' @param interactive_editing Should the interactive point removal interface be used to manually clean temperature and depth data? If TRUE, must have graphic devices set to view plots in actual size (in R Studio: View > Actual Size or Ctrl+0)
#' @param min_depth Optional (default = -0.1). Minimum valid depth (m).
#' @param max_depth Optional (default = 1000). Maximum valid depth (m).
#' @param min_temperature Optional (default = -2). Maximum valid temperature (Celsius).
#' @param max_temperature Optional (default = 20). Maximum valid temperature (Celsius).
#' @param VESSEL Optional. Default = NA. The vessel number (e.g., 162 for AK Knight, 94 for Vesteraalen). If NA or not called in the function, a prompt will appear asking for this data.
#' @param CRUISE Optional. Default = NA. The cruise number, which is usually the year + sequential two digit cruise (e.g., 202101). If NA or not called in the function, a prompt will appear asking for this data.
#' @param HAUL Optional. Default = NA. The haul number that you are trying to convert data for (e.g., 3). If NA or not called in the function, a prompt will appear asking for this data.
#' @param MODEL_NUMBER Optional. Default = "Marport TE". The model name/number of the Marport sensor (e.g., 123 or 999, you can put in NA or a dummy number here instead of the actual model number without any negative repercussions). This field may have restrictions on length.
#' @param VERSION_NUMBER Optional. Default = NA. The version number of the Marport sensor (e.g., 123 or 999, you can put in NA or a dummy number here instead of the actual version number without any negative repercussions).
#' @param SERIAL_NUMBER Optional. Default = NA. The serial number of the Marport sensor (e.g., 123 or 999, you can put in NA or a dummy number here instead of the actual serial number without any negative repercussions).
#' @param ... additional arguments
#' @noRd
#' @examples \dontrun{
#' # Run this to select Marport (.marp files)
#' convert_nmea_btd()
#' }
#' @importFrom stats complete.cases
#' @import graphics
#' @author Sean Rohan <sean.rohan@@noaa.gov>


convert_nmea_btd <- function(nmea_strings = NULL, interactive_editing = TRUE, min_depth = -0.1, max_depth = 800, min_temperature = -2, max_temperature = 20, VESSEL = NA, CRUISE = NA, HAUL = NA, MODEL_NUMBER = "Marport TE", VERSION_NUMBER = NA, SERIAL_NUMBER = NA, ...) {

  format_date <- function(x, ...) {
    tmp <- format(x, ...)
    tmp <- sub("^[0]+", "", tmp)
    tmp <- sub('/0', "/", tmp)
    return(tmp)
  }

  if(is.null(nmea_strings)) {
    message("convert_nmea_btd: nmea_strings is NULL. Select a .marp file.")
    nmea_strings <-
      choose.files(
        default = "*.marp",
        caption = "Select .marp file(s)",
        multi = TRUE,
        filters = matrix(c("Marport (.marp)", "*.marp"),
                         ncol = 2)
      )

    stopifnot("convert_nmea_btd: Must select a file." = length(nmea_strings) >= 1)
  }

  if(all(grepl(pattern = ".marp", x = nmea_strings))) {

    message("convert_nmea_btd: Extracting NMEA strings from .marp files.")

    nmea_list <- vector(mode = "list", length = length(nmea_strings))
    nmea_strings <- lapply(
      X = nmea_strings,
      FUN = function(x) {
        lines <- readLines(x)
        lines[any(
          c(grepl(lines, pattern = "\\$GPZDA"),
            grepl(lines, pattern = "\\$GPGLL"),
            grepl(lines, pattern = "\\$GPRMC"),
            grepl(lines, pattern = "\\$GPVTG"),
            grepl(lines, pattern = "\\$GPGGA"),
            grepl(lines, pattern = "\\$01TE"),
            grepl(lines, pattern = "\\:::m"),
            grepl(lines, pattern = "\\$01DST"))
        )]
      })

    nmea_strings <- unname(unlist(nmea_strings))

  }

  # Add tests to check that NMEA strings include temperature and depth
  if(is.na(VESSEL)){ VESSEL <- readline("Type vessel code:  ") }
  if(is.na(CRUISE)){ CRUISE <- readline("Type cruise number:  ") }
  if(is.na(HAUL)){ HAUL <- readline("Type haul number:  ") }
  if(is.na(MODEL_NUMBER)){ MODEL_NUMBER <- readline("Type model number (optional):  ") }
  if(is.na(VERSION_NUMBER)){ VERSION_NUMBER <- readline("Type version number (optional):  ") }
  if(is.na(SERIAL_NUMBER)){ SERIAL_NUMBER <- readline("Type serial number of sensor (optional):  ") }

  # Initialize lists to store parsed data
  matched_bt <- list()

  # Function to convert HHMMSS.SSS to POSIXct
  parse_time <- function(hhmmss, date_str) {
    as.POSIXct(
      strptime(
        paste0(
          date_str,
          # sprintf("%06.3f", as.numeric(hhmmss))
          gsub(
            pattern = " ",
            replacement = "0",
            x =
              format(
                as.numeric(hhmmss),
                nsmall = 3,
                width = 10,
                trim = FALSE)
          )
        ),
        "%Y-%m-%d%H%M%OS"
      ),
      tz = "UTC"
    )
  }

  # Track current time from $GPZDA or :::msg yyyymmdd-HHMMSS
  current_time <- NA
  current_date <- NA
  year <- NA
  month <- NA
  day <- NA

  last_data_time <- list(depth = NA, temp = NA, height = NA)
  pending <- list()

  # Line-by-line processing
  for(line in nmea_strings) {

    # Parse lines to extract dates/times
    if(grepl(pattern = ".* (\\d{8})-\\d{6}Z", x = line)) {

      year <- as.numeric(sub(".* (\\d{4})\\d{4}-\\d{6}Z", "\\1", line))
      month <- as.numeric(sub(".*\\d{4}(\\d{2})\\d{2}-\\d{6}Z", "\\1", line))
      day   <- as.numeric(sub(".*\\d{6}(\\d{2})-\\d{6}Z", "\\1", line))
      time_str <- sub(".*-(\\d{6})Z", "\\1", line)
      current_date <- sprintf("%04d-%02d-%02d", as.integer(year), as.integer(month), as.integer(day))
      current_time <- parse_time(time_str, current_date)

    } else if(grepl("^\\$GPZDA", line)) {
      parts <- strsplit(line, ",")[[1]]
      time_str <- parts[2]
      day <- parts[3]
      month <- parts[4]
      year <- parts[5]
      current_date <- sprintf("%04d-%02d-%02d", as.integer(year), as.integer(month), as.integer(day))
      current_time <- parse_time(time_str, current_date)

    } else if(grepl("^\\$GPGGA", line) | grepl("^\\$GPRMC", line)) {
      parts <- strsplit(line, ",")[[1]]
      time_str <- parts[2]
      current_date <- sprintf("%04d-%02d-%02d", as.integer(year), as.integer(month), as.integer(day))
      current_time <- parse_time(time_str, current_date)

    } else if(grepl("^\\$01TED", line)) {
      val <- as.numeric(sub(",m.*", "", strsplit(line, ",")[[1]][2]))
      if(!is.na(current_time)) {
        last_data_time$depth <- current_time
        pending$depth <- list(value = val, time = current_time)
      }

    } else if(grepl("^\\$01TET", line)) {
      val <- as.numeric(sub(",C.*", "", strsplit(line, ",")[[1]][2]))
      if(!is.na(current_time)) {
        last_data_time$temp <- current_time
        pending$temp <- list(value = val, time = current_time)
      }

    } else if(grepl("^\\$01TEH", line)) {
      val <- as.numeric(sub(",m.*", "", strsplit(line, ",")[[1]][2]))
      if(!is.na(current_time)) {
        last_data_time$height <- current_time
        pending$height <- list(value = val, time = current_time)
      }
    } else if(grepl("^\\$01DST", line)) {
      val <- as.numeric(sub(",m.*", "", strsplit(line, ",")[[1]][4]))
      if(!is.na(current_time)) {
        last_data_time$net_spread <- current_time
        pending$net_spread <- list(value = val, time = current_time)
      }
    } else{
      next
    }

    # When epth and temperature are available, or at least one changes, record a row
    if(!is.na(current_time)) {
      values <- list(
        DATE_TIME = current_time,
        DEPTH = if(!is.null(pending$depth) &&
                   difftime(current_time, pending$depth$time, units = "secs") <= 10) pending$depth$value else NA,
        TEMPERATURE = if(!is.null(pending$temp) &&
                         difftime(current_time, pending$temp$time, units = "secs") <= 10) pending$temp$value else NA,
        NET_HEIGHT = if(!is.null(pending$height) &&
                        difftime(current_time, pending$height$time, units = "secs") <= 1) pending$height$value else NA,
        NET_SPREAD = if(!is.null(pending$net_spread) &&
                        difftime(current_time, pending$net_spread$time, units = "secs") <= 1) pending$net_spread$value else NA
      )
      matched_bt[[length(matched_bt) + 1]] <- values
    }
  }

  # Convert list to data.frame
  matched_bt <- do.call(rbind, lapply(matched_bt, as.data.frame))

  if(is.null(matched_bt)) {
    warning("convert_nmea_btd: No temperature, depth, spread, or height observations. No valid output.")
    return(NULL)
  }

  if(!("NET_HEIGHT" %in% names(matched_bt))) {
    matched_bt$NET_HEIGHT <- NA
  }

  if(!("NET_SPREAD" %in% names(matched_bt))) {
    matched_bt$NET_SPREAD <- NA
  }

  output_btd <- matched_bt[c("DATE_TIME", "DEPTH", "TEMPERATURE")]

  if(!is.null(output_btd)) {

    output_btd <- output_btd[!duplicated(output_btd$DATE_TIME), ]  # Remove duplicates

    output_btd <-
      output_btd[complete.cases(output_btd), ]

    output_btd <- output_btd[!(output_btd$TEMPERATURE == 0 & output_btd$DEPTH == 0), ]

    if(!is.na(min_depth) & !is.na(max_depth)) {
      output_btd <- output_btd[output_btd$DEPTH >= min_depth & output_btd$DEPTH <= max_depth, ]
    }

    if(!is.na(min_temperature) & !is.na(max_temperature)) {
      output_btd <- output_btd[output_btd$TEMPERATURE >= min_temperature & output_btd$TEMPERATURE <= max_temperature, ]
    }

    if(nrow(output_btd) < 3) {
      warning("convert_nmea_btd: No outputs created. Fewer than three valid temperature/depth observations.")
      return(NULL)
    }

    rownames(output_btd) <- NULL

    # Convert DATE_TIME to Alaska time and format for .BTD
    attr(output_btd$DATE_TIME, "tzone") <- "UTC"
    attr(output_btd$DATE_TIME, "tzone") <- "America/Anchorage"

    # Write .BTH file
    output_bth <-
      data.frame(
        VESSEL = VESSEL,
        CRUISE = CRUISE,
        HAUL = HAUL,
        MODEL_NUMBER = MODEL_NUMBER,
        VERSION_NUMBER = VERSION_NUMBER,
        SERIAL_NUMBER = SERIAL_NUMBER,
        HOST_TIME = format(max(output_btd$DATE_TIME, na.rm = TRUE), "%m/%d/%Y %H:%M:%S"),
        LOGGER_TIME = format(max(output_btd$DATE_TIME, na.rm = TRUE), "%m/%d/%Y %H:%M:%S"),
        LOGGING_START = format(min(output_btd$DATE_TIME, na.rm = TRUE), "%m/%d/%Y %H:%M:%S"),
        LOGGING_END = format(max(output_btd$DATE_TIME, na.rm = TRUE), "%m/%d/%Y %H:%M:%S"),
        SAMPLE_PERIOD = as.integer(median(diff(output_btd$DATE_TIME), na.rm = TRUE)),
        NUMBER_CHANNELS = 2,
        NUMBER_SAMPLES = nrow(output_btd),
        MODE = 2
      )

    output_bth[which(is.na(output_bth))] <- ""

    bth_path <- paste0(getwd(), "/HAUL", numbers0(x = HAUL, number_places = 4), ".BTH")

    utils::write.csv(
      x = output_bth,
      file = bth_path,
      quote = FALSE,
      row.names = FALSE
    )

    cat(paste0("convert_nmea_btd: .BTH file saved to ", bth_path, "\n"))


    if(interactive_editing) {

      par(mfrow = c(2,1))
      plot(output_btd$DATE_TIME, output_btd$TEMPERATURE, xlab = "Datetime", ylab = "TEMPERATURE")
      mtext("Raw data.")
      plot(output_btd$DATE_TIME, output_btd$DEPTH, xlab = "Datetime", ylab = "DEPTH")
      mtext("Raw data.")

      dummy <- readline("Plotting raw data. Set plot to actual size (RStudio: View > Actual Size) then press ENTER to begin manual point editing.")

      output_btd <- interactive_point_editing(x = output_btd, x_col = "DATE_TIME", y_col = "DEPTH", tol = 0.5)
      output_btd <- interactive_point_editing(x = output_btd, x_col = "DATE_TIME", y_col = "TEMPERATURE", tol = 0.5)

      par(mfrow = c(2,1))
      plot(output_btd$DATE_TIME, output_btd$TEMPERATURE, xlab = "Datetime", ylab = "TEMPERATURE")
      mtext("Cleaned data.")
      plot(output_btd$DATE_TIME, output_btd$DEPTH, xlab = "Datetime", ylab = "DEPTH")
      mtext("Cleaned data.")

    } else {
      par(mfrow = c(2,1))
      plot(output_btd$DATE_TIME, output_btd$TEMPERATURE, xlab = "Datetime", ylab = "TEMPERATURE")
      mtext("Delete temp outlier rows\nfrom .BTD in a text editor.")
      plot(output_btd$DATE_TIME, output_btd$DEPTH, xlab = "Datetime", ylab = "DEPTH")
      mtext("Delete depth outlier rows\nfrom .BTD in a text editor.")
    }



    # Write .BTD file
    output_btd$DATE_TIME <-
      format_date(
        format(output_btd$DATE_TIME, "%m/%d/%Y %H:%M:%S")
      )

    output_btd <-
      data.frame(
        VESSEL = VESSEL,
        CRUISE = CRUISE,
        HAUL = HAUL,
        SERIAL_NUMBER = SERIAL_NUMBER,
        DATE_TIME = output_btd$DATE_TIME,
        TEMPERATURE = format(output_btd$TEMPERATURE, nsmall = 3),
        DEPTH = format(output_btd$DEPTH, nsmall = 1)
      )

    output_btd[which(is.na(output_btd), arr.ind = TRUE)] <- ""

    btd_path <- paste0(getwd(), "/HAUL", numbers0(x = HAUL, number_places = 4), ".BTD")

    utils::write.csv(
      x = output_btd,
      file = btd_path,
      quote = FALSE,
      row.names = FALSE
    )

    cat(paste0("convert_nmea_btd: .BTD file saved to ", btd_path, "\n"))

  } else {
    output_btd <- NULL
    output_bth <- NULL
    warning("convert_nmea_btd: Fewer than three temperature/depth observations. No valid output.")
  }

  output_hs <- matched_bt[c("DATE_TIME", "NET_HEIGHT", "NET_SPREAD")]

  output_hs <- output_hs[!duplicated(output_hs$DATE_TIME), ]

  output_hs <- output_hs[!is.na(output_hs$NET_HEIGHT) | !is.na(output_hs$NET_SPREAD), ]

  if(nrow(output_hs) > 0) {
    output_hs <-
      data.frame(
        VESSEL = VESSEL,
        CRUISE = CRUISE,
        HAUL = HAUL,
        DATE_TIME = output_hs$DATE_TIME,
        NET_HEIGHT = output_hs$NET_HEIGHT,
        NET_SPREAD = output_hs$NET_SPREAD
      )

    output_hs[which(is.na(output_hs), arr.ind = TRUE)] <- ""

    hs_path <- paste0(getwd(), "/HAUL", numbers0(x = HAUL, number_places = 4), ".hs")

    utils::write.csv(
      x = output_hs,
      file = hs_path,
      quote = FALSE,
      row.names = FALSE
    )

    cat(paste0("convert_nmea_btd: Height-spread (.hs) file saved to ", hs_path, "\n"))

  } else {
    output_hs <- NULL
  }

  return(list(btd = output_btd, bth = output_bth, height_spread = output_hs))

}
