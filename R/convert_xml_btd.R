#' Convert Marport Trident Pro temperature/depth to BTD
#'
#' Convert data tags from Poseidon/SCS .xml files that have Marport Trident Pro NMEA tags (DPT, TMP) to BTD and BTH files.
#'
#' @param xml_path File path to a Poseidon/SCS .xml file.
#' @param interactive_editing Should the interactive point removal interface be used to manually clean temperature and depth data? If TRUE, must have graphic devices set to view plots in actual size (in R Studio: View > Actual Size or Ctrl+0)
#' @param min_depth Optional (default = -0.1). Minimum valid depth (m).
#' @param max_depth Optional (default = 1000). Maximum valid depth (m).
#' @param min_temperature Optional (default = -2). Maximum valid temperature (Celsius).
#' @param max_temperature Optional (default = 20). Maximum valid temperature (Celsius).
#' @param VESSEL Optional. Default = NA. The vessel number (e.g., 162 for AK Knight, 148 for Ocean Explorer). If NA or not called in the function, a prompt will appear asking for this data.
#' @param CRUISE Optional. Default = NA. The cruise number, which is usually the year + sequential two digit cruise (e.g., 202101). If NA or not called in the function, a prompt will appear asking for this data.
#' @param HAUL Optional. Default = NA. The haul number that you are trying to convert data for (e.g., 3). If NA or not called in the function, a prompt will appear asking for this data.
#' @param MODEL_NUMBER Optional. Default = "Marport Trident Pro". The model name/number of the temperature/depth sensor. This field may have restrictions on length.
#' @param VERSION_NUMBER Optional. Default = NA. The version number of the Marport sensor (e.g., 123 or 999, you can put in NA or a dummy number here instead of the actual version number without any negative repercussions).
#' @param SERIAL_NUMBER Optional. Default = NA. The serial number of the Marport sensor (e.g., 123 or 999, you can put in NA or a dummy number here instead of the actual serial number without any negative repercussions).
#' @param ... additional arguments
#' @export
#' @examples \dontrun{
#' # Run this to select Poseidon/SCS XML file (.xml)
#' convert_xml_btd()
#' }
#' @importFrom stats complete.cases
#' @import graphics
#' @author Sean Rohan <sean.rohan@@noaa.gov>


convert_xml_btd <-
  function(
    xml_path = NULL,
    interactive_editing = TRUE,
    min_depth = -0.1,
    max_depth = 800,
    min_temperature = -2,
    max_temperature = 20,
    VESSEL = NA,
    CRUISE = NA,
    HAUL = NA,
    MODEL_NUMBER = "Marport Trident Pro",
    VERSION_NUMBER = NA,
    SERIAL_NUMBER = NA, ...
  ) {

    # Select
    if(is.null(xml_path)) {
      message("convert_xml_btd: xml_path is NULL. Select an .xml file.")
      xml_path <-
        choose.files(
          default = "*.xml",
          caption = "Select SCS/Poseidon .xml file(s)",
          multi = TRUE,
          filters =
            matrix(
              c("XML SCS/Poseidon (.xml)", "*.xml"),
              ncol = 2)
        )

      stopifnot("convert_xml_btd: Must select a file." = length(xml_path) >= 1)
    }

    # Read files .xml files
    if(all(grepl(pattern = ".xml", x = xml_path))) {

      message("convert_xml_btd: Reading lines Poseidon/SCS .xml file(s).")

      xml_lines <-
        lapply(
          X = xml_path,
          FUN = function(x) {
            readLines(x)
          }
        )

      xml_lines <- unname(unlist(xml_lines))

    }

    # Add tests to check that NMEA strings include temperature and depth
    if(is.na(VESSEL)){ VESSEL <- readline("Type vessel code:  ") }
    if(is.na(CRUISE)){ CRUISE <- readline("Type cruise number:  ") }
    if(is.na(HAUL)){ HAUL <- readline("Type haul number:  ") }
    if(is.na(MODEL_NUMBER)){ MODEL_NUMBER <- readline("Type model number (optional):  ") }
    if(is.na(VERSION_NUMBER)){ VERSION_NUMBER <- readline("Type version number (optional):  ") }
    if(is.na(SERIAL_NUMBER)){ SERIAL_NUMBER <- readline("Type serial number of sensor (optional):  ") }

    # Function to parse time
    parse_time_xml <-
      function(dt_line) {
        dt_str <- gsub('.*timestamp="([^"]+)".*', "\\1", dt_line)
        dt_full <- as.POSIXct(dt_str, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
        dt_trunc <- trunc(dt_full, units = "secs")
      }

    # Function to set data type for each tag with the correct data
    set_data_type <- function(line) {
      mappings <- list(
        list(must_have = c("HR,", "DPT"), type = "DEPTH"),
        list(must_have = c("HR,", "TMP"), type = "TEMPERATURE")
      )

      for(map in mappings) {
        if (all(sapply(map$must_have, grepl, x = line))) {
          return(map$type)
        }
      }

      return(NA)
    }

    # Extract data from <DataItem> tags
    string_data <-
      lapply(
        X = xml_lines,
        FUN =
          function(line) {

            line_data <- NULL

            if(grepl("DataItem", line)) {
              line_data <-
                data.frame(
                  DATE_TIME = parse_time_xml(line),
                  type = set_data_type(line),
                  value = as.numeric(gsub(".*,|\\*.*", "", line)),
                  stringsAsFactors = FALSE
                )
            }

            return(line_data)

          }
      )

    # Change to wide format
    string_data_long <-
      do.call(what = rbind, args = string_data)

    string_data_long <- string_data_long[!is.na(string_data_long$type), ]

    output_btd <-
      stats::reshape(
        data = string_data_long,
        idvar = "DATE_TIME",
        timevar = "type",
        v.names = NULL,
        direction = "wide"
      )

    names(output_btd) <- gsub("value\\.", "", names(output_btd))

    rownames(output_btd) <- NULL

    output_btd <- output_btd[order(output_btd$DATE_TIME), ]

    # Convert UTC to Alaska time
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

    cat(paste0("convert_xml_btd: .BTH file saved to ", bth_path, "\n"))

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
      format_date_btd(
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

    output_btd[['DEPTH']][grepl(pattern = "NA", x = output_btd[['DEPTH']])] <- ""
    output_btd[['TEMPERATURE']][grepl(pattern = "NA", x = output_btd[['TEMPERATURE']])] <- ""

    btd_path <- paste0(getwd(), "/HAUL", numbers0(x = HAUL, number_places = 4), ".BTD")

    utils::write.csv(
      x = output_btd,
      file = btd_path,
      quote = FALSE,
      row.names = FALSE
    )

    cat(paste0("convert_xml_btd: .BTD file saved to ", btd_path, "\n"))

    return(output_btd)

  }
