#' Find historical catch data from previous years
#'
#' @param survey (character) A character string of the survey you are interested in reivewing. Options are those from public_data$survey, which are "AI", "GOA", "EBS", "NBS", "BSS".
#' @param species_codes (numeric) A species code number of a species or species you are specifically interested in reviewing data from. If NA/not entered, the function will return data for all species caught in the haul.
#' @param station (character) A character string of the current station name (as a grid cell; e.g., "264-85")
#' @param grid_buffer (numeric) GOA/AI only. The number of cells around the current station where you would like to see catches from. Typically, use grid_buffer = 3.
#' @param years (numeric) the years you want returned in the output. If years = NA, script will default to the last 10 years. If you would like to see all years, simply choose a large range that covers all years of the survey (e.g., 1970:2030)
#'
#' @export
#' @return a data.frame of past catches and hauls
#'
#' @examples
#' #' # EBS (or NBS) --------------------------------------------------------------
#'
#' ## for one year and only 1 station for all species --------------------------
#' get_catch_haul_history(
#'      survey = "EBS",
#'      years = 2021,
#'      station = "I-13")
#'
#' ## for default 10 years and only 1 station  for PCOD and walleye pollock ----
#' get_catch_haul_history(
#'      species_codes = c(21720, 21740), # pacific cod and walleye pollock
#'      survey = "EBS",
#'      station = "I-13")
#'
#' # AI (or GOA) ---------------------------------------------------------------
#'
#' ## for two specific years and nearby stations -------------------------------
#' get_catch_haul_history(
#'       survey = "AI",
#'       years = c(2016, 2018),
#'       station = "324-73",
#'       grid_buffer = 3)
#'
#' ## for default 10 years and nearby stations for all species (a typical use-case) ----
#' get_catch_haul_history(
#'      survey = "AI",
#'      years = NA, # default
#'      station = "324-73",
#'      grid_buffer = 3)
#'
#' ## for default 10 years and nearby stations for Bering Flounder (0 results returned!) ---
#' get_catch_haul_history(
#'      survey = "AI",
#'      species_codes = 10140, # Bering flounder which would be VERY unlikely to be found
#'      years = NA, # default
#'      station = "324-73",
#'      grid_buffer = 3)
get_catch_haul_history <- function(
    survey,
    species_codes = NA,
    years = NA,
    station,
    grid_buffer = NA) {

  utils::data("public_data", envir=environment())

  public_data0 <-
    GAPsurvey::public_data[GAPsurvey::public_data$srvy == survey,
                           c("year", "srvy", "haul", "stratum", "station",
                             "vessel_name", "vessel_id", "date_time", "latitude_dd_start", "longitude_dd_start",
                             "species_code", "common_name", "scientific_name", "taxon_confidence",
                             "cpue_kgkm2", "cpue_nokm2", "weight_kg", "count",
                             "bottom_temperature_c", "surface_temperature_c", "depth_m",
                             "distance_fished_km", "net_width_m", "net_height_m", "area_swept_km2", "duration_hr")]

  if (!is.na(years[1])) {
    public_data0 <- public_data0[public_data0$year %in% years,]
  } else { # default: show 10 years average
    public_data0 <- public_data0[public_data0$year %in% sort(unique(public_data0$year),
                                                             decreasing = TRUE)[1:10], ]
  }

  if (is.na(grid_buffer)) {
    public_data0 <- public_data0[public_data0$station == station,]
  }

  public_data1 <- public_data0 # so we can calculate the total_weight_kg

  if (!is.na(species_codes[1])) {
    public_data0 <- public_data0[public_data0$species_code %in% species_codes,]
  }

  # if (survey == "EBS" | survey == "NBS") {
  #
  #   lat <- mean(unique(public_data0$latitude_dd_start[public_data0$station == station]), na.rm = TRUE)
  #   lon <- mean(unique(public_data0$longitude_dd_start[public_data0$station == station]), na.rm = TRUE)
  #
  #   possible_stations <-
  #   unique(public_data0$station[
  #     (public_data0$latitude_dd_start >= lat-deg_range &
  #        public_data0$latitude_dd_start <= lat+deg_range) &
  #       (public_data0$longitude_dd_start >= lon-deg_range &
  #          public_data0$longitude_dd_start <= lon+deg_range)])
  #
  # }

  if (nrow(public_data0) == 0) {
    out <- "Your quiery returned 0 results."
  } else {

    if (survey == "AI" | survey == "GOA") {

      y <- as.numeric(strsplit(x = station, split = "-", fixed = TRUE)[[1]])

      if (grid_buffer != 3) {
        stop("the grid cell buffer is fixed at 3 for now.")
      }
      possible_stations <- expand.grid(
        data.frame(
          rbind(
            y + grid_buffer,
            y + grid_buffer - 1,
            y + grid_buffer - 2,
            y,
            y - grid_buffer,
            y - grid_buffer - 1,
            y - grid_buffer - 2
          )
        )
      )

      possible_stations$station <- paste(possible_stations$X1,
                                         possible_stations$X2,
                                         sep = "-")
      possible_stations <- possible_stations$station

      xx <- public_data0[public_data0$station %in% possible_stations,]
      public_data1 <- public_data1[public_data1$station %in% possible_stations,] # for calc total weight of haul

      catch <- stats::aggregate(xx[, c("count", "weight_kg", "cpue_kgkm2", "cpue_nokm2")],
                                by = list(
                                  haul = factor(xx$haul),
                                  year = factor(xx$year),
                                  scientific_name = factor(xx$scientific_name),
                                  common_name = factor(xx$common_name),
                                  station = factor(xx$station)),
                                sum)

      haul <- unique(xx[,c("year", "haul", "station", "stratum",
                           "vessel_name", "date_time", "latitude_dd_start", "longitude_dd_start",
                           "bottom_temperature_c", "surface_temperature_c", "depth_m",
                           "distance_fished_km", "net_width_m", "net_height_m", "area_swept_km2", "duration_hr")])

    } else if (survey == "EBS" | survey == "NBS") {
      catch <- public_data0[,c("year", "station", "scientific_name", "common_name",
                               "count", "weight_kg", "cpue_kgkm2", "cpue_nokm2")]
      haul <- unique(public_data0[,c("year", "haul", "station", "stratum",
                                     "vessel_name", "date_time", "latitude_dd_start", "longitude_dd_start",
                                     "bottom_temperature_c", "surface_temperature_c", "depth_m",
                                     "distance_fished_km", "net_width_m", "net_height_m",
                                     "area_swept_km2", "duration_hr")])
    }

    # add total weight to haul table
    haul <- base::merge(
      x = haul,
      y = stats::aggregate(public_data1[, c("weight_kg")],
                           by = list(
                             year = factor(public_data1$year),
                             station = factor(public_data1$station)),
                           sum, na.rm = TRUE),
      by = c("year", "station"))
    names(haul)[ncol(haul)] <- "total_weight_kg"
    haul[,ncol(haul)] <- round(x = haul[,ncol(haul)], digits = 2)


    catch$year <- as.numeric(as.character(catch$year))
    catch <- catch[order(-catch$year, -catch$weight_kg), ]
    rownames(catch)<-1:nrow(catch)
    catch$count[catch$count == 0] <- NA

    cc <- split(catch, catch$year)
    cc <- lapply(cc, function(df) { (df[order(-df$year, -df$weight_kg), names(catch) != c("year")]) })

    if (length(unique(catch$year))>1 | length(unique(catch$station))>1) {

      temp <- data.frame(table(catch[,c("scientific_name")]))
      if (sum(names(temp) == "Var1") == 1) {
        names(temp)[names(temp) == "Var1"] <- "scientific_name"
      }
      catch_means <- base::merge(
        x = stats::aggregate(catch[, c("count", "weight_kg", "cpue_kgkm2", "cpue_nokm2")],
                             by = list(
                               scientific_name = factor(catch$scientific_name),
                               common_name = factor(catch$common_name),
                               station = factor(catch$station)),
                             mean, na.rm = TRUE),
        y = temp,
        by = "scientific_name"#,
        # by.x = "scientific_name",
        # by.y = "Var1"
      )
      if (nrow(catch_means) == 0) {
        catch_means <- "There was no data available for these function parameters"
      } else {
        catch_means <- catch_means[order(-catch_means$cpue_kgkm2),]
        catch_means$count <- round(x = catch_means$count, digits = 1)
        catch_means$weight_kg <- round(x = catch_means$weight_kg, digits = 2)
        catch_means$cpue_kgkm2 <- round(x = catch_means$cpue_kgkm2, digits = 2)
        catch_means$cpue_nokm2 <- round(x = catch_means$cpue_nokm2, digits = 2)
        rownames(catch_means) <- 1:nrow(catch_means)

      }

    } else {
      catch_means <- "A summary of catch data would not be helpful with these function parameters"
    }

    catch$weight_kg <- round(x = catch$weight_kg, digits = 2)
    catch$cpue_kgkm2 <- round(x = catch$cpue_kgkm2, digits = 2)
    catch$cpue_nokm2 <- round(x = catch$cpue_nokm2, digits = 2)

    out <- list("catch" = cc,
                "catch_means" = catch_means,
                "haul" = haul)
  }

  return(out)
}


# Helper Functions ------------------------------------------------------------------------

#' Takes a string of words and combines them into a sentence that lists them.
#'
#' This function allows you to take a string of words and combine them into a sentence list. For example, 'apples', 'oranges', 'pears' would become 'apples, oranges, and pears'. This function uses oxford commas.
#' @param x Character strings you want in your string.
#' @param oxford T/F: would you like to use an oxford comma? Default = TRUE
#' @param sep string. default = "," but ";" might be what you need!
#' @keywords strings
#' @noRd
#' @examples text_list(c(1,2,"hello",4,"world",6))
text_list<-function(x, oxford = TRUE, sep = ",") {
  x<-x[which(x!="")]
  # x<-x[which(!is.null(x))]
  x<-x[which(!is.na(x))]
  # x<-x[order(x)]
  if (length(x)==2) {
    str1<-paste(x, collapse = " and ")
  } else if (length(x)>2) {
    str1<-paste(x[1:(length(x)-1)], collapse = paste0(sep, " "))
    str1<-paste0(str1,
                 ifelse(oxford == TRUE, sep, ""),
                 " and ", x[length(x)])
  } else {
    str1<-x
  }
  return(str1)
}

#' Make numbers the same length preceded by 0s
#'
#' @param x a single or vector of values that need to be converted from something like 1 to "001"
#' @param number_places default = NA. If equal to NA, the function will take use the longest length of a value provided in x (example 1). If equal to a number, it will make sure that every number is the same length of number_places (example 2) or larger (if a value of x has more places than number_places(example 3)).
#'
#' @noRd
#' @return A string of the values in x preceeded by "0"s
#'
#' @examples
#' # example 1
#' numbers0(x = c(1,11,111))
#' # example 2
#' numbers0(x = c(1,11,111), number_places = 4)
#' # example 3
#' numbers0(x = c(1,11,111), number_places = 2)
numbers0 <- function (x, number_places = NA) {
  x<-as.numeric(x)
  xx <- rep_len(x = NA, length.out = length(x))
  if (is.na(number_places)){
    number_places <- max(nchar(x))
  }
  for (i in 1:length(x)) {
    xx[i] <- paste0(ifelse(number_places<nchar(x[i]),
                           "",
                           paste(rep_len(x = 0,
                                         length.out = number_places-nchar(x[i])),
                                 collapse = "")), as.character(x[i]))
  }
  return(xx)
}


#' Make sure file path is complete
#'
#' Function adds '/' or '\\' to the end of directories and recognizes when there are file extensions at the end of strings.
#'
#' @param path A string with the complete path of the directory or file.
#'
#' @noRd
#' @return A fixed path string.
#'
#' @examples
#' fix_path("sdfg/sdfg/sdfg/dfg.dd")
#' fix_path("sdfg/sdfg/sdfg")
#' fix_path("sdfg/sdfg/sdfg/")
fix_path <- function(path) {
  path0 <- ifelse(
    # Does the string end with a back slash?
    substr(x = path,
           start = nchar(path),
           stop = nchar(path)) %in% c("/", "\\") |
      # or if there is a file extention?
      grepl(pattern = "\\.",
            x = substr(x = path,
                       start = nchar(path)-7,
                       stop = nchar(path))),
    path,
    paste0(path, "/") )

  return(path0)
}


#' Format date for BTD/BTH file
#'
#' @param x Date as a character vector or POSIXct object
#' @param ... Additional argument passed to format()
#' @noRd
#' @return A formatted date string

format_date_btd <- function(x, ...) {
  tmp <- format(x, ...)
  tmp <- sub("^[0]+", "", tmp)
  tmp <- sub('/0', "/", tmp)
  return(tmp)
}

# Data ------------------------------------------------------------------------------------

#' @title PolySpecies Data Set
#' @description polynumbers
#' @usage data(PolySpecies)
#' @author Jason Conner (jason.conner AT noaa.gov)
#' @format A data frame with 172 rows and 4 variables:
#' \describe{
#'   \item{\code{SPECIES_CODE}}{integer Species code}
#'   \item{\code{POLY_SPECIES_CODE}}{integer Poly species code}
#'   \item{\code{SPECIES_NAME}}{character Species scientific latin name}
#'   \item{\code{COMMON_NAME}}{character Species common names}
#'}
#' @details DETAILS
#' @keywords catch data
#' @examples
#' data(PolySpecies)
"PolySpecies"
