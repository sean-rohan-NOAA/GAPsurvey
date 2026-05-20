#' Get sunrise and sunset times by day, latitude, and longitude
#'
#' @param chosen_date Date or charater. Formatted as "YYYY-MM-DD"
#' @param latitude Numeric. Fill in only if survey and station are not entered. latitude in either decimal degrees or a character latitude in degrees and decimal minutes
#' @param longitude Numeric. Fill in only if survey and station are not entered. Longitude in either decimal degrees or a character longitude in degrees and decimal minutes
#' @param survey Character. Fill in only if latitude and longitude are not entered. A character string of the survey you are interested in reivewing. Options are those from public_data$survey, which are "AI", "GOA", "EBS", "NBS", "BSS".
#' @param station Character. Fill in only if latitude and longitude are not entered. A character string of the current station name (as a grid cell; e.g., "264-85"). Stations defined in the station_coords dataset.
#' @param verbose Logical. Default = FALSE. If you would like a readout of what the file looks like in the console, set to TRUE.
#' @param timezone Character. Default = "US/Alaska." Other options include: "US/Aleutian"
#'
#' @return Time of sunrise and sunset in text. Also shows a pop-up with sunrise and sunset times.
#' @export
#'
#' @examples
#' # Find times based on lat/lon for today's date, where date is a date object
#' get_sunrise_sunset(chosen_date = Sys.Date(),
#'                    latitude = 63.3,
#'                    longitude = -170.5)
#'
#' # Find times based on lat/lon for today's date, where date is a character
#' # and lat/lon in degree decimal-minutes
#' get_sunrise_sunset(chosen_date = "2023-06-05",
#'                    latitude = "63 18.0",
#'                    longitude = "-170 30.0")
#'
#' # Find times based on a survey (AI) station's recorded lat/lon for today's date
#' get_sunrise_sunset(chosen_date = "2025-06-10",
#'                    survey = "AI",
#'                    station = "8-55")
#'
#' # Find times based on a survey (GOA) station's recorded lat/lon for today's date
#' get_sunrise_sunset(chosen_date = Sys.Date(),
#'                    survey = "GOA",
#'                    station = "264-18-511")
#'
#' # Find times based on a survey (EBS) station's recorded lat/lon for today's date
#' get_sunrise_sunset(chosen_date = "2025-08-04",
#'                    survey = "EBS",
#'                    station = "P-31")
#'
#' # Find times based on a survey (NBS) station's recorded lat/lon for today's date
#' get_sunrise_sunset(chosen_date = "2025-06-04",
#'                    survey = "NBS",
#'                    station = "ZZ-01")
get_sunrise_sunset <- function(
    chosen_date,
    latitude = NULL,
    longitude = NULL,
    survey = NULL,
    station = NULL,
    verbose = FALSE,
    timezone = "US/Alaska") {

  chosen_date <- as.POSIXct(x = as.character(chosen_date), tz = "UTC")

  if (timezone == "US/Alaska") {
    sel_tz <- -8
  } else if (timezone == "US/Aleutian") {
    sel_tz <- -9
  }

  chosen_date <- chosen_date + -1 * sel_tz * 3600 + 1

  # Are lat/long in degrees and decimal mins? If so, convert to decimal degrees.
  if (!is.null(latitude) | !is.null(longitude)) {
    message("Using latitude and longitude to calcualte sunrise and sunset. ")
    ddm <- is.character(latitude) | is.character(longitude)
    if (ddm) {
      if (!grepl(" ", x = latitude) | !grepl(" ", x = longitude)) {
        stop("You have chosen degrees and decimal minutes but have no space in the character string you entered. Please format your lat and/or long as D mm.m OR enter a numeric value for decimal degrees")
      }
      lat_deg <- as.numeric(gsub(" .*$", "", latitude))
      lat_min <- as.numeric(gsub("^\\S+\\s+", "", latitude)) / 60
      latitude <- lat_deg + lat_min

      lon_deg <- as.numeric(gsub(" .*$", "", longitude))
      lon_min <- as.numeric(gsub("^\\S+\\s+", "", longitude)) / 60
      longitude <- lon_deg + lon_min
    }
  }

  if (!is.null(survey) | !is.null(station)) {

    utils::data("station_coords", envir=environment())

    station_coords0 <-
      GAPsurvey::station_coords[GAPsurvey::station_coords$srvy == survey &
                                  GAPsurvey::station_coords$station == station,
                                c("srvy", "station",
                                  "latitude_dd", "longitude_dd")]
    if (nrow(station_coords0) == 0) {
      stop("This station does not exist in this survey. ")
    }

    latitude <- station_coords0$latitude[1]
    longitude <- station_coords0$longitude[1]
    message(paste0("Using survey station (",survey,
                   " ",station,
                   ") centroid location information (lat = ",
                   round(x = latitude, digits = 3),
                   ", lon = ",
                   round(x = longitude, digits = 3),
                   ") to calculate sunrise and sunset. "))

  }

  date_vec <- unlist(strsplit(as.character(chosen_date), split = ""))

  ac4r_output <- astrcalc4r(
    day = as.numeric(paste(date_vec[9:10], collapse = "")),
    month = as.numeric(paste(date_vec[6:7], collapse = "")),
    year = as.numeric(paste(date_vec[1:4], collapse = "")),
    hour = ifelse(length(date_vec) > 12,
                  as.numeric(paste(date_vec[12:13], collapse = "")),
                  12),
    timezone = 0, # UTC
    lat = latitude,
    lon = longitude,
    withinput = FALSE,
    seaorland = "maritime",
    acknowledgment = FALSE)

  sunrise <- format_date(x = ac4r_output$sunrise,
                         x_date = chosen_date,
                         tz = timezone,
                         hour_offset = sel_tz)

  sunset <- format_date(x = ac4r_output$sunset,
                        x_date = chosen_date,
                        tz = timezone,
                        hour_offset = sel_tz)

  message(
    "Sunrise is at ", format(sunrise, "%Y-%m-%d %H:%M:%S %Z"),
    "\nSunset is at ", format(sunset, "%Y-%m-%d %H:%M:%S %Z")
  )
}


#' Format output of astrocalc4r to a date/time object string
#'
#' Internal function used by get_sunrise_sunset()
#'
#' @param tz time zone as a character vector. See ?as.POSIXlt for details
#' @param hour_offset offset in hours relative to UTC (America/Anchorage = -8)
#' @param x_date date as a character, Date, or POSIXct object
#' @noRd

format_date <- function(x, x_date, hour_offset, tz) {

  x_date <- as.Date(x_date)

  if(x > 24) {
    x <- x - 24
  }

  if(x < 0) {
    x <- 24 + x
  }

  hour <- unlist(strsplit(as.character(floor(x)), split = ""))

  hour_vec <- c("0", "0")

  if(length(hour) == 2) {
    hour_vec <- hour
  } else {
    hour_vec[2] <- hour
  }

  min_vec <- c("0", "0")

  minutes <- unlist(strsplit(as.character(floor(x%%1*60)), split = ""))

  if(length(minutes) == 2) {
    min_vec <- minutes
  } else {
    min_vec[2] <- minutes
  }

  out <- paste0(paste(hour_vec, collapse = ""), ":", paste(min_vec, collapse = ""))

  out <- as.POSIXct(x = paste0(as.character(x_date), " ", out), tz = tz) + (3600 * hour_offset)

  return(out)

}



#' astrocalc4r: Solar zenith angles for biological research
#'
#' From Jacobsen et al. (2011). Documentation copied from Jacobsen et al. (2011). This function calculates the solar zenith, azimuth and declination angles, time at sunrise, local noon and sunset, day length, and PAR (photosynthetically available radiation, 400-700 nm) under clear skies and average atmospheric conditions (marine or continental) anywhere on the surface of the earth based on date, time, and location.
#'
#' @param day day of month in the local time zone (integers). Value is required. Multiple observations should be enclosed with the c() function.
#' @param month month of year in the local time zone (integers). Value is required. Multiple observations should be enclosed with the c() function.
#' @param year year in the local time zone (integers).Value is required. Multiple observations should be enclosed with the c() function.
#' @param hour local time for each observation (decimal hours, e.g. 11:30 PM is 23.5, real numbers). Value is required. Multiple observations should be enclosed with the c() function.
#' @param timezone local time zone in +/- hours relative to GMT to link local time and GMT. For example, the difference between Eastern Standard Time and GMT is -5 hours. Value is required. Multiple observations should be enclosed with the c() function. timezone should include any necessary adjustments for daylight savings time.
#' @param lat Latitude in decimal degrees (0o to 90 o in the northern hemisphere and -90 o to 0 o degrees in the southern hemisphere, real numbers). For example, 42o 30' N is 42.5 o and 42o 30' S is -42.5o. Value is required. Multiple observations should be enclosed with the c() function.
#' @param lon Longitude in decimal degrees (-0 o to 180 o in the western hemisphere and 0o to 180 o in the eastern hemisphere, real numbers). For example, 110o 15' W is -110.25 o and 110o 15' E is 110.25o. Value is required. Multiple observations should be enclosed with the c() function.
#' @param withinput logical:TRUE to return results in a dataframe with the input data; otherwise FALSE returns a dataframe with just results. Default is FALSE.
#' @param seaorland text: "maritime" for typical maritime conditions or "continental" for typical continental conditions. Users must select one option or the other based on proximity to the ocean or other factors.
#' @param acknowledgement logical: use TRUE to output acknowledgement. Default is FALSE.
#' @noRd
#' @details Astronomical definitions are based on definitions in Meeus (2009) and Seidelmann (2006). The solar zenith angle is measured between a line drawn "straight up" from the center of the earth through the observer and a line drawn from the observer to the center of the solar disk. The zenith angle reaches its lowest daily value at local noon when the sun is highest. It reaches its maximum value at night after the sun drops below the horizon. The zenith angle and all of the solar variables calculated by astrocalc4r depend on latitude, longitude, date and time of day. For example, solar zenith angles measured at the same time of day and two different locations would differ due to differences in location. Zenith angles at the same location and two different dates or times of day also differ. Local noon is the time of day when the sun reaches its maximum elevation and minimum solar zenith angle at the observers location. This angle occurs when the leading edge of the sun first appears above, or the trailing edge disappears below the horizon (0.83o accounts for the radius of the sun when seen from the earth and for refraction by the atmosphere). Day length is the time in hours between sunrise and sunset. Solar declination and azimuth angles describe the exact position of the sun in the sky relative to an observer based on an equatorial coordinate system (Meeus 2009). Solar declination is the angular displacement of the sun above the equatorial plane. The equation of time accounts for the relative position of the observer within the time zone and is provided because it is complicated to calculate. PAR isirradiance in lux (lx, approximately W m-2) at the surface of the earth under clear skies calculated based on the solar zenith angle and assumptions about marine or terrestrial atmospheric properties. astrocalc4r calculates PAR for wavelengths between 400-700 nm. Calculations for other wavelengths can be carried out by modifying the code to use parameters from Frouin et al. (1989). Following Frouin et al. (1989), PAR is assumed to be zero at solar zenith angles >= 90o although some sunlight may be visible in the sky when the solar zenith angle is < 108o. Angles in astrocalc4r output are in degrees although radians are used internally for calculations. Time data and results are in decimal hours (e.g. 11:30 pm = 23.5 h) local time but internal calculations are in Greenwich Mean Time (GMT). The user must specify the local time zone in terms of +/- hours relative to GMT to link local time and GMT. For example, the difference between Eastern Standard Time and GMT is -5 hours. The user must ensure that any adjustments for daylight savings time are included in the timezone value. For example, timezone=-6 for Eastern daylight time.
#' @return Time of solar noon, sunrise and sunset, angles of azimuth and zenith, eqtime, declination of sun, daylight length (hours) and PAR.
#' @author Larry Jacobson, Alan Seaver, and Jiashen Tang NOAA National Marine Fisheries Service Northeast Fisheries Science Center, 166 Water St., Woods Hole, MA 02543
#' @examples astrocalc4r(day=12,month=9,year=2000,hour=12,timezone=-5,lat=40.9,lon=-110)

astrcalc4r <- function (day, month, year, hour, timezone, lat, lon, withinput = FALSE,
                        seaorland = "maritime", acknowledgment = FALSE)
{
  if (acknowledgment) {
    cat("\n", "---------------------------------------------------------")
    cat("\n", "                AstroCalcPureR Version 2.3")
    cat("\n", "Documentation: Jacobson L, Seaver A, Tang J. 2011. AstroCalc4R:")
    cat("\n", "software to calculate solar zenith angle; time at sunrise,")
    cat("\n", "local noon and sunset; and photosynthetically available")
    cat("\n", "radiation based on date, time and location. US Dept Commer,")
    cat("\n", "Northeast Fish Sci Cent Ref Doc. 11-14; 10 p. Available from:")
    cat("\n", "National Marine Fisheries Service, 166 Water Street, ")
    cat("\n", "Woods Hole, MA 02543-1026, or online at")
    cat("\n", "http://nefsc.noaa.gov/publications/")
    cat("\n \n", "Available in fishmethods library.  Contact the fishmethods")
    cat("\n", "administrator or Larry Jacobson (NOAA, National Marine")
    cat("\n", "Fisheries Service-retired) at larryjacobson6@gmail.com")
    cat("\n", "for assitance.")
    cat("\n\n", "Useage:")
    cat("\n", "    AstroCalcPureR(day,month,year,hour,timezone,")
    cat("\n", "                   lat,lon,withinput=F,")
    cat("\n", "                   seaorland='maritime',")
    cat("\n", "                   acknowledgment=TRUE)")
    cat("\n\n", "HINT: set acknowledgment=FALSE to avoid this message")
    cat("\n", "---------------------------------------------------------",
        "\n")
  }
  options(digits = 9)
  deg2rad <- pi/180
  null.c <- function(x) return(sum(is.null(x)))
  if (sum(null.c(day), null.c(month), null.c(year), null.c(hour),
          null.c(timezone), null.c(lat), null.c(lon)) > 0)
    stop("\n Null or missing required data vector for day, month, year, timezone, lat or lon \n")
  if ((length(day) != length(month)) | (length(month) != length(year)) |
      (length(year) != length(hour)) | (length(hour) != length(timezone)) |
      (length(timezone) != length(lat)) | (length(lat) != length(lon)))
    stop("\n Input vectors are not the same length \n")
  times <- length(day)
  na.c <- function(x) return(sum(is.na(x)))
  if (sum(na.c(day), na.c(month), na.c(year), na.c(hour), na.c(timezone),
          na.c(lat), na.c(lon)) > 0)
    stop("\n NA values in input data \n")
  logic1 <- year < 0
  if (sum(logic1) > 0)
    stop(cat("\n Error in year at rows:", (1:times)[logic1],
             " \n\n"))
  is.leap <- function(x) return((((x%%4 == 0) & (x%%100 !=
                                                   0))) | (x%%400 == 0))
  date.list <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30,
                 31)
  logic1 <- abs(month - 6) > 6
  if (sum(logic1) > 0)
    stop(cat("\n Error in month at rows:", (1:times)[logic1],
             " \n\n"))
  logic1 <- day > (date.list[month] + is.leap(year) * (month ==
                                                         2))
  logic2 <- day <= 0
  if ((sum(logic1) > 0) | (sum(logic2) > 0))
    stop(cat("\n Incorrect month-day-year combination at rows: ",
             (1:times)[logic1 | logic2], " \n\n"))
  logic1 <- abs(hour - 12) > 12
  if (sum(logic1) > 0)
    stop(cat("\n Error in hour at rows:", (1:times)[logic1],
             " \n\n"))
  logic1 <- abs(timezone) > 12
  if (sum(logic1) > 0)
    stop(cat("\n Error in time zone at rows:", (1:times)[logic1],
             " \n\n"))
  logic1 <- abs(lon) > 180
  if (sum(logic1) > 0)
    stop(cat("\n Error in longitude at rows:", (1:times)[logic1],
             " \n\n"))
  logic1 <- abs(lat) > 90
  if (sum(logic1) > 0)
    stop(cat("\n Error in latitude at rows:", (1:times)[logic1],
             " \n\n"))
  logic1 <- sign(lon) == sign(timezone)
  logic2 <- timezone == 0
  logic3 <- !(logic1 | logic2)
  if (sum(logic3) != 0)
    stop(cat("\n \n Arguments longitude and timezone must have the same sign if input time is",
             "\n not UTC (timezone != 0).  In particular, if timezone !=0, both lon and timezone must",
             "\n be negative for locations in western hemisphere and positive for locations in the",
             "\n eastern hemisphere.  Check and fix input data if warranted. If data are correct",
             "\n then convert input time (argument hour) to UTC and use timezone=zero.",
             "\n This problem  occurs ", sum(logic3), " times at rows: ",
             (1:times)[logic3], "\n\n"))
  JulianDay <- function(xday, xmonth, xyear) {
    mm <- xmonth
    xmonth[mm <= 2] <- xmonth[mm <= 2] + 12
    xyear[mm <= 2] <- xyear[mm <= 2] - 1
    xa <- floor(xyear/100)
    xb <- 2 - xa + floor(xa/4)
    jd <- floor(365.25 * (xyear + 4716)) + floor(30.6001 *
                                                   (xmonth + 1)) + xday + xb - 1524.5
    return(jd)
  }
  daymonth <- function(mth, yr) {
    day[is.leap(yr)] <- c(31, 29, 31, 30, 31, 30, 31, 31,
                          30, 31, 30, 31)[mth[is.leap(yr)]]
    day[!is.leap(yr)] <- c(31, 28, 31, 30, 31, 30, 31, 31,
                           30, 31, 30, 31)[mth[!is.leap(yr)]]
    return(day)
  }
  parcalc <- function(zenith, setting = seaorland) {
    I0 <- 531.2
    V <- 23
    uv <- 1.4
    u0 <- 0.34
    r <- 0.05
    d <- 1
    if (!setting %in% c("maritime", "continental"))
      stop("setting value is neither 'maritime' nor 'continental'!")
    if (setting == "maritime") {
      a <- 0.068
      b <- 0.379
      a1 <- 0.117
      b1 <- 0.493
      av <- 0.002
      bv <- 0.87
      a0 <- 0.052
      b0 <- 0.99
    }
    else if (setting == "continental") {
      a <- 0.078
      b <- 0.882
      a1 <- 0.123
      b1 <- 0.594
      av <- 0.002
      bv <- 0.87
      a0 <- 0.052
      b0 <- 0.99
    }
    zrad <- zenith * deg2rad
    x1 <- uv/cos(zrad)
    xx <- exp(-av * x1^bv)
    x2 <- u0/cos(zrad)
    xxx <- exp(-a0 * x2^b0)
    xa <- a + b/V
    xb <- d - r * (a1 + b1/V)
    par <- I0 * cos(zrad) * exp(-xa/cos(zrad))/xb * xx *
      xxx
    par[zenith > 89.9999] <- 0
    return(par)
  }
  output <- as.data.frame(matrix(nrow = 0, ncol = 9))
  names(output) <- c("noon", "sunrise", "sunset", "azimuth",
                     "zenith", "eqtime", "declin", "daylight", "PAR")
  hourtemp <- hour - timezone
  hour <- ifelse(hourtemp > 24, hourtemp - 24, hourtemp)
  change_day <- !(hour == hourtemp)
  dm <- daymonth(month, year)
  daytemp <- day
  daytemp[change_day] <- ifelse((day[change_day] < dm[change_day]),
                                day[change_day] + 1, 1)
  change_month <- abs(day - daytemp) > 1
  monthtemp <- month
  monthtemp[change_month] <- ifelse(month[change_month] < 12,
                                    month[change_month] + 1, 1)
  change_year <- abs(month - monthtemp) > 1
  yeartemp <- year
  yeartemp[change_year] <- year[change_year] + 1
  xy <- yeartemp
  xm <- monthtemp
  xd <- daytemp + hourtemp/24
  jd <- JulianDay(xd, xm, xy) * 100/100
  jc <- (jd - 2451545)/36525
  xx <- 280.46646 + 36000.76983 * jc + 0.0003032 * jc^2
  gmls <- xx%%360
  xx <- 357.52911 + 35999.05029 * jc - 0.0001537 * jc^2
  gmas <- xx%%360
  eeo <- 0.016708634 - 4.2037e-05 * jc - 1.267e-07 * jc^2
  scx <- (1.914602 - 0.004817 * jc - 1.4e-05 * jc^2) * sin(gmas *
                                                             deg2rad) + (0.019993 - 0.000101 * jc) * sin(2 * gmas *
                                                                                                           deg2rad) + 0.000289 * sin(3 * gmas * deg2rad)
  Stl <- gmls + scx
  Sta <- gmas + scx
  srv <- 1.000001018 * (1 - eeo^2)/(1 + eeo * cos(Sta * deg2rad))
  omega <- 125.04 - 1934.136 * jc
  lambda <- Stl - 0.00569 - 0.00478 * sin(omega * deg2rad)
  epsilon <- (23 + 26/60 + 21.448/60^2) - (46.815/60^2) * jc -
    (0.00059/60^2) * jc^2 + (0.001813/60^2) * jc^3
  oblx <- 0.00256 * cos(omega * deg2rad)
  epsilon <- epsilon + oblx
  alpha <- atan2(cos(epsilon * deg2rad) * sin(lambda * deg2rad),
                 cos(lambda * deg2rad))/deg2rad
  declin <- asin(sin(epsilon * deg2rad) * sin(lambda * deg2rad))/deg2rad
  y <- tan(epsilon * deg2rad/2)^2
  eqtime <- (y * sin(2 * gmls * deg2rad) - 2 * eeo * sin(gmas *
                                                           deg2rad) + 4 * eeo * y * sin(gmas * deg2rad) * cos(2 *
                                                                                                                gmls * deg2rad) - y^2 * sin(4 * gmls * deg2rad)/2 - 5/4 *
               eeo^2 * sin(2 * gmas * deg2rad))/deg2rad * 4
  h0 <- -0.8333 * deg2rad
  phi <- lat * deg2rad
  hangle <- acos((sin(h0) - sin(declin * deg2rad) * sin(phi))/cos(declin *
                                                                    deg2rad)/cos(phi))/deg2rad
  noon <- (720 - 4 * lon + timezone * 60 - eqtime)/1440
  sunrise <- (noon * 1440 - hangle * 4)/1440 * 24
  sunset <- (noon * 1440 + hangle * 4)/1440 * 24
  noon <- noon * 24
  daylight <- hangle * 8
  tst <- (hourtemp * 60 + eqtime + 4 * lon)%%1440
  tsa <- ifelse(tst < 0, tst/4 + 180, tst/4 - 180)
  zenith <- 90 - asin(sin(lat * deg2rad) * sin(declin * deg2rad) +
                        cos(lat * deg2rad) * cos(declin * deg2rad) * cos(tsa *
                                                                           deg2rad))/deg2rad
  azimuth <- acos((sin(lat * deg2rad) * sin((90 - zenith) *
                                              deg2rad) - sin(declin * deg2rad))/cos(lat * deg2rad)/cos((90 -
                                                                                                          zenith) * deg2rad))/deg2rad + 180
  azimuth <- ifelse(tsa > 0, azimuth%%360, 360 - azimuth%%360)
  daylight <- daylight/60
  PAR <- parcalc(zenith)
  if (any(is.nan(sunrise))) {
    message(paste("Warning: Polar day/night (daylength 0 or 24 hrs) at record(s):",
                  (1:times)[is.nan(sunrise)], "\n Check input data (i.e. latitude)?"))
    daylight <- ifelse(PAR > 0, 24, 0)
  }
  output <- rbind(output, data.frame(noon = noon, sunrise = sunrise,
                                     sunset = sunset, azimuth = azimuth, zenith = zenith,
                                     eqtime = eqtime, declin = declin, daylight = daylight,
                                     PAR = PAR))
  if (withinput)
    return(cbind(data.frame(tzone = timezone, day = day,
                            month = month, year = year, hhour = hour, xlat = lat,
                            xlon = lon), output))
  else return(output)
}
