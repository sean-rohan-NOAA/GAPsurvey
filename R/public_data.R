#' @title Presence-only public data from FOSS
#' @description snapshot table for snapshot GAP_PRODUCTS.FOSS_CATCH
#' @usage data('public_data')
#' @author Emily Markowitz (Emily.Markowitz AT noaa.gov)
#' @format A data frame with 891144 observations on the following 37 variables.
#' \describe{
#'   \item{\code{date_time}}{Date and time. The date (MM/DD/YYYY) and time (HH:MM) of the haul. All dates and times are in Alaska time (AKDT) of Anchorage, AK, USA (UTC/GMT -8 hours).}
#'   \item{\code{depth_m}}{Depth (m). Bottom depth (meters).}
#'   \item{\code{distance_fished_km}}{Distance fished (km). Distance the net fished (kilometers).}
#'   \item{\code{duration_hr}}{Tow duration (decimal hr). This is the elapsed time between start and end of a haul (decimal hours).}
#'   \item{\code{haul}}{Haul number. This number uniquely identifies a sampling event (haul) within a cruise. It is a sequential number, in chronological order of occurrence.}
#'   \item{\code{hauljoin}}{Haul ID. This is a unique numeric identifier assigned to each (vessel, cruise, and haul) combination.}
#'   \item{\code{id_rank}}{Lowest taxonomic rank. Lowest taxonomic rank of a given species entry.}
#'   \item{\code{itis}}{Integrated taxonomic information system (ITIS) serial number. Species code as identified in the Integrated Taxonomic Information System (https://itis.gov/).}
#'   \item{\code{latitude_dd_end}}{End latitude (decimal degrees). Latitude (one hundred thousandth of a decimal degree) of the end of the haul.}
#'   \item{\code{latitude_dd_start}}{Start latitude (decimal degrees). Latitude (one hundred thousandth of a decimal degree) of the start of the haul.}
#'   \item{\code{longitude_dd_end}}{End longitude (decimal degrees). Longitude (one hundred thousandth of a decimal degree) of the end of the haul.}
#'   \item{\code{longitude_dd_start}}{Start longitude (decimal degrees). Longitude (one hundred thousandth of a decimal degree) of the start of the haul.}
#'   \item{\code{net_height_m}}{Net height (m). Measured or estimated distance (meters) between footrope and headrope of the trawl.}
#'   \item{\code{net_width_m}}{Net width (m). Measured or estimated distance (meters) between wingtips of the trawl.}
#'   \item{\code{performance}}{Haul performance code. This denotes what, if any, issues arose during the haul. For more information, review the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).}
#'   \item{\code{scientific_name}}{Taxon scientific name. The scientific name of the organism associated with the common_name and species_code columns. For a complete taxon list, review the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).}
#'   \item{\code{species_code}}{Taxon code. The species code of the organism associated with the common_name and scientific_name columns. For a complete species list, review the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).}
#'   \item{\code{srvy}}{Survey abbreviation. Abbreviated survey names. The column srvy is associated with the survey and survey_definition_id columns. Northern Bering Sea (NBS), Southeastern Bering Sea (EBS), Bering Sea Slope (BSS), Gulf of Alaska (GOA), Aleutian Islands (AI).}
#'   \item{\code{station}}{Station ID. Alpha-numeric designation for the station established in the design of a survey.}
#'   \item{\code{stratum}}{Stratum ID. RACE database statistical area for analyzing data. Strata were designed using bathymetry and other geographic and habitat-related elements. The strata are unique to each survey region. Stratum of value 0 indicates experimental tows.}
#'   \item{\code{surface_temperature_c}}{Surface temperature (degrees Celsius). Surface temperature (tenths of a degree Celsius); NA indicates removed or missing values.}
#'   \item{\code{survey}}{Survey name. Name and description of survey. The column survey is associated with the srvy and survey_definition_id columns.}
#'   \item{\code{survey_definition_id}}{Survey ID. The survey definition ID key code is an integer that uniquely identifies a survey region/survey design. The column survey_definition_id is associated with the srvy and survey columns. Full list of survey definition IDs are in RACE_DATA.SURVEY_DEFINITIONS and in the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).}
#'   \item{\code{taxon_confidence}}{Taxon confidence rating. Confidence in the ability of the survey team to correctly identify the taxon to the specified level, based solely on identification skill (e.g., not likelihood of a taxon being caught at that station on a location-by-location basis). Quality codes follow: **High**: High confidence and consistency. Taxonomy is stable and reliable at this level, and field identification characteristics are well known and reliable. **Moderate**: Moderate confidence. Taxonomy may be questionable at this level, or field identification characteristics may be variable and difficult to assess consistently. **Low**: Low confidence. Taxonomy is incompletely known, or reliable field identification characteristics are unknown. Documentation: [Species identification confidence in the eastern Bering Sea shelf survey (1982-2008)](http://apps-afsc.fisheries.noaa.gov/Publications/ProcRpt/PR2009-04.pdf), [Species identification confidence in the eastern Bering Sea slope survey (1976-2010)](http://apps-afsc.fisheries.noaa.gov/Publications/ProcRpt/PR2014-05.pdf), and [Species identification confidence in the Gulf of Alaska and Aleutian Islands surveys (1980-2011)](http://apps-afsc.fisheries.noaa.gov/Publications/ProcRpt/PR2014-01.pdf).}
#'   \item{\code{vessel_id}}{Vessel ID. ID number of the vessel used to collect data for that haul. The column vessel_id is associated with the vessel_name column. Note that it is possible for a vessel to have a new name but the same vessel id number. For a complete list of vessel ID key codes, review the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).}
#'   \item{\code{vessel_name}}{Vessel name. Name of the vessel used to collect data for that haul. The column vessel_name is associated with the vessel_id column. Note that it is possible for a vessel to have a new name but the same vessel id number. For a complete list of vessel ID key codes, review the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).}
#'   \item{\code{weight_kg}}{Sample or taxon weight (kg). Weight (thousandths of a kilogram) of individuals in a haul by taxon.}
#'   \item{\code{worms}}{World register of marine species (WoRMS) taxonomic serial number. Species code as identified in the World Register of Marine Species (WoRMS) (https://www.marinespecies.org/).}
#'   \item{\code{year}}{Survey year. Year the observation (survey) was collected.}
#'   \item{\code{area_swept_km2}}{Area swept (km). The area the net covered while the net was fishing (kilometers squared), defined as the distance fished times the net width.}
#'   \item{\code{bottom_temperature_c}}{Bottom temperature (degrees Celsius). Bottom temperature (tenths of a degree Celsius); NA indicates removed or missing values.}
#'   \item{\code{common_name}}{Taxon common name. The common name of the marine organism associated with the scientific_name and species_code columns. For a complete species list, review the [code books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).}
#'   \item{\code{count}}{Taxon count. Total whole number of individuals caught in haul or samples collected.}
#'   \item{\code{cpue_kgkm2}}{Weight CPUE (kg/km2). Catch weight (kilograms) per unit effort (area swept by the net, units square kilometers).}
#'   \item{\code{cpue_nokm2}}{Number CPUE (no/km2). Numerical catch per unit effort (area swept by the net, units square kilometers).}
#'   \item{\code{cruise}}{Cruise Name. This is a six-digit integer identifying the cruise number of the form: YYYY99 (where YYYY = year of the cruise; 99 = 2-digit number and is sequential; 01 denotes the first cruise that vessel made in this year, 02 is the second, etc.).}
#'   \item{\code{cruisejoin}}{Cruise ID. Unique integer ID assigned to each survey, vessel, and year combination.}#'   }
#' @source https://github.com/afsc-gap-products/gap_products and https://www.fisheries.noaa.gov/foss/f?p=215:28:14951401791129:::::
#' @keywords species code data
#' @examples
#' data(public_data)
#' @details The Resource Assessment and Conservation Engineering (RACE) Division Groundfish Assessment Program (GAP) of the Alaska Fisheries Science Center (AFSC) conducts fisheries-independent bottom trawl surveys to assess the populations of demersal fish and crab stocks of Alaska.

'public_data'
