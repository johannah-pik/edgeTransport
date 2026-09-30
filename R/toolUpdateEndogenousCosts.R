#' @title toolUpdateEndogenousCosts
#' @description Provides updates for endogenous cost components e.g. inconvenience costs for cars
#'
#' @param dataEndoCosts data.table containing all cost components for cars over the full range
#'                      of policy years in a yearly resolution. Exogenous CAPEX and OPEX are provided over the full range.
#'                      Endogenous cost components and FS3 shares are provided until 2020. Rest is filled with NA.
#' @param depreciationFactors data.table containing vehicle depreciation factor for each year of service Life
#' @param timeValue data.table containing mode specific time value costs based on speed and gdp
#' @param preferences preference factor trends
#' @param lambdas data.table containing exponents for discrete choice calculation
#' @param helpers list containing helpers like mappings, decisionTree etc.
#' @param vehiclesPerTech data.table containing total number of vehicles for all years and regions
#' @return list containing data.table with endogenous cost components over the full time span and additional data.tables
#'         for model behavior analysis
#' @author Johanna Hoppe
#' @import data.table
#' @export

toolUpdateEndogenousCosts <- function(dataEndoCosts,
                                      depreciationFactors,
                                      allEqYear,
                                      timeValue,
                                      preferences,
                                      lambdas,
                                      helpers,
                                      vehiclesPerTech = NULL) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  totVeh <- technology <- period <- region <- type <- endoCostRaw <- value <- indexUsagePeriod <- NULL
  depreciationFactor <- FS3share <- variable <- FS3shareUpdate <- unit <- NULL
  coefficient <- techFleetProxy <- proxyUpdate <- NULL
  # Coefficient driving the endogenous decline of the inconvenience cost with fleet adoption.
  # The calibrated inconvenience cost is (for now) not split into types (was: "Stations
  # availability"/"Model availability"/"Risk aversion", Greene 2001, Pettifor 2017); a single
  # coefficient is used for the one remaining ("Total") type as a starting point.
  incoCostCoeff <- -12
  # preventing dataEndoCosts to be updated outside of the function
  dataEndoCosts <- copy(dataEndoCosts)
  # the endogenous update starts after the last year covered by the calibrated inconvenience cost start values
  anchorYear <- max(dataEndoCosts[type == "Inconvenience costs" & !is.na(value)]$period)
  if (anchorYear < allEqYear) anchorYear <- allEqYear
  policyYears <- seq(anchorYear + 1, 2100, 1)

  if (is.null(vehiclesPerTech)) {
    dataEndoCosts[, totVeh := 1]
  } else {
    dataEndoCosts <- merge(dataEndoCosts, vehiclesPerTech,
                           by = c("region", "period", "sector", "subsectorL2", "subsectorL3", "technology"))
  }


  # single coefficient for the one inconvenience cost type; other rows (e.g. Monetary costs) get NA
  dataEndoCosts[, coefficient := ifelse(type == "Inconvenience costs", incoCostCoeff, NA_real_)]

  # NOTE: a policy mask phasing inconvenience costs in/out over policy years previously sat here.
  # Removed for now: it depended on undefined objects and could never have run. Re-introduce it
  # deliberately, with a clear design, when policy differentiation of inconvenience costs is
  # needed again; until then endoCostRaw is not policy-masked.

  dataEndoCosts[type == "Inconvenience costs", endoCostRaw := value]

  dataEndoCosts[, techFleetProxy := 0]

  # the techFleetProxy is also calculated for "historic years", where the shares are fixed
  yearsSpinup <- seq(2010, anchorYear, 1)

  for (t in c(yearsSpinup, policyYears)) {
    # calculate proxy for total vehicles of one technology in the fleet at the start of year t ------
    # weighted average of the market sales of the previous years multiplied with the total vehicle number
    # depreciating in time. It is stored in period t, so that it also works for vehicle types
    # that are not resolved annually (only the available previous periods contribute)
    vehDepreciation <- copy(depreciationFactors)
    vehDepreciation <- vehDepreciation[!indexUsagePeriod == 0]
    vehDepreciation[, period := t - indexUsagePeriod]
    dataEndoCosts <- merge(dataEndoCosts, vehDepreciation[, c("period", "univocalName", "depreciationFactor")],
                           by = c("period", "univocalName"), all.x = TRUE)
    proxy <- dataEndoCosts[!is.na(depreciationFactor),
                           .(proxyUpdate = sum(FS3share * totVeh * depreciationFactor) / sum(totVeh * depreciationFactor)),
                           by = c("region", "univocalName", "technology", "variable")]
    dataEndoCosts <- merge(dataEndoCosts, proxy, by = c("region", "univocalName", "technology", "variable"), all.x = TRUE)
    dataEndoCosts[period == t & !is.na(proxyUpdate), techFleetProxy := proxyUpdate]
    dataEndoCosts[, c("proxyUpdate", "depreciationFactor") := NULL]

    if (t %in% yearsSpinup) next

    dataEndoCosts[type == "Inconvenience costs" & !is.na(coefficient),
                  endoCostRaw := ifelse(period == t,
                                        value[period == anchorYear] * exp(coefficient[1] * techFleetProxy[period == t]),
                                        endoCostRaw),
                  by = c("region", "technology", "vehicleType", "univocalName", "variable")]

    # check whether the (single) inconvenience cost type was updated
    if (anyNA(dataEndoCosts[period == t & type == "Inconvenience costs"]$endoCostRaw)) {
      stop(paste0("Something went wrong with the calculation of the raw endogenous costs in toolUpdateEndogenousCosts() ", t))
    }
    dataEndoCosts[period == t & type == "Inconvenience costs", value := endoCostRaw]

    # calculate FS3 share --------------------------------------------------------------------
    # vehicle types that are not resolved annually have no entries in every year
    if (nrow(dataEndoCosts[period == t]) == 0) next
    FS3shares <- toolCalculateFS3share(dataEndoCosts, t, timeValue, preferences, lambdas, helpers)
    setnames(FS3shares, "FS3share", "FS3shareUpdate")
    dataEndoCosts <- merge(dataEndoCosts, FS3shares, by = intersect(names(dataEndoCosts), names(FS3shares)), all.x = TRUE)
    dataEndoCosts[period == t, FS3share := FS3shareUpdate][, c("FS3shareUpdate") := NULL]
  }
  dataEndoCosts[, endoCostRaw := NULL]

  # Monetary costs and time value costs were needed to calculate the technology fleet proxy
  # They are stored already and should not be stored again together with the inconvenience costs
  dataEndoCosts <- dataEndoCosts[!type == "Monetary costs"]
  dataEndoCosts[, c("FS3share", "type", "coefficient") := NULL]

  # For model behavior analysis all data is stored
  # variable is already "Inconvenience costs" (no split into subtypes anymore)
  updatedEndogenousCosts <- copy(dataEndoCosts)[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType",
                                                       "technology", "univocalName", "variable", "unit", "period", "value")]

  techFleetProxy <- copy(dataEndoCosts)
  techFleetProxy[, value := techFleetProxy]
  techFleetProxy[, variable := "Technology fleet proxy"][, unit := "-"]
  techFleetProxy <- unique(techFleetProxy[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType", "technology",
                                               "univocalName", "variable", "unit", "period", "value")])

  endogenousCosts <- list(updatedEndogenousCosts = updatedEndogenousCosts, techFleetProxy = techFleetProxy)

  outputYears <- c(1990, seq(2005, 2100, by = 1), 2110, 2130, 2150)
  endogenousCosts <- lapply(endogenousCosts, approx_dt, outputYears, "period", "value",
                       setdiff(names(updatedEndogenousCosts), c("period", "value")), extrapolate = TRUE)


  return(endogenousCosts)
}
