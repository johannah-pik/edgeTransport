#' @title toolUpdateEndogenousCosts
#' @description Provides updates for endogenous cost components e.g. inconvenience costs for cars
#'
#' @param dataEndoCosts data.table containing all cost components for cars over the full range
#'                      of policy years in a yearly resolution. Exogenous CAPEX and OPEX are provided over the full range.
#'                      Endogenous cost components and FS3 shares are provided until 2020. Rest is filled with NA.
#' @param depreciationFactors data.table containing vehicle depreciation factor for each year of service Life
#' @param allEqYear year from which scenario-specific differentiation begins
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
  totVeh <- technology <- startValue <- period <- startYear <- targetYear <- targetValue <- NULL
  FVvehvar <- regionCode12 <- region <- type <- endoCostRaw <- value <- indexUsagePeriod <- NULL
  depreciationFactor <- FS3share <- variable <- FS3shareUpdate <- unit <- lateStart <- startYearCat <- NULL
  coefficient <- incoCostType <- NULL
  # coefficients driving endogenous decline of inconvenience costs with fleet adoption
  # Greene 2001 (orig. -20 for Stations), Pettifor 2017 (Risk aversion)
  incoCostCoeffs <- data.table(
    incoCostType = c("Stations availability", "Model availability", "Risk aversion"),
    coefficient  = c(-5, -12, 3800))
  policyYears <- seq(2021, 2100, 1)
  # preventing dataEndoCosts to be updated outside of the function
  dataEndoCosts <- copy(dataEndoCosts)

  if (is.null(vehiclesPerTech)) {
    dataEndoCosts[, totVeh := 1]
  } else {
    dataEndoCosts <- merge(dataEndoCosts, vehiclesPerTech,
                           by = c("region", "period", "sector", "subsectorL2", "subsectorL3", "technology"))
  }


  # join per-type coefficients; rows without a matching incoCostType get coefficient = NA
  dataEndoCosts <- merge(dataEndoCosts, incoCostCoeffs,
                         by.x = "variable", by.y = "incoCostType", all.x = TRUE)

  ## Check if transportPol or SSPscen change is introduced with allEqYear
  # If both stay the same, set allEqYear out of bounds such that it does not affect the calculation here
  if (!"final" %in% scenParIncoCost$startYearCat){
    allEqYear <- 2200
  }


  policyMask <- copy(scenParIncoCost)
  # Expand regional and temporal resolution
  regions <- unique(dataEndoCosts$region)
  tempAndregions <- CJ(region = regions, period = policyYears)
  tempAndregions[, all := "All"]
  policyMask[, all := "All"]
  policyMaskO <- merge(policyMask[startYearCat == "origin"], tempAndregions[period <= allEqYear], by = "all", allow.cartesian = TRUE)[, all := NULL]
  policyMaskF <- merge(policyMask[startYearCat == "final"], tempAndregions[period > allEqYear], by = "all", allow.cartesian = TRUE)[, all := NULL]
  policyMask <- rbind(policyMaskO, policyMaskF)
  policyMask[, "startYearCat" := NULL]

  policyMask <- dcast(policyMask, region + period + FVvehvar + technology ~ param, value.var = "value")
  # At the start of the policy intervention, the inconvenience costs for ICEs are zero, as they are the predominant and well-established technology.
  policyMask[, lateStart := FALSE]
  policyMask[technology == "Liquids", startValue := ifelse(!is.na(startValue), startValue, 0)]
  policyMask[technology == "Liquids" & startValue >0 , lateStart := TRUE]
  policyMask[, `:=`(
    startYear = as.numeric(startYear),
    startValue = as.numeric(startValue),
    targetYear = as.numeric(targetYear),
    targetValue = as.numeric(targetValue)
  )]
  policyMask[, policyMask := linFunc(period, startYear, startValue, targetYear, targetValue, lateStart), by = c("region", "period", "technology")]
  policyMask <- policyMask[, c("region", "period", "FVvehvar", "technology", "policyMask")]
  policyMask <- rbind(policyMask, policyMaskPHEV)
  policyMask <- merge(policyMask, helpers$mitigationTechMap[, c("univocalName", "FVvehvar")], all.x = TRUE, allow.cartesian = TRUE)[, FVvehvar := NULL]


  # check whether policy mask is calculated correctly for respective technologys
  if (anyNA(policyMask)) {
    stop("Something went wrong with the calculation of the policyMask in toolUpdateEndogenousCosts() ")
  }
  dataEndoCosts <- merge(dataEndoCosts, policyMask, by = c("region", "period", "univocalName", "technology"), all.x = TRUE)

  dataEndoCosts[type == "Inconvenience costs", endoCostRaw := value]

  # initialize techFleetProxy so the ifelse statement below works
  dataEndoCosts[, techFleetProxy := 0]

  # calculate the techFleetProxy values also for "historic years", where the shares are fixed
  yearsSpinup <- seq(2010, policyYears[1] - 1, 1)


  for (t in yearsSpinup) {
    # calculate proxy for total vehicles of one technology in the fleet ----------------------------

    vehDepreciation <- copy(depreciationFactors)
    vehDepreciation <- vehDepreciation[!indexUsagePeriod == 0]
    vehDepreciation[, period := t - indexUsagePeriod]
    dataEndoCosts <- merge(dataEndoCosts, vehDepreciation[, c("period", "univocalName", "depreciationFactor")], by = c("period", "univocalName"), all.x = TRUE)
    # calculate weighted average of the market sales multiplied with total vehicle number depreciating in time
    # to get a proxy for total vehicles of one technology in the fleet

    dataEndoCosts[!is.na(depreciationFactor), techFleetProxy := ifelse(period == t-1,
                                                                       sum(FS3share * totVeh * depreciationFactor) / sum(totVeh * depreciationFactor),
                                                                       techFleetProxy),
                  by = c("region", "univocalName", "technology", "variable")]
    dataEndoCosts[, c("depreciationFactor") := NULL]
  }

  for (t in policyYears) {
    # calculate proxy for total vehicles of one technology in the fleet ----------------------------

    vehDepreciation <- copy(depreciationFactors)
    vehDepreciation <- vehDepreciation[!indexUsagePeriod == 0]
    vehDepreciation[, period := t - indexUsagePeriod]
    dataEndoCosts <- merge(dataEndoCosts, vehDepreciation[, c("period", "univocalName", "depreciationFactor")], by = c("period", "univocalName"), all.x = TRUE)
    # calculate weighted average of the market sales multiplied with total vehicle number depreciating in time
    # to get a proxy for total vehicles of one technology in the fleet
    dataEndoCosts[!is.na(depreciationFactor), techFleetProxy := ifelse(period == t-1,
                                                                       sum(FS3share * totVeh * depreciationFactor) / sum(totVeh * depreciationFactor),
                                                                       techFleetProxy),
                  by = c("region", "univocalName", "technology", "variable")]

    dataEndoCosts[type == "Inconvenience costs" & !is.na(coefficient),
                  endoCostRaw := ifelse(period == t,
                      pmax(value[period == 2020] - coefficient[1] * techFleetProxy[period == (t - 1)], 0),
                    endoCostRaw),
                  by = c("region", "technology", "vehicleType", "univocalName", "variable")]

    # update raw endogenous costs-------------------------------------------------------------------
    ## Stations availability featured by BEV, FCEV, Hybrid electric, Gases
    dataEndoCosts[variable == "Stations availability" & technology %in% c("Gases"), endoCostRaw := ifelse(period == t,
                                                                                                          pmax(value[period == 2020], value[period == 2020] * exp(techFleetProxy[period == (t - 3)] * bfuelav)),
                                                                                                          endoCostRaw), by = c("region", "technology", "vehicleType", "univocalName")]

    # check whether all inconvenience cost types were updated
    if (anyNA(dataEndoCosts[period == t & type == "Inconvenience costs"]$endoCostRaw)) {
      stop(paste0("Something went wrong with the calculation of the raw endogenous costs in toolUpdateEndogenousCosts() ", t))
    }

    # calculate FS3 share --------------------------------------------------------------------
    FS3shares <- toolCalculateFS3share(dataEndoCosts, t, timeValue, preferences, lambdas, helpers)
    setnames(FS3shares, "FS3share", "FS3shareUpdate")
    dataEndoCosts <- merge(dataEndoCosts, FS3shares, by = intersect(names(dataEndoCosts), names(FS3shares)), all.x = TRUE)
    dataEndoCosts[period == t, FS3share := FS3shareUpdate][, c("FS3shareUpdate", "depreciationFactor") := NULL]
  }

  # Monetary costs and time value costs were needed to calculate the technology fleet proxy
  # They are stored already and should not be stored again together with the inconvenience costs
  dataEndoCosts <- dataEndoCosts[!type == "Monetary costs"]
  dataEndoCosts[, c("FS3share", "type", "coefficient") := NULL]

  # For model behavior analysis all data is stored
  updatedEndogenousCosts <- copy(dataEndoCosts)[, variable := paste0("Inconvenience costs|", variable)]
  updatedEndogenousCosts <- updatedEndogenousCosts[, c("region", "sector", "subsectorL1", "subsectorL2", "subsectorL3", "vehicleType",
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
