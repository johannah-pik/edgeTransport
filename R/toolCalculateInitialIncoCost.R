#' Prepare calibrated inconvenience cost start values
#'
#' @author Johanna Hoppe
#' @param incoCostStartVal calibrated inconvenience cost start values in US$2017/pkm or US$2017/tkm,
#'   already inverted from calibrated FV preferences (see ResembleScenarioParameters.RMD)
#' @param helpers list with helpers
#' @import data.table
#' @importFrom rmndt approx_dt
#' @returns data.table including initial inconvenience costs from 1990-2025 in US$2017/(p|t)km
#' @export

toolCalculateInitialIncoCost <- function(incoCostStartVal, helpers) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  period <- value <- variable <- region <- unit <- vehicleType <- technology <- NULL

  incoCostStartVal <- copy(incoCostStartVal)
  incoCostStartVal <- melt(incoCostStartVal, id.vars = c("region", "vehicleType", "technology", "unit"),
                           variable.name = "period")
  incoCostStartVal[, period := as.numeric(as.character(period))]
  incoCostStartVal[, value := as.double(value)]

  # interpolate onto the model time resolution, up to the point where scenario differentiation sets in
  incoCostStartVal <- approx_dt(incoCostStartVal, unique(helpers$dtTimeRes[period <= 2025]$period),
                                "period", "value", idxcols = c("region", "vehicleType", "technology", "unit"),
                                extrapolate = TRUE)

  # map onto the decision tree to add sector/subsector columns and univocalName
  # an inner merge naturally restricts the result to the modes covered by incoCostStartVal
  # (LDV 4W/2W, Bus, heavy trucks), no explicit mode filter is needed here
  initialIncoCost <- merge(helpers$decisionTree, incoCostStartVal,
                           by = c("region", "vehicleType", "technology"), allow.cartesian = TRUE)
  initialIncoCost[, variable := "Inconvenience costs"]
  # keep only the temporal resolution of each vehicle type (e.g. 2W are not resolved annually)
  initialIncoCost <- merge(initialIncoCost, helpers$dtTimeRes, by = c("univocalName", "period"))

  if (anyNA(initialIncoCost)) {
    stop("Inconvenience cost start values contain NAs after mapping onto the decision tree.
         Check that the calibrated inconvenience cost start values cover all region/vehicleType/
         technology combinations of the targeted subsectors in the decision tree.")
  }

  return(initialIncoCost)
}
