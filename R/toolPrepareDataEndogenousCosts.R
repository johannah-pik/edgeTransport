#' Format all cost components
#'
#' @author Johanna Hoppe
#' @param inputData List containing inputData
#' @param lambdas exponents for discrete choice calculation
#' @param helpers List with helpers
#' @returns data.table including all cost components
#' @import data.table
#' @export

toolPrepareDataEndogenousCosts <- function(inputData, lambdas, helpers) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  univocalName <- type <- . <- value <- region <- period <- technology <- unit <- variable <- NULL

  # format input data
  # merge input data with decision tree
  # select only modes that feature inconvenience costs
  # bind with inconvenience cost start values
  inconvenienceCosts <- copy(inputData$initialIncoCosts)
  inconvenienceCosts <- inconvenienceCosts[, c("region", "period", "technology", "univocalName", "unit", "variable", "value")]
  inconvenienceCosts[, type := "Inconvenience costs"]
  incoCostModes <- unique(inconvenienceCosts$univocalName)

  monetaryCosts <- inputData$combinedCAPEXandOPEX[univocalName %in% incoCostModes]
  monetaryCosts[, type := "Monetary costs"]
  monetaryCosts <- monetaryCosts[, .(value = sum(value)), by = .(region, period, technology, univocalName, type, unit)]
  monetaryCosts[, variable := "CAPEX and OPEX"]

  # insert NAs for future years (in the temporal resolution of each vehicle type) to be filled in toolUpdateEndogenousCosts()
  futureTimeRes <- helpers$dtTimeRes[univocalName %in% incoCostModes & period > max(inconvenienceCosts$period)]
  futureIncoCosts <- merge(unique(inconvenienceCosts[, -c("period", "value")]), futureTimeRes,
                           by = "univocalName", allow.cartesian = TRUE)
  futureIncoCosts[, value := NA_real_]
  inconvenienceCosts <- rbind(inconvenienceCosts, futureIncoCosts, use.names = TRUE)
  combinedCosts <- rbind(monetaryCosts, inconvenienceCosts, use.names = TRUE)
  combinedCosts <- merge(combinedCosts, helpers$decisionTree, by = intersect(names(combinedCosts),
                                                                             names(helpers$decisionTree)), all.x = TRUE)

  # calculate FS3 share, hence the share of each technology in the overall car sales, in 2020
  # (needed to calculate the vehicle sales depreciating in time
  # to get a proxy for the fleet share in the iterative section)
  timesteps <- unique(combinedCosts$period)
  FS3share <- toolCalculateFS3share(combinedCosts, timesteps[timesteps <= 2025], inputData$timeValueCosts,
                                    inputData$scenSpecPrefTrends, lambdas, helpers)

  # merge back to combined costs
  combinedCosts <- merge(combinedCosts, FS3share,
                         by = intersect(names(combinedCosts), names(FS3share)),
                         all = TRUE)

  return(combinedCosts)
}
