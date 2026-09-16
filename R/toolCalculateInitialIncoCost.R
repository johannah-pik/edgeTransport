#' Apply regional differences for inconvenience cost start values based on ICE cost differences
#'
#' @author Johanna Hoppe
#' @param combinedCost total cost of ownership
#' @param incoCostStartVal start values for inconvenience costs
#' @param annuity calculated annuity for different vehicle types
#' @param loadFactor load factor data
#' @param annualMileage annual mileage data
#' @param helpers list with helpers
#' @import data.table
#' @returns data.table including initial inconvenience costs from 1990-2020 for LDV 4W US$/(p|t)km

toolCalculateInitialIncoCost <- function(combinedCost, incoCostStartVal, annuity, loadFactor, annualMileage, helpers) {
  # bind variables locally to prevent NSE notes in R CMD CHECK
  period <- value <- variable <- region <- unit <- univocalName <- subsectorL3 <- . <- NULL
  ratio <- average <- technology <- incoCostType <- regionCode12 <- NULL
browser()
  incoCostStartVal <- copy(incoCostStartVal)
  incoCostStartVal <- melt(incoCostStartVal, id.vars = c("region", "vehicleType", "technology", "unit"),
                           variable.name = "period")
  incoCostStartVal[, value := as.double(value)]
  
  # get rid of levels for the years, as approx_dt cannot handle them
  incoCostStartValReg[, period := as.numeric(as.character(period))]
  incoCostStartValReg <- approx_dt(incoCostStartValReg, unique(helpers$dtTimeRes[period <= 2020]$period),
                                   "period", "value", idxcols = c("region", "univocalName",
                                                                  "technology", "unit"), extrapolate = TRUE)

  # map on decision tree for LDV 4 Wheelers
  incoCostStartValReg <- merge(helpers$decisionTree, incoCostStartValReg, by = c("region", "univocalName", "technology"),
                               all.y = TRUE, allow.cartesian = TRUE)
  setnames(incoCostStartValReg, "incoCostType", "variable")

  # convert to US$/pkm
  # Annualize and discount to convert to US$/veh yr
  annualizedincoCostStartVal <- merge(incoCostStartValReg, annuity, by = "univocalName", allow.cartesian = TRUE)
  annualizedincoCostStartVal[, value := value * annuity][, unit := gsub("veh", "veh yr", unit)][, annuity := NULL]

  loadFactor <- copy(loadFactor)
  loadFactor[, c("variable", "unit") := NULL]
  setnames(loadFactor, "value", "loadFactor")
  annualMileage <- copy(annualMileage)
  annualMileage[, c("variable", "unit") := NULL]
  setnames(annualMileage, "value", "annualMileage")

  annualizedincoCostStartVal <- merge(annualizedincoCostStartVal, loadFactor,
                                      c("region", "univocalName", "technology", "period"), all.x = TRUE)
  annualizedincoCostStartVal <- merge(annualizedincoCostStartVal, annualMileage,
                                      c("region", "univocalName", "technology", "period"), all.x = TRUE)
  annualizedincoCostStartVal[, value := value / (annualMileage * loadFactor)][, c("loadFactor", "annualMileage") := NULL]
  #unit US$/pkm for passenger and unit US$/tkm for freight
  annualizedincoCostStartVal[, unit := ifelse(univocalName %in% c(helpers$filterEntries$trn_pass, "International Aviation"),
                                              gsub("veh yr", "pkm", unit), gsub("veh yr", "tkm", unit))]

  if (anyNA(annualizedincoCostStartVal) == TRUE) {
    stop("Inconvenience cost start values contain NAs")
  }

  return(annualizedincoCostStartVal)
}

