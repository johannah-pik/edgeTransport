#' Read in all information from previous EDGE runs and create
#' the comparison.pdf
#'
#' @param listofruns folder hosting REMIND input files. If NULL, a list of magclass objects is returned (set this option in case of a REMIND preprocessing run)
#' @param hist path to a REMIND historical data file ("historical.mif")
#' @param fileName path to the PDF file to be produced
#'
#' @import mip
#' @import data.table
#' @import utils
#' @import rmndt
#' @importFrom luplot magpie2ggplot2
#' @importFrom ggplot2 facet_grid ggplot geom_col facet_wrap geom_point aes_ geom_ribbon guides guide_legend
#' @importFrom rmndt magpie2dt
#' @importFrom lusweave swopen swlatex swfigure swclose
#' @importFrom quitte as.quitte
#' @importFrom magclass read.report mbind getRegions new.magpie getYears add_dimension setNames getNames<- time_interpolate


lvl2_compareScen <- function(listofruns, hist, y_bar=c(2010,2030,2050,2100),
                             mainReg="EUR", fileName="CompareScenarios.pdf"){

  scenNames <- c()
  demand_km <- demand_ej <- vintcomp <- newcomp <- shares <-pref <- mj_km_data <- loadFactor <- annual_mileage <- annual_sale <- list()
  count_scen <- 2


  ## ---- Load historical data ----


  ## models for historical data
  histmap = list(
    "Population"="WDI",
    "GDP|PPP"="James_IMF",
    "FE"="IEA",
    "FE|Transport"="IEA",
    "FE|Buildings"="IEA",
    "FE|Industry"="IEA"
  )
  ## read historical data
  ## histData <- read.report(hist,as.list=FALSE)
  ## y_hist <- intersect(y_hist, getYears(histData, as.integer=TRUE))
  ## if(all(getRegions(data) %in% getRegions(histData))) {
  ##   histData = histData[getRegions(data),,]
  ##   if ( any(grepl("EDGE_SSP2",getNames(histData)))){
  ##     hist_edge = histData[,union(y_hist,y),]
  ##     histData = histData[,,"EDGE_SSP2", invert = T]
  ##   }
  ##   histData <- histData[,y_hist,]
  ## } else {
  ##   if(!is.null(reg)){
  ##     ## fill up historical data for additional regions with 0
  ##     dataReg    <- getRegions(data)[-which(getRegions(data) %in% getRegions(histData))]
  ##     dummy_hist <- new.magpie(dataReg,getYears(histData),getNames(histData),fill=NA)
  ##     histData       <- mbind(histData,dummy_hist)
  ##     histData = histData[getRegions(data),,]
  ##     if ( any(grepl("EDGE_SSP2",getNames(histData)))){
  ##       ##EDGE projections are stored in histData. Retrieve them
  ##       hist_edge = histData[,union(y_hist,y),]
  ##       histData = histData[,,"EDGE_SSP2", invert = T]
  ##     }
  ##     histData <- histData[,y_hist,]


  ##   } else {
  ##     stop("historical data do not contain the choosen region")
  ##   }
  ## }

  # copy of historic data replacing 0 with NA
  ## histData_NA <- histData
  ## histData_NA[histData_NA == 0] <- NA




  ## ---- Create mappings ----

 #Color mapping

cols <- c("NG" = "#d11141",
  "Liquids" = "#8c8c8c",
  "Hybrid Liquids" = "#ffc425",
  "Hybrid Electric" = "#f37735",
  "BEV" = "#00b159",
  "Electricity" = "#00b159",
  "Electric" = "#00b159",
  "FCEV" = "#00aedb",
  "char" = "#00aedb",
  "tot" = "#00b159",
  "av" = "#f37735",
  "range" = "#ffc425",
  "ref" = "#8c8c8c",
  "risk" = "#d11141",
  "Hydrogen" = "#00aedb",
  "Biodiesel" = "#66a182",
  "Synfuel" = "orchid",
  "Oil" = "#2e4057",
  "fuel price pkm" = "#edae49",
  "Operating costs registration and insurance" = "#8d96a3",
  "Operating costs maintenance" = "#00798c",
  "Capital cost" = "#d1495b",
  "International Aviation" = "#9acd32",
  "AVBUNK" = "#9acd32",
  "Domestic Aviation" = "#7cfc00",
  "DOMESAIR" = "#7cfc00",
  "Bus" = "#32cd32",
  "Passenger Rail" = "#2e8b57",
  "RAIL" = "#2e8b57",
  "Freight Rail" = "#ee4000",
  "Trucks" = "#ff6a6a",
  "ROAD" = "#ff6a6a",
  "ROAD LDV" = "#d11141",
  "ROAD HDV" = "#f37735",
  "International Shipping" = "#cd2626",
  "MARBUNK" = "#cd2626",
  "Domestic Shipping" = "#ff4040",
  "DOMESNAV" = "#ff4040",
  "Shipping" = "#ff4040",
  "Dom. Shipping" = "#ff4040",
  "no bunkers" = "#9acd32",
  "bunkers" = "#87cefa",
  "Truck" = "#ff7f50",
  "Trucks (<3.5t)" = "#ff7f50",
  "Trucks (3.5t-16)" = "#8b0000",
  "Trucks (>16)" = "#fa8072",
  "Motorbikes" = "#1874cd", #"dodgerblue3",
  "Small Cars" = "#87cefa",
  "Large Cars" = "#6495ed",
  "Van" = "#40e0d0",
  "LDV" = "#00bfff",
  "Non motorized" = "#da70d6",
  "Freight"="#ff0000",
  "Freight (Inland)" = "#cd5555",
  "Pass non LDV" = "#6b8e23",
  "Pass" = "#66cdaa",
  "Pass non LDV (Domestic)" = "#54ff9f",
  "refined liquids enduse" = "#8c8c8c")


  #Region Mapping
  RegionMappingH12 <- fread(system.file("extdata", "regionmappingH12.csv", package = "edgeTransport"))
  Regionmapping_21_EU11 <- fread(system.file("extdata", "regionmapping_21_EU11.csv", package = "edgeTransport"))
  setnames(Regionmapping_21_EU11,"RegionCode","region")
  Regionmapping_21_H12 <-copy(Regionmapping_21_EU11)
  Regionmapping_21_H12[, missingH12 := ifelse(missingH12 == "rest", region, missingH12)]
  Regionmapping_21_H12 <- Regionmapping_21_H12[,-c("X","CountryCode")]
  Regionmapping_21_H12 <- Regionmapping_21_H12[!duplicated(Regionmapping_21_H12)]
  Regionmapping_H12_world <- copy(Regionmapping_21_H12[,-c("region")])
  Regionmapping_H12_world[,world:="GLO"]
  Regionmapping_H12_world <- Regionmapping_H12_world[!duplicated(Regionmapping_H12_world)]

  GDP_country <- {
      x <- calcOutput("GDP", aggregate = F)
      getSets(x)[1] <- "ISO3"
      getSets(x)[2] <- "Year"
      x
  }
  GDP_country <- as.data.table(GDP_country)
  GDP_country[, year := as.numeric(gsub("y", "", Year))][, Year := NULL]
  GDP_country[, variable := paste0(sub("gdp_", "",variable))]
  setnames(GDP_country, c("ISO3","variable", "value"),c("CountryCode","scenario", "weight"))
  GDP_country <- aggregate_dt(GDP_country,Regionmapping_21_EU11[,-c("X","missingH12")],fewcol = "region", manycol = "CountryCode",datacols = "scenario",valuecol = "weight")

  #Maping for vehicle type aggregation
  Mapp_Aggr_vehtype = data.table(
    gran_vehtype = c("Compact Car","Large Car","Large Car and SUV","Light Truck and SUV", "Midsize Car","Mini Car","Subcompact Car","Van", "International Aviation_tmp_vehicletype",
                     "Domestic Ship_tmp_vehicletype","Freight Rail_tmp_vehicletype","Truck (0-3.5t)" ,"Truck (18t)","Truck (26t)","Truck (40t)","Truck (7.5t)","Domestic Aviation_tmp_vehicletype",
                     "HSR_tmp_vehicletype","Passenger Rail_tmp_vehicletype","Bus_tmp_vehicletype", "Moped","Motorcycle (50-250cc)","Motorcycle (>250cc)","International Ship_tmp_vehicletype", "Cycle_tmp_vehicletype","Walk_tmp_vehicletype") ,
    aggr_vehtype= c("Small Cars","Large Cars","Large Cars","Large Cars", "Large Cars","Small Cars","Small Cars", "Large Cars", "Aircraft international",
                    "Ships domestic","Freight Trains","Trucks" ,"Trucks","Trucks","Trucks","Trucks","Aircraft domestic",
                    "Passenger Trains","Passenger Trains","Busses", "Motorbikes","Motorbikes","Motorbikes","Ships international","Cycling","Walking") ,
    international=c("no bunkers","no bunkers","no bunkers","no bunkers","no bunkers","no bunkers","no bunkers","no bunkers","bunkers","no bunkers","no bunkers","no bunkers","no bunkers",
                    "no bunkers","no bunkers","no bunkers","no bunkers","no bunkers","no bunkers","no bunkers","no bunkers","no bunkers","no bunkers","bunkers","no bunkers","no bunkers")
  )

  #Mapping efficiencies for useful energy

  Mapp_UE = data.table(
    technology = c("FCEV","BEV","Electric","Liquids","Hydrogen"),
    UE_efficiency =c(0.5,0.8,0.8, 0.3, 0.35))


  ## ---- Load scenario data ----

  level2path <- function(folder, fname){
    path <- file.path(folder, "level_2", fname)
  }

   for (i in 1:length(listofruns)) {
    if(any(grepl(sub("_.*", "", listofruns[[i]]),scenNames))) {
      scenNames[i] <- paste0(sub("_.*", "", listofruns[[i]]),"_",count_scen)
      count_scen=count_scen+1
    }
    else {scenNames[i] <- sub("_.*", "", listofruns[[i]])}
    #add path to output folder if not provided

    ## load input data from EDGE runs for comparison
    demand_km[[i]] <- readRDS(level2path(listofruns[[i]],"demandF_plot_pkm.RDS")) ## detailed energy services demand, million km
    demand_km[[i]]$scenario=scenNames[i]
    demand_ej[[i]] <- readRDS(level2path(listofruns[[i]],"demandF_plot_EJ.RDS")) ## detailed final energy demand, EJ
    demand_ej[[i]]$scenario=scenNames[i]
    vintcomp[[i]] <- readRDS(level2path(listofruns[[i]], "vintcomp.RDS"))
    vintcomp[[i]]$scenario=scenNames[i]
    newcomp[[i]] <- readRDS(level2path(listofruns[[i]],"newcomp.RDS"))
    newcomp[[i]]$scenario=scenNames[i]
    shares[[i]] <- readRDS(level2path(listofruns[[i]],"shares.RDS"))
    shares[[i]]$scenario=scenNames[i]
    pref[[i]] <- readRDS(level2path(listofruns[[i]],"pref_output.RDS"))
    pref[[i]]$scenario=scenNames[i]
    mj_km_data[[i]] <- readRDS(level2path(listofruns[[i]],"mj_km_data.RDS"))
    mj_km_data[[i]]$scenario=scenNames[i]
    loadFactor[[i]] <- readRDS(level2path(listofruns[[i]],"loadFactor.RDS"))
    loadFactor[[i]]$scenario=scenNames[i]
    annual_mileage[[i]] <- readRDS(level2path(listofruns[[i]],"annual_mileage.RDS"))
    annual_mileage[[i]]$scenario=scenNames[i]
    annual_sale[[i]] <- readRDS(level2path(listofruns[[i]],"annual_sales.RDS"))
    annual_sale[[i]]$scenario=scenNames[i]
  }



  ## ---- Open output-pdf ----

  template <-  c("\\documentclass[a4paper,landscape,twocolumn]{article}",
                 "\\setlength{\\oddsidemargin}{-0.8in}",
                 "\\setlength{\\evensidemargin}{-0.5in}",
                 "\\setlength{\\topmargin}{-0.8in}",
                 "\\setlength{\\parindent}{0in}",
                 "\\setlength{\\headheight}{0in}",
                 "\\setlength{\\topskip}{0in}",
                 "\\setlength{\\headsep}{0in}",
                 "\\setlength{\\footskip}{0.2in}",
                 "\\setlength\\textheight{0.95\\paperheight}",
                 "\\setlength\\textwidth{0.95\\paperwidth}",
                 "\\setlength{\\parindent}{0in}",
                 "\\setcounter{tocdepth}{4}",
                 "\\setcounter{secnumdepth}{4}",
                 "\\usepackage{float}",
                 "\\usepackage[bookmarksopenlevel=section,colorlinks=true,linkbordercolor={0.9882353 0.8352941 0.7098039}]{hyperref}",
                 "\\hypersetup{bookmarks=true,pdfauthor={GES group, PIK}}",
                 "\\usepackage{graphicx}",
                 "\\catcode`_=12",
                 "\\usepackage{Sweave}",
                 "\\begin{document}",
                 "<<echo=false>>=",
                 "options(width=110)",
                 "@")

  sw <- swopen(fileName,template = template)
  swlatex(sw,"\\tableofcontents\\newpage")

  ## empty page
  swlatex(sw,"\\newpage")
  swlatex(sw,"\\thispagestyle{empty}")
  swlatex(sw,"\\mbox{}")
  swlatex(sw,"\\newpage")


  swlatex(sw,"\\section{Energy}")

  ## ---- FE energy by carrier ----
  swlatex(sw,"\\subsection{Final energy by carrier}")

  dem_ej <- do.call(rbind.data.frame, demand_ej)

  plot_dem_ej <- copy(dem_ej)

  #rename columns for mip
  setnames(plot_dem_ej,c("demand_EJ","year"),c("value","period"))
  plot_dem_ej <- plot_dem_ej[,c("value","period","region","scenario","sector","technology", "vehicle_type")]
  plot_dem_ej[,unit:= "EJ/yr"]
  #Aggregate regions
  plot_dem_ej <- aggregate_dt(plot_dem_ej,Regionmapping_21_H12,fewcol ="missingH12",yearcol = "period", manycol = "region" ,datacols = c("technology","scenario","sector","vehicle_type","unit"),valuecol = "value")
  plot_dem_ej_glo <- aggregate_dt(plot_dem_ej,Regionmapping_H12_world,fewcol ="world",yearcol = "period", manycol = "missingH12" ,datacols = c("technology","scenario","sector","vehicle_type","unit"),valuecol = "value")
  setnames(plot_dem_ej,"missingH12", "region")
  setnames(plot_dem_ej_glo, "world", "region")
  plot_dem_ej<- rbind(plot_dem_ej,plot_dem_ej_glo)
  plot_dem_ej <- plot_dem_ej[!duplicated(plot_dem_ej)]

  #Rename technologies by energy carrier
  plot_dem_ej[technology %in% c("FCEV","Hydrogen"),technology:="FE|Hydrogen"]
  plot_dem_ej[technology %in% c("BEV","Electric"),technology:="FE|Electricity"]
  plot_dem_ej[technology == "Liquids",technology:="FE|Liquids"]
  plot_dem_ej[technology == "NG",technology:="FE|Gases"]
  setnames(plot_dem_ej,"technology","FE_carrier")
  #Group vehicle types for plotting
  plot_dem_ej <- merge(plot_dem_ej,Mapp_Aggr_vehtype, by.x="vehicle_type" ,by.y="gran_vehtype")
  plot_dem_ej <- plot_dem_ej[,-c("vehicle_type")]
  setnames(plot_dem_ej,"aggr_vehtype","vehicle_type")

  #Filter for bunkers/no bunkers
  plot_dem_ej_wobunk <- plot_dem_ej[international=="no bunkers"]
  plot_dem_ej_bunk <- plot_dem_ej[international=="bunkers"]
  plot_dem_ej_wwobunk<- copy(plot_dem_ej)
  plot_dem_ej_wwobunk <- plot_dem_ej_wwobunk[, c("value","period","region","scenario","international","unit")]
  plot_dem_ej <- plot_dem_ej[,-c("international")]
  plot_dem_ej_wobunk <- plot_dem_ej_wobunk[,-c("international")]
  plot_dem_ej_bunk <- plot_dem_ej_bunk[,-c("international")]

  #Aggregate by technology for each sector and vehicle type
  plot_dem_ej[, value:=sum(value), by= c("period","region","scenario","sector","FE_carrier", "vehicle_type", "unit")]
  plot_dem_ej <- plot_dem_ej[!duplicated(plot_dem_ej)]
  plot_dem_ej_wobunk[, value:=sum(value), by= c("period","region","scenario","sector","FE_carrier", "vehicle_type", "unit")]
  plot_dem_ej_wobunk <- plot_dem_ej_wobunk[!duplicated(plot_dem_ej_wobunk)]
  plot_dem_ej_bunk[, value:=sum(value), by= c("period","region","scenario","sector","FE_carrier", "vehicle_type", "unit")]
  plot_dem_ej_bunk <- plot_dem_ej_bunk[!duplicated(plot_dem_ej_bunk)]
  plot_dem_ej_wwobunk[, value:=sum(value), by= c("period","region","scenario","international", "unit")]
  plot_dem_ej_wwobunk <- plot_dem_ej_wwobunk[!duplicated(plot_dem_ej_wwobunk)]

  ### ---- Total ----
  swlatex(sw,"\\subsubsection{Total}")
  #Aggregate sectors
  plot_dem_ej_Tot <- copy(plot_dem_ej)
  plot_dem_ej_Tot <- plot_dem_ej_Tot[,sector:=NULL][,vehicle_type:=NULL]
  plot_dem_ej_Tot <- plot_dem_ej_Tot[, value:=sum(value), by= c("period","region","scenario","FE_carrier")]
  plot_dem_ej_Tot <- plot_dem_ej_Tot[!duplicated(plot_dem_ej_Tot)]
  #Set technology as variable
  setnames(plot_dem_ej_Tot,"FE_carrier","variable")

  p <- mipArea(plot_dem_ej_Tot[region== mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4,width=7")

  p <- mipBarYearData(plot_dem_ej_Tot[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Tot[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_dem_ej_Tot[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ### ---- Passenger ----
  swlatex(sw,"\\subsubsection{Passenger}")
  #Choose and aggregate passenger sectors
  plot_dem_ej_Pass <- copy(plot_dem_ej)
  plot_dem_ej_Pass <- plot_dem_ej_Pass[sector %in% c("trn_pass","trn_aviation_intl")]
  plot_dem_ej_Pass_tot <- plot_dem_ej_Pass[,-c("sector","vehicle_type")]
  plot_dem_ej_Pass_tot <- plot_dem_ej_Pass_tot[, value:=sum(value), by= c("period","region","scenario","FE_carrier")]
  plot_dem_ej_Pass_tot <- plot_dem_ej_Pass_tot[!duplicated(plot_dem_ej_Pass_tot)]
  #Set FE_carrier as variable
  setnames(plot_dem_ej_Pass_tot,"FE_carrier","variable")

  p <- mipArea(plot_dem_ej_Pass_tot[region==mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_tot[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_tot[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_dem_ej_Pass_tot[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  #### ---- Passenger LDV ----
  swlatex(sw,"\\subsubsection{Passenger LDV}")

  #Choose only LDVs
  plot_dem_ej_Pass_LDV <- plot_dem_ej_Pass[vehicle_type %in% c("Small Cars","Large Cars")][,-c("sector","vehicle_type")]
  #Aggregate by FE_carrier
  plot_dem_ej_Pass_LDV <- plot_dem_ej_Pass_LDV[, value:=sum(value), by= c("period","region","scenario","FE_carrier")]
  plot_dem_ej_Pass_LDV <- plot_dem_ej_Pass_LDV[!duplicated(plot_dem_ej_Pass_LDV)]
  #Set FE_carrier as variable
  setnames(plot_dem_ej_Pass_LDV,"FE_carrier","variable")

  p <- mipArea(plot_dem_ej_Pass_LDV[region==mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_LDV[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_LDV[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_dem_ej_Pass_LDV[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  #### ---- Passenger non-LDV ----
  swlatex(sw,"\\subsubsection{Passenger non-LDV}")
  #Choose only LDVs
  plot_dem_ej_Pass_nonLDV <- plot_dem_ej_Pass[!vehicle_type %in% c("Small Cars","Large Cars")][,-c("sector","vehicle_type")]
  #Aggregate by FE_carrier
  plot_dem_ej_Pass_nonLDV <- plot_dem_ej_Pass_nonLDV[, value:=sum(value), by= c("period","region","scenario","FE_carrier")]
  plot_dem_ej_Pass_nonLDV <- plot_dem_ej_Pass_nonLDV[!duplicated(plot_dem_ej_Pass_nonLDV)]
  #Set FE_carrier as variable
  setnames(plot_dem_ej_Pass_nonLDV,"FE_carrier","variable")

  p <- mipArea(plot_dem_ej_Pass_nonLDV[region==mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_nonLDV[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_nonLDV[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_dem_ej_Pass_nonLDV[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ### ---- Freight ----
  swlatex(sw,"\\subsubsection{Freight}")

  #Choose and aggregate freight sectors
  plot_dem_ej_Frght <- copy(plot_dem_ej)
  plot_dem_ej_Frght <- plot_dem_ej_Frght[sector %in% c("trn_freight","trn_shipping_intl")]
  plot_dem_ej_Frght_tot <- plot_dem_ej_Frght[,-c("sector","vehicle_type")]
  plot_dem_ej_Frght_tot <- plot_dem_ej_Frght_tot[, value:=sum(value), by= c("period","region","scenario","FE_carrier")]
  plot_dem_ej_Frght_tot <- plot_dem_ej_Frght_tot[!duplicated(plot_dem_ej_Frght_tot)]
  #Set FE_carrier as variable
  setnames(plot_dem_ej_Frght_tot,"FE_carrier","variable")

  p <- mipArea(plot_dem_ej_Frght_tot[region==mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Frght_tot[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Frght_tot[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_dem_ej_Frght_tot[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  # ### ---- Bunkers ----
  swlatex(sw,"\\subsubsection{Bunkers}")

  #Aggregate sectors
  plot_dem_ej_bunk_Tot <- copy(plot_dem_ej_bunk)
  plot_dem_ej_bunk_Tot <- plot_dem_ej_bunk_Tot[,sector:=NULL][,vehicle_type:=NULL]
  plot_dem_ej_bunk_Tot <- plot_dem_ej_bunk_Tot[, value:=sum(value),by= c("period","region","scenario","FE_carrier")]
  plot_dem_ej_bunk_Tot <- plot_dem_ej_bunk_Tot[!duplicated(plot_dem_ej_bunk_Tot)]
  #Set FE_carrier as variable
  setnames(plot_dem_ej_bunk_Tot,"FE_carrier","variable")

  p <- mipArea(plot_dem_ej_bunk_Tot[region== mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4,width=7")

  p <- mipBarYearData(plot_dem_ej_bunk_Tot[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_bunk_Tot[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_dem_ej_bunk_Tot[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")


  # ### ---- No Bunkers ----
  swlatex(sw,"\\subsubsection{W/O Bunkers}")
  #
  # #Aggregate sectors
  plot_dem_ej_wobunk_Tot <- copy(plot_dem_ej_wobunk)
  plot_dem_ej_wobunk_Tot <- plot_dem_ej_wobunk_Tot[,sector:=NULL][,vehicle_type:=NULL]
  plot_dem_ej_wobunk_Tot <- plot_dem_ej_wobunk_Tot[, value:=sum(value),by= c("period","region","scenario","FE_carrier")]
  plot_dem_ej_wobunk_Tot <- plot_dem_ej_wobunk_Tot[!duplicated(plot_dem_ej_wobunk_Tot)]
  #Set FE_carrier as variable
  setnames(plot_dem_ej_wobunk_Tot,"FE_carrier","variable")

  p <- mipArea(plot_dem_ej_wobunk_Tot[region== mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4,width=7")

  p <- mipBarYearData(plot_dem_ej_wobunk_Tot[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_wobunk_Tot[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_dem_ej_wobunk_Tot[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")
  #

  ## ---- FE Transport modes----
  swlatex(sw,"\\subsection{Final energy by transport modes}")
  swlatex(sw,"\\subsubsection{Passenger without bunkers by transport modes}")


  plot_dem_ej_modes <- copy(plot_dem_ej)
  #Choose Passenger without bunkers
  plot_dem_ej_modes <- plot_dem_ej_modes[sector=="trn_pass"]
  #Aggregate by transport modes
  plot_dem_ej_modes <- plot_dem_ej_modes[,sector:=NULL][,FE_carrier:=NULL]
  plot_dem_ej_modes <- plot_dem_ej_modes[, value:=sum(value), by= c("period","region","scenario","vehicle_type")]
  plot_dem_ej_modes <- plot_dem_ej_modes[!duplicated(plot_dem_ej_modes)]
  #Set vehicle_types as variable
  setnames( plot_dem_ej_modes,"vehicle_type","variable")

  p <- mipArea(plot_dem_ej_modes[region==mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_modes[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_modes[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_dem_ej_modes[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  swlatex(sw,"\\subsubsection{Passenger bunkers by transport modes}")


  plot_dem_ej_modes <- copy(plot_dem_ej)
  #Choose Passenger bunkers
  plot_dem_ej_modes <- plot_dem_ej_modes[sector=="trn_aviation_intl"]
  #Aggregate by transport modes
  plot_dem_ej_modes <- plot_dem_ej_modes[,sector:=NULL][,FE_carrier:=NULL]
  plot_dem_ej_modes <- plot_dem_ej_modes[, value:=sum(value), by= c("period","region","scenario","vehicle_type")]
  plot_dem_ej_modes <- plot_dem_ej_modes[!duplicated(plot_dem_ej_modes)]
  #Set vehicle_types as variable
  setnames( plot_dem_ej_modes,"vehicle_type","variable")

  p <- mipArea(plot_dem_ej_modes[region==mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_modes[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_modes[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_dem_ej_modes[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  swlatex(sw,"\\subsubsection{Freight without bunkers by transport modes}")


  plot_dem_ej_modes <- copy(plot_dem_ej)
  #Choose Freight without bunkers
  plot_dem_ej_modes <- plot_dem_ej_modes[sector=="trn_freight"]
  #Aggregate by transport modes
  plot_dem_ej_modes <- plot_dem_ej_modes[,sector:=NULL][,FE_carrier:=NULL]
  plot_dem_ej_modes <- plot_dem_ej_modes[, value:=sum(value), by= c("period","region","scenario","vehicle_type")]
  plot_dem_ej_modes <- plot_dem_ej_modes[!duplicated(plot_dem_ej_modes)]
  #Set vehicle_types as variable
  setnames( plot_dem_ej_modes,"vehicle_type","variable")

  p <- mipArea(plot_dem_ej_modes[region==mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_modes[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_modes[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_dem_ej_modes[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  swlatex(sw,"\\subsubsection{Freight bunkers by transport modes}")

  plot_dem_ej_modes <- copy(plot_dem_ej)
  #Choose Freight bunkers
  plot_dem_ej_modes <- plot_dem_ej_modes[sector=="trn_shipping_intl"]
  #Aggregate by transport modes
  plot_dem_ej_modes <- plot_dem_ej_modes[,sector:=NULL][,FE_carrier:=NULL]
  plot_dem_ej_modes <- plot_dem_ej_modes[, value:=sum(value), by= c("period","region","scenario","vehicle_type")]
  plot_dem_ej_modes <- plot_dem_ej_modes[!duplicated(plot_dem_ej_modes)]
  #Set vehicle_types as variable
  setnames( plot_dem_ej_modes,"vehicle_type","variable")

  p <- mipArea(plot_dem_ej_modes[region==mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_modes[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_modes[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_dem_ej_modes[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- FE Transport modes bunkers vs no bunkers----
  swlatex(sw,"\\subsection{Final energy bunkers vs. no bunkers}")


  #Set bunkers vs no bunkers as variable
  setnames(plot_dem_ej_wwobunk,"international","variable")

  p <- mipArea(plot_dem_ej_wwobunk[region==mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_wwobunk[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_wwobunk[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_dem_ej_wwobunk[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- FE Line Transport ----

  swlatex(sw,"\\subsection{Final Energy line plots}")
  swlatex(sw,"\\subsubsection{Total}")

  #Aggregate
  plot_dem_ej_Tot_aggr <- copy(plot_dem_ej_Tot)
  plot_dem_ej_Tot_aggr <- plot_dem_ej_Tot_aggr[, value:=sum(value), by= c("period","region","scenario")][,variable:="FE|Transport (EJ/yr)"]
  plot_dem_ej_Tot_aggr <- plot_dem_ej_Tot_aggr[!duplicated(plot_dem_ej_Tot_aggr)]


  # p <- mipLineHistorical(plot_dem_ej_Tot_aggr[region==mainReg & variable=="FE|Transport (EJ/yr)"],x_hist=histData[mainReg,,"FE|Transport w/ Bunkers (EJ/yr)"],
  #                        ylab='FE|Transport [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  # swfigure(sw,print,p,sw_option="height=8,width=8")
  p <- mipLineHistorical(plot_dem_ej_Tot_aggr[region==mainReg],
                         ylab='FE|Transport [EJ/yr]',scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=8")
  # p <- mipLineHistorical(plot_dem_ej_Tot_aggr[region==!mainReg & variable=="FE|Transport (EJ/yr)"],x_hist=histData[,,"FE|Transport w/ Bunkers (EJ/yr)"][mainReg,,,invert=TRUE],
  #                        ylab='FE|Transport [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  p <- mipLineHistorical(plot_dem_ej_Tot_aggr[!region==mainReg],
                         ylab='FE|Transport [EJ/yr]',scales="free_y",facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")


  swlatex(sw,"\\subsubsection{W/O Bunkers}")

  #Aggregate
  plot_dem_ej_Tot_wobunk_aggr <- copy(plot_dem_ej_wobunk_Tot)
  plot_dem_ej_Tot_wobunk_aggr <- plot_dem_ej_Tot_wobunk_aggr[, value:=sum(value), by= c("period","region","scenario")][,variable:="FE|Transport (EJ/yr)"]
  plot_dem_ej_Tot_wobunk_aggr <- plot_dem_ej_Tot_wobunk_aggr[!duplicated(plot_dem_ej_Tot_wobunk_aggr)]

  # p <- mipLineHistorical(plot_dem_ej_Tot_wobunk_woBunk[region==mainReg],x_hist=histData[mainReg,,"Eurostat.FE|Transport (EJ/yr)"],
  #                        ylab='FE|Transport|w/o Bunkers [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  p <- mipLineHistorical(plot_dem_ej_Tot_wobunk_aggr[region==mainReg],
                         ylab='FE|Transport|w/o Bunkers [EJ/yr]',scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=8")
  # p <- mipLineHistorical(plot_dem_ej_Tot_aggr_woBunk[!region==mainReg],x_hist=histData[,,"Eurostat.FE|Transport (EJ/yr)"][mainReg,,,invert=TRUE],
  #                        ylab='FE|Transport|w/o Bunkers [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  p <- mipLineHistorical(plot_dem_ej_Tot_wobunk_aggr[!region==mainReg],
                         ylab='FE|Transport|w/o Bunkers [EJ/yr]',scales="free_y",facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")


  swlatex(sw,"\\subsubsection{Bunkers}")

  #Aggregate
  plot_dem_ej_bunk_Tot_aggr <- copy(plot_dem_ej_bunk_Tot)
  plot_dem_ej_bunk_Tot_aggr  <- plot_dem_ej_bunk_Tot_aggr [, value:=sum(value), by= c("period","region","scenario")][,variable:="FE|Transport (EJ/yr)"]
  plot_dem_ej_bunk_Tot_aggr  <- plot_dem_ej_bunk_Tot_aggr[!duplicated(plot_dem_ej_bunk_Tot_aggr)]

  # p <- mipLineHistorical(plot_dem_ej_Tot_aggr_woBunk[region==mainReg],x_hist=histData[mainReg,,"Eurostat.FE|Transport (EJ/yr)"],
  #                        ylab='FE|Transport|w/o Bunkers [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"))
  p <- mipLineHistorical(plot_dem_ej_bunk_Tot_aggr[region==mainReg],
                         ylab='FE|Transport|Bunkers [EJ/yr]',scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=8")
  # p <- mipLineHistorical(plot_dem_ej_Tot_aggr_woBunk[!region==mainReg],x_hist=histData[,,"Eurostat.FE|Transport (EJ/yr)"][mainReg,,,invert=TRUE],
  #                        ylab='FE|Transport|w/o Bunkers [EJ/yr]',scales="free_y",plot.priority=c("x_hist","x","x_proj"),facet.ncol=3)
  p <- mipLineHistorical(plot_dem_ej_bunk_Tot_aggr[!region==mainReg],
                         ylab='FE|Transport|Bunkers [EJ/yr]',scales="free_y",facet.ncol=3)
  swfigure(sw,print,p,sw_option="height=9,width=8")

  ## ---- UE energy by carrier ----
  swlatex(sw,"\\subsection{Useful energy by carrier}")


  plot_dem_ej_UE <- copy(dem_ej)

  #rename columns for mip
  setnames(plot_dem_ej_UE,c("demand_EJ","year"),c("value","period"))
  plot_dem_ej_UE <- plot_dem_ej_UE[,c("value","period","region","scenario","sector","technology", "vehicle_type")]
  plot_dem_ej_UE[,unit:= "EJ/yr"]
  #Aggregate regions
  plot_dem_ej_UE <- aggregate_dt(plot_dem_ej_UE,Regionmapping_21_H12,fewcol ="missingH12",yearcol = "period", manycol = "region" ,datacols = c("technology","scenario","sector","vehicle_type","unit"),valuecol = "value")
  plot_dem_ej_glo_UE <- aggregate_dt(plot_dem_ej_UE,Regionmapping_H12_world,fewcol ="world",yearcol = "period", manycol = "missingH12" ,datacols = c("technology","scenario","sector","vehicle_type","unit"),valuecol = "value")
  setnames(plot_dem_ej_UE,"missingH12", "region")
  setnames(plot_dem_ej_glo_UE, "world", "region")
  plot_dem_ej_UE <- rbind(plot_dem_ej_UE,plot_dem_ej_glo_UE)
  plot_dem_ej_UE <- plot_dem_ej_UE[!duplicated(plot_dem_ej_UE)]

  #Calculate UE
  plot_dem_ej_UE <- merge(plot_dem_ej_UE,Mapp_UE)
  plot_dem_ej_UE[,value:=value*UE_efficiency][,UE_efficiency:=NULL]

  #Rename and aggregate technologies by energy carrier
  plot_dem_ej_UE[technology %in% c("FCEV"),technology:="UE|Hydrogen"]
  plot_dem_ej_UE[technology %in% c("Hydrogen"),technology:="UE|Hydrogen"]
  plot_dem_ej_UE[technology %in% c("BEV","Electric"),technology:="UE|Electricity"]
  plot_dem_ej_UE[technology == "Liquids",technology:="UE|Liquids"]
  plot_dem_ej_UE[technology == "NG",technology:="UE|Gas"]
  #Group vehicle types for plotting
  plot_dem_ej_UE <- merge(plot_dem_ej_UE,Mapp_Aggr_vehtype, by.x="vehicle_type" ,by.y="gran_vehtype")
  plot_dem_ej_UE <- plot_dem_ej_UE[,-c("vehicle_type")]
  setnames(plot_dem_ej_UE,"aggr_vehtype","vehicle_type")

  #Filter for bunkers/no bunkers
  plot_dem_ej_wobunk_UE <- plot_dem_ej_UE[international=="no bunkers"]
  plot_dem_ej_bunk_UE <- plot_dem_ej_UE[international=="bunkers"]
  plot_dem_ej_wwobunk_UE<- copy(plot_dem_ej_UE)
  plot_dem_ej_wwobunk_UE <- plot_dem_ej_wwobunk_UE[, c("value","period","region","scenario","international","unit")]
  plot_dem_ej_UE <- plot_dem_ej_UE[,-c("international")]
  plot_dem_ej_wobunk_UE <- plot_dem_ej_wobunk_UE[,-c("international")]
  plot_dem_ej_bunk_UE <- plot_dem_ej_bunk_UE[,-c("international")]

  #Aggregate by technology for each sector and vehicle type
  plot_dem_ej_UE[, value:=sum(value), by= c("period","region","scenario","sector","technology", "vehicle_type", "unit")]
  plot_dem_ej_UE <- plot_dem_ej_UE[!duplicated(plot_dem_ej_UE)]
  plot_dem_ej_wobunk_UE[, value:=sum(value), by= c("period","region","scenario","sector","technology", "vehicle_type", "unit")]
  plot_dem_ej_wobunk_UE <- plot_dem_ej_wobunk_UE[!duplicated(plot_dem_ej_wobunk_UE)]
  plot_dem_ej_bunk_UE[, value:=sum(value), by= c("period","region","scenario","sector","technology", "vehicle_type", "unit")]
  plot_dem_ej_bunk_UE <- plot_dem_ej_bunk_UE[!duplicated(plot_dem_ej_bunk_UE)]
  plot_dem_ej_wwobunk_UE[, value:=sum(value), by= c("period","region","scenario","international", "unit")]
  plot_dem_ej_wwobunk_UE <- plot_dem_ej_wwobunk_UE[!duplicated(plot_dem_ej_wwobunk_UE)]


  ### ---- Total ----
  swlatex(sw,"\\subsubsection{Total}")
  #Aggregate sectors
  plot_dem_ej_Tot_UE <- copy(plot_dem_ej_UE)
  plot_dem_ej_Tot_UE <- plot_dem_ej_Tot_UE[,-c("sector","vehicle_type")]
  plot_dem_ej_Tot_UE <- plot_dem_ej_Tot_UE[, value:=sum(value), by= c("period","region","scenario","technology")]
  plot_dem_ej_Tot_UE <- plot_dem_ej_Tot_UE[!duplicated(plot_dem_ej_Tot_UE)]
  #Set technology as variable
  setnames(plot_dem_ej_Tot_UE,"technology","variable")

  p <- mipArea(plot_dem_ej_Tot_UE[region== mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4,width=7")

  p <- mipBarYearData(plot_dem_ej_Tot_UE[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Tot_UE[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_dem_ej_Tot_UE[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ### ---- Passenger ----
  swlatex(sw,"\\subsubsection{Passenger}")
  #Choose and aggregate passenger sectors
  plot_dem_ej_Pass_UE <- copy(plot_dem_ej_UE)
  plot_dem_ej_Pass_UE <- plot_dem_ej_Pass_UE[sector %in% c("trn_pass","trn_aviation_intl")]
  plot_dem_ej_Pass_tot_UE <- plot_dem_ej_Pass_UE[,-c("sector","vehicle_type")]
  plot_dem_ej_Pass_tot_UE <- plot_dem_ej_Pass_tot_UE[, value:=sum(value), by= c("period","region","scenario","technology")]
  plot_dem_ej_Pass_tot_UE <- plot_dem_ej_Pass_tot_UE[!duplicated(plot_dem_ej_Pass_tot_UE)]
  #Set technology as variable
  setnames(plot_dem_ej_Pass_tot_UE,"technology","variable")

  p <- mipArea(plot_dem_ej_Pass_tot_UE[region==mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_tot_UE[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_tot_UE[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_dem_ej_Pass_tot_UE[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  #### ---- Passenger LDV ----
  swlatex(sw,"\\subsubsection{Passenger LDV}")

  #Choose only LDVs
  plot_dem_ej_Pass_LDV_UE <- plot_dem_ej_Pass_UE[vehicle_type %in% c("Small Cars","Large Cars")][,-c("sector","vehicle_type")]
  #Aggregate by technology
  plot_dem_ej_Pass_LDV_UE <- plot_dem_ej_Pass_LDV_UE[, value:=sum(value), by= c("period","region","scenario","technology")]
  plot_dem_ej_Pass_LDV_UE <- plot_dem_ej_Pass_LDV_UE[!duplicated(plot_dem_ej_Pass_LDV_UE)]
  #Set technology as variable
  setnames(plot_dem_ej_Pass_LDV_UE,"technology","variable")

  p <- mipArea(plot_dem_ej_Pass_LDV_UE[region==mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_LDV_UE[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_LDV_UE[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_dem_ej_Pass_LDV_UE[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  #### ---- Passenger non-LDV ----
  swlatex(sw,"\\subsubsection{Passenger non-LDV}")
  #Choose only LDVs
  plot_dem_ej_Pass_nonLDV_UE <- plot_dem_ej_Pass_UE[!vehicle_type %in% c("Small Cars","Large Cars")][,-c("sector","vehicle_type")]
  #Aggregate by technology
  plot_dem_ej_Pass_nonLDV_UE <- plot_dem_ej_Pass_nonLDV_UE[, value:=sum(value), by= c("period","region","scenario","technology")]
  plot_dem_ej_Pass_nonLDV_UE <- plot_dem_ej_Pass_nonLDV_UE[!duplicated(plot_dem_ej_Pass_nonLDV_UE)]
  #Set technology as variable
  setnames(plot_dem_ej_Pass_nonLDV_UE,"technology","variable")

  p <- mipArea(plot_dem_ej_Pass_nonLDV_UE[region==mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_nonLDV_UE[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Pass_nonLDV_UE[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_dem_ej_Pass_nonLDV_UE[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ### ---- Freight ----
  swlatex(sw,"\\subsubsection{Freight}")

  #Choose and aggregate freight sectors
  plot_dem_ej_Frght_UE <- copy(plot_dem_ej_UE)
  plot_dem_ej_Frght_UE <- plot_dem_ej_Frght_UE[sector %in% c("trn_freight","trn_shipping_intl")]
  plot_dem_ej_Frght_tot_UE <- plot_dem_ej_Frght_UE[,-c("sector","vehicle_type")]
  plot_dem_ej_Frght_tot_UE <- plot_dem_ej_Frght_tot_UE[, value:=sum(value), by= c("period","region","scenario","technology")]
  plot_dem_ej_Frght_tot_UE <- plot_dem_ej_Frght_tot_UE[!duplicated(plot_dem_ej_Frght_tot_UE)]
  #Set technology as variable
  setnames(plot_dem_ej_Frght_tot_UE,"technology","variable")

  p <- mipArea(plot_dem_ej_Frght_tot_UE[region==mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Frght_tot_UE[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_dem_ej_Frght_tot_UE[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_dem_ej_Frght_tot_UE[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")


  #### ---- Trucks----

  swlatex(sw,"\\subsubsection{Freight Trucks}")
  #Choose only Trucks
  plot_dem_ej_Frght_Trucks_UE <- plot_dem_ej_Frght_UE[vehicle_type=="Trucks"]
  plot_dem_ej_Frght_Trucks_UE <- plot_dem_ej_Frght_Trucks_UE[,sector:=NULL][,vehicle_type:=NULL]
  plot_dem_ej_Frght_Trucks_UE <- plot_dem_ej_Frght_Trucks_UE[, value:=sum(value), by= c("period","region","scenario","technology")]
  plot_dem_ej_Frght_Trucks_UE <-  plot_dem_ej_Frght_Trucks_UE[!duplicated( plot_dem_ej_Frght_Trucks_UE)]
  #Set technology as variable
  setnames( plot_dem_ej_Frght_Trucks_UE,"technology","variable")

  p <- mipArea( plot_dem_ej_Frght_Trucks_UE[region==mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")

  p <- mipBarYearData( plot_dem_ej_Frght_Trucks_UE[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData( plot_dem_ej_Frght_Trucks_UE[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=8")

  swlatex(sw,"\\onecolumn")
  p <- mipArea( plot_dem_ej_Frght_Trucks_UE[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")


  # ### ---- Bunkers ----
  # swlatex(sw,"\\subsubsection{Bunkers}")
  #
  # #Aggregate sectors
  # plot_dem_ej_bunk_Tot_UE <- copy(plot_dem_ej_bunk_UE)
  # plot_dem_ej_bunk_Tot_UE <- plot_dem_ej_bunk_Tot_UE[,sector:=NULL][,vehicle_type:=NULL]
  # plot_dem_ej_bunk_Tot_UE <- plot_dem_ej_bunk_Tot_UE[, value:=sum(value), by= c("period","region","scenario","technology")]
  # plot_dem_ej_bunk_Tot_UE <- plot_dem_ej_bunk_Tot_UE[!duplicated(plot_dem_ej_bunk_Tot_UE)]
  # #Set technology as variable
  # setnames(plot_dem_ej_bunk_Tot_UE,"technology","variable")
  #
  # p <- mipArea(plot_dem_ej_bunk_Tot_UE[region == mainReg], scales="free_y")
  # p <- p + theme(legend.position="none")
  # swfigure(sw,print,p,sw_option="height=4,width=7")
  #
  # p <- mipBarYearData(plot_dem_ej_bunk_Tot_UE[region==mainReg & period %in% y_bar])
  # p <- p + theme(legend.position="none")
  # swfigure(sw,print,p,sw_option="height=4.5,width=7")
  #
  # p <- mipBarYearData(plot_dem_ej_bunk_Tot_UE[!region==mainReg & period %in% y_bar])
  # swfigure(sw,print,p,sw_option="height=9,width=16")
  #
  # swlatex(sw,"\\onecolumn")
  # p <- mipArea(plot_dem_ej_bunk_Tot_UE[!region==mainReg],scales="free_y")
  # swfigure(sw,print,p,sw_option="height=8,width=16")
  # swlatex(sw,"\\twocolumn")
  #
  # ### ---- No Bunkers ----
  # swlatex(sw,"\\subsubsection{W/O Bunkers}")
  #
  # #Aggregate sectors
  # plot_dem_ej_wobunk_Tot_UE <- copy(plot_dem_ej_wobunk_UE)
  # plot_dem_ej_wobunk_Tot_UE <- plot_dem_ej_wobunk_Tot_UE[,sector:=NULL][,vehicle_type:=NULL]
  # plot_dem_ej_wobunk_Tot_UE <- plot_dem_ej_wobunk_Tot_UE[, value:=sum(value), by= c("period","region","scenario","technology")]
  # plot_dem_ej_wobunk_Tot_UE <- plot_dem_ej_wobunk_Tot_UE[!duplicated(plot_dem_ej_wobunk_Tot_UE)]
  # #Set technology as variable
  # setnames(plot_dem_ej_wobunk_Tot_UE,"technology","variable")
  #
  # p <- mipArea(plot_dem_ej_wobunk_Tot_UE[region== mainReg], scales="free_y")
  # p <- p + theme(legend.position="none")
  # swfigure(sw,print,p,sw_option="height=4,width=7")
  #
  # p <- mipBarYearData(plot_dem_ej_wobunk_Tot_UE[region==mainReg & period %in% y_bar])
  # p <- p + theme(legend.position="none")
  # swfigure(sw,print,p,sw_option="height=4.5,width=7")
  #
  # p <- mipBarYearData(plot_dem_ej_wobunk_Tot_UE[!region==mainReg & period %in% y_bar])
  # swfigure(sw,print,p,sw_option="height=9,width=16")
  #
  # swlatex(sw,"\\onecolumn")
  # p <- mipArea(plot_dem_ej_wobunk_Tot_UE[!region==mainReg],scales="free_y")
  # swfigure(sw,print,p,sw_option="height=8,width=16")
  # swlatex(sw,"\\twocolumn")
  #
  ###Energy Services

  swlatex(sw,"\\section{Energy services}")

  energy_services <- do.call(rbind.data.frame, demand_km)
  setkey(energy_services,NULL)

  plot_Energy_services <- copy(energy_services)



  #rename columns for mip
  setnames(plot_Energy_services,c("demand_F","year"),c("value","period"))
  plot_Energy_services <- plot_Energy_services[,c("value","period","region","scenario","sector","technology", "vehicle_type")]
  plot_Energy_services[,unit:= "km/yr"]
  #Aggregate regions
  plot_Energy_services <- aggregate_dt(plot_Energy_services,Regionmapping_21_H12,fewcol ="missingH12",yearcol = "period", manycol = "region" , datacols = c("technology","scenario","sector","vehicle_type","unit"),valuecol = "value")
  plot_Energy_services_glo <- aggregate_dt(plot_Energy_services,Regionmapping_H12_world,fewcol ="world",yearcol = "period", manycol = "missingH12" ,datacols = c("technology","scenario","sector","vehicle_type","unit"),valuecol = "value")
  setnames(plot_Energy_services,"missingH12", "region")
  setnames(plot_Energy_services_glo, "world", "region")
  plot_Energy_services<- rbind(plot_Energy_services,plot_Energy_services_glo)
  plot_Energy_services <- plot_Energy_services[!duplicated(plot_Energy_services)]

  #Rename technologies by energy carrier
  plot_Energy_services[technology %in% c("FCEV","Hydrogen"),technology:="Hydrogen"]
  plot_Energy_services[technology %in% c("BEV","Electric"),technology:="Electricity"]
  plot_Energy_services[technology == "Liquids",technology:="Liquids"]
  plot_Energy_services[technology == "NG",technology:="Gases"]


  #Group vehicle types for plotting
  plot_Energy_services <- merge(plot_Energy_services,Mapp_Aggr_vehtype, by.x="vehicle_type" ,by.y="gran_vehtype")
  plot_Energy_services <- plot_Energy_services[,-c("vehicle_type")]
  setnames(plot_Energy_services,"aggr_vehtype","vehicle_type")

  #Filter for bunkers/no bunkers
  plot_Energy_services_Pass_wobunk <- plot_Energy_services[sector %in% c("trn_pass","trn_aviation_intl" ) & international == "no bunkers"][,international:=NULL][,technology:=NULL][,sector:=NULL]
  plot_Energy_services_Pass_bunk <- plot_Energy_services[sector %in% c("trn_pass","trn_aviation_intl" ) & international == "bunkers"][,international:=NULL][,technology:=NULL][,sector:=NULL]
  plot_Energy_services_Frght_wobunk <- plot_Energy_services[sector %in% c("trn_freight","trn_shipping_intl" ) & international == "no bunkers"][,international:=NULL][,technology:=NULL][,sector:=NULL]
  plot_Energy_services_Frght_bunk <- plot_Energy_services[sector %in% c("trn_freight","trn_shipping_intl" ) & international == "bunkers"][,international:=NULL][,technology:=NULL][,sector:=NULL]

  #Aggregate copy by vehicle type and technology for each sector
  plot_Energy_services[, value:=sum(value), by= c("period","region","scenario","sector", "vehicle_type", "unit","international","technology")]
  plot_Energy_services <- plot_Energy_services[!duplicated(plot_Energy_services)]
  #Aggregate by vehicle_type
  plot_Energy_services_Pass_wobunk[, value:=sum(value), by= c("period","region","scenario", "vehicle_type", "unit")]
  plot_Energy_services_Pass_wobunk  <- plot_Energy_services_Pass_wobunk [!duplicated(plot_Energy_services_Pass_wobunk )]
  plot_Energy_services_Pass_bunk[, value:=sum(value), by= c("period","region","scenario","vehicle_type", "unit")]
  plot_Energy_services_Pass_bunk  <- plot_Energy_services_Pass_bunk[!duplicated(plot_Energy_services_Pass_bunk )]
  plot_Energy_services_Frght_wobunk[, value:=sum(value), by= c("period","region","scenario","vehicle_type", "unit")]
  plot_Energy_services_Frght_wobunk  <- plot_Energy_services_Frght_wobunk [!duplicated(plot_Energy_services_Frght_wobunk )]
  plot_Energy_services_Frght_bunk[, value:=sum(value), by= c("period","region","scenario", "vehicle_type", "unit")]
  plot_Energy_services_Frght_bunk  <- plot_Energy_services_Frght_bunk[!duplicated(plot_Energy_services_Frght_bunk )]

  swlatex(sw,"\\subsection{Passenger without bunkers by vehicle type}")
  #Set vehicle type as variable
  setnames(plot_Energy_services_Pass_wobunk,"vehicle_type","variable")

  p <- mipArea(plot_Energy_services_Pass_wobunk[region== mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4,width=7")

  p <- mipBarYearData(plot_Energy_services_Pass_wobunk[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_Energy_services_Pass_wobunk[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_Energy_services_Pass_wobunk[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  swlatex(sw,"\\subsection{Passenger bunkers}")
  #Set vehicle type as variable
  setnames(plot_Energy_services_Pass_bunk,"vehicle_type","variable")

  p <- mipArea(plot_Energy_services_Pass_bunk[region== mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4,width=7")

  p <- mipBarYearData(plot_Energy_services_Pass_bunk[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_Energy_services_Pass_bunk[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_Energy_services_Pass_bunk[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  swlatex(sw,"\\subsection{Freight without bunkers by vehicle type}")
  #Set vehicle type as variable
  setnames(plot_Energy_services_Frght_wobunk,"vehicle_type","variable")

  p <- mipArea(plot_Energy_services_Frght_wobunk[region== mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4,width=7")

  p <- mipBarYearData(plot_Energy_services_Frght_wobunk[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_Energy_services_Frght_wobunk[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_Energy_services_Frght_wobunk[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  swlatex(sw,"\\subsection{Freight bunkers by vehicle type}")
  #Set technology as variable
  setnames(plot_Energy_services_Frght_bunk,"vehicle_type","variable")

  p <- mipArea(plot_Energy_services_Frght_bunk[region== mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4,width=7")

  p <- mipBarYearData(plot_Energy_services_Frght_bunk[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_Energy_services_Frght_bunk[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_Energy_services_Frght_bunk[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  swlatex(sw,"\\subsection{LDV's by technology}")

  plot_Energy_services_LDV <- plot_Energy_services[vehicle_type %in% c("Small Cars","Large Cars")]
  plot_Energy_services_LDV <- plot_Energy_services_LDV[,-c("international","vehicle_type","sector")]
  plot_Energy_services_LDV[,value:=sum(value), by=c("period","region","scenario","unit", "technology")]
  plot_Energy_services_LDV <- plot_Energy_services_LDV[!duplicated(plot_Energy_services_LDV)]
  setnames(plot_Energy_services_LDV,"technology","variable")

  p <- mipArea(plot_Energy_services_LDV[region== mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4,width=7")

  p <- mipBarYearData(plot_Energy_services_LDV[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_Energy_services_LDV[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_Energy_services_LDV[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  swlatex(sw,"\\subsection{Busses by technology}")

  plot_Energy_services_Bus <- plot_Energy_services[vehicle_type == "Busses"]
  plot_Energy_services_Bus <- plot_Energy_services_Bus[,-c("international","vehicle_type","sector")]
  setnames(plot_Energy_services_Bus,"technology","variable")
  plot_Energy_services_Bus <- plot_Energy_services_Bus[,value:=sum(value), by = c("period", "region", "scenario", "variable", "unit")]
  plot_Energy_services_Bus <- plot_Energy_services_Bus[!duplicated(plot_Energy_services_Bus)]

  p <- mipArea(plot_Energy_services_Bus[region== mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4,width=7")

  p <- mipBarYearData(plot_Energy_services_Bus[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_Energy_services_Bus[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_Energy_services_Bus[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  swlatex(sw,"\\subsection{Trucks by technology}")

  plot_Energy_services_Truck <- plot_Energy_services[vehicle_type == "Trucks"]
  plot_Energy_services_Truck <- plot_Energy_services_Truck[,-c("international","vehicle_type","sector")]
  setnames(plot_Energy_services_Truck,"technology","variable")
  plot_Energy_services_Truck <- plot_Energy_services_Truck[,value:=sum(value), by = c("period", "region", "scenario", "variable", "unit")]
  plot_Energy_services_Truck <- plot_Energy_services_Truck[!duplicated(plot_Energy_services_Truck)]

  p <- mipArea(plot_Energy_services_Truck[region== mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4,width=7")

  p <- mipBarYearData(plot_Energy_services_Truck[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")

  p <- mipBarYearData(plot_Energy_services_Truck[!region==mainReg & period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_Energy_services_Truck[!region==mainReg],scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")

  ## ---- Costs----
  FV_final_pref <- list()

  swlatex(sw,"\\section{Cost trends}")
  for (i in 1:length(pref)) {
    FV_final_pref[[i]] <- copy(pref[[i]]$FV_final_pref)
    FV_final_pref[[i]]$scenario <- copy(pref[[i]]$scenario)
  }

  FV_final_pref <- do.call(rbind.data.frame, FV_final_pref)
  setindex(FV_final_pref,NULL)
  #change variable names for mip
  FV_final_pref[logit_type=="pinco_tot", logit_type:="tot"]
  FV_final_pref[logit_type=="pchar", logit_type:="char"]
  FV_final_pref[logit_type=="pmod_av", logit_type:="av"]
  FV_final_pref[logit_type=="prange", logit_type:="range"]
  FV_final_pref[logit_type=="pref", logit_type:="ref"]
  FV_final_pref[logit_type=="prisk", logit_type:="risk"]
  FV_final_pref[,SSP:=sub("-.*","",scenario)]
  setnames(FV_final_pref, c("scenario","SSP"),c("techscen","scenario"))

  FV_final_pref <- aggregate_dt(FV_final_pref,
                       Regionmapping_21_H12,
                       manycol = "region",
                       fewcol = "missingH12",
                       datacols = c("vehicle_type", "technology", "sector", "subsector_L1", "subsector_L2", "subsector_L3", "logit_type","techscen","scenario"),
                       weights = GDP_country[year%in%unique(FV_final_pref$year)& scenario%in%unique(FV_final_pref$scenario)])
  FV_final_pref[,scenario:=NULL]
  setnames(FV_final_pref,c("techscen","missingH12"),c("scenario","region"))

  swlatex(sw,"\\subsection{Example large car and SUV}")

  swlatex(sw,"\\subsubsection{BEV}")
  FV_final_pref_LSUV <- FV_final_pref[vehicle_type == "Large Car and SUV" & technology == "BEV"]
  FV_final_pref_LSUV <- FV_final_pref_LSUV[,c("year","region","scenario","logit_type","value")][,model:= "EDGE-Transport"][,unit:="$/pkm"]
  FV_final_pref_LSUV <- FV_final_pref_LSUV[!duplicated(FV_final_pref_LSUV)]
  setnames(FV_final_pref_LSUV, c("year", "logit_type"),c("period", "variable"))
  colours <- cols[names(cols) %in% FV_final_pref_LSUV$variable]


  p <- mipBarYearData(FV_final_pref_LSUV[period %in% y_bar],colours)
  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\subsubsection{FCEV}")
  FV_final_pref_LSUV <- FV_final_pref[vehicle_type == "Large Car and SUV"& technology == "FCEV"]
  FV_final_pref_LSUV <- FV_final_pref_LSUV[,c("year","region","scenario","logit_type","value")][,model:= "EDGE-Transport"][,unit:="EJ/yr"]
  FV_final_pref_LSUV <- FV_final_pref_LSUV[!duplicated(FV_final_pref_LSUV)]
  setnames(FV_final_pref_LSUV, c("year", "logit_type"),c("period", "variable"))
  colours <- cols[names(cols) %in% FV_final_pref_LSUV$variable]

  p <- mipBarYearData(FV_final_pref_LSUV[period %in% y_bar],colours)
  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\subsubsection{Liquids}")
  FV_final_pref_LSUV <- FV_final_pref[vehicle_type == "Large Car and SUV"& technology == "Liquids"]
  FV_final_pref_LSUV <- FV_final_pref_LSUV[,c("year","region","scenario","logit_type","value")][,model:= "EDGE-Transport"][,unit:="EJ/yr"]
  FV_final_pref_LSUV <- FV_final_pref_LSUV[!duplicated(FV_final_pref_LSUV)]
  setnames(FV_final_pref_LSUV, c("year", "logit_type"),c("period", "variable"))
  #set colours
  colours <- cols[names(cols) %in% FV_final_pref_LSUV$variable]



  p <- mipBarYearData(FV_final_pref_LSUV[period %in% y_bar],colours)
  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\subsubsection{NG}")
  FV_final_pref_LSUV <- FV_final_pref[vehicle_type == "Large Car and SUV"& technology == "NG"]
  FV_final_pref_LSUV <- FV_final_pref_LSUV[,c("year","region","scenario","logit_type","value")][,model:= "EDGE-Transport"][,unit:="EJ/yr"]
  FV_final_pref_LSUV <- FV_final_pref_LSUV[!duplicated(FV_final_pref_LSUV)]
  setnames(FV_final_pref_LSUV, c("year", "logit_type"),c("period", "variable"))
  colours <- cols[names(cols) %in% FV_final_pref_LSUV$variable]

  p <- mipBarYearData(FV_final_pref_LSUV[period %in% y_bar],colours)
  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\subsection{Example compact car}")

  swlatex(sw,"\\subsubsection{BEV}")
  FV_final_pref_CCar <- FV_final_pref[vehicle_type == "Compact Car" & technology == "BEV"]
  FV_final_pref_CCar <- FV_final_pref_CCar[,c("year","region","scenario","logit_type","value")][,model:= "EDGE-Transport"][,unit:="EJ/yr"]
  FV_final_pref_CCar <- FV_final_pref_CCar[!duplicated(FV_final_pref_CCar)]
  setnames(FV_final_pref_CCar, c("year", "logit_type"),c("period", "variable"))
  colours <- cols[names(cols) %in% FV_final_pref_CCar$variable]

  p <- mipBarYearData(FV_final_pref_CCar[period %in% y_bar],colours)
  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\subsubsection{FCEV}")
  FV_final_pref_CCar <- FV_final_pref[vehicle_type == "Compact Car"& technology == "FCEV"]
  FV_final_pref_CCar <- FV_final_pref_CCar[,c("year","region","scenario","logit_type","value")][,model:= "EDGE-Transport"][,unit:="EJ/yr"]
  FV_final_pref_CCar <- FV_final_pref_CCar[!duplicated(FV_final_pref_CCar)]
  setnames(FV_final_pref_CCar, c("year", "logit_type"),c("period", "variable"))
  colours <- cols[names(cols) %in% FV_final_pref_CCar$variable]

  p <- mipBarYearData(FV_final_pref_CCar[period %in% y_bar],colours)
  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\subsubsection{Liquids}")
  FV_final_pref_CCar <- FV_final_pref[vehicle_type == "Compact Car"& technology == "Liquids"]
  FV_final_pref_CCar <- FV_final_pref_CCar[,c("year","region","scenario","logit_type","value")][,model:= "EDGE-Transport"][,unit:="EJ/yr"]
  FV_final_pref_CCar <- FV_final_pref_CCar[!duplicated(FV_final_pref_CCar)]
  setnames(FV_final_pref_CCar, c("year", "logit_type"),c("period", "variable"))
  colours <- cols[names(cols) %in% FV_final_pref_CCar$variable]

  p <- mipBarYearData(FV_final_pref_CCar[period %in% y_bar],colours)
  swfigure(sw,print,p,sw_option="height=9,width=16")

  swlatex(sw,"\\subsubsection{NG}")
  FV_final_pref_CCar <- FV_final_pref[vehicle_type == "Compact Car"& technology == "NG"]
  FV_final_pref_CCar <- FV_final_pref_CCar[,c("year","region","scenario","logit_type","value")][,model:= "EDGE-Transport"][,unit:="EJ/yr"]
  FV_final_pref_CCar <- FV_final_pref_CCar[!duplicated(FV_final_pref_CCar)]
  setnames(FV_final_pref_CCar, c("year", "logit_type"),c("period", "variable"))
  colours <- cols[names(cols) %in% FV_final_pref_CCar$variable]

  p <- mipBarYearData(FV_final_pref_CCar[period %in% y_bar],colours)
  swfigure(sw,print,p,sw_option="height=9,width=16")



  ## Close output-pdf
  swclose(sw)
}
