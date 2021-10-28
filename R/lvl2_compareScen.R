#' Read in all information from previous EDGE runs and create
#' the comparison.pdf
#' 
#'
#' @import mip  
#' @import data.table
#' @importFrom luplot magpie2ggplot2
#' @importFrom ggplot2 facet_grid ggplot geom_col facet_wrap geom_point aes_ geom_ribbon guides guide_legend
#' @importFrom rmndt magpie2dt
#' @importFrom lusweave swopen swlatex swfigure swclose
#' @importFrom quitte as.quitte
#' @importFrom magclass read.report mbind getRegions new.magpie getYears add_dimension setNames getNames<- time_interpolate



lvl2_compareScen <- function(output_folder, listofruns, fileName="CompareScenarios.pdf"){
  
  # if no path in "listofruns" starts with "output_folder/" insert it at the beginning
  # this is the case if listofruns was created in the lower case above !exists("outputdirs"), i.e. if this script was not called via Rscript output.R
  
  
  scenNames <- c()
  demand_km <- demand_ej <- vintcomp <- newcomp <- shares <-pref <- mj_km_data <- loadFactor <- annual_mileage <- annual_sale <- list()
  count_scen <- 2
  #Maping for vehicle type aggregation
  Mapp_Aggr_vehtype = data.table(
    gran_vehtype = c("Compact Car","Large Car","Large Car and SUV","Light Truck and SUV", "Midsize Car","Mini Car","Subcompact Car","Van", "International Aviation_tmp_vehicletype",
                     "Domestic Ship_tmp_vehicletype","Freight Rail_tmp_vehicletype","Truck (0-3.5t)" ,"Truck (18t)","Truck (26t)","Truck (40t)","Truck (7.5t)","Domestic Aviation_tmp_vehicletype",
                     "HSR_tmp_vehicletype","Passenger Rail_tmp_vehicletype","Bus_tmp_vehicletype", "Moped","Motorcycle (50-250cc)","Motorcycle (>250cc)","International Ship_tmp_vehicletype") ,
    aggr_vehtype= c("Small Cars","Large Cars","Large Cars","Trucks", "Large Cars","Small Cars","Small Cars", "Large Cars", "Aircraft international",
                    "Ships domestic","Freight Trains","Trucks" ,"Trucks","Trucks","Trucks","Trucks","Aircraft domestic",
                    "Passenger Trains","Passenger Trains","Busses", "Motorbikes","Motorbikes","Motorbikes","Ships international") 
  )  
  
  #Color code EDGE-T report
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
            "Non Bunkers" = "#9acd32",
            "Bunkers" = "#87cefa",
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
  
  
  
  
  for (i in 1:length(listofruns)) {
    if(any(grepl(sub("_.*", "", listofruns[[i]]),scenNames))) {
      scenNames[i] <- paste0(sub("_.*", "", listofruns[[i]]),"_",count_scen)
      count_scen=count_scen+1
    }
    else {scenNames[i] <- sub("_.*", "", listofruns[[i]])}
    #add path to output folder if not provided
    if(!any(grepl(output_folder,listofruns[[i]]))) {
      listofruns[[i]] <- paste0(output_folder,"/",listofruns[[i]],"/level_2/")
    }
    
    ## load input data from EDGE runs for comparison
    demand_km[[i]] <- readRDS(paste0(listofruns[[i]],"demandF_plot_pkm.RDS")) ## detailed energy services demand, million km
    demand_km[[i]]$scenario=scenNames[i]
    demand_ej[[i]] <- readRDS(paste0(listofruns[[i]],"demandF_plot_EJ.RDS")) ## detailed final energy demand, EJ
    demand_ej[[i]]$scenario=scenNames[i]
    vintcomp[[i]] <- readRDS(paste0(listofruns[[i]], "vintcomp.RDS"))
    vintcomp[[i]]$scenario=scenNames[i]
    newcomp[[i]] <- readRDS(paste0(listofruns[[i]],"newcomp.RDS"))
    newcomp[[i]]$scenario=scenNames[i]
    shares[[i]] <- readRDS(paste0(listofruns[[i]],"shares.RDS"))
    shares[[i]]$scenario=scenNames[i]
    pref[[i]] <- readRDS(paste0(listofruns[[i]],"pref_output.RDS"))
    pref[[i]]$scenario=scenNames[i]
    mj_km_data[[i]] <- readRDS(paste0(listofruns[[i]],"mj_km_data.RDS"))
    mj_km_data[[i]]$scenario=scenNames[i]
    loadFactor[[i]] <- readRDS(paste0(listofruns[[i]],"loadFactor.RDS"))
    loadFactor[[i]]$scenario=scenNames[i]
    annual_mileage[[i]] <- readRDS(paste0(listofruns[[i]],"annual_mileage.RDS"))
    annual_mileage[[i]]$scenario=scenNames[i]
    annual_sale[[i]] <- readRDS(paste0(listofruns[[i]],"annual_sales.RDS"))
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
  
  ## ---- ++++ S U M M A R Y ++++ ----
  
  swlatex(sw,"\\section{Energy}")
  
  
  
  ## ---- FE energy by carrier ----
  swlatex(sw,"\\subsection{FE by carrier}")
  #colours <- plotstyle("Hydrogen","Electricity","Liquids","Gases")
  dem_ej <- do.call(rbind.data.frame, demand_ej)
  #Rename technologies
  plot_dem_ej <- copy(dem_ej)
  plot_dem_ej[technology %in% c("FCEV","Hydrogen"),technology:="FE|Hydrogen"]
  plot_dem_ej[technology %in% c("BEV","Electric"),technology:="FE|Electricity"]
  plot_dem_ej[technology == "Liquids",technology:="FE|Liquids"]
  plot_dem_ej[technology == "NG",technology:="FE|Gases"]
  #Aggregate by technology for each sector
  plot_dem_ej[, demand_EJ:=sum(demand_EJ), by= c("year","region","scenario","sector","technology")]
  #Remove unneeded columns and add model and unit column for mip
  plot_dem_ej <- plot_dem_ej[, c("year","region","technology","demand_EJ","scenario","sector")][,model:= "EDGE-Transport"][,unit:="EJ/yr"]
  setnames(plot_dem_ej, c("year","demand_EJ","technology"),c("period","value","variable"))
  plot_dem_ej <- plot_dem_ej[!duplicated(plot_dem_ej)]
  
  swlatex(sw,"\\subsubsection{Total}")
  #Aggregate sectors
  plot_dem_ej_Tot <- copy(plot_dem_ej)
  plot_dem_ej_Tot <- plot_dem_ej_Tot[, value:=sum(value), by= c("period","region","scenario","variable")][,sector:=NULL]
  plot_dem_ej_Tot <- plot_dem_ej_Tot[!duplicated(plot_dem_ej_Tot)]
  plot_dem_ej_Tot <- as.quitte(plot_dem_ej_Tot)
  
  p <- mipArea(plot_dem_ej_Tot[region== mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4,width=7")
  
  p <- mipBarYearData(plot_dem_ej_Tot[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")
  
  p <- mipBarYearData(plot_dem_ej_Tot[period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=16")
  
  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_dem_ej_Tot,scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")
  swlatex(sw,"\\subsubsection{Passenger}")
  #Choose and aggregate passenger sectors
  plot_dem_ej_Pass <- copy(plot_dem_ej)
  plot_dem_ej_Pass <- plot_dem_ej_Pass[sector %in% c("trn_pass","trn_aviation_intl")]
  plot_dem_ej_Pass <- plot_dem_ej_Pass[, value:=sum(value), by= c("period","region","scenario","variable")][,sector:=NULL]
  
  plot_dem_ej_Pass <- plot_dem_ej_Pass[!duplicated(plot_dem_ej_Pass)]
  plot_dem_ej_Pass <- as.quitte(plot_dem_ej_Pass)
  
  p <- mipArea(plot_dem_ej_Pass[region==mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7")  
  
  p <- mipBarYearData(plot_dem_ej_Pass[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")
  
  p <- mipBarYearData(plot_dem_ej_Pass[period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_dem_ej_Pass,scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")
  
  swlatex(sw,"\\subsubsection{Freight}")
  #Choose and aggregate freight sectors
  plot_dem_ej_Frght <- copy(plot_dem_ej)
  plot_dem_ej_Frght <- plot_dem_ej_Frght[sector %in% c("trn_freight","trn_shipping_intl")]
  plot_dem_ej_Frght <- plot_dem_ej_Frght[, value:=sum(value), by= c("period","region","scenario","variable")][,sector:=NULL]
  plot_dem_ej_Frght <- plot_dem_ej_Frght[!duplicated( plot_dem_ej_Frght)]
  plot_dem_ej_Frght <- as.quitte( plot_dem_ej_Frght)
  
  p <- mipArea(plot_dem_ej_Frght[region==mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7") 
  
  p <- mipBarYearData(plot_dem_ej_Frght[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")
  
  p <- mipBarYearData(plot_dem_ej_Frght[period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_dem_ej_Frght,scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")
  
  swlatex(sw,"\\subsection{FE by transport modes}")
  
  plot_dem_ej_modes <- merge(dem_ej,Mapp_Aggr_vehtype,by.x="vehicle_type",by.y="gran_vehtype")
  plot_dem_ej_modes[,demand_EJ:=sum(demand_EJ), by=c("year","region","scenario","aggr_vehtype")]
  #Remove unneeded columns and add model and unit column for mip
  plot_dem_ej_modes <- plot_dem_ej_modes[, c("year","region","aggr_vehtype","demand_EJ","scenario")][,model:= "EDGE-Transport"][,unit:="EJ/yr"]
  setnames(plot_dem_ej_modes, c("year","demand_EJ","aggr_vehtype"),c("period","value","variable"))
  plot_dem_ej_modes <- plot_dem_ej_modes[!duplicated(plot_dem_ej_modes)]
  
  p <- mipArea(plot_dem_ej_modes[region==mainReg], scales="free_y")
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=3.5,width=7") 
  
  p <- mipBarYearData(plot_dem_ej_modes[region==mainReg & period %in% y_bar])
  p <- p + theme(legend.position="none")
  swfigure(sw,print,p,sw_option="height=4.5,width=7")
  
  p <- mipBarYearData(plot_dem_ej_modes[period %in% y_bar])
  swfigure(sw,print,p,sw_option="height=9,width=8")
  
  swlatex(sw,"\\onecolumn")
  p <- mipArea(plot_dem_ej_modes,scales="free_y")
  swfigure(sw,print,p,sw_option="height=8,width=16")
  swlatex(sw,"\\twocolumn")
  
  
  swlatex(sw,"\\subsection{Energy intensity}")
  
  FV_final_pref <- list()
  
  swlatex(sw,"\\section{Cost trends}")
  for (i in 1:length(pref)) {
    FV_final_pref[[i]] <- pref[[i]]$FV_final_pref
    FV_final_pref[[i]]$scenario <- pref[[i]]$scenario
  }
  
  FV_final_pref <- do.call(rbind.data.frame, FV_final_pref)
  #change variable names for mip
  FV_final_pref[logit_type=="pinco_tot", logit_type:="tot"]
  FV_final_pref[logit_type=="pchar", logit_type:="char"] 
  FV_final_pref[logit_type=="pmod_av", logit_type:="av"] 
  FV_final_pref[logit_type=="prange", logit_type:="range"]
  FV_final_pref[logit_type=="pref", logit_type:="ref"] 
  FV_final_pref[logit_type=="prisk", logit_type:="risk"] 
  
  swlatex(sw,"\\subsection{Example large car and SUV}")
  
  swlatex(sw,"\\subsubsection{BEV}")
  FV_final_pref_LSUV <- FV_final_pref[vehicle_type == "Large Car and SUV" & technology == "BEV"]
  FV_final_pref_LSUV <- FV_final_pref_LSUV[,c("year","region","scenario","logit_type","value")][,model:= "EDGE-Transport"][,unit:="EJ/yr"]
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

