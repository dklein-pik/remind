
getMagpieData <- function(path_to_report = "report.mif", interfaceRem2Mag = "couplingMag2Rem.csv") {
  
  require(gamstransfer), quietly = TRUE, warn.conflicts = FALSE)
  require(quitte),       quietly = TRUE, warn.conflicts = FALSE)
  require(dplyr),        quietly = TRUE, warn.conflicts = FALSE)
  
  # ---- Define functions ----
  
  # apply eval(parse() to each element of x. 
  # Example: converts the string "1/1000*12/44" to the number 0.0002727273
  calcFromString <- function(x){
    sapply(x, function(i){
      eval(parse(text = i))
    })
  }
  
  # ---- Read mapping of MAgPIE variables to REMIND variables ----
  
  map <- read.csv(interfaceRem2Mag, sep = ";", na.strings = "")
  map$magName <- gsub(" \\(.*\\)$","",map$magName) # remove unit
  
  # ---- Read and prepare MAgPIE data ----
  
  rep <- read.quitte(path_to_report) %>%                    # read MAgPIE report
    inner_join(map, by = c("variable" = "magName")) %>%     # select variables defined in the mapping file
    mutate(factor = calcFromString(factor)) %>%             # calculate the conversion factor given as string
    mutate(value = value * factor) %>%                      # apply the conversion factor (from MAgPIE to REMIND unit)
    rename(ttot = period, regi = region) %>%                # use REMIND set names 
    filter(regi != "World", between(ttot, 2005, 2150)) %>%  # "World" region and years before 2005 are not needed in the input to REMIND
    select(regi, ttot, set, variable, remName, value)       # keep only columns required for import to REMIND
  
  # check if all variables defined in coupling interface (mapping) exist in MAgPIE report
  missingVariables <- !map$magName %in% rep$variable
  if (any(missingVariables)) {
    stop("The following variables defined in the coupling interface do not exist in the MAgPIE report: ", 
         map$magName[missingVariables])
  }
  
  # ---- Create gdx ----
  
  # ---- Define SETS ----
  
  m <- Container$new()
  
  regi <- m$addSet(
    "regi",
    records = unique(rep$regi),
    description = "regions"
  )
  
  ttot <- m$addSet(
    "ttot",
    records = unique(rep$ttot),
    description = "years"
  )
  
  emiMacMagpie <- m$addSet(
    "emiMacMagpie",
    records = filter(map, remName == "f_macBaseMagpie_coupling")[["set"]],
    description = "emission types coming from MAgPIE"
  )
  
  # ---- Define PARAMETERS ----
  
  f_macBaseMagpie_coupling <- m$addParameter(
    "f_macBaseMagpie_coupling",
    domain = c(ttot, regi, emiMacMagpie),
    records = filter(rep, remName == "f_macBaseMagpie_coupling") %>% 
              select(ttot, regi, set, value) %>%
              rename(emiMacMagpie = set),
    description = "emissions from MAgPIE"
  )
  
  p30_pebiolc_pricemag_coupling <- m$addParameter(
    "p30_pebiolc_pricemag_coupling",
    domain = c(ttot, regi),
    records = filter(rep, remName == "p30_pebiolc_pricemag_coupling") %>% 
              select(ttot, regi, value),
    description = "bioenergy price from MAgPIE"
  )
    
  pm_pebiolc_demandmag_coupling <- m$addParameter(
    "pm_pebiolc_demandmag_coupling",
    domain = c(ttot, regi),
    records = filter(rep, remName == "pm_pebiolc_demandmag_coupling") %>% 
              select(ttot, regi, value),
    description = "demand for bioenergy in MAgPIE from which the prices result"
  )
  
  p26_totLUcost_coupling <- m$addParameter(
    "p26_totLUcost_coupling",
    domain = c(ttot, regi),
    records = filter(rep, remName == "p26_totLUcost_coupling") %>% 
              select(ttot, regi, value),
    description = "total production costs from MAgPIE without costs for GHG"
  )
  
  # ---- Write to gdx file ----
  
  m$write("magpieData.gdx")
}

# Coupling REMIND-MAgPIE

# run REMIND reporting and give path to mif to MAgPIE
# - remind2::convGDX2MIF_REMIND2MAgPIE(gdx = "fulldata.gdx", file = "REMIND_rem2mag.mif")

load("config.Rdata")
# - load from REMIND config in the REMIND run folder:
#   - path_to_magpie
#   - MAgPIE settings
#   - run name used for MAgPIE run
# - start MAgPIE
    # outfolder_mag <- start_run(cfg_mag, codeCheck=FALSE)
# - obtain path to individual MAgPIE report
    # report_mag <- file.path(path_magpie, outfolder_mag, "report.mif")
path_to_report <- "/p/projects/remind/runs/REMIND-MAgPIE-2023-11-27/magpie/output/C_SSP1-PkBudg1050-mag-4/report.mif" # cfg$pathToMagpieReport

getMagpieData(path_to_report = path_to_report, interfaceRem2Mag = "mappingMAgPIE2REMIND.csv")
