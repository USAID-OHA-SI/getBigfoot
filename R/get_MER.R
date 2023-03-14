#' get_MER
#'
#' @param filepath The path where mer data should be saved
#' @param filename The name of the MSD or genie export
#' @param disaggregate The level(s) of disaggregation to filter by. Defaults to "all" for no filtering.
#' @param indc The indicator(s) to filter by. Defaults to "all" for no filtering.
#' @importFrom magrittr %>%
#' @export


get_mer <- function(filepath, filename, disaggregate = "all", indc = "all") {

#  indc <- c("PrEP_CURR", "TB_PREV", "HTS_TST", "HTS_TST_POS", "SC_CURR", "SC_ARVDISP")

  df <- gophr::read_psd(file.path(filepath, filename)) %>%
    filter(
      operatingunit %in%  c(
        "Angola",
        "Botswana",
        "Cameroon",
        "Haiti",
        "Lesotho",
        "Malawi",
        "Mozambique",
        "Namibia",
        "Nigeria",
        "Uganda",
        "Zambia",
        "Zimbabwe"
      )
    )

  # df <- df %>%
  #   dplyr::select(sitename, orgunituid, fundingagency, operatingunit, snu1, psnu, standardizeddisaggregate,
  #          otherdisaggregate, trendscoarse, indicator, fiscal_year, targets, qtr1, qtr2, qtr3, qtr4, cumulative)

  # #split off txcurr
  # dftx <- df %>%
  #   dplyr::filter(indicator == "TX_CURR",
  #          standardizeddisaggregate == "Age/Sex/HIVStatus") %>%
  #   dplyr::group_by(across(c(sitename:fiscal_year))) %>%
  #   dplyr::summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  #   dplyr::ungroup()

  # Filter df by disaggregate
  if(disaggregate == "all"){
    dfo = df
  } else {
    dfo <- df %>%
      dplyr::filter(standardizeddisaggregate %in% disaggregate)
  }
  if(length(dfo$standardizeddisaggregate==0)){
    disags = df %>% dplyr::distinct(standardizeddisaggregate) %>% as.list()
    stop(paste0("Specified disaggregate not found. Please specify one or more of the following: ", paste(disags$standardizeddisaggregate, collapse = ", "), ", or all."))
  }

  # Filter df by indc
  if(idc == "all"){
    dfo = dfo
  } else {
    dfo <- dfo %>%
      dplyr::filter(indicator %in% indc)
  }
  if(length(dfo$indicator==0)){
    ind_opts = df %>% dplyr::distinct(indicator) %>% as.list()
    stop(paste0("Specified indicator not found or not included in disaggregate. Indicators include: ", paste(ind_opts$indicator, collapse = ", "), ", or all."))
  }

  dfall = dfo %>% gophr::reshape_msd(direction = "long")


  ##bind them and make long, remove periods that don't align w/SC_FACT
  dfall <- dfo %>%
    #dplyr::bind_rows(dftx, dfo) %>%
    gophr::reshape_msd(direction = "long") %>%
    dplyr::filter(!period %in% c("fy2019q1", "fy2019q2"))


  #Add months of treatment
  dfall <- dfall %>%
    dplyr::mutate(val = dplyr::case_when(
      otherdisaggregate == "ARV Dispensing Quantity - 3 to 5 months" ~ (val*3),
      otherdisaggregate== "ARV Dispensing Quantity - 6 or more months" ~ (val*6),
      TRUE ~ val)) %>%
    dplyr::rename(value = val)

  return(dfall)

}
