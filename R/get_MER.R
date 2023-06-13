#' get_MER
#'
#' @param path The path to the raw MSD file
#' @importFrom magrittr %>%
#' @export


get_mer <- function(path) {

  df = gophr::read_psd(path)

  msd = df %>%
    dplyr::filter(
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
    ) %>%
    dplyr::filter(trendscoarse == "15+",
           standardizeddisaggregate == "Age/Sex/HIVStatus",
           indicator == "TX_CURR") %>%
    gophr::reshape_msd(direction = "long") %>%
    dplyr::filter(period_type != "cumulative") %>%
    dplyr::group_by(orgunituid,
             sitename,
             operatingunit,
             country,
             snu1,
             psnu,
             funding_agency,
             community,
             facilityuid,
             facility,
             sitetype,
             indicator,
             disaggregate,
             standardizeddisaggregate,
             period,
             source_name,
             period_type) %>%
    dplyr::summarize(value = sum(value, na.rm = T))


  return(msd)

}
