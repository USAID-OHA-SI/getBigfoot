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
    dplyr::filter(!period_type %in% c("cumulative", "targets")) %>%
    dplyr::group_by(orgunituid,
             sitename,
             operatingunit,
             snu1,
             psnu,
             facilityuid,
             facility,
             indicator,
             disaggregate,
             standardizeddisaggregate,
             period,
             period_type) %>%
    dplyr::summarize(value = sum(value, na.rm = T))


  return(msd)

}
