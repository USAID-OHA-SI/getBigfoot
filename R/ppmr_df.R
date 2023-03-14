#' ppmr_df
#'
#' @param filepath The path where ppmr data was saved
#' @param filename The name of the ppmr file to be used
#' @importFrom magrittr %>%
#' @export

ppmr_df <- function(filepath, filename) {

  df <- readr::read_csv(filename)

  #munge, fix period to match sc_fact
  #update, changing to no longer match SC_FACT
  #convert to date, and create pepfar quarter
  df <- df %>%
    janitor::clean_names() %>%
    dplyr::select(-notes) %>%
    dplyr::rename(product = standardized_product) %>%
    dplyr::mutate(
      country = stringr::str_to_sentence(country),
      period = lubridate::mdy(period),
      quarter = lubridate::quarter(period, with_year = TRUE, fiscal_start = 10)
    )


  #bring in meta

  df_meta <-
    googlesheets4::read_sheet(
      "1UJv_LAzcD-lkteET9wGPGmdeFn07WnFf7g8sjs-upgk",
      sheet = "regimen",
      col_types = c(.default = "c")
    ) %>%
    janitor::clean_names() %>%
    dplyr::mutate(mot = as.numeric(mot))

  #join on meta
  df <- df %>%
    dplyr::left_join(df_meta) %>%
    dplyr::mutate(mot_ami = round(h_ami_amc * mot, 0),
                  mot_soh = round(soh * mot, 0)) %>%
    tidyr::pivot_longer(
      cols = c("soh", "h_ami_amc", "lmi", "mot", "mot_ami", "mot_soh"),
      names_to = "indicator",
      values_to = "value",
      values_drop_na = TRUE
    ) %>%
    dplyr::filter(value != 0) %>%
    dplyr::mutate(value = round(value, 0))

  df %>% readr::write_csv(paste0(
    filepath,
    "/ppmr_processed_",
    format(Sys.Date(), "%Y%m%d"),
    ".csv"
  ))

  return(df)

}




