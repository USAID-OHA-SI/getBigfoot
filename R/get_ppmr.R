#' get_ppmr
#'
#' @param path The path to the raw PPMR file or the folder to save it in if download == T
#' @param download Whether to download a new PPMR file, defaults to FALSE
#' @importFrom magrittr %>%
#' @export
#'
#' @examples


get_ppmr <- function(path = here::here("Data"), download = F){

  if(download == T){
    # Download zip file and arrange by date
    folder <- "1OzobjBR-f_U8Y88N3fvEvlaMDjpb2pMn"
    files_in_folder <- googledrive::drive_ls(googledrive::as_id(folder)) %>%
      googledrive::drive_reveal("created_time") %>%
      dplyr::arrange(desc(created_time))
    googledrive::drive_download(file = files_in_folder$id[1],
                                path = paste0(path, "/", files_in_folder$name[1]),
                                overwrite = T)

    path = paste0(path, "/", files_in_folder$name[1])
  }

  df = readxl::read_excel(path, sheet = "All months")

  #munge, fix period to match sc_fact
  #update, changing to no longer match SC_FACT
  #convert to date, and create pepfar quarter
  df <- df %>%
    janitor::clean_names() %>%
    dplyr::select(-notes, -column1) %>%
    dplyr::rename(product = product_name) %>%
    dplyr::mutate(country = stringr::str_to_sentence(country),
                  smp = lubridate::quarter(x = lubridate::ymd(period), with_year = TRUE, fiscal_start = 10),
                  mer_pd = paste0("FY", substr(smp, 3, 4), "Q", substr(smp, 6, 6))) %>%
    dplyr::select(-smp)


  #bring in meta

  df_meta <- googlesheets4::read_sheet("1UJv_LAzcD-lkteET9wGPGmdeFn07WnFf7g8sjs-upgk",
                                       sheet = "regimen",
                                       col_types= c(.default = "c")) %>%
    janitor::clean_names() %>%
    dplyr::mutate(mot = as.numeric(mot))

  #join on meta
  df <- df %>%
    dplyr::left_join(df_meta) %>%
    dplyr::mutate(mot_ami = round(h_ami_amc*mot,0),
                  mot_soh = round(soh*mot,0)) %>%
    tidyr::pivot_longer(cols = c("soh", "h_ami_amc", "lmi", "mot", "mot_ami", "mot_soh"),
                        names_to = "indicator",
                        values_to = "value",
                        values_drop_na = TRUE) %>%
    dplyr::filter(value !=0) %>%
    dplyr::mutate(value = round(value,0)) %>%
    dplyr::select(country, period, product_category, product, mer_pd, pill_count, combination_type, age_group, product_type, indicator, value)

  path = dirname(path)

  df %>% readr::write_csv(., paste0(path, "/ppmr_processed_",
                                    format(Sys.Date(),"%Y%m%d"), ".csv"))

  return(df)

}
