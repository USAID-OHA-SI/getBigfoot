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

  df = readr::read_csv(path)

  #munge, fix period to match sc_fact
  #update, changing to no longer match SC_FACT
  #convert to date, and create pepfar quarter
  df <- df %>%
    janitor::clean_names() %>%
    dplyr::select(-notes) %>%
    dplyr::rename(product = standardized_product) %>%
    dplyr::mutate(country = stringr::str_to_sentence(country),
                  period = lubridate::mdy(period),
                  quarter = lubridate::quarter(period, with_year = TRUE, fiscal_start = 10))


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
    dplyr::mutate(value = round(value,0))

  df %>% readr::write_csv(., paste0(path, "/ppmr_processed_",
                                    format(Sys.Date(),"%Y%m%d"), ".csv"))

  return(df)

}
