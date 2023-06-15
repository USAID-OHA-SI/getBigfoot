#' sc_fact_df
#'
#' @param filepath The path where sc_fact data set was saved via get_scfact.
#' @param outpath The local path where output data will be saved
#' @param download Whether to download a new version of sc_fact
#'
#' @importFrom magrittr %>%
#' @export

sc_fact_df <- function(filepath = here::here("Data", "sc_fact"), outpath, download = F) {

  # download sc_fact_collated if necessary
  if(download == T){
    folder <- "1A2VfmKOxfmmN3h3P_hTI-B5V1B3KSZlx"
    files_in_folder <- googledrive::drive_ls(googledrive::as_id(folder)) %>%
      googledrive::drive_reveal("created_time") %>%
      dplyr::filter(stringr::str_detect(name, "collated")) %>%
      dplyr::arrange(created_time)
    googledrive::drive_download(file = files_in_folder$id[1],
                                path = paste0(filepath, "/", files_in_folder$name[1]))
  }

  # get latest sc_fact locally
  files = data.frame("names" = list.files(filepath)) %>%
    dplyr::filter(stringr::str_detect(names, "collated")) %>%
    dplyr::arrange(dplyr::desc(names))

  sc_fact = readr::read_csv(paste0(filepath, "/", files$names[1]))

  # get df_meta
  df_meta <- googlesheets4::read_sheet("1UJv_LAzcD-lkteET9wGPGmdeFn07WnFf7g8sjs-upgk",
                                       sheet = "regimen",
                                       col_types= c(.default = "c")) %>%
    dplyr::rename_all(~tolower(.)) %>%
    dplyr::mutate(mot = as.numeric(mot))


  # process sc_fact
  sc_fact = sc_fact %>%
    janitor::clean_names() %>%
    dplyr::mutate_at(dplyr::vars(soh, ami, mos), ~as.numeric(.)) %>%
    dplyr::mutate(country = stringr::str_to_sentence(country)) %>%
    dplyr::select(period,
           orgunituid = datim_code,
           product_category,
           product,
           sku,
           pack_size,
           soh,
           ami,
           mos) %>%
    dplyr::left_join(df_meta, by = "product") %>%
    dplyr::mutate(mot_ami = ami*mot,
           mot_soh = soh*mot) %>%
    dplyr::mutate(smp = lubridate::quarter(x = lubridate::ym(period), with_year = TRUE, fiscal_start = 10),
                  mer_pd = paste0("FY", substr(smp, 3, 4), "Q", substr(smp, 6, 6))) %>%
    dplyr::select(-smp)

  # Save processed file
  readr::write_csv(sc_fact, paste0(filepath, "sc_fact_processed_", Sys.Date(), ".csv"))

  # Write collated file to drive
  googledrive::drive_put(media = paste0(filepath, "sc_fact_processed_", Sys.Date(), ".csv"), path = googledrive::as_id("1A2VfmKOxfmmN3h3P_hTI-B5V1B3KSZlx"),
                         name = paste0("sc_fact_processed_", Sys.Date(), ".csv"))


  return(sc_fact)

}







