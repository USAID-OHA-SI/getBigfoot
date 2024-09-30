#' get_scfact
#'
#' @param datapath The path for downloaded data
#' @param upload Whether to upload sc_fact_collated, defaults to F
#' @param download Whether to download a new file or look for a local one, defaults to T
#'
#' @importFrom magrittr %>%
#' @export

get_scfact <- function(datapath = here::here("Data"), upload = F, download = T){

  if(download == T){
  # Download zip file and arrange by date
  folder <- "1nRslZsUFvjdTN1Kp6pzFk7h3ZWJE-xXX"
  files_in_folder <-
    googledrive::drive_ls(googledrive::as_id(folder)) %>%
    dplyr::filter(stringr::str_detect(name, ".zip"))

  downloader <- function(filename){
    googledrive::drive_download(file = files_in_folder$id[files_in_folder$name == filename],
                                  path = paste0(datapath, "/", filename))
    unzip(paste0(datapath, "/", filename), exdir = paste0(datapath, "/sc_fact"))
    file.remove(paste0(datapath, "/", filename))
  }

  purrr::map(
    files_in_folder$name, ~downloader(.x))

  # Define not_all_na
  not_all_na <- function(x) any(!is.na(x))

  # Bind files together
  sc_fact_file = list.files(paste0(datapath, "/sc_fact"))
  sc_fact = data.frame()
  for(file in sc_fact_file){
    temp = readr::read_csv(paste0(datapath, "/sc_fact/", file)) %>%
      dplyr::select(dplyr::where(not_all_na))
    sc_fact = sc_fact %>%
      dplyr::bind_rows(temp)
  }

  # Save collated file and remove individual files
  for(file in sc_fact_file){
    file.remove(paste0(datapath, "/sc_fact/", file))
  }
  readr::write_csv(sc_fact, paste0(datapath, "/sc_fact/", "sc_fact_collated_", Sys.Date(), ".csv"))
}

  if(upload == T){
  # Write collated file to drive
  googledrive::drive_put(media = paste0(datapath, "/sc_fact/", "sc_fact_collated_", Sys.Date(), ".csv"), path = googledrive::as_id("1A2VfmKOxfmmN3h3P_hTI-B5V1B3KSZlx"),
                         name = paste0("sc_fact_collated_", Sys.Date(), ".csv"))
  }
  if(download == F){
    if(length(list.files(paste0(datapath, "/sc_fact"), patt = "sc_fact_collated*"))>0){
      sc_fact = list.files(paste0(datapath, "/sc_fact"), patt = "sc_fact_collated*") %>%
        tibble::as_tibble() %>%
        dplyr::mutate(date = stringr::str_remove(value, "sc_fact_collated_")) %>%
        dplyr::mutate(date = lubridate::as_date(stringr::str_remove(date, ".csv"))) %>%
        dplyr::arrange(desc(date))
      sc_fact = readr::read_csv(paste0(datapath, "/sc_fact/", sc_fact$value[1]))
    } else {
      print("No local version of sc_fact found on datapath.")
      sc_fact = NULL
    }
  }

  return(sc_fact)
}
