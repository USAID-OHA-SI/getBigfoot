#' get_scfact
#'
#' @param datapath The path for downloaded data
#' @param upload Whether to upload sc_fact_collated, defaults to F
#'
#' @importFrom magrittr %>%
#' @export

get_scfact <- function(datapath = here::here("Data"), upload = F){

  # Download zip file and arrange by date
  folder <- "1nRslZsUFvjdTN1Kp6pzFk7h3ZWJE-xXX"
  files_in_folder <- googledrive::drive_ls(googledrive::as_id(folder)) %>%
    googledrive::drive_reveal("created_time") %>%
    dplyr::arrange(created_time)
  googledrive::drive_download(file = files_in_folder$id[1],
                              path = paste0(datapath, "/", files_in_folder$name[1]))

  # Unzip and remove zip file
  unzip(paste0(datapath, "/", files_in_folder$name[1]), exdir = paste0(datapath, "/sc_fact"))
  file.remove(paste0(datapath, "/", files_in_folder$name[1]))

  # Bind files together
  sc_fact_file = list.files(paste0(datapath, "/sc_fact"))
  sc_fact = data.frame()
  for(file in sc_fact_file){
    temp = readr::read_csv(paste0(datapath, "/sc_fact/", file))
    sc_fact = sc_fact %>%
      dplyr::bind_rows(temp)
  }

  # Save collated file and remove individual files
  for(file in sc_fact_file){
    file.remove(paste0(datapath, "/sc_fact/", file))
  }
  readr::write_csv(sc_fact, paste0(datapath, "/sc_fact/", "sc_fact_collated_", Sys.Date(), ".csv"))


  if(upload == T){
  # Write collated file to drive
  googledrive::drive_put(media = paste0(datapath, "/sc_fact/", "sc_fact_collated_", Sys.Date(), ".csv"), path = googledrive::as_id("1A2VfmKOxfmmN3h3P_hTI-B5V1B3KSZlx"),
                         name = paste0("sc_fact_collated_", Sys.Date(), ".csv"))
  }

  return(sc_fact)
}
