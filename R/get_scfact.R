#' get_scfact
#'
#' @param datapath The path for downloaded data
#'
#' @importFrom magrittr %>%
#' @export

get_scfact <- function(datapath){

  file <- googledrive::drive_ls(googledrive::as_id("1og4f-ZVzIF2H3TjfxfvLlJv1HYRU_0zD"))

  filename <- file %>%
    dplyr::filter(stringr::str_detect(name, pattern = "raw")) %>%
    dplyr::pull(name)


  glamr::import_drivefile(drive_folder = "1og4f-ZVzIF2H3TjfxfvLlJv1HYRU_0zD",
                          filename = filename,
                          folderpath = datapath,
                          zip = FALSE)
}
