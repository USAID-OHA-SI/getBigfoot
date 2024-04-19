#' get_MER
#'
#' @param path The path to the raw MSD file or the folder to save it in if download == T
#' @param download Whether to download a new MSD file, defaults to FALSE
#' @param simple Whether to simplify to 15+ TX_CURR or not, defaults to TRUE
#' @importFrom magrittr %>%
#' @export


get_mer <- function(path = here::here("Data"), download = F, simple = T) {

  if(download == T){
    # Download zip file and arrange by date
    folder <- "1d5dhTK_UCY2RaEW_m-JHKUWjBEkWggNs"
    files_in_folder <- googledrive::drive_ls(googledrive::as_id(folder)) %>%
      googledrive::drive_reveal("created_time") %>%
      dplyr::arrange(desc(created_time))
    googledrive::drive_download(file = files_in_folder$id[1],
                                path = paste0(path, "/", files_in_folder$name[1]),
                                overwrite = T)

    # Unzip and remove zip file
    archive::archive_extract(paste0(path, "/", files_in_folder$name[1]), dir = here(path, "MSD"))
    file.remove(paste0(path, "/", files_in_folder$name[1]))
    path = paste0(path, "/MSD/", stringr::str_replace(files_in_folder$name[1], ".7z", ".txt"))
  } else {
    path = here::here(path, "MSD", list.files(here(path, "MSD"))[1])
  }

  df = gophr::read_psd(path)


  if(simple == T) {
    msd = df %>%
      dplyr::filter(
        trendscoarse == "15+",
        standardizeddisaggregate == "Age/Sex/HIVStatus",
        indicator == "TX_CURR"
      ) %>%
      gophr::reshape_msd(direction = "long") %>%
      dplyr::filter(!period_type %in% c("cumulative", "targets")) %>%
      dplyr::group_by(
        orgunituid,
        sitename,
        operatingunit,
        snu1,
        psnu,
        facilityuid,
        facility,
        indicator,
        standardizeddisaggregate,
        period,
        period_type
      ) %>%
      dplyr::summarize(value = sum(value, na.rm = T))
  } else {
    msd = df
  }


  return(msd)

}
