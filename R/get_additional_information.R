#' Get more information from the LPM DATA
#'
#' This function takes three paths and returns an object of type sf with all the columns from the LPM_shapefile
#' specified in the parameter \code{keep_lm}
#'
#'
#' @importFrom sf read_sf st_drop_geometry()
#' @imoprtFrom readxl read_excel
#' @importFrom from tidyr extract
#' @importFrom dplyr mutate
#'
#' @return an object of type \code{sf}
#'
#' @param path_IFFI10 Path to the IFFI10 shapefile
#' @param path_LPM_shape Path to the LPM shapefile
#'
#' @export

get_additional_information = function(path_IFFI10 = "\\\\projectdata.eurac.edu/projects/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/Shapefiles/IFFI10_1.shp",
                                      path_LPM_shape = "\\\\projectdata.eurac.edu/projects/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/Shapefiles/LPM_Akten.shp",
                                      path_stato = "\\\\projectdata.eurac.edu/projects/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/database/Stato delle conoscenze.xls",
                                      keep_lpm = c("ACCGE_IDEN", "GEOKAT_ID", "OGGETTO")) {
  # verify that there actually are some paths
  stopifnot("The path to the IFFI10-shapefile does not result in a file..." = file.exists(path_IFFI10))
  stopifnot("The path to the LPM-shapefile does not result in a file..." = file.exists(path_LPM_shape))
  stopifnot("The path to the Stato delle conoscenze does not result in a file..." = file.exists(path_stato))


  # read in the data
  iffi10 = read_sf(path_IFFI10)
  lpm_shape = read_sf(path_LPM_shape)
  stato = read_excel(path_stato)

  # check if they use the same crs
  crs_iffi = st_crs(iffi10)
  crs_lpm = st_crs(lpm_shape)
  # if not the same --> reproject
  if (!crs_iffi == crs_lpm) {
    message("Reprojecting the lpm-shapefile to the iffi shape")
    lpm_shape = st_transform(lpm_shape, crs_iffi)
  }

  #

  # calculate the iffi kodex in the stato excel -----------------------------
  # there are duplicates in the calculated iffi-index --> What to do?
  stato = stato %>%
    mutate(iffi = 10000 * GEN_ID + 100 * GEN_SUBID + 00)

  # merge the iffi with stato based on the iffi kodex -----------------------
  df_iffi = merge(
    iffi10,
    stato,
    by.x = "PIFF_ID",
    by.y = "iffi",
    all.x,
    all.y = F
  ) %>%
    dplyr::distinct(PIFF_ID, .keep_all = TRUE)


  # get the ACCGE_IDEN from the excel table ---------------------------------
  df_iffi = df_iffi %>%
    tidyr::extract(
      col = c("NOTE"),
      into = c("ACCGE_IDEN"),
      "(\\d{5})",
      remove = FALSE
    )

  # remove the spatial information from the lpm_shapefile -------------------
  lpm_no_shape = lpm_shape %>% st_drop_geometry() %>% .[, keep_lpm]



  # join the lpm_no_shape_information to the iffi ---------------------------
  all_joined = merge(lpm_no_shape, df_iffi, by = "ACCGE_IDEN") %>% st_as_sf()


  # return the joined shapefile
  return(all_joined)



}
