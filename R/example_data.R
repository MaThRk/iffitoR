#' Example landslide data from the iffi database
#'
#' @docType data
#'
#' @usage data(landsld)
#'
#' @format An object of class \code{"sf"}
#'
#' @keywords dataset
#' @examples
#' data(lndsld)
#' dim(lndsld)
#'
#'

"landsld"



# define some paths -------------------------------------------------------

# we want the point data
points = T

# which os to automatically set the paths
os = Sys.info()["sysname"]

if(os == "Linux"){
  path_ncdf = "/mnt/CEPH_PROJECTS/Proslide/PREC_GRIDS_updated/"
  poly_landslide_path = "/mnt/CEPH_PROJECTS/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/Shapefiles/IFFI10_5.shp"
  points_landslide_path = "/mnt/CEPH_PROJECTS/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/Shapefiles/IFFI10_1.shp"
  database_dir = "/mnt/CEPH_PROJECTS/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/database"
}else if(os == "Windows"){
  path_ncdf = "\\\\projectdata.eurac.edu/projects/Proslide/PREC_GRIDS_updated/"
  poly_landslide_path = "\\\\projectdata.eurac.edu/projects/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/Shapefiles/IFFI10_5.shp"
  points_landslide_path = "\\\\projectdata.eurac.edu/projects/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/Shapefiles/IFFI10_1.shp"
  database_dir = "\\\\projectdata.eurac.edu/projects/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/database"
}else{
  stop(call. = F, "what the hell are you working on...")
}

# which shape to read
if(points){
  path_spatial = points_landslide_path
}else{
  path_spatial = poly_landslide_path
}


# query the landslide data and its attributes -----------------------------

res = iffitoR::make_shapefile(database_dir = database_dir,
                              attribute_database_name = "tbl_frane",
                              # the name without extension
                              dictionary_database_name = "diz_frane",
                              shapefile = path_spatial,
                              # normally null only setting it here for me
                              # the colums we want to retrieve directly
                              attri = c("anno_min",
                                        "mese_min",
                                        "giorno_min",
                                        "area"),

                              # tables to join the description
                              joins = list(
                                "tbl_frane.Generalita.Cod_tipo" = c(
                                  "diz_frane.diz_tipo_movi.cod_tipo",
                                  "diz_frane.diz_tipo_movi.tipologia"
                                ),
                                "tbl_frane.clas_ii_liv.movimento" = c(
                                  "diz_frane.diz_movimenti.movimento",
                                  "diz_frane.diz_movimenti.nome_movimento"
                                ),
                                "tbl_frane.ass_gen_cause.causa" = c(
                                  "diz_frane.diz_cause.causa",
                                  "diz_frane.diz_cause.nome_causa"
                                )
                              )
)


# translate it to english
res_engl = iffitoR::translate_iffi(res)
landsld = iffitoR::get_date_information(res_engl)


usethis::use_data(landsld, overwrite = T)
