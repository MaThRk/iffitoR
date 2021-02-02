#' Main Function

#' @import dplyr
#' @import RODBC
#' @import tools
#' @import sf
#' @import stringr


#' @description
#' Generate a shapefile from a given spatial data-object and attributes as
#' produced by a query on the iffi database
#'
#' @return An sf-oject
#'
#' @param database_dir Path to the directory of the databases
#' @param attribute_database_name Name without extension of the attributes database
#' @param dictionary_database_name Name without extension of the dictionary database
#' @param shapefile A shape that cointains the iffi_kodex in a column called PIFF_ID
#' @param attri A vector of attributes to query directly from the attributes database
#' @param join A named list of the tables that need to be joined in order to get the desciptions from the dictionary
#' @export
#' @examples
#' \dontrun{
#'database_dir = "data/database/",
#'attribute_database_name = "tbl_frane",
#'dictionary_database_name = "diz_frane",
#'shapefile = "data/Shapefiles/IFFI10_5.shp",
#'attri = c(
#'   "anno_min",
#'   "anno_max",
#'   "mese_min",
#'   "mese_max",
#'   "giorno_min",
#'   "giorno_max"
#'),
#'joins = list(
#'   "tbl_frane.Geologia.litologia" = c(
#'      "diz_frane.diz_litologie.litologia",
#'      "diz_frane.diz_litologie.nome_litologia"
#'   ),
#'
#'   "tbl_frane.clas_ii_liv.movimento" = c(
#'      "diz_frane.diz_movimenti.movimento",
#'      "diz_frane.diz_movimenti.nome_movimento"
#'   ),
#'   "tbl_frane.Uso_Suolo.uso_suolo" = c(
#'      "diz_frane.diz_usi_suolo.uso_suolo",
#'      "diz_frane.diz_usi_suolo.nome_uso_suolo"
#'   )
#'),
#'plot = FALSE
#')
#'}

make_shapefile = function(database_dir=NULL,
                          attribute_database_name=NULL,
                          dictionary_database_name=NULL,
                          shapefile=NULL,
                          attri=NULL,
                          joins=NULL) {

   # establish connections
   conns = set_connection(database_dir)

   # set the right ones
   # there are some issues with indexing the list, for some reason we need to index conns with [[]]
   # to maintain a valid and open connection
   index_attr = which(grepl(attribute_database_name, names(conns)))

   if (!is.null(dictionary_database_name)) {
      index_dict = which(grepl(dictionary_database_name, names(conns)))
   }

   attr_database_conn = conns[[index_attr]]

   if (!is.null(dictionary_database_name)) {
      dict_database_conn = conns[[index_dict]]
   }

   # the table names are the attributes we can query
   # Especially the one in the attributes table are interesting
   table_names_attr = make_vector_table_names(attr_database_conn)
   if (!is.null(dictionary_database_name)) {
      table_names_dict = make_vector_table_names(dict_database_conn)
   }

   # create a csv file of the names of the databases
   # write_csvs(table_names = table_names, database_dir = database_dir)

   # make a list of dataframes(tables) for the attributes database
   dfs_attr = make_list_dataframes(attr_database_conn)


   # make a list of dataframes(tables) for the dictionary database
   if (!is.null(dictionary_database_name)) {
      dfs_dict = make_list_dataframes(dict_database_conn)
   }

   # check for each dataframe if they have an id and subid column
   log_vec = check_id(dfs_attr)

   # for the rest create the iffi index for the attribute tables
   dfs_attr_iffi = create_iffi_index(dfs_attr, log_vec)

   # find the tables that we can join directly
   tables_to_append_diretly = find_tables(dfs_attr_iffi, attri)

   ### join those tables
   # read the shape
   shape = read_shape(shapefile)
   shape_joined_attri = join_shape_attributes(shape, tables_to_append_diretly, dfs_attr_iffi)

   # make the joins to the dictionary
   if (!is.null(dictionary_database_name) | !is.null(joins)) {
      joined_dicionary_tables_with_iffi_kodex = join_descriptions(joins, dfs_attr_iffi, dfs_dict)
   }

   # join them to the shape
   if (!is.null(dictionary_database_name) | !is.null(joins)) {
      final_joined = join_descriptions_shape(joined_dicionary_tables_with_iffi_kodex, shape_joined_attri)
   }

   # filter the columns we wanted
   final_selected = select_cols(final_joined, attri, joins)

   return(final_selected)

}
