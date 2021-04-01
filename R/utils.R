#' Functions that help to query the data from the databases and build the shapefile

#' @importFrom dplyr select filter distinct mutate if_else
#' @importFrom  RODBC sqlFetch sqlTables
#' @importFrom  tools file_path_sans_ext
#' @importFrom  sf st_transform st_read
#' @importFrom  stringr str_split str_replace
#' @importFrom  GADMTools gadm_sf_loadCountries

#' @export
#' @param database_dir The directory where all the databases are
#' @return A list with database-connections
set_connection = function(database_dir) {

  #list all mdb file
  databases_found = list.files(database_dir, full.names = TRUE) %>%
    grep(".*\\.mdb", ., value = TRUE)


  # if it did not find anything
  if (length(databases_found) < 1) {
    stop(call. = F, "No database 'mdb' could be found...")
  } else{
    print(paste0("Found Databases: ", databases_found))
  }

  # get the names
  names_databases = databases_found %>% basename(.) %>%
    tools::file_path_sans_ext(.)


  # setup the connections
  connections = lapply(databases_found, function(x) {
    odbcDriverConnect(sprintf(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                  DBQ=%s",
      x
    ))
  })

  # set the names
  names(connections) = names_databases


  # return character vector with the databases
  return(connections)

}

##----------------------------------------------------------------
##                  read the appropriate shape                  --
##----------------------------------------------------------------

read_shape = function(shapefile) {

 if(!is.character(shapefile)) {
    stop(call. = FALSE, "The shapefile-argument must be passed a character-path")
 }

  # make sure it's a valid path
  if (!file.exists(shapefile)) {
    stop(call. = FALSE, "the shape does not exist")
  }

  # connect to shape
  shape = st_read(shapefile)

  # return shape
  return(shape)
}




""
select_database = function(conns, database) {
  # select the right one
  return(conns[grepl(database, names(conns))])
}

#' return a list of table names for a list of db connection
#' @export
make_list_tables = function(conns) {
  # declare names list to store the names
  table_names_list = vector("list", length = length(conns))
  names(table_names_list) = names(conns)

  # fill it with the names
  for (i in seq_along(conns)) {
    table_names = sqlTables(conns[[i]]) %>%

      # dont choose those tables that have SYSTEM in them
      filter(!grepl(pattern = ".*SYSTEM.*", TABLE_TYPE)) %>%
      select(TABLE_NAME) %>% .[["TABLE_NAME"]]

    table_names_list[[i]] = table_names

  }
  # return the named list
  return(table_names_list)
}


#' Creates a vector of table names from a RODBC connection
#' @return  A single vector of table names for a single db connection
#' @export
#' @param db_conn A object if type RODBC
make_vector_table_names = function(db_conn) {
  table_names = sqlTables(db_conn) %>%
    # dont choose those tables that have SYSTEM in them
    filter(!grepl(pattern = ".*SYSTEM.*", TABLE_TYPE)) %>%
    select(TABLE_NAME) %>% .[["TABLE_NAME"]]

  # return the named list
  return(table_names)
}



#' non-value returning function

write_csvs = function(table_names, database_dir) {
  # must be a list of table names
  if (!is(table_names, "list")) {
    stop(call. = FALSE,
         "In order to create the csvs you must crate a list of table names")
  }


  # create the csvs
  for (i in seq_along(table_names)) {
    # create name for csv
    name = names(table_names)[[i]]
    filepath = file.path(database_dir, paste0(name, ".csv"))

    # write it out
    print("")
    print(paste0("writing csv: ", filepath))
    print("")
    write.csv(table_names[[i]],
              file = filepath,
              row.names = FALSE)
  }

}

#' turn a database connection into a list of dbs
#' @export
make_list_dataframes = function(db_conn) {

  # get all the tables
  tables_names = make_vector_table_names(db_conn)
  tables_names = tolower(tables_names)

  # make a list of dfs(tables)
  tables_list = vector("list", length = length(tables_names))
  names(tables_list) = tables_names

  for (tbl in seq_along(tables_names)) {

    # for every table make a df
    df = sqlFetch(db_conn, tables_names[[tbl]])
    names(df) = tolower(names(df))

    tables_list[[tbl]] = df
  }

  return(tables_list)

}

#' for each dataframe in a list check if there is a column called id and subid

check_id = function(list_of_dfs) {
  # declare the logical vector in order to afterwards not compute the iffi index here
  logical_vec = vector(length = length(list_of_dfs))

  # check it for each dataframe
  for (df in seq_along(list_of_dfs)) {

    colnames_df = names(list_of_dfs[[df]])
    subid = any(grepl("\\bsubid\\b", colnames_df, ignore.case = TRUE))
    subid_idx = which(grepl("\\bsubid\\b", colnames_df, ignore.case = TRUE))
    id = any(grepl("\\bid\\b", colnames_df, ignore.case = TRUE))
    id_idx = which(grepl("\\bid\\b", colnames_df, ignore.case = TRUE))

    if (! (subid & id)) {

      print(paste0(names(list_of_dfs)[[df]], ": does not have the id and subid col"))
      logical_vec[[df]] = FALSE

    } else if (length(list_of_dfs[[df]][[id_idx]]) < 1 ||
               length(list_of_dfs[[df]][[subid_idx]]) < 1) {

      print(paste0(names(list_of_dfs)[[df]], ": subid or id colum have length 0"))
      logical_vec[[df]] = FALSE
    }

    else{
      logical_vec[[df]] = TRUE
    }
  }

  # return the vecor for indexing later
  return(logical_vec)
}


#' create the index to join it later to the shapefile

create_iffi_index = function(list_of_dfs, log_idx) {

  for (df in seq_along(list_of_dfs)) {
    # if the df has those two columns
    if (log_idx[[df]]) {

      # get the indices of id and subid
      colnames_df = names(list_of_dfs[[df]])
      id_idx = which(grepl("\\bid\\b", colnames_df, ignore.case = TRUE))
      subid_idx = which(grepl("\\bsubid\\b", colnames_df, ignore.case = TRUE))

      id = list_of_dfs[[df]][[id_idx]] * 10000
      subid = list_of_dfs[[df]][[subid_idx]] * 100

      iffi_kodex = id + subid + 00

      # add it to the df
      list_of_dfs[[df]][["iffi_kodex"]] = iffi_kodex

      print(paste0("Created iffi-kodex for: ", names(list_of_dfs)[[df]]))
    }

  }
  return(list_of_dfs)

}


#' find the necessary tables

find_tables = function(list_of_dfs_with_iffi_kodex, attri){ # great name...

  # make a list of all tables-names (names) and the columns (keys)
  tables_names_columns = lapply(list_of_dfs_with_iffi_kodex, names)

  # now find the tables you need
  tables = vector()

  # counter to count up the tables vector
  j = 1

  # for each attribute
  for (attr in attri) {

    # for each tables
    for (i in seq_along(tables_names_columns)) {

      # if the attribute is in one of the colums of the table
      if (any(grepl(attr, tables_names_columns[[i]], ignore.case = TRUE))) {
        print(paste0("The variable: ", attr, " is found in table: ", names(tables_names_columns)[[i]]))
        # it its not yet in the list of tables
        if (! names(tables_names_columns)[[i]] %in% tables) {
          tables[[j]] = names(tables_names_columns)[[i]]
          j = j+1

        }

      }

    }

  }

  return(tables)

}


#' @export
# joins a single sf-object with a column "PIFF_ID" to another table
# with a column "iffi_kodex" that gets created in the function "create_iffi_kodex"
join_on_iffikodex = function(shape, table2){

  # if the table to join on the shape has duplicates in the iffi-kodex, this will become huge
  # so remove the duplicates ??
  table2 = table2 %>% dplyr::distinct(iffi_kodex, .keep_all = TRUE)

  if (! "PIFF_ID" %in% names(shape)) {
    stop(call. = FALSE, "The does not have a column named NUMEROGIS")
  }


  merged_sf = merge(shape, table2, by.x="PIFF_ID", by.y="iffi_kodex", all.x = T, all.y=F)

  return(merged_sf)

}


#' return a specific dataframe from a named list of dataframes

return_df_on_table_name = function(list_of_dfs, table_name) {
  idx = which(names(list_of_dfs) == table_name)
  return(list_of_dfs[[idx]])
}


join_shape_attributes = function(shape, names_of_tables, dfs_attr_iffi){

  merged_shape = shape

  for (table in names_of_tables) {

    # get the attribute table
    single_df_attr = return_df_on_table_name(dfs_attr_iffi, table)

    # update the shape
    merged_shape = join_on_iffikodex(merged_shape, single_df_attr)
  }

  return(merged_shape)

}


# small helper

split_and_return = function(x){
  cols = vector()
  i = 1
  for (table in x) {
    col = str_split(table, "\\.") %>% .[[1]] %>% .[[3]]
    cols[[i]] = col
    i = i +1
  }
  return(cols)
}



join_descriptions = function(joins, dfs_attr_iffi, dfs_dict){

  # list of joined tables
  joined_tables = vector("list")

  # make them all lower case
  joins = lapply(joins, tolower)
  names(joins) = tolower(names(joins))

  for (i in seq_along(joins)) {

    # make a vector of the three
    elements_in_join = c(names(joins)[[i]], joins[[i]][[1]], joins[[i]][[2]]) %>% tolower(.)

    # assert that all have 3 entries (db, table, col)
    length_attri = elements_in_join[[1]] %>% stringr::str_split(., pattern = "\\.") %>% unlist() %>% length()
    length_diz_key = elements_in_join[[2]] %>% stringr::str_split(., pattern = "\\.") %>% unlist() %>% length()
    length_diz_value = elements_in_join[[3]] %>% stringr::str_split(., pattern = "\\.") %>% unlist() %>% length()

    # vector of the three lengths
    ll = c(length_attri, length_diz_key, length_diz_value)

    for (l in seq_along(ll)) {
      if (ll[[l]] != 3) {
        stop(call. = FALSE, paste0("You gave a wrong joins attribute\n", elements_in_join[[l]] ,"\n does not thave the three elements <database>.<table>.<column>"))
      }
    }

    #------------

    # get the database, Table, column of the attribute datbase
    table_attri = names(joins)[[i]] %>% stringr::str_split(., pattern = "\\.") %>% .[[1]] %>% .[[2]]
    col_attri = names(joins)[[i]] %>% stringr::str_split(., pattern = "\\.") %>% .[[1]] %>% .[[3]]

    # find the attribute table
    df_index = grep(pattern = paste0("^", table_attri, "$"), names(dfs_attr_iffi))
    if(length(df_index) == 0){
      stop(paste0("The attribute table ", table_attri," specified in the joins list does not have a corresponding table in the database..."))
    }
    # select the column to join and the iffi kodex
    df = dfs_attr_iffi[[df_index]] %>% select(c(col_attri, "iffi_kodex"))

    #------------

    # get the database, Table, column of the dictionary datbase
    table_dict = joins[[i]] %>% stringr::str_split(., pattern = "\\.") %>% .[[1]] %>% .[[2]]

    cols_dict = sapply(joins[[i]], function(x) split_and_return(x), USE.NAMES = F)

    # find the dict table
    df_dict_index = grep(table_dict, names(dfs_dict))
    df_dict = dfs_dict[[df_dict_index]] %>% select(cols_dict)

    #--------------------

    # join them
    merged = merge(df, df_dict, by.x = col_attri, by.y = cols_dict[[1]], all.x = T, all.y=F)

    print(paste0("joind the tables: ", table_attri, " and ", table_dict))
    print(paste0("    on the columns ", col_attri, " and ", cols_dict[[1]]))
    print(paste0("    Resulting table is has ", dim(merged)[[1]], " rows, and ", dim(merged)[[2]], " columns"))
    print("")


    joined_tables[[i]] = merged

  }

  return(joined_tables)

}

#' join each table in a list of tables from the dict on the shape

join_descriptions_shape = function(list_of_dfs_dict, shape) {

  final_shape = shape

  for(table_dict in list_of_dfs_dict){

    final_shape = join_on_iffikodex(final_shape, table_dict)

    # i = i+1
  }

  return(final_shape)

}

#' @export
select_cols = function(df, attri, joins){

  # the directly selected columns
  column_names = attri

  # the columns we want from the dictionary
  cols_dict = vector(length = length(joins))

  for (i in seq_along(joins)) {

    # will get the column-name of interest from the joins-list
    col = sapply(joins[[i]], function(x) split_and_return(x), USE.NAMES = F) %>% .[[2]]
    cols_dict[[i]] = col
  }

  final_cols = c("PIFF_ID", column_names, cols_dict)

  # select them
  df = df %>% select(final_cols)

  return(df)

}

#' @export
get_shape_southtyrol = function(level=3){
  italy = gadm_sf_loadCountries("ITA", level=level)$sf %>% st_transform(st_crs(25832))
  south_tyrol = italy %>% filter(grepl("Bolzano", NAME_2))
  return(south_tyrol)
}
