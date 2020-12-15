## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(iffitoR)

## -----------------------------------------------------------------------------
connections = iffitoR::set_connection(".")

## -----------------------------------------------------------------------------
connections

## -----------------------------------------------------------------------------
# connect to the dictionary database
list_of_dfs = iffitoR::make_list_dataframes(connections[[1]])

for (i in seq_along(list_of_dfs)) {
  print(paste0("Colums-Names in", names(list_of_dfs)[[i]]))
  print(names(list_of_dfs[[i]]))
  print("")
}

