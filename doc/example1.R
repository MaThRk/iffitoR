## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, results='hide', message=FALSE, warning=FALSE----------------------
library(iffitoR)
library(RODBC)
library(dplyr)
library(tools)
library(stringr)
library(sf)

## -----------------------------------------------------------------------------
joins = list(
    "tbl_frane.Geologia.litologia" = c(
      "diz_frane.diz_litologie.litologia",
      "diz_frane.diz_litologie.nome_litologia"
    ),
    
    "tbl_frane.clas_ii_liv.movimento" = c(
      "diz_frane.diz_movimenti.movimento",
      "diz_frane.diz_movimenti.nome_movimento"
    ))

## -----------------------------------------------------------------------------

# Not run

#res_sf = make_shapefile(database_dir = "vignettes/data/",
#                       attribute_database_name = "test",
#                       dictionary_database_name = "dic_db",
#                       shape = "vignettes/data/IFFI10_1.shp",
#                       attri=c("anno_min", "mese_min"))

## ---- fig.width=8, fig.height=8-----------------------------------------------

# Not run

#library(ggplot2)
#res_sf = res_sf %>% filter(anno_min > 2000)
#
#res_gg = ggplot(res_sf) +
#  geom_sf(aes(color=anno_min)) +
#  scale_color_continuous(low="white", high="darkgreen", na.value="grey", name="year")
#
#res_gg

## ---- echo=F, fig.align='center', out.width="80%", out.height="80%"-----------
knitr::include_graphics("map1.png")

