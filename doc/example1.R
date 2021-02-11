## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, results='hide', message=FALSE, warning=FALSE----------------------
library(iffitoR)
library(plotly)
library(glue)
library(RODBC)
library(forcats)
library(dplyr)
library(ggplot2)
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
  ),
  "tbl_frane.Generalita.Cod_tipo" = c(
    "diz_frane.diz_tipo_movi.cod_tipo",
    "diz_frane.diz_tipo_movi.tipologia"
  )
)

## ----paths--------------------------------------------------------------------
# I use the path to the databsaes on the Eurac Network (am I connected to the VPN?)

# the path to the iffi polygons
landslide_poly_path = "\\\\projectdata.eurac.edu/projects/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/Shapefiles/IFFI10_5.shp"

# the path to the iffi points
landslide_point_path = "\\\\projectdata.eurac.edu/projects/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/Shapefiles/IFFI10_1.shp" 

# the path to the folder with the iffi-databases 
database_dir = "\\\\projectdata.eurac.edu/projects/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/database" 

## -----------------------------------------------------------------------------
# we only want the dates from the attributes tables
attri = c("anno_min",
          "mese_min",
          "giorno_min")

## ----getdata, warning=F, message=F, cache=TRUE--------------------------------
# this only works under windows
os = Sys.info()["sysname"]

if (os == "Windows") {
  iffi_sf = iffitoR::make_shapefile(
    database_dir = database_dir,
    attribute_database_name = "tbl_frane",
    dictionary_database_name = "diz_frane",
    shapefile = landslide_point_path,
    attri = attri,
    joins = joins
  )
  
}else{
  iffi_sf = landsld
}

## -----------------------------------------------------------------------------
dplyr::glimpse(iffi_sf)

## ----translate----------------------------------------------------------------
# if we are not working with the data from the package we still need to preprocess it a little bit
if(os == "Windows"){
  iffi_eng = iffitoR::translate_iffi(iffi_sf)
  iffi_eng %>% select(c(first_level, second_level)) %>% head()
}

## ----getdate------------------------------------------------------------------
# same for the time information that is alrady in the data from the package
if(os=="Windows"){
  iffi_date = iffitoR::get_date_information(iffi_eng)
  iffi_date %>% select(matches("date|year|month|day")) %>% glimpse()
}

## -----------------------------------------------------------------------------
if(os=="Windows"){
iffi_sf_poly = iffitoR::make_shapefile(database_dir=database_dir,
                                  attribute_database_name = "tbl_frane",
                                  dictionary_database_name = "diz_frane",
                                  shapefile = landslide_poly_path,
                                  attri = attri,
                                  joins=joins)
}

# how many polygons do we have?
dim(iffi_sf_poly)

## ----translatepoly------------------------------------------------------------
if(os=="Windows"){
#trasnlate
iffi_sf_poly = iffitoR::translate_iffi(iffi_sf_poly)

# get date information
iffi_sf_poly = iffitoR::get_date_information(iffi_sf_poly)
}

## ---- fig.width=8-------------------------------------------------------------
if(os=="Windows"){
iffi_sf_poly %>% 
  count(second_level, sort=T) %>% 
  mutate(second_level = glue("{second_level} ({n})")) %>% 
  mutate(second_level = fct_reorder(second_level, n)) %>% 
  ggplot(aes(x=n, y=second_level)) +
  geom_col()  +
  labs(title="Distribution of polygons in the iffi database",
       subtitle = "in the second-level classification",
       x = "# of events",
       y = "") +
  theme_light()
}

## ----whenslides, fig.width=10-------------------------------------------------
if(os=="Windows"){
iffi_sf_poly %>% 
  filter(year.int >= 1990) %>% 
  filter(str_detect(second_level, "transla|rota")) %>% 
  ggplot(aes(x=date, fill=second_level)) +
  geom_histogram(position="dodge", bindwidth=30) +
  scale_x_date(date_breaks = "1 year") +
  theme_light() +
  labs(x="", y="Number of Events", title="When did the slides happen?",
       fill="") +
  theme(
    axis.text.x = element_text(angle=90)
  )
}

## ----plotpoints, fig.width=8--------------------------------------------------
landsld %>% 
  # filter(year.int > 1980) %>% 
  count(second_level, sort = TRUE) %>% 
  mutate(second_level = glue("{second_level} ({n})")) %>% 
  mutate(second_level = fct_reorder(second_level, n)) %>% 
  ggplot() +
  geom_col(aes(x = n, y=second_level)) +
  labs(title="Distribution of points in the iffi database",
       subtitle = "in the second-level classification",
       x = "# of events",
       y = "") +
  theme_light()

## ----timepoints, fig.width=8--------------------------------------------------
landsld %>% 
  filter(str_detect(second_level, "rotational|translational")) %>% 
  filter(date_info != "no date") %>% st_drop_geometry() %>% 
  plot_ly(data = ., x = ~year.posix, color=~second_level) %>% 
  layout(title="Distriubtion of rotational and translational slides", legend=list(x=0.3, orientation="h", y=-0.2), xaxis = list(title="Year"), yaxis=list(title="# of slides"))

