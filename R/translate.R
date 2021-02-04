#' function to translate some of the column to english
#'
#' @export
#'
translate_iffi = function(df) {

  # do some checking
  if(!any(grepl("anno_min|mese_min|giorno_min|tipologia|nome_movimento", names(df)))){
    stop("some of the necessary columns are'n present in the dataframe provided, ",
         "Did you pass the dataframe directly to the translate-funtion after you created it with make_shapefile?")
  }


  df = df %>%

    # rename the values in nome_movimento
    mutate(
      nome_movimento = str_replace(nome_movimento, pattern = "[Cc]omplesso", replacement = "complex"),
      nome_movimento = str_replace(nome_movimento, pattern = "Aree soggette a crolli/ribaltamenti diffusi", replacement = "area subject to rockfall/topple"),
      nome_movimento = str_replace(nome_movimento, pattern = "crollo", replacement = "fall-type"),
      nome_movimento = str_replace(nome_movimento, pattern = "scivolamento traslativo", replacement = "translational slide"),
      nome_movimento = str_replace(nome_movimento, pattern = "scivolamento rotazionale", replacement = "rotational slide"),
      nome_movimento = str_replace(nome_movimento, pattern = "Aree soggette a sprofondamenti diffusi", replacement = "areas subject to subsidence"),
      nome_movimento = str_replace(nome_movimento, pattern = 'col.*lento"', replacement = "slow flow-type"),
      nome_movimento = str_replace_all(nome_movimento, pattern = 'col.*rapido"', replacement = "fast flow-type"),
      nome_movimento = str_replace(nome_movimento, pattern = "ribaltamento", replacement = "topple"),
      nome_movimento = str_replace(nome_movimento, pattern = "DGPV", replacement = "deep-seated movement"),
      nome_movimento = str_replace(nome_movimento, pattern = "sprofondamento", replacement = "subsidence"),
      nome_movimento = str_replace(nome_movimento, pattern = "Aree soggette a frane superficiali diffuse", replacement = "area subject to shallow-slides"),
      nome_movimento = str_replace(nome_movimento, pattern = "espansione", replacement = "area of expansion"),

    ) %>%

    # rename the values in tipologia
    mutate(

      tipologia = str_replace(tipologia, pattern="Complesso", replacement="complex"),
      tipologia = str_replace(tipologia, pattern="Aree soggette a crolli/ribaltamenti diffusi", replacement="area subject to rockfall/topple"),
      tipologia = str_replace(tipologia, pattern="Crollo/Ribaltamento", replacement="fall-type/topple"),
      tipologia = str_replace(tipologia, pattern="Scivolamento rotazionale/traslativo", replacement="translational/rotational slide"),
      tipologia = str_replace(tipologia, pattern = "Espansione", replacement = "area of expansion"),
      tipologia = str_replace(tipologia, pattern = "Aree soggette a frane superficiali diffuse", replacement = "area subject to shallow slides"),
      tipologia = str_replace(tipologia, pattern = "Colamento lento", replacement = "slow flow-type"),
      tipologia = str_replace(tipologia, pattern = "Colamento rapido", replacement = "fast flow-type"),
      tipologia = str_replace(tipologia, pattern = "DGPV", replacement = "deep-seated landslide"),
      tipologia = str_replace(tipologia, pattern = "Sprofondamento", replacement = "subsidence"),
      tipologia = str_replace(tipologia, pattern = "Aree soggette a sprofondamenti diffusi", replacement = "area subject to subsidence")
    ) %>%

    # rename the actual variables
    mutate(
      first_level = tipologia,
      second_level = nome_movimento
    )


  return(df)

}
