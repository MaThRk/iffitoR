#' function to translate some of the column to english
#'
#' @export
#'
translate_iffi = function(df) {

  # do some checking
  if(!"anno_min|mese_min|giorno_min|tipologia|nome_movimento" %in% names(df)){
    stop("some of the necessary columns are'n present in the dataframe provided, ",
         "Did you pass the dataframe directly to the translate-funtion after you created it with make_shapefile?")
  }


  df = df %>%

    # rename the values in nome_movimento
    mutate(
      nome_movimento = str_replace(nome_movimento, pattern = "[Cc]omplesso", replacement = "complex"),
      nome_movimento = str_replace(nome_movimento, pattern = "Aree soggette a crolli/ribaltamenti diffusi", replacement = "area of rockfall/topple"),
      nome_movimento = str_replace(nome_movimento, pattern = "crollo", replacement = "rockfall"),
      nome_movimento = str_replace(nome_movimento, pattern = "scivolamento traslativo", replacement = "translational"),
      nome_movimento = str_replace(nome_movimento, pattern = "scivolamento rotazionale", replacement = "rotational"),
      nome_movimento = str_replace(nome_movimento, pattern = "Aree soggette a sprofondamenti diffusi", replacement = "areas prone to sinking"),
      nome_movimento = str_replace(nome_movimento, pattern = 'col.*lento"', replacement = "slow flow"),
      nome_movimento = str_replace_all(nome_movimento, pattern = 'col.*rapido"', replacement = "fast flow"),
      nome_movimento = str_replace(nome_movimento, pattern = "ribalmento", replacement = "overturn"),
      nome_movimento = str_replace(nome_movimento, pattern = "DGPV", replacement = "deep seated landslide"),
      nome_movimento = str_replace(nome_movimento, pattern = "sprofondamento", replacement = "subsidence"),
      nome_movimento = str_replace(nome_movimento, pattern = "Aree soggette a frane superficiali diffuse", replacement = "area of diffuse surface slides"),
      nome_movimento = str_replace(nome_movimento, pattern = "espansione", replacement = "expansion"),

    ) %>%

    # rename the values in tipologia
    mutate(

      tipologia = str_replace(tipologia, pattern="Complesso", replacement="complex"),
      tipologia = str_replace(tipologia, pattern="Aree soggette a crolli/ribaltamenti diffusi", replacement="area of rockfall/topple"),
      tipologia = str_replace(tipologia, pattern="Crollo/Ribaltamento", replacement="rockfall"),
      tipologia = str_replace(tipologia, pattern="Scivolamento rotazionale/traslativo", replacement="translational/rotational slide"),
      tipologia = str_replace(tipologia, pattern = "Espansione", replacement = "expansion"),
      tipologia = str_replace(tipologia, pattern = "Aree soggette a frane superficiali diffuse", replacement = "area of diffuse surface slides"),
      tipologia = str_replace(tipologia, pattern = "Aree soggette a frane superficiali diffuse", replacement = "area of diffuse surface slides"),
      tipologia = str_replace(tipologia, pattern = "Colamento lento", replacement = "flow (slow)"),
      tipologia = str_replace(tipologia, pattern = "Colamento rapido", replacement = "flow (fast)"),
      tipologia = str_replace(tipologia, pattern = "DGPV", replacement = "deep seated landslide"),
      tipologia = str_replace(tipologia, pattern = "Sprofondamento", replacement = "subsidence"),
      tipologia = str_replace(tipologia, pattern = "Aree soggette a sprofondamenti diffusi", replacement = "area of diffuse subsidence")
    ) %>%

    # rename the actual variables
    mutate(
      first_level = tipologia,
      second_level = nome_movimento
    )


  return(df)

}
