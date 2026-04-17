TRY_Mapa <- map_DIST



for (i in seq_along(full_years)) {
  #year_full <- full_years[i]
  #year_short <- short_years[i]
  
  # Nombre del documento usando paste0 [concatenar] y leer archivo con el nombre correspondiente
  file_name <- paste0(municipal_transfer_path, full_years[i], " Gobiernos Locales.xls")
  df <- read_xls(file_name)
  
  #Asignar nombres para cada data frame y añadirla columna de año
  names(df)[1:4] <- c("code", "name", "authorised", "credited")
  df <- cbind(df[, 1:2], year = full_years[i], df[, 3:ncol(df)])
  df[["authorised"]] <- as.numeric(gsub("[^0-9.-]", "", df[["authorised"]])) / 1000000
  df[["credited"]]   <- as.numeric(gsub("[^0-9.-]", "", df[["credited"]])) / 1000000
  
  # Asignar nuevo nombre al data frame
  assign(paste0("TRY_", short_years[i]), df)
}


##Limpiar filas
for (y in sprintf("%02d", 4:23)) assign(paste0("TRY_", y),
                                        get(paste0("TRY_", y))[-c(1:11), ])

##Bind all df in one
TRY <- do.call(rbind, lapply(short_years, function(y) {
  get(paste0("TRY_", y))
}))

##Remove Transferencias_Regionales_XX
df_remove <- paste0("TRY_", sprintf("%02d", 4:23))
rm(list = df_remove)

summary(TRY)


TRY <- TRY %>%
  mutate(
    name = str_remove(name, "^MUN\\. PRO\\. DE "),
    name = str_remove(name, "^MUN\\. PRO\\. DEL "),
    name = str_remove(name, "^MUN\\. PRO\\. "),
    name = str_remove(name, "^MUNICIPALIDAD PROVINCIAL DEL "),
    name = str_remove(name, "^MUNICIPALIDAD PROVINCIAL DE "),
    name = str_remove(name, "^MUNICIPALIDAD PROVINCIAL "),
    name = str_replace(name, "^MUNICIPALIDAD METROPOLITANA DE LIMA$", "LIMA METROPOLITANA"),
                      name = str_remove(name, "^MUN\\. DIS\\. DE "),
                      name = str_remove(name, "^MUN\\. DIS\\. "),
                      name = str_remove(name, "^MUN\\. DIST\\. "),
                      name = str_remove(name, "^MUNICIPALIDAD DISTRITAL DE "),
                      name = str_remove(name, "^MUNICIPALIDAD DISTRITAL "),
                      name = str_remove(name, "^ DE "),
                      name = str_remove(name, "^ "),
                      name = str_remove(name, "\\s*-.*"))



TRY <- TRY %>% 
  mutate(name = case_when(
    name == "NAZCA" ~ "NASCA",
    name == "CAÑETE" ~ "CANETE",
    name == "SANCHEZ CERRO" ~ "GENERAL SANCHEZ CERRO",
    name == "DANIEL CARRION" ~ "DANIEL ALCIDES CARRION",
    name == "CUZCO" ~ "CUSCO",
    name == "QUISPICANCHIS" ~ "QUISPICANCHI",
    name == "MARAÑON" ~ "MARANON",
    name == "FERREÑAFE" ~ "FERRENAFE",
    name == "DANIEL A. CARRION" ~ "DANIEL ALCIDES CARRION",
    name == "DATEM DEL MARAÑON" ~ "DATEM DEL MARANON",
    TRUE ~ name
  ))


TRY <- TRY %>%
  mutate(
    name = str_remove(name, "\\s*\\([^\\)]+\\)"),
    name = stri_trans_general(name, "Latin-ASCII"),
  )


TRY <- TRY %>% 
  mutate(name = case_when(
    name == "SAN ISIDRO DE MAYNO" ~ "SAN ISIDRO DE MAINO",
    name == "IMASA" ~ "IMAZA",
    name == "PIZUQUIA" ~ "PISUQUIA",
    name == "SAN CRISTOBAL DE OLTO" ~ "SAN CRISTOBAL",
    name == "SAN JERONIMO DE PACLAS" ~ "SAN JERONIMO",
    name == "ABELARDO PARDO LAZAMETA" ~ "ABELARDO PARDO LEZAMETA",
    name == "MATACOTA" ~ "MATACOTO",
    name == "SAN MIGUEL CHACCRAMPA" ~ "SAN MIGUEL DE CHACCRAMPA",
    name == "HUAYLLO" ~ "HUAYO",
    name == "QUEQUE¥A" ~ "QUEQUENA",
    name == "HUAC" ~ "HUAC HUAS",
    name == "SAN FRANCISCO DE RAVACAYCO" ~ "SAN FRANCISCO DE RIVACAYCO",
    name == "HUAYA" ~ "HUALLA",
    name == "LOS BA¥OS DEL INCA" ~ "LOS BANOS DEL INCA",
    name == "ACOSTAMBAO" ~ "ACOSTAMBO",
    name == "DANIEL HERNANDES" ~ "DANIEL HERNANDEZ",
    name == "YAHUIMPUQUIO" ~ "NAHUIMPUQUIO",
    name == "SAN MARCOS ROCCHAC" ~ "SAN MARCOS DE ROCCHAC",
    name == "TINGUINA" ~ "LA TINGUINA",
    name == "LEONOR ORDO¥EZ" ~ "LEONOR ORDONEZ",
    name == "PAMPA HERMOZA" ~ "PAMPA HERMOSA",
    name == "HUAY" ~ "HUAY HUAY",
    name == "AYAUCA" ~ "ALLAUCA",
    name == "VILCHAYAL" ~ "VICHAYAL",
    name == "CAPASO" ~ "CAPAZO",
    name == "HUANAHUARA" ~ "HUANUARA",
    name == "CHUCATAMANI" ~ "HEROES ALBARRACIN",
    name == "QUIMBIRI" ~ "KIMBIRI",
    name == "CRESPO Y CASTILLO" ~ "JOSE CRESPO Y CASTILLO",
    name == "CODO DE POZUZO" ~ "CODO DEL POZUZO",
    name == "CHUPAN" ~ "APARICIO POMARES",
    name == "SAN FRANCISCO DEL YESO" ~ "SAN FRANCISCO DE YESO",
    name == "MACHAHUAY" ~ "MACHAGUAY",
    name == "PACAPAUZA" ~ "PACAPAUSA",
    name == "CCORCCA" ~ "CCORCA",
    name == "PACCARECTAMBO" ~ "PACCARICTAMBO",
    name == "KCOSNIPATA" ~ "KOSNIPATA",
    name == "KICHKI" ~ "QUISQUI",
    name == "ZANA" ~ "SANA",
    name == "KANARIS" ~ "CANARIS",
    name == "FUNDICION DE TINYAHUARCO" ~ "TINYAHUARCO",
    name == "HUATTA" ~ "HUATA",
    name == "SAN JOSE DE LOS CHORRILLOS" ~ "CUENCA",
    name == "TAPAYRIHUA" ~ "TAPAIRIHUA",
    name == "PACAICASA" ~ "PACAYCASA",
    name == "ECHARATI" ~ "ECHARATE",
    name == "ANDRES AVELINO CACERES DORREGAY" ~ "ANDRES AVELINO CACERES DORREGARAY",
    name == "EL ARENAL" ~ "ARENAL",
    name == "JALCA GRANDE" ~ "LA JALCA",
    name == "CASTA" ~ "SAN PEDRO DE CASTA",
    name == "SAN PEDRO DE LARAOS" ~ "LARAOS",
    name == "PEVAS" ~ "PEBAS",
    name == "PACCARICTAMBO" ~ "PACCARITAMBO",
    TRUE ~ name
  ))



UNIQUE <- unique(TRY$name)



TRY_Mapa <- TRY_Mapa %>% 
  mutate(
    DISTRITO = str_replace_all(DISTRITO, "_", " "),
    DISTRITO = str_replace_all(DISTRITO, "-", " ")
  )

TRY_Mapa <- TRY_Mapa %>%
  mutate(DISTRITO_UPPER = toupper(DISTRITO))

TRY_Mapa <- TRY_Mapa %>%
  mutate(
    DISTRITO_UPPER = str_remove(DISTRITO_UPPER, "\\s*\\([^\\)]+\\)"),
    DISTRITO_UPPER = stri_trans_general(DISTRITO_UPPER, "Latin-ASCII"),
  )

TRY_Mapa <- TRY_Mapa %>%
  mutate(
    Mineria = ifelse(DISTRITO_UPPER %in% UNIQUE, "Mining District", "Non-mining District"))

TRY_Mapa <- st_as_sf(TRY_Mapa)

ggplot(TRY_Mapa) +
  geom_sf(aes(fill = Mineria), color = "white") +
  geom_sf(data = TRY_Mapa, fill = NA, color = "black", size = 0.3) +
  scale_fill_manual(values = c("Mining District" = "goldenrod", "Non-mining District" = "gray80")) +
  labs(title = "Mining vs. Non-Mining Districts in Peru", fill = "") +
  theme_minimal()

setdiff(TRY_Mapa$DISTRITO_UPPER, UNIQUE)



length(setdiff(Mapa_Distrito$DISTRITO_UPPER, UNIQUE))
#Si usamos los distritos y provincias juntos suponiendo que mun. prov. solo hace
#referencia a la capital y no a la provincia en total. Existen tan solo 65 distritos
#no mineros.

TRY_Full <- TRY %>% 
  group_by(name) %>% 
  summarise(presente = n_distinct(year)) %>% 
  filter(presente == length(full_years))

#El grupo de comparación aumenta a 444 al observar solo las observaciones que
#aparecen durante todo el periodo trabajado usando dist y prov en conjunto
length(setdiff(Mapa_Distrito$DISTRITO_UPPER, TRY_Full$name))

#Usando dist y prov control vs treatment por año:
TRY_Control <- sapply(full_years, function(t){
  TRY_by_year <- TRY %>%
    filter(year == t) %>%
    pull(name) %>%
    unique()
  length(setdiff(TRY_Mapa$DISTRITO_UPPER, TRY_by_year))
})

tibble(
  year = full_years,
  control = TRY_Control
)
