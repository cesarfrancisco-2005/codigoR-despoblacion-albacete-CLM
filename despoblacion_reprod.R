# ==============================================================================
# PROYECTO FINAL: Análisis Espaciotemporal de la Despoblación en Albacete
# ==============================================================================

# 1. Cargo las librerías base
library(tidyverse) 
library(sf)        
library(mapSpain)
library(ggplot2)

# 2. Cargo la cartografía
mapa_ab <- mapSpain::esp_get_munic(region = "Castilla-La Mancha")%>% 
  filter(cpro == "02")

# Compruebo
plot(st_geometry(mapa_ab), main = "Municipios de Albacete", col = "lightgrey")

# ==============================================================================
# CARGA DE LA POBLACIÓN TOTAL
# ==============================================================================
poblacion_raw <- read.csv("./33576.csv", sep = ";", fileEncoding = "latin1")

poblacion_limpia <- poblacion_raw %>%
  mutate(
    # arreglo los números quitando los sep de miles para que R no los lea como sep decimal
    Total = as.numeric(gsub("\\.", "", as.character(Total))),
    
    # Extraigo únicamente el código del municipio
    Cod_INE = substr(Municipios, 1, 5),
    
    # En vez de la fecha entera, extraigo el año únicamente
    Año = as.numeric(str_extract(Periodo, "\\d{4}"))
  ) %>%
  filter(Año >= 2012 & Año <= 2022) %>%
  select(Cod_INE, Año, Poblacion = Total)

# ==============================================================================
# CARGA, LIMPIEZA Y CÁLCULO DEL ÍNDICE DE ENVEJECIMIENTO
# ==============================================================================
edades_raw <- read.csv("./33576_edades.csv", sep = ";", fileEncoding = "latin1", check.names = FALSE)

edades_limpia <- edades_raw %>%
  # Renombro la columna de edad para que sea más fácil trabajar con ella
  rename(Edad_Texto = `Edad (grupos quinquenales)`) %>%
  
  mutate(
    Total = as.numeric(gsub("\\.", "", as.character(Total))),
    Cod_INE = substr(Municipios, 1, 5),
    Año = as.numeric(str_extract(Periodo, "\\d{4}")),
    
    Grupo_Edad = case_when(
      grepl("0 a 4|5 a 9|10 a 14", Edad_Texto) ~ "Menores_15",
      TRUE ~ "Mayores_65" 
    )
  ) %>%
  filter(Año >= 2012 & Año <= 2022) %>%
  group_by(Cod_INE, Año) %>%
  summarise(
    Pob_Joven = sum(Total[Grupo_Edad == "Menores_15"], na.rm = TRUE),
    Pob_Mayor = sum(Total[Grupo_Edad == "Mayores_65"], na.rm = TRUE),
    .groups = "drop" # Quitamos la agrupación para evitar problemas después
  ) %>%
  
  # Cálculo del Índice de Envejecimiento
  mutate(
    # Imputación del valor mínimo
    Indice_Envejecimiento = ifelse(Pob_Joven == 0, (Pob_Mayor / 1) * 100, 
                                   (Pob_Mayor / Pob_Joven) * 100)
  ) %>%
  select(Cod_INE, Año, Indice_Envejecimiento)

# ==============================================================================
# DATOS DE EMIGRACIÓN (2012 - 2020)
# ==============================================================================
emig_2012 <- read.csv("./a11_2012.csv", sep = ";", fileEncoding = "latin1") %>%
  rename(Municipio = Municipios) %>%
  mutate(Total = as.numeric(gsub("\\.", "", Total)), Año = 2012)

emig_2013 <- read.csv("./a4_11_2013.csv", sep = ";", fileEncoding = "latin1") %>%
  rename(Municipio = Municipios) %>%
  mutate(Total = as.numeric(gsub("\\.", "", Total)), Año = 2013)

emig_2014 <- read.csv("./a4_11_2014.csv", sep = ";", fileEncoding = "latin1") %>%
  mutate(Total = as.numeric(gsub("\\.", "", Total)), Año = 2014)

emig_2015 <- read.csv("./a4_11_2015.csv", sep = ";", fileEncoding = "latin1") %>%
  mutate(Total = as.numeric(gsub("\\.", "", Total)), Año = 2015)

emig_2016 <- read.csv("./a4_11_2016.csv", sep = ";", fileEncoding = "latin1") %>%
  mutate(Total = as.numeric(gsub("\\.", "", Total)), Año = 2016)

emig_2017 <- read.csv("./a4_11_2017.csv", sep = ";", fileEncoding = "latin1") %>%
  mutate(Total = as.numeric(gsub("\\.", "", Total)), Año = 2017)

emig_2018 <- read.csv("./a4_11_2018.csv", sep = ";", fileEncoding = "latin1") %>%
  mutate(Total = as.numeric(gsub("\\.", "", Total)), Año = 2018)

emig_2019 <- read.csv("./36360_2019.csv", sep = ";", fileEncoding = "latin1") %>%
  mutate(Total = as.numeric(gsub("\\.", "", Total)), Año = 2019)

emig_2020 <- read.csv("./48511_2020.csv", sep = ";", fileEncoding = "latin1") %>%
  mutate(Total = as.numeric(gsub("\\.", "", Total)), Año = 2020)

# fusionarlos
emigracion_bruta <- bind_rows(
  emig_2012, emig_2013, emig_2014, emig_2015, emig_2016, 
  emig_2017, emig_2018, emig_2019, emig_2020
)

rm(emig_2012, emig_2013, emig_2014, emig_2015, emig_2016, emig_2017, emig_2018, emig_2019, emig_2020)

# LIMPIEZA DE DESTINOS Y CÓDIGO INE
emigracion_limpia <- emigracion_bruta %>%
  mutate(
    # Unificamos los destinos rebeldes
    Destino = case_when(
      Destino %in% c("Misma comunidad autónoma, distinta provincia", 
                     "Misma comunidad autónoma, distinta provincia", 
                     "Misma COMUNIDAD AUTÓNOMA distinta provincia", 
                     "Misma CCAA distinta provincia") ~ "Misma CCAA",
      Destino %in% c("Otra CCAA", "Otra COMUNIDAD AUTÓNOMA", 
                     "Otra comunidad autónoma", "Otra comunidad autónoma") ~ "Otra CCAA",
      TRUE ~ Destino
    ),
    Cod_INE = substr(Municipio, 1, 5)
  ) %>%
  # Quito columnas que no aportan al modelo
  select(Cod_INE, Año, Destino, Total)


# PIVOTAR A FORMATO ANCHO
emigracion_final <- emigracion_limpia %>%
  pivot_wider(
    names_from = Destino, 
    values_from = Total,
    values_fill = list(Total = 0)
  ) %>%
  rename(
    Emig_Misma_Provincia = `Misma provincia`,
    Emig_Misma_CCAA = `Misma CCAA`,
    Emig_Otra_CCAA = `Otra CCAA`,
    Emig_Extranjero = Extranjero
  )

# ==============================================================================
# JOIN DEFINITIVO ESPACIOTEMPORAL
# ==============================================================================
# 1. Creo la base de datos tabular unida (uso Población como base porque tiene hasta 2022)
panel_datos <- poblacion_limpia %>%
  left_join(edades_limpia, by = c("Cod_INE", "Año")) %>%
  left_join(emigracion_final, by = c("Cod_INE", "Año"))

# 2. Uno cartografía con datos
panel_sf <- mapa_ab %>%
  # Renombro la columna LAU_CODE a Cod_INE para que las llaves coincidan
  rename(Cod_INE = LAU_CODE) %>%
  
  mutate(Cod_INE = as.character(Cod_INE)) %>%
  
  left_join(panel_datos, by = "Cod_INE") %>%
  
  arrange(Cod_INE, Año)

# ==============================================================================
# ANÁLISIS ESPACIAL: MAPA DE DESPOBLACIÓN
# ==============================================================================
# 1. CÁLCULO DE LA TASA DE CAMBIO
mapa_variacion <- panel_sf %>%
  filter(Año %in% c(2012, 2022)) %>%
  st_drop_geometry() %>%
  select(Cod_INE, name, Año, Poblacion) %>%
  pivot_wider(names_from = Año, values_from = Poblacion, names_prefix = "Pob_") %>%
  mutate(
    # Cálculo de la variación real
    Var_Real = ((Pob_2022 - Pob_2012) / Pob_2012) * 100,
    
    # Si pierde más del 40%, lo limito en -40. Si gana más del 40%, lo limito en +40.
    Var_Recortada = case_when(
      Var_Real > 40 ~ 40,
      Var_Real < -40 ~ -40,
      TRUE ~ Var_Real
    )
  ) %>%
  left_join(mapa_ab %>% rename(Cod_INE = LAU_CODE), by = "Cod_INE") %>%
  st_as_sf()

ggplot(data = mapa_variacion) +
  geom_sf(aes(fill = Var_Recortada), color = "black", size = 0.2) +
  
  scale_fill_distiller(
    palette = "RdYlBu", 
    direction = 1,
    limits = c(-40, 40),
    name = "% de Variación",
    labels = c("≤ -40%", "-20%", "0%", "+20%", "≥ +40%") 
  ) +
  
  theme_minimal() +
  labs(title = "Sangría Demográfica en Albacete",
       subtitle = "Variación de la Población por municipio (2012 - 2022)",
       caption = "Fuente: INE y elaboración propia.\n*Valores extremos topados al ±40% para mayor contraste.") +
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12, color = "darkgrey"))

# ==============================================================================
# CUADRÍCULA DE ENVEJECIMIENTO
# ==============================================================================
ggplot(data = panel_sf) +
  geom_sf(aes(fill = Indice_Envejecimiento), color = "black", size = 0.05) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Índice de\nEnvejecimiento",
    na.value = "grey90",
    
    # Topo visualmente entre 0 y 800%
    limits = c(0, 800), 
    
    # squish coge los valores >800 y los "aplasta" para que cojan el color máximo
    oob = scales::squish, 
    
    breaks = c(0, 200, 400, 600, 800),
    labels = c("0", "200", "400", "600", "≥ 800")
  ) +
  
  facet_wrap(~ Año, ncol = 4) +
  theme_minimal() +
  labs(
    title = "Envejecimiento Crónico en la Provincia de Albacete",
    subtitle = "Evolución del Índice de Envejecimiento por municipio (2012 - 2022)",
    caption = "Fuente: INE y elaboración propia.\nÍndice = (Población ≥ 65 años / Población < 15 años) * 100\n*Valores extremos topados a 800 para mejorar el contraste visual."
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "darkgrey"),
    strip.text = element_text(face = "bold", size = 11),
    axis.text = element_blank(),  
    axis.ticks = element_blank(), 
    panel.grid = element_blank()  
  )

# ==============================================================================
# CÁLCULO DEL ÍNDICE DE MORAN (2022)
# ==============================================================================
library(spdep)

datos_2022 <- panel_sf %>%
  filter(Año == 2022) %>%
  filter(!is.na(Indice_Envejecimiento)) 

vecinos <- poly2nb(datos_2022, queen = TRUE)

# MATRIZ DE PESOS ESPACIALES
pesos_espaciales <- nb2listw(vecinos, style = "W", zero.policy = TRUE)

# TEST DE MORAN
test_moran <- moran.test(datos_2022$Indice_Envejecimiento, 
                         listw = pesos_espaciales, 
                         zero.policy = TRUE)
print(test_moran)

# ==============================================================================
# MAPA DE CLÚSTERES LISA - AÑO 2022
# ==============================================================================
local_m <- localmoran(datos_2022$Indice_Envejecimiento, pesos_espaciales, zero.policy = TRUE)

z_envejecimiento <- scale(datos_2022$Indice_Envejecimiento) %>% as.vector()

lag_envejecimiento <- lag.listw(pesos_espaciales, z_envejecimiento, zero.policy = TRUE)

p_valores <- local_m[, 5] 

cuadrante <- rep("No Significativo", nrow(datos_2022))

# Uso un nivel de confianza del 95% (p-value < 0.05)
alfa <- 0.05

# Asignación lógica de los clústeres
cuadrante[z_envejecimiento > 0 & lag_envejecimiento > 0 & p_valores <= alfa] <- "Alto-Alto (Puntos Críticos)"
cuadrante[z_envejecimiento < 0 & lag_envejecimiento < 0 & p_valores <= alfa] <- "Bajo-Bajo (Zonas Dinámicas)"
cuadrante[z_envejecimiento > 0 & lag_envejecimiento < 0 & p_valores <= alfa] <- "Alto-Bajo (Atípico)"
cuadrante[z_envejecimiento < 0 & lag_envejecimiento > 0 & p_valores <= alfa] <- "Bajo-Alto (Atípico)"

datos_2022$Cluster_LISA <- factor(cuadrante, 
                                  levels = c("Alto-Alto (Puntos Críticos)", 
                                             "Bajo-Bajo (Zonas Dinámicas)", 
                                             "Alto-Bajo (Atípico)", 
                                             "Bajo-Alto (Atípico)", 
                                             "No Significativo"))

ggplot(data = datos_2022) +
  geom_sf(aes(fill = Cluster_LISA), color = "white", size = 0.2) +
  
  scale_fill_manual(
    values = c("Alto-Alto (Puntos Críticos)" = "#d7191c",  
               "Bajo-Bajo (Zonas Dinámicas)" = "#2c7bb6",  
               "Alto-Bajo (Atípico)" = "#fdae61",          
               "Bajo-Alto (Atípico)" = "#abd9e9",          
               "No Significativo" = "grey80"),             
    drop = FALSE,
    name = "Tipo de Clúster Espacial"
  ) +
  theme_minimal() +
  labs(
    title = "Mapa de Clústeres de Envejecimiento (LISA)",
    subtitle = "Identificación de puntos críticos (Hotspots) en Albacete (2022)",
    caption = "Fuente: INE y elaboración propia.\nNivel de significatividad: 95% (p < 0.05)."
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "darkgrey"),
    legend.position = "right"
  )

# ==============================================================================
# EVOLUCIÓN DE LA EMIGRACIÓN Y EL EFECTO COVID (2012-2020)
# ==============================================================================
datos_migracion <- panel_sf %>%
  st_drop_geometry() %>%
  # Filtro hasta 2020 porque es el último año con datos de emigración
  filter(Año <= 2020) %>%
  group_by(Año) %>%
  summarise(
    `Misma Provincia` = sum(Emig_Misma_Provincia, na.rm = TRUE),
    `Otra CCAA` = sum(Emig_Otra_CCAA, na.rm = TRUE),
    `Extranjero` = sum(Emig_Extranjero, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c(`Misma Provincia`, `Otra CCAA`, `Extranjero`),
    names_to = "Destino",
    values_to = "Total_Emigrantes"
  )

ggplot(data = datos_migracion, aes(x = Año, y = Total_Emigrantes, color = Destino)) +
  # Líneas y puntos para marcar cada año
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  
  # Paleta de colores clara y profesional
  scale_color_manual(values = c("Misma Provincia" = "#2c7bb6", 
                                "Otra CCAA" = "#d7191c", 
                                "Extranjero" = "#fdae61")) +
  
  # Aseguramos que el eje X muestre todos los años como números enteros
  scale_x_continuous(breaks = 2012:2020) +
  
  theme_minimal() +
  labs(
    title = "Fuga de Población: Evolución de la Emigración en Albacete",
    subtitle = "Flujos migratorios por lugar de destino (2012 - 2020)",
    x = "Año",
    y = "Número total de emigrantes",
    caption = "Fuente: INE y elaboración propia."
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "darkgrey"),
    legend.position = "bottom",
    legend.title = element_blank()
  )

# ==============================================================================
# ANÁLISIS MULTIVARIANTE: ENVEJECIMIENTO vs EMIGRACIÓN vs POBLACIÓN
# ==============================================================================
datos_burbujas <- panel_sf %>%
  st_drop_geometry() %>% 
  filter(Año == 2020) %>%
  mutate(
    Total_Emigrantes = Emig_Misma_Provincia + Emig_Misma_CCAA + Emig_Otra_CCAA + Emig_Extranjero,
    Tasa_Emigracion = (Total_Emigrantes / Poblacion) * 1000
  ) %>%
  filter(!is.na(Indice_Envejecimiento) & !is.na(Tasa_Emigracion) & Tasa_Emigracion < 1000)

ggplot(data = datos_burbujas, aes(x = Indice_Envejecimiento, y = Tasa_Emigracion)) +
  
  # Burbujas: Tamaño = Población, Color = Población
  geom_point(aes(size = Poblacion, color = Poblacion), alpha = 0.7) +
  
  # Tamaños mínimo y máximo
  scale_size_continuous(range = c(2, 20), guide = "none") + 
  
  scale_color_viridis_c(option = "magma", direction = -1, trans = "log10", guide = "none") +
  
  geom_text(
    data = datos_burbujas %>% filter(Poblacion > 10000 | Indice_Envejecimiento > 600 | Tasa_Emigracion > 50),
    aes(label = name),
    size = 2.9, 
    fontface = "bold", 
    color = "black",
    vjust = -1.2, # Sube el texto un poco por encima de la burbuja
    check_overlap = TRUE # ¡El truco mágico para no usar ggrepel!
  ) +
  
  theme_minimal() +
  labs(
    title = "Radiografía Multivariante de la Despoblación en Albacete (2020)",
    subtitle = "Relación entre Envejecimiento y Tasa de Fuga. El tamaño representa la Población.",
    x = "Índice de Envejecimiento",
    y = "Tasa de Emigración",
    caption = "Fuente: INE y elaboración propia."
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 12, color = "darkgrey"),
    panel.grid.minor = element_blank()
  )

# ==============================================================================
# FASE 2: SPATIAL MACHINE LEARNING (ENTRENAMIENTO EN ALBACETE)
# ==============================================================================
library(dplyr)
library(tidyr)
library(tidymodels)
library(spatialsample)
library(ranger)
library(waywiser)
library(vip)

# 1. PREPARACIÓN DE DATOS (YA CON TASAS E IMPUTACIÓN DE JÓVENES)
suppressWarnings({
  datos_sml <- panel_sf %>%
    dplyr::filter(Año == 2020) %>%
    mutate(
      # Coordenadas espaciales
      X = sf::st_coordinates(sf::st_centroid(geometry))[,1],
      Y = sf::st_coordinates(sf::st_centroid(geometry))[,2],
      
      # Tasas relativas (por cada 1.000 hab.)
      Tasa_Emig_Provincia = (Emig_Misma_Provincia / Poblacion) * 1000,
      Tasa_Emig_CCAA = (Emig_Otra_CCAA / Poblacion) * 1000,
      Tasa_Emig_Extranjero = (Emig_Extranjero / Poblacion) * 1000
    ) %>%
    dplyr::select(Cod_INE, name, Poblacion, Indice_Envejecimiento, 
                  Tasa_Emig_Provincia, Tasa_Emig_CCAA, Tasa_Emig_Extranjero, X, Y) %>%
    na.omit()
})

# 2. DEFINICIÓN DEL FLUJO DE TRABAJO (TIDYMODELS)
set.seed(2026) 

# Receta apuntando a las tasas relativas
rf_recipe <- recipes::recipe(Poblacion ~ Indice_Envejecimiento + Tasa_Emig_Provincia + 
                               Tasa_Emig_CCAA + Tasa_Emig_Extranjero + X + Y, 
                             data = datos_sml)

rf_model <- parsnip::rand_forest(trees = 100, mtry = 3, min_n = 5, mode = "regression") %>%
  parsnip::set_engine("ranger", importance = "impurity")

rf_workflow <- workflows::workflow() %>%
  workflows::add_recipe(rf_recipe) %>%
  workflows::add_model(rf_model)

# 3. VALIDACIÓN CRUZADA ESPACIAL (EN ALBACETE)
spatial_folds <- spatialsample::spatial_block_cv(datos_sml, v = 5)

rf_resamples <- tune::fit_resamples(
  rf_workflow,
  resamples = spatial_folds,
  control = tune::control_resamples(save_pred = TRUE)
)

cat("\n--- MÉTRICAS DEL MODELO ESPACIAL EN ALBACETE (RMSE y R2) ---\n")
print(tune::collect_metrics(rf_resamples))

# 4. MODELO FINAL (Ajuste sobre todo Albacete)
rf_final <- parsnip::fit(rf_workflow, data = datos_sml)

# 5. ENTRENAMIENTO DEL MODELO DE FIABILIDAD (AoA)
vars_entrenamiento <- datos_sml %>%
  sf::st_drop_geometry() %>%
  dplyr::select(Indice_Envejecimiento, Tasa_Emig_Provincia, Tasa_Emig_CCAA, Tasa_Emig_Extranjero, X, Y)

modelo_aoa <- waywiser::ww_area_of_applicability(
  x = vars_entrenamiento, 
  importance = vip::vi_model(parsnip::extract_fit_engine(rf_final))
)

# ==============================================================================
# EXTRAPOLACIÓN A CASTILLA-LA MANCHA
# ==============================================================================
# ------------------------------------------------------------------------------
# 1. LECTURA Y FUSIÓN DE LOS DATOS BRUTOS
# ------------------------------------------------------------------------------
# Leemos las poblaciones (Asegúrate de que sep = ";" es el correcto en tus CSV)
pob_cr  <- read.csv2("33776_CR_20.csv", fileEncoding = "latin1")
pob_cu  <- read.csv2("33758_CU_20.csv", fileEncoding = "latin1")
pob_gu  <- read.csv2("33800_GU_20.csv", fileEncoding = "latin1")
pob_to  <- read.csv2("33938_TO_20.csv", fileEncoding = "latin1")

# Unimos las 4 provincias de población
poblacion_bruta <- dplyr::bind_rows(pob_cr, pob_cu, pob_gu, pob_to)

# Leemos las migraciones
mig_cr  <- read.csv2("./48511_migr_CR.csv", fileEncoding = "latin1")
mig_cu  <- read.csv2("./48511_migr_CU.csv", fileEncoding = "latin1")
mig_gu  <- read.csv2("./48511_migr_GU.csv", fileEncoding = "latin1")
mig_to  <- read.csv2("./48511_migr_TO.csv", fileEncoding = "latin1")

# Unimos las 4 provincias de migración
migracion_bruta <- dplyr::bind_rows(mig_cr, mig_cu, mig_gu, mig_to)

# ------------------------------------------------------------------------------
# 2. LIMPIEZA Y CÁLCULO DE VARIABLES (AL ESTILO DEL PANEL ORIGINAL)
# ------------------------------------------------------------------------------
# A. Limpiamos Población y calculamos Envejecimiento (Estructura real confirmada)
# A. Limpiamos Población y calculamos Envejecimiento (BLOQUE DEFINITIVO Y BLINDADO)
poblacion_clm <- poblacion_bruta %>%
  dplyr::rename(
    Nombre_Municipio = 3, 
    Edad = 4, 
    Total_Bruto = 6 # Lo llamamos bruto para no pisar el nombre
  ) %>% 
  dplyr::mutate(
    Cod_INE = stringr::str_sub(Nombre_Municipio, 1, 5),
    
    # EL TRUCO ESTÁ AQUÍ: 
    # 1º Quitamos el punto del texto. 2º Lo pasamos a número.
    Total_Limpio = as.numeric(gsub("\\.", "", Total_Bruto))
  ) %>%
  dplyr::filter(!is.na(Total_Limpio)) %>%
  
  dplyr::group_by(Cod_INE) %>%
  dplyr::summarise(
    # Sumamos ahora que los números son 100% reales
    Poblacion = sum(Total_Limpio[Edad == "Todas las edades"], na.rm = TRUE),
    
    Jovenes = sum(Total_Limpio[Edad %in% c("De 0 a 4 años", "De 5 a 9 años", "De 10 a 14 años")], na.rm = TRUE),
    
    Mayores = sum(Total_Limpio[Edad %in% c("De 65 a 69 años", "De 70 a 74 años", "De 75 a 79 años", 
                                           "De 80 a 84 años", "De 85 a 89 años", "De 90 a 94 años", 
                                           "De 95 a 99 años", "100 y más años")], na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    # Calculamos el índice
    Indice_Envejecimiento = (Mayores / Jovenes) * 100,
    Indice_Envejecimiento = ifelse(is.infinite(Indice_Envejecimiento), NA, Indice_Envejecimiento)
  )

# B. Limpiamos Migraciones
migracion_clm <- migracion_bruta %>%
  dplyr::rename(Nombre_Municipio = Municipio) %>%
  dplyr::mutate(
    Cod_INE = stringr::str_sub(Nombre_Municipio, 1, 5),
    # Limpiamos posibles puntos de miles y pasamos a numérico
    Total_Limpio = as.numeric(gsub("\\.", "", Total)) 
  ) %>%
  dplyr::group_by(Cod_INE) %>%
  dplyr::summarise(
    Emig_Misma_Provincia = sum(Total_Limpio[Destino == "Misma provincia"], na.rm = TRUE),
    # Agrupamos toda la emigración fuera de la provincia (a otra CCAA y a otra provincia de CLM)
    Emig_Otra_CCAA = sum(Total_Limpio[Destino %in% c("Otra comunidad autónoma", "Misma comunidad autónoma, distinta provincia")], na.rm = TRUE),
    Emig_Extranjero = sum(Total_Limpio[Destino == "Extranjero"], na.rm = TRUE)
  ) %>%
  dplyr::ungroup()

# C. Unimos todo en un solo dataset regional
datos_clm_estadistica <- poblacion_clm %>%
  dplyr::inner_join(migracion_clm, by = "Cod_INE")

# ==============================================================================
# PREDICCIÓN ESPACIAL PARA TODA CASTILLA-LA MANCHA
# ==============================================================================
# ------------------------------------------------------------------------------
# 1. GEOMETRÍA Y COORDENADAS
# ------------------------------------------------------------------------------
# Descargamos el mapa de toda Castilla-La Mancha
mapa_clm <- mapSpain::esp_get_munic(region = "Castilla-La Mancha") %>%
  # Fabricamos el Cod_INE pegando la provincia y el municipio
  dplyr::mutate(Cod_INE = paste0(cpro, cmun))

# ==============================================================================
# APLICACIÓN DE TASAS Y CORRECCIÓN DE INFINITOS
# ==============================================================================
datos_clm_tasas <- poblacion_clm %>%
  dplyr::inner_join(migracion_clm, by = "Cod_INE") %>%
  dplyr::mutate(
    # IMPUTACIÓN DE LOS JÓVENES:
    Jovenes_Fix = ifelse(Jovenes == 0, 1, Jovenes),
    Indice_Envejecimiento = (Mayores / Jovenes_Fix) * 100,
    
    # TASAS: Emigrantes por cada 1.000 habitantes
    Tasa_Emig_Provincia = (Emig_Misma_Provincia / Poblacion) * 1000,
    Tasa_Emig_CCAA = (Emig_Otra_CCAA / Poblacion) * 1000,
    Tasa_Emig_Extranjero = (Emig_Extranjero / Poblacion) * 1000
  ) %>%
  dplyr::select(Cod_INE, Poblacion, Indice_Envejecimiento, Tasa_Emig_Provincia, Tasa_Emig_CCAA, Tasa_Emig_Extranjero)

# Cruzamos con el mapa de toda CLM y calculamos coordenadas
clm_sf_tasas <- mapa_clm %>%
  dplyr::inner_join(datos_clm_tasas, by = "Cod_INE") %>%
  dplyr::mutate(
    X = sf::st_coordinates(sf::st_centroid(geometry))[,1],
    Y = sf::st_coordinates(sf::st_centroid(geometry))[,2]
  ) %>%
  na.omit() # Ahora SÍ conservará todos los pueblos porque ya no hay Infinitos

# Lanzamos las predicciones
clm_prediccion <- stats::predict(rf_final, new_data = clm_sf_tasas)
clm_aoa <- stats::predict(modelo_aoa, new_data = clm_sf_tasas %>% sf::st_drop_geometry())

# Averiguamos cómo ha llamado waywiser a la columna (.aoa)
col_name_aoa <- names(clm_aoa)[1]

# Juntamos todo apuntando EXACTAMENTE a la columna .aoa
mapa_final_tasas <- clm_sf_tasas %>%
  dplyr::bind_cols(clm_prediccion) %>%
  dplyr::bind_cols(clm_aoa) %>%
  # Forzamos que sea factor usando el nombre directo
  dplyr::mutate(Categoria_AoA = as.factor(aoa))

# ==============================================================================
# 3. LOS DOS MAPAS MAESTROS
# ==============================================================================

# MAPA 1: PREDICCIÓN CON TASAS (Cero sesgos por tamaño)
mapa_pred <- ggplot(mapa_final_tasas) +
  geom_sf(aes(fill = .pred), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "magma", direction = -1, trans = "log10", 
    labels = scales::comma, name = "Población\nPredicha"
  ) +
  theme_minimal() +
  labs(
    title = "Población Predicha en CLM (Modelo de Tasas Relativas)",
    subtitle = "Extrapolación neutralizada frente al volumen demográfico absoluto",
    caption = "Elaboración propia."
  ) +
  theme(plot.title = element_text(face = "bold", size = 14))

# MAPA 2: ÁREA DE APLICABILIDAD (Con todos los municipios)
mapa_fiabilidad <- ggplot(mapa_final_tasas) +
  geom_sf(aes(fill = Categoria_AoA), color = "white", size = 0.1) +
  scale_fill_manual(
    # Blindamos todas las opciones posibles que arroje la librería
    values = c("TRUE" = "#21908C", "FALSE" = "#440154", 
               "1" = "#21908C", "0" = "#440154",
               "inside" = "#21908C", "outside" = "#440154"), 
    name = "Dominio (AoA)"
  ) +
  theme_minimal() +
  labs(
    title = "Fiabilidad Espacial (Área de Aplicabilidad)",
    subtitle = "Zonas aplicables (Verde) vs. Zonas demográficamente anómalas (Morado)",
    caption = "Elaboración propia."
  ) +
  theme(plot.title = element_text(face = "bold", size = 14))

# Mostramos los mapas (ejecutar uno por uno para verlos en la pestaña Plots)
print(mapa_pred)
print(mapa_fiabilidad)
