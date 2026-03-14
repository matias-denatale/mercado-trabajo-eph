# ==============================================================================
# calcular_indicadores.R
# Descarga y procesamiento de microdatos EPH para el sitio de mercado de trabajo
#
# Diseño de registro: post-4T2023 (EPH_registro_3T2025)
# Fuente: package eph (CRAN), get_microdata()
#
# Indicadores calculados:
#   1. Tasa de empleo       (ocupados / PET, base PONDERA)
#   2. Tasa de desempleo    (desocupados / PEA, base PONDERA)
#   3. Tasa de informalidad (EMPLEO == 2 entre asalariados + indep, desde 4T2023)
#
# NOTA: La variable EMPLEO fue incorporada a partir del 4T2023 junto con SECTOR.
# Para períodos anteriores la serie de informalidad no está disponible con esta
# definición. Se documenta esta limitación en metodologia.qmd.
# ==============================================================================

library(eph)
library(dplyr)
library(purrr)
library(tidyr)

# ------------------------------------------------------------------------------
# 1. Definir períodos a descargar
# ------------------------------------------------------------------------------

# Todos los trimestres disponibles: 2T2016 a 3T2025
periodos <- expand.grid(
  anio     = 2016:2025,
  trimestre = 1:4
) |>
  arrange(anio, trimestre) |>
  # Excluir trimestres futuros, no publicados y el 1T2016 que no existe
  filter(!(anio == 2025 & trimestre == 4),
         !(anio == 2016 & trimestre == 1))

# ------------------------------------------------------------------------------
# 2. Función de descarga con caché en disco
# ------------------------------------------------------------------------------

descargar_trimestre <- function(anio, trimestre,
                                dir_cache = "data/cache") {
  dir.create(dir_cache, showWarnings = FALSE, recursive = TRUE)
  ruta <- file.path(dir_cache,
                    paste0("individual_", anio, "T", trimestre, ".rds"))

  if (file.exists(ruta)) {
    message(glue::glue("  [caché] {anio} T{trimestre}"))
    return(readRDS(ruta))
  }

  message(glue::glue("  [descarga] {anio} T{trimestre}"))

  # Variables base: disponibles en todos los trimestres
  vars_base <- c("ANO4", "TRIMESTRE", "PONDERA", "ESTADO", "CAT_OCUP", "INTENSI", "PP03J", "PP03G")

  # Variables de informalidad: sólo existen desde 4T2023
  es_post_cambio <- anio > 2023 | (anio == 2023 & trimestre >= 4)
  vars_pedir <- if (es_post_cambio) c(vars_base, "EMPLEO", "SECTOR") else vars_base

  base <- tryCatch(
    get_microdata(
      year   = anio,
      period = trimestre,
      type   = "individual",
      vars   = vars_pedir
    ),
    error = function(e) {
      warning(glue::glue("Error en {anio} T{trimestre}: {e$message}"))
      NULL
    }
  )

  # Descartar bases vacías o sin columnas esperadas
  cols_minimas <- c("ANO4", "TRIMESTRE", "PONDERA", "ESTADO", "INTENSI", "PP03J", "PP03G")
  if (!is.null(base) && nrow(base) > 0 && all(cols_minimas %in% names(base))) {
    saveRDS(base, ruta)
    return(base)
  }

  warning(glue::glue("Base descartada (vacía o incompleta): {anio} T{trimestre}"))
  NULL
}

# ------------------------------------------------------------------------------
# 3. Descarga de todos los trimestres
# ------------------------------------------------------------------------------

# Limpiar archivos de caché vacíos o corruptos de corridas anteriores
if (dir.exists("data/cache")) {
  cache_files <- list.files("data/cache", pattern = "\\.rds$", full.names = TRUE)
  cols_minimas <- c("ANO4", "TRIMESTRE", "PONDERA", "ESTADO", "INTENSI", "PP03J", "PP03G")
  for (f in cache_files) {
    obj <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(obj) || !all(cols_minimas %in% names(obj))) {
      message(glue::glue("  [limpieza caché] eliminando antiguo o incompleto: {basename(f)}"))
      file.remove(f)
    }
  }
}

message("Descargando microdatos EPH...")
lista_bases <- map2(
  periodos$anio,
  periodos$trimestre,
  descargar_trimestre
)

# Descartar trimestres con error
lista_bases <- lista_bases[!sapply(lista_bases, is.null)]
message(glue::glue("Bases cargadas: {length(lista_bases)} trimestres"))

# ------------------------------------------------------------------------------
# 4. Calcular indicadores por trimestre
# ------------------------------------------------------------------------------

calcular_tasas <- function(base) {

  anio      <- unique(base$ANO4)
  trimestre <- unique(base$TRIMESTRE)

  # --- Población de referencia ---
  # PET: personas de 10 y más años (ESTADO != 4)
  # PEA: ocupados + desocupados (ESTADO %in% 1:2)
  # Ocupados: ESTADO == 1
  # Desocupados: ESTADO == 2

  base_activa <- base |>
    filter(ESTADO %in% 1:4)

  PET  <- sum(base_activa$PONDERA[base_activa$ESTADO != 4], na.rm = TRUE)
  PEA  <- sum(base_activa$PONDERA[base_activa$ESTADO %in% 1:2], na.rm = TRUE)
  ocup <- sum(base_activa$PONDERA[base_activa$ESTADO == 1], na.rm = TRUE)
  desocup <- sum(base_activa$PONDERA[base_activa$ESTADO == 2], na.rm = TRUE)

  tasa_empleo    <- if (PET > 0) ocup / PET else NA_real_
  tasa_desempleo <- if (PEA > 0) desocup / PEA else NA_real_

  tasa_actividad <- if (PET > 0) PEA / PET else NA_real_
  # A. Desocupados abiertos
  desoc_abiertos <- desocup
  # B. Ocupados demandantes
  ocup_dem <- sum(base_activa$PONDERA[base_activa$ESTADO == 1 & base_activa$PP03J == 1], na.rm = TRUE)
  subocup_dem <- sum(base_activa$PONDERA[base_activa$ESTADO == 1 & base_activa$PP03J == 1 & base_activa$INTENSI == 1], na.rm = TRUE)
  otros_ocup_dem <- sum(base_activa$PONDERA[base_activa$ESTADO == 1 & base_activa$PP03J == 1 & base_activa$INTENSI != 1], na.rm = TRUE)
  # C. Ocupados no demandantes disponibles
  subocup_nodem <- sum(base_activa$PONDERA[base_activa$ESTADO == 1 & base_activa$PP03J == 2 & base_activa$INTENSI == 1], na.rm = TRUE)
  otros_ocup_disp <- sum(base_activa$PONDERA[base_activa$ESTADO == 1 & base_activa$PP03J == 2 & base_activa$INTENSI != 1 & base_activa$PP03G == 1], na.rm = TRUE)
  ocup_nodem_disp <- subocup_nodem + otros_ocup_disp
  # D. Ocupados no demandantes ni disponibles
  ocup_nodem_nodisp <- sum(base_activa$PONDERA[base_activa$ESTADO == 1 & base_activa$PP03J == 2 & (base_activa$INTENSI != 1 & base_activa$PP03G != 1 | is.na(base_activa$PP03G))], na.rm = TRUE)

  # --- Informalidad (sólo desde 4T2023) ---
  # EMPLEO: 1 = Formal, 2 = Informal, 9 = Ns/Nr
  # Base: ocupados (ESTADO == 1) con EMPLEO válido (1 o 2)
  tiene_empleo <- anio > 2023 | (anio == 2023 & trimestre >= 4)

  tasa_informalidad <- if (tiene_empleo && "EMPLEO" %in% names(base)) {
    base_ocup_inf <- base_activa |>
      filter(ESTADO == 1, EMPLEO %in% 1:2)
    inform <- sum(base_ocup_inf$PONDERA[base_ocup_inf$EMPLEO == 2], na.rm = TRUE)
    total  <- sum(base_ocup_inf$PONDERA, na.rm = TRUE)
    if (total > 0) inform / total else NA_real_
  } else {
    NA_real_
  }

  tibble(
    anio              = anio,
    trimestre         = trimestre,
    periodo           = paste0(anio, "-T", trimestre),
    tasa_actividad    = round(tasa_actividad * 100, 1),
    tasa_empleo       = round(tasa_empleo * 100, 1),
    tasa_desempleo    = round(tasa_desempleo * 100, 1),
    tasa_informalidad = round(tasa_informalidad * 100, 1),
    tasa_desoc_abiertos       = if(PEA>0) round((desoc_abiertos / PEA) * 100, 1) else NA_real_,
    tasa_ocup_demandantes     = if(PEA>0) round((ocup_dem / PEA) * 100, 1) else NA_real_,
    tasa_subocup_demandantes  = if(PEA>0) round((subocup_dem / PEA) * 100, 1) else NA_real_,
    tasa_otros_ocup_demandantes= if(PEA>0) round((otros_ocup_dem / PEA) * 100, 1) else NA_real_,
    tasa_ocup_nodem_disp      = if(PEA>0) round((ocup_nodem_disp / PEA) * 100, 1) else NA_real_,
    tasa_subocup_nodem        = if(PEA>0) round((subocup_nodem / PEA) * 100, 1) else NA_real_,
    tasa_otros_ocup_disp      = if(PEA>0) round((otros_ocup_disp / PEA) * 100, 1) else NA_real_,
    tasa_ocup_nodem_nodisp    = if(PEA>0) round((ocup_nodem_nodisp / PEA) * 100, 1) else NA_real_,
    tasa_presion_mercado      = if(PEA>0) round(((desoc_abiertos + ocup_dem + ocup_nodem_disp) / PEA) * 100, 1) else NA_real_
  )
}

message("Calculando indicadores...")
indicadores <- map_dfr(lista_bases, calcular_tasas) |>
  arrange(anio, trimestre)

# Etiqueta de período para gráficos (ej. "2T2016")
indicadores <- indicadores |>
  mutate(
    periodo_label = paste0(trimestre, "T", anio),
    fecha_aprox   = as.Date(paste0(
      anio, "-",
      c("03", "06", "09", "12")[trimestre], "-01"
    ))
  )

# ------------------------------------------------------------------------------
# 5. Guardar resultado
# ------------------------------------------------------------------------------

dir.create("data", showWarnings = FALSE)
saveRDS(indicadores, "data/indicadores.rds")
message("Guardado en data/indicadores.rds")
message(glue::glue(
  "Períodos: {min(indicadores$anio)}T{indicadores$trimestre[which.min(indicadores$anio)]} — ",
  "{max(indicadores$anio)}T{indicadores$trimestre[which.max(indicadores$anio)]}"
))
