# =============================================================================
# EPH - PIPELINE HOGARES: POBREZA, GRUPOS E INGRESOS DEFLACTADOS
# Versión automatizada para GitHub Actions
#
# Reemplaza: eph_script1_grupos.R + eph_script2_complementario.R +
#            eph_script3_deflactar.R
#
# Fuentes externas (descarga automática):
#   IPC  : https://www.indec.gob.ar/ftp/cuadros/economia/sh_ipc_MM_AA.xls
#          (MM = mes con cero adelante, AA = año 2 dígitos; fallback al mes anterior)
#   CBA/CBT: https://www.indec.gob.ar/ftp/cuadros/sociedad/serie_cba_cbt.xls
#            (URL fija, siempre actualizada por INDEC)
#
# Salidas:
#   data/hogares_long.rds            — base hogar × trimestre (con deflactor)
#   data/hogares_indicadores.rds     — resumen por grupo y trimestre (para QMD)
#   data/hogares_complementario.rds  — tablas complementarias (para QMD)
#   data/ipc_trimestral.rds          — factores de deflactación usados
#
# El base_year/base_month (periodo base de deflactación) se actualiza
# automáticamente al último mes disponible en el IPC descargado.
# =============================================================================

library(eph)
library(dplyr)
library(purrr)
library(tidyr)
library(readxl)
library(stringr)

# -----------------------------------------------------------------------------
# 0. PARÁMETROS GLOBALES
# -----------------------------------------------------------------------------

CACHE_DIR  <- "data/cache"
DATA_DIR   <- "data"
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(DATA_DIR,  recursive = TRUE, showWarnings = FALSE)

# Cobertura: desde 4T2023 (primer trimestre con variable EMPLEO) hasta hoy
PRIMER_YEAR   <- 2023
PRIMER_PERIOD <- 4

# Ratios regionales CBA/CBT respecto a GBA
# Fuente: INDEC, Metodología N° 22 (Paridades de Poder de Compra del Consumidor)
# Estos ratios son estructurales (reflejan diferencias de precios regionales) y
# se mantienen estables en el tiempo. Se usan para extrapolar los trimestres
# más recientes donde aún no hay valores regionales publicados.
RATIOS_REGION_BASE <- tibble(
  REGION    = c(1L,   40L,  41L,  42L,  43L,  44L),
  ratio_cbt = c(1.00, 0.86, 0.85, 0.95, 0.97, 1.22),
  ratio_cba = c(1.00, 0.86, 0.85, 0.95, 0.97, 1.22)
)

GRUPOS_ORDEN <- c(
  "Hogares de trabajadores formales",
  "Hogares de trabajadores informales",
  "Hogares mixtos",
  "Hogares sin ocupados"
)

# Variables de política social (1 = percibe en hogar o miembro individual)
VARS_SOCIAL <- c("V2_01","V2_02","V2_03","V4","V5_1","V5_2","V5_3","V11_1","V11_2")

# -----------------------------------------------------------------------------
# 1. CONSTRUIR LISTA DE TRIMESTRES DISPONIBLES
# -----------------------------------------------------------------------------

# Genera todos los trimestres desde el inicio hasta el trimestre anterior al actual
# (el trimestre en curso nunca está publicado)
build_trimestres <- function(primer_year, primer_period) {
  hoy    <- Sys.Date()
  yr_hoy <- as.integer(format(hoy, "%Y"))
  mo_hoy <- as.integer(format(hoy, "%m"))
  # Trimestre actual (puede no estar publicado aún)
  tr_hoy <- ceiling(mo_hoy / 3)
  # Retroceder un trimestre para estar en zona segura
  if (tr_hoy == 1) { yr_fin <- yr_hoy - 1; tr_fin <- 4 } else { yr_fin <- yr_hoy; tr_fin <- tr_hoy - 1 }

  out <- list()
  yr  <- primer_year
  per <- primer_period
  repeat {
    out <- c(out, list(list(year = yr, period = per)))
    if (yr == yr_fin && per == tr_fin) break
    per <- per + 1
    if (per > 4) { per <- 1; yr <- yr + 1 }
    if (yr > yr_fin + 1) break  # seguridad
  }
  out
}

trimestres <- build_trimestres(PRIMER_YEAR, PRIMER_PERIOD)
cat(sprintf("Trimestres a procesar: %d (%s → %s)\n",
            length(trimestres),
            paste0(trimestres[[1]]$year,  "T", trimestres[[1]]$period),
            paste0(trimestres[[length(trimestres)]]$year, "T",
                   trimestres[[length(trimestres)]]$period)))

# -----------------------------------------------------------------------------
# 2. DESCARGAR Y PARSEAR CBA/CBT (URL fija del INDEC)
# -----------------------------------------------------------------------------

descargar_canasta <- function() {
  url   <- "https://www.indec.gob.ar/ftp/cuadros/sociedad/serie_cba_cbt.xls"
  local <- file.path(CACHE_DIR, "serie_cba_cbt.xls")

  # Refrescar si tiene más de 7 días
  if (!file.exists(local) ||
      as.numeric(difftime(Sys.time(), file.mtime(local), units = "days")) > 7) {
    cat("Descargando serie_cba_cbt.xls...\n")
    tryCatch(
      download.file(url, local, mode = "wb", quiet = TRUE),
      error = function(e) {
        if (file.exists(local)) {
          message("Advertencia: no se pudo actualizar la canasta. Usando caché existente.")
        } else {
          stop("No se pudo descargar la canasta CBA/CBT y no hay caché local: ", e$message)
        }
      }
    )
  } else {
    cat("Usando caché de serie_cba_cbt.xls (< 7 días)\n")
  }

  raw <- tryCatch(
    suppressWarnings(read_excel(local, sheet = 1, col_names = FALSE)),
    error = function(e) stop("Error al leer serie_cba_cbt.xls: ", e$message)
  )

  # Estructura real del INDEC (verificada):
  #   Fila 4 : encabezados ("Mes", "Canasta básica alimentaria", ..., "Canasta básica total")
  #   Fila 8+: datos — col 1 = fecha serial Excel, col 2 = CBA, col 4 = CBT
  #
  # Detección robusta: buscar la primera fila con un serial de fecha válido en col 1
  # (>30000 = post 1982), que es donde empiezan los datos.
  find_data_start <- function(df) {
    for (i in seq_len(nrow(df))) {
      v <- suppressWarnings(as.numeric(df[[1]][i]))
      if (!is.na(v) && v > 30000) return(i)
    }
    return(8L)  # fallback a posición conocida
  }

  row_data_start <- find_data_start(raw)
  cat(sprintf("  serie_cba_cbt.xls: datos desde fila %d\n", row_data_start))

  # Extraer columnas: 1 = fecha serial, 2 = CBA, 4 = CBT
  datos <- raw[row_data_start:nrow(raw), ]
  fechas_raw <- suppressWarnings(as.numeric(unlist(datos[, 1])))
  cba_vals   <- suppressWarnings(as.numeric(unlist(datos[, 2])))
  cbt_vals   <- suppressWarnings(as.numeric(unlist(datos[, 4])))

  fechas <- as.Date(ifelse(!is.na(fechas_raw) & fechas_raw > 30000,
                           fechas_raw, NA_real_),
                    origin = "1899-12-30")

  canasta_mensual <- tibble(
    fecha      = fechas,
    CBA_AE_GBA = cba_vals,
    CBT_AE_GBA = cbt_vals
  ) %>%
    filter(!is.na(fecha), !is.na(CBA_AE_GBA), !is.na(CBT_AE_GBA),
           CBA_AE_GBA > 0, CBT_AE_GBA > 0) %>%
    mutate(
      year   = as.integer(format(fecha, "%Y")),
      month  = as.integer(format(fecha, "%m")),
      period = ceiling(month / 3)
    )

  if (nrow(canasta_mensual) < 12)
    stop("La canasta parseada tiene menos de 12 filas. Verificar estructura del XLS.")

  # Promedio trimestral GBA
  canasta_gba_trim <- canasta_mensual %>%
    group_by(year, period) %>%
    summarise(
      CBA_AE_GBA = mean(CBA_AE_GBA, na.rm = TRUE),
      CBT_AE_GBA = mean(CBT_AE_GBA, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(year, period)

  cat(sprintf("  Canasta GBA: %d trimestres (%dT%d → %dT%d)\n",
              nrow(canasta_gba_trim),
              min(canasta_gba_trim$year),
              canasta_gba_trim$period[which.min(canasta_gba_trim$year)],
              max(canasta_gba_trim$year),
              canasta_gba_trim$period[which.max(canasta_gba_trim$year)]))

  # -------------------------------------------------------------------------
  # Valores regionales por extrapolación con ratio estructural
  #
  # INDEC publica CBA/CBT solo para GBA en serie_cba_cbt.xls. Los valores
  # regionales se calculan aplicando ratios de las Paridades de Poder de
  # Compra del Consumidor (INDEC, Metodología N° 22). Estos ratios son
  # estructurales: reflejan diferencias de precios entre regiones, no niveles,
  # por lo que son estables en el tiempo. La práctica estándar —incluyendo
  # la usada por INDEC internamente— es asumir que la última relación
  # publicada se mantiene para los trimestres más recientes.
  #
  #   CBT_region(t) = CBT_GBA(t) × ratio_cbt_region
  #   CBA_region(t) = CBA_GBA(t) × ratio_cba_region
  #
  # Si INDEC actualizara los ratios, alcanza con actualizar RATIOS_REGION_BASE.
  # -------------------------------------------------------------------------

  canasta_completa <- canasta_gba_trim %>%
    crossing(RATIOS_REGION_BASE) %>%
    mutate(
      CBT_AE = CBT_AE_GBA * ratio_cbt,
      CBA_AE = CBA_AE_GBA * ratio_cba
    ) %>%
    select(year, period, REGION, CBA_AE_GBA, CBT_AE_GBA, CBT_AE, CBA_AE)

  # Diagnóstico: último trimestre por región
  ultimo_yr  <- max(canasta_completa$year)
  ultimo_per <- max(canasta_completa$period[canasta_completa$year == ultimo_yr])
  cat("  Último trimestre con canasta:\n")
  canasta_completa %>%
    filter(year == ultimo_yr, period == ultimo_per) %>%
    select(year, period, REGION, CBA_AE, CBT_AE) %>%
    arrange(REGION) %>%
    print()

  canasta_completa
}

canasta_completa <- descargar_canasta()

# -----------------------------------------------------------------------------
# 3. DESCARGAR IPC (URL variable por mes, con fallback automático)
# -----------------------------------------------------------------------------

descargar_ipc <- function() {
  # Intentar desde el mes actual hacia atrás (máximo 4 meses)
  hoy   <- Sys.Date()
  yr    <- as.integer(format(hoy, "%Y"))
  mo    <- as.integer(format(hoy, "%m"))

  for (retroceso in 0:4) {
    mo_int <- mo - retroceso
    yr_int <- yr
    if (mo_int <= 0) { mo_int <- mo_int + 12; yr_int <- yr_int - 1 }

    nombre <- sprintf("sh_ipc_%02d_%02d.xls", mo_int, yr_int %% 100)
    url    <- paste0("https://www.indec.gob.ar/ftp/cuadros/economia/", nombre)
    local  <- file.path(CACHE_DIR, nombre)

    if (!file.exists(local)) {
      cat(sprintf("  Intentando IPC: %s\n", nombre))
      resultado <- tryCatch({
        download.file(url, local, mode = "wb", quiet = TRUE)
        "ok"
      }, error = function(e) "error")

      if (resultado == "error") {
        if (file.exists(local)) file.remove(local)
        next
      }
    } else {
      cat(sprintf("  Usando IPC en caché: %s\n", nombre))
    }

    # Verificar que se puede leer
    ipc_raw <- tryCatch(
      read_excel(local, sheet = "Índices IPC Cobertura Nacional",
                 col_names = FALSE, n_max = 12),
      error = function(e) NULL
    )
    if (is.null(ipc_raw)) { file.remove(local); next }

    # Parsear: fila 6 = fechas (serial Excel), fila 10 = IPC Nivel General
    dates_raw <- suppressWarnings(as.numeric(ipc_raw[6, 2:ncol(ipc_raw)]))
    ipc_vals  <- suppressWarnings(as.numeric(ipc_raw[10, 2:ncol(ipc_raw)]))

    n <- min(sum(!is.na(dates_raw)), sum(!is.na(ipc_vals)))
    if (n < 12) { next }

    fechas <- as.Date(dates_raw[1:n], origin = "1899-12-30")

    ipc_mensual <- tibble(
      fecha = fechas,
      ipc   = ipc_vals[1:n]
    ) %>%
      filter(!is.na(fecha), !is.na(ipc), ipc > 0) %>%
      mutate(
        year    = as.integer(format(fecha, "%Y")),
        month   = as.integer(format(fecha, "%m")),
        quarter = ceiling(month / 3),
        periodo = paste0(year, "T", quarter)
      )

    cat(sprintf("  IPC cargado: %s (%d meses, hasta %s)\n",
                nombre, nrow(ipc_mensual),
                format(max(ipc_mensual$fecha), "%Y-%m")))

    # Periodo base = último mes completo disponible
    ultimo_mes  <- max(ipc_mensual$fecha)
    base_year   <- as.integer(format(ultimo_mes, "%Y"))
    base_month  <- as.integer(format(ultimo_mes, "%m"))
    ipc_base    <- ipc_mensual %>%
      filter(year == base_year, month == base_month) %>%
      pull(ipc)

    cat(sprintf("  Base de deflactación: %d-%02d | IPC base: %.2f\n",
                base_year, base_month, ipc_base))

    ipc_trimestral <- ipc_mensual %>%
      group_by(periodo, year, quarter) %>%
      summarise(ipc_prom = mean(ipc, na.rm = TRUE), .groups = "drop") %>%
      arrange(year, quarter) %>%
      mutate(factor_defl = ipc_base / ipc_prom)

    attr(ipc_trimestral, "base_label") <- sprintf("precios de %s %d",
      c("enero","febrero","marzo","abril","mayo","junio",
        "julio","agosto","septiembre","octubre","noviembre","diciembre")[base_month],
      base_year)

    return(ipc_trimestral)
  }
  stop("No se encontró ningún archivo IPC válido en los últimos 4 meses.")
}

ipc_trimestral <- descargar_ipc()
saveRDS(ipc_trimestral, file.path(DATA_DIR, "ipc_trimestral.rds"))
cat(sprintf("\nFactores de deflactación (últimos trimestres):\n"))
print(tail(ipc_trimestral %>% select(periodo, ipc_prom, factor_defl), 8))

# -----------------------------------------------------------------------------
# 4. FUNCIONES DE PROCESAMIENTO (reutilizadas de scripts originales)
# -----------------------------------------------------------------------------

calc_numeqad <- function(ind_df) {
  eph::adulto_equivalente %>%
    { left_join(ind_df, ., by = c("CH04", "CH06")) } %>%
    mutate(adequi = replace_na(adequi, 1.0)) %>%
    group_by(CODUSU, NRO_HOGAR) %>%
    summarise(numeqad = sum(adequi, na.rm = TRUE), .groups = "drop")
}

calc_cbt_cba_hh <- function(ind_df, hog_df, yr, per) {
  canasta_per <- canasta_completa %>% filter(year == yr, period == per)
  if (nrow(canasta_per) == 0) {
    # Usar el trimestre más reciente disponible
    canasta_per <- canasta_completa %>%
      arrange(desc(year), desc(period)) %>%
      slice(1:nrow(RATIOS_REGION_BASE))
    warning(sprintf("Sin canasta para %dT%d — usando más reciente disponible.", yr, per))
  }
  calc_numeqad(ind_df) %>%
    left_join(hog_df %>% select(CODUSU, NRO_HOGAR, REGION) %>% distinct(),
              by = c("CODUSU", "NRO_HOGAR")) %>%
    left_join(canasta_per, by = "REGION") %>%
    mutate(
      CBT_hh = numeqad * CBT_AE,
      CBA_hh = numeqad * CBA_AE
    ) %>%
    select(CODUSU, NRO_HOGAR, numeqad, CBT_hh, CBA_hh)
}

calc_fuentes_laborales <- function(ind_df) {
  ind_df %>%
    mutate(
      p21_v   = ifelse(is.na(P21)     | P21 < 0,     0, P21),
      tp12_v  = ifelse(is.na(TOT_P12) | TOT_P12 < 0, 0, TOT_P12),
      ing_lab = p21_v + tp12_v,
      es_formal   = !is.na(EMPLEO) & EMPLEO == 1 & ing_lab > 0,
      es_informal = !is.na(EMPLEO) & EMPLEO == 2 & ing_lab > 0
    ) %>%
    group_by(CODUSU, NRO_HOGAR) %>%
    summarise(
      fuente_formal   = as.integer(any(es_formal,   na.rm = TRUE)),
      fuente_informal = as.integer(any(es_informal, na.rm = TRUE)),
      .groups = "drop"
    )
}

calc_fuente_social <- function(hog_df, ind_df) {
  vars_hog <- intersect(VARS_SOCIAL, names(hog_df))
  vars_ind <- intersect(setdiff(VARS_SOCIAL, vars_hog), names(ind_df))

  s_hog <- if (length(vars_hog) > 0) {
    hog_df %>%
      select(CODUSU, NRO_HOGAR, all_of(vars_hog)) %>%
      rowwise() %>%
      mutate(s_hog = as.integer(any(c_across(all_of(vars_hog)) == 1, na.rm = TRUE))) %>%
      ungroup() %>%
      select(CODUSU, NRO_HOGAR, s_hog)
  } else {
    hog_df %>% select(CODUSU, NRO_HOGAR) %>% mutate(s_hog = 0L)
  }

  s_ind <- if (length(vars_ind) > 0) {
    ind_df %>%
      select(CODUSU, NRO_HOGAR, all_of(vars_ind)) %>%
      rowwise() %>%
      mutate(s_ind = as.integer(any(c_across(all_of(vars_ind)) == 1, na.rm = TRUE))) %>%
      ungroup() %>%
      group_by(CODUSU, NRO_HOGAR) %>%
      summarise(s_ind = as.integer(any(s_ind == 1L)), .groups = "drop")
  } else {
    hog_df %>% select(CODUSU, NRO_HOGAR) %>% mutate(s_ind = 0L)
  }

  s_hog %>%
    left_join(s_ind, by = c("CODUSU", "NRO_HOGAR")) %>%
    mutate(
      s_ind       = replace_na(s_ind, 0L),
      fuente_social = as.integer(s_hog == 1L | s_ind == 1L)
    ) %>%
    select(CODUSU, NRO_HOGAR, fuente_social)
}

clasificar_grupos <- function(df) {
  df %>%
    mutate(grupo_hogar = factor(case_when(
      fuente_formal == 1 & fuente_informal == 1 ~ "Hogares mixtos",
      fuente_formal == 1 & fuente_informal == 0 ~ "Hogares de trabajadores formales",
      fuente_formal == 0 & fuente_informal == 1 ~ "Hogares de trabajadores informales",
      TRUE                                      ~ "Hogares sin ocupados"
    ), levels = GRUPOS_ORDEN))
}

# Armonizar tipos entre trimestres antes de bind_rows
armonizar_tipos <- function(lista) {
  if (length(lista) <= 1) return(lista)
  tipos    <- lapply(lista, function(df) sapply(df, class))
  all_cols <- unique(unlist(lapply(tipos, names)))
  for (col in all_cols) {
    clases <- unique(na.omit(sapply(tipos, function(t) t[col])))
    if (length(clases) > 1) {
      lista <- lapply(lista, function(df) {
        if (col %in% names(df)) df[[col]] <- as.character(df[[col]])
        df
      })
    }
  }
  lista
}

# -----------------------------------------------------------------------------
# 5. BUCLE PRINCIPAL: DESCARGAR Y PROCESAR MICRODATOS
# -----------------------------------------------------------------------------

lista_hogares <- list()
lista_ind     <- list()

for (tr in trimestres) {
  yr  <- tr$year
  per <- tr$period
  lbl <- paste0(yr, "T", per)

  cat(sprintf("\n=== %s ===\n", lbl))

  ind <- tryCatch(
    eph::get_microdata(year = yr, period = per, type = "individual", vars = "all"),
    error = function(e) { message("ERROR ind ", lbl, ": ", e$message); NULL }
  )
  hog <- tryCatch(
    eph::get_microdata(year = yr, period = per, type = "hogar", vars = "all"),
    error = function(e) { message("ERROR hog ", lbl, ": ", e$message); NULL }
  )

  if (is.null(ind) || is.null(hog)) { cat("  SKIP:", lbl, "\n"); next }
  if (nrow(ind) < 1000 || nrow(hog) < 100) {
    cat("  SKIP: base vacía:", lbl, "\n"); next
  }

  # Asegurar REGION en individual
  if (!"REGION" %in% names(ind)) {
    ind <- ind %>% left_join(
      hog %>% select(CODUSU, NRO_HOGAR, REGION) %>% distinct(),
      by = c("CODUSU", "NRO_HOGAR")
    )
  }

  # Tipos numéricos críticos
  hog <- hog %>%
    mutate(
      ITF  = suppressWarnings(as.numeric(ITF)),
      IPCF = suppressWarnings(as.numeric(IPCF))
    )

  if (!"V2_M" %in% names(ind)) ind$V2_M <- NA_real_
  if (!"V2_01_M" %in% names(ind)) ind$V2_01_M <- NA_real_
  if (!"V2_02_M" %in% names(ind)) ind$V2_02_M <- NA_real_
  if (!"V3_M" %in% names(ind)) ind$V3_M <- NA_real_
  if (!"V4_M" %in% names(ind)) ind$V4_M <- NA_real_
  if (!"V5_M" %in% names(ind)) ind$V5_M <- NA_real_
  if (!"V8_M" %in% names(ind)) ind$V8_M <- NA_real_
  if (!"V9_M" %in% names(ind)) ind$V9_M <- NA_real_
  if (!"V10_M" %in% names(ind)) ind$V10_M <- NA_real_
  if (!"V11_M" %in% names(ind)) ind$V11_M <- NA_real_
  if (!"V12_M" %in% names(ind)) ind$V12_M <- NA_real_
  if (!"V18_M" %in% names(ind)) ind$V18_M <- NA_real_
  if (!"V19_AM" %in% names(ind)) ind$V19_AM <- NA_real_
  if (!"V21_M" %in% names(ind)) ind$V21_M <- NA_real_

  ind <- ind %>%
    mutate(
      P21      = suppressWarnings(as.numeric(P21)),
      TOT_P12  = suppressWarnings(as.numeric(TOT_P12)),
      P47T     = suppressWarnings(as.numeric(P47T)),
      T_VI     = suppressWarnings(as.numeric(T_VI)),
      PP3E_TOT = suppressWarnings(as.numeric(PP3E_TOT)),
      PP3F_TOT = suppressWarnings(as.numeric(PP3F_TOT)),
      V2_M     = suppressWarnings(as.numeric(V2_M)),
      V2_01_M  = suppressWarnings(as.numeric(V2_01_M)),
      V2_02_M  = suppressWarnings(as.numeric(V2_02_M)),
      V3_M     = suppressWarnings(as.numeric(V3_M)),
      V4_M     = suppressWarnings(as.numeric(V4_M)),
      V5_M     = suppressWarnings(as.numeric(V5_M)),
      V8_M     = suppressWarnings(as.numeric(V8_M)),
      V9_M     = suppressWarnings(as.numeric(V9_M)),
      V10_M    = suppressWarnings(as.numeric(V10_M)),
      V11_M    = suppressWarnings(as.numeric(V11_M)),
      V12_M    = suppressWarnings(as.numeric(V12_M)),
      V18_M    = suppressWarnings(as.numeric(V18_M)),
      V19_AM   = suppressWarnings(as.numeric(V19_AM)),
      V21_M    = suppressWarnings(as.numeric(V21_M)),
      P21      = ifelse(P21      < 0, NA_real_, P21),
      TOT_P12  = ifelse(TOT_P12  < 0, NA_real_, TOT_P12),
      P47T     = ifelse(P47T     < 0, NA_real_, P47T),
      T_VI     = ifelse(T_VI     < 0, NA_real_, T_VI),
      PP3E_TOT = ifelse(PP3E_TOT < 0, NA_real_, PP3E_TOT),
      PP3F_TOT = ifelse(PP3F_TOT < 0, NA_real_, PP3F_TOT),
      V2_M     = ifelse(is.na(V2_M) | V2_M < 0, 0, V2_M),
      V2_01_M  = ifelse(is.na(V2_01_M) | V2_01_M < 0, 0, V2_01_M),
      V2_02_M  = ifelse(is.na(V2_02_M) | V2_02_M < 0, 0, V2_02_M),
      V3_M     = ifelse(is.na(V3_M) | V3_M < 0, 0, V3_M),
      V4_M     = ifelse(is.na(V4_M) | V4_M < 0, 0, V4_M),
      V5_M     = ifelse(is.na(V5_M) | V5_M < 0, 0, V5_M),
      V8_M     = ifelse(is.na(V8_M) | V8_M < 0, 0, V8_M),
      V9_M     = ifelse(is.na(V9_M) | V9_M < 0, 0, V9_M),
      V10_M    = ifelse(is.na(V10_M) | V10_M < 0, 0, V10_M),
      V11_M    = ifelse(is.na(V11_M) | V11_M < 0, 0, V11_M),
      V12_M    = ifelse(is.na(V12_M) | V12_M < 0, 0, V12_M),
      V18_M    = ifelse(is.na(V18_M) | V18_M < 0, 0, V18_M),
      V19_AM   = ifelse(is.na(V19_AM) | V19_AM < 0, 0, V19_AM),
      V21_M    = ifelse(is.na(V21_M) | V21_M < 0, 0, V21_M),
      ing_lab_ind   = replace_na(P21, 0) + replace_na(TOT_P12, 0),
      ing_nolab_ind = replace_na(T_VI, 0),
      es_publico    = !is.na(PP04A) & PP04A == 1,
      ing_pub_ind   = ifelse(es_publico, replace_na(P21, 0), 0),
      es_ocupado    = !is.na(ESTADO) & ESTADO == 1,
      horas_tot     = replace_na(PP3E_TOT, 0) + replace_na(PP3F_TOT, 0),
      horas_tot     = ifelse(horas_tot == 0 & es_ocupado, NA_real_, horas_tot),
      jub_contributiva = ifelse(V2_01_M + V2_02_M > 0, V2_01_M, V2_M),
      jub_moratoria    = V2_02_M,
      periodo       = lbl
    )

  # --- Base hogares ---
  cbt_cba     <- calc_cbt_cba_hh(ind, hog, yr, per)
  fuentes_lab <- calc_fuentes_laborales(ind)
  fuente_soc  <- calc_fuente_social(hog, ind)

  hog_trim <- hog %>%
    select(CODUSU, NRO_HOGAR, REGION, ITF_hh = ITF, IPCF, PONDIH) %>%
    mutate(
      ITF_hh = ifelse(ITF_hh < 0, NA_real_, ITF_hh),
      IPCF   = ifelse(IPCF   < 0, NA_real_, IPCF)
    ) %>%
    left_join(cbt_cba,     by = c("CODUSU", "NRO_HOGAR")) %>%
    left_join(fuentes_lab, by = c("CODUSU", "NRO_HOGAR")) %>%
    left_join(fuente_soc,  by = c("CODUSU", "NRO_HOGAR")) %>%
    mutate(
      fuente_formal   = replace_na(fuente_formal,   0L),
      fuente_informal = replace_na(fuente_informal, 0L),
      fuente_social   = replace_na(fuente_social,   0L),
      pobre     = ifelse(is.na(ITF_hh)|is.na(CBT_hh), NA_integer_,
                         as.integer(ITF_hh < CBT_hh)),
      indigente = ifelse(is.na(ITF_hh)|is.na(CBA_hh), NA_integer_,
                         as.integer(ITF_hh < CBA_hh)),
      periodo   = lbl
    ) %>%
    clasificar_grupos() %>%
    select(periodo, CODUSU, NRO_HOGAR, REGION, ITF_hh, IPCF, PONDIH,
           numeqad, CBT_hh, CBA_hh, pobre, indigente,
           fuente_formal, fuente_informal, fuente_social, grupo_hogar)

  tasa_pob <- weighted.mean(hog_trim$pobre,     hog_trim$PONDIH, na.rm = TRUE)
  tasa_ind <- weighted.mean(hog_trim$indigente, hog_trim$PONDIH, na.rm = TRUE)
  cat(sprintf("  Hogares: %d | Personas: %d | Pob: %.1f%% | Ind: %.1f%% | EMPLEO: %s\n",
              nrow(hog), nrow(ind), tasa_pob*100, tasa_ind*100,
              ifelse("EMPLEO" %in% names(ind), "OK", "ausente")))

  if (tasa_pob > 0.90)
    warning("ATENCIÓN: pobreza >90% en ", lbl, " — verificar canasta.")

  lista_hogares[[lbl]] <- hog_trim
  lista_ind[[lbl]]     <- ind %>%
    select(-any_of("PONDIH")) %>%   # se une desde hogares
    left_join(
      hog_trim %>% select(periodo, CODUSU, NRO_HOGAR, PONDIH, grupo_hogar, fuente_social),
      by = c("periodo", "CODUSU", "NRO_HOGAR")
    )
}

# -----------------------------------------------------------------------------
# 6. CONSOLIDAR Y DEFLACTAR
# -----------------------------------------------------------------------------

hogares_long <- bind_rows(lista_hogares)
cat(sprintf("\nhogares_long: %d filas | %d trimestres\n",
            nrow(hogares_long), n_distinct(hogares_long$periodo)))

# Unir factor de deflactación
hogares_long <- hogares_long %>%
  left_join(ipc_trimestral %>% select(periodo, factor_defl), by = "periodo") %>%
  mutate(
    ITF_hh_defl = round(ITF_hh * factor_defl, 0),
    IPCF_defl   = round(IPCF   * factor_defl, 0)
  )

sin_factor <- hogares_long %>% filter(is.na(factor_defl)) %>% distinct(periodo)
if (nrow(sin_factor) > 0)
  warning("Periodos sin factor de deflactación: ",
          paste(sin_factor$periodo, collapse = ", "))

saveRDS(hogares_long, file.path(DATA_DIR, "hogares_long.rds"))
cat("Guardado: data/hogares_long.rds\n")

# -----------------------------------------------------------------------------
# 7. RESUMEN DE INDICADORES POR GRUPO (para QMD)
# -----------------------------------------------------------------------------

base_label <- attr(ipc_trimestral, "base_label") %||% "precios constantes"

agregar_total <- function(df_grupo) {
  w <- df_grupo$hogares_expandidos
  tibble(
    grupo_hogar          = factor("Total", levels = c(GRUPOS_ORDEN, "Total")),
    hogares_expandidos   = sum(w, na.rm = TRUE),
    tasa_pobreza_pond    = round(sum(df_grupo$tasa_pobreza_pond    * w, na.rm=TRUE) / sum(w, na.rm=TRUE), 2),
    tasa_indigencia_pond = round(sum(df_grupo$tasa_indigencia_pond * w, na.rm=TRUE) / sum(w, na.rm=TRUE), 2),
    media_ITF_pond       = round(sum(df_grupo$media_ITF_pond       * w, na.rm=TRUE) / sum(w, na.rm=TRUE), 0),
    media_IPCF_pond      = round(sum(df_grupo$media_IPCF_pond      * w, na.rm=TRUE) / sum(w, na.rm=TRUE), 0),
    numeqad_media        = round(sum(df_grupo$numeqad_media        * w, na.rm=TRUE) / sum(w, na.rm=TRUE), 2),
    n_hogares_muestra    = sum(df_grupo$n_hogares_muestra, na.rm = TRUE)
  )
}

resumen_por_grupo <- hogares_long %>%
  group_by(periodo, grupo_hogar) %>%
  summarise(
    hogares_expandidos   = round(sum(PONDIH, na.rm = TRUE), 0),
    tasa_pobreza_pond    = round(weighted.mean(pobre,       PONDIH, na.rm=TRUE) * 100, 2),
    tasa_indigencia_pond = round(weighted.mean(indigente,   PONDIH, na.rm=TRUE) * 100, 2),
    media_ITF_pond       = round(weighted.mean(ITF_hh_defl, PONDIH, na.rm=TRUE), 0),
    media_IPCF_pond      = round(weighted.mean(IPCF_defl,   PONDIH, na.rm=TRUE), 0),
    numeqad_media        = round(mean(numeqad, na.rm = TRUE), 2),
    n_hogares_muestra    = n(),
    .groups = "drop"
  ) %>%
  mutate(grupo_hogar = factor(grupo_hogar, levels = c(GRUPOS_ORDEN, "Total")))

resumen_con_total <- resumen_por_grupo %>%
  group_by(periodo) %>%
  group_modify(~bind_rows(.x, agregar_total(.x))) %>%
  ungroup() %>%
  arrange(periodo, grupo_hogar) %>%
  mutate(base_deflactor = base_label)

saveRDS(resumen_con_total, file.path(DATA_DIR, "hogares_indicadores.rds"))
cat("Guardado: data/hogares_indicadores.rds\n")

cat("\n=== Resumen global (fila Total) ===\n")
resumen_con_total %>%
  filter(grupo_hogar == "Total") %>%
  select(periodo, tasa_pobreza_pond, tasa_indigencia_pond, media_ITF_pond) %>%
  print()

# -----------------------------------------------------------------------------
# 8. TABLAS COMPLEMENTARIAS (para QMD)
# -----------------------------------------------------------------------------

# Armonizar tipos en base individual antes de consolidar
lista_ind <- armonizar_tipos(lista_ind)
ind_long  <- bind_rows(lista_ind) %>%
  filter(!is.na(grupo_hogar)) %>%
  mutate(grupo_hogar = factor(grupo_hogar, levels = GRUPOS_ORDEN))

complementario <- list()

# 8a. Política social por grupo y trimestre
complementario$politica_social <- hogares_long %>%
  group_by(periodo, grupo_hogar) %>%
  summarise(
    hogares_expandidos = round(sum(PONDIH, na.rm = TRUE), 0),
    pct_con_social     = round(weighted.mean(fuente_social, PONDIH, na.rm=TRUE) * 100, 2),
    n_muestra          = n(),
    .groups = "drop"
  ) %>%
  bind_rows(
    hogares_long %>%
      group_by(periodo) %>%
      summarise(
        hogares_expandidos = round(sum(PONDIH, na.rm = TRUE), 0),
        pct_con_social     = round(weighted.mean(fuente_social, PONDIH, na.rm=TRUE) * 100, 2),
        n_muestra          = n(),
        .groups = "drop"
      ) %>%
      mutate(grupo_hogar = factor("Total", levels = c(GRUPOS_ORDEN, "Total")))
  ) %>%
  arrange(periodo, grupo_hogar)

# 8b. Ocupados promedio y horas trabajadas por grupo y trimestre
complementario$ocupacion_horas <- ind_long %>%
  group_by(periodo, grupo_hogar) %>%
  summarise(
    hogares_expandidos = round(sum(PONDIH[!duplicated(paste(CODUSU, NRO_HOGAR))],
                                   na.rm = TRUE), 0),
    ocup_por_hogar = round(
      weighted.mean(as.integer(es_ocupado), PONDERA, na.rm=TRUE), 2),
    horas_promedio = round(
      weighted.mean(horas_tot[es_ocupado], PONDERA[es_ocupado], na.rm=TRUE), 1),
    .groups = "drop"
  )

# 8c. Composición de ingresos (laboral vs no laboral, público vs privado)
complementario$composicion_ing <- ind_long %>%
  group_by(periodo, grupo_hogar) %>%
  summarise(
    masa_lab    = sum(ing_lab_ind   * PONDERA, na.rm = TRUE),
    masa_nolab  = sum(ing_nolab_ind * PONDERA, na.rm = TRUE),
    masa_pub    = sum(ing_pub_ind   * PONDERA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    masa_total   = masa_lab + masa_nolab,
    share_lab    = round(masa_lab   / masa_total * 100, 1),
    share_nolab  = round(masa_nolab / masa_total * 100, 1),
    share_pub    = round(masa_pub   / masa_lab   * 100, 1)
  )

# 8d. Serie de horas por grupo (wide para visualización)
complementario$serie_horas <- complementario$ocupacion_horas %>%
  select(periodo, grupo_hogar, horas_promedio) %>%
  pivot_wider(names_from = periodo, values_from = horas_promedio)

# 8e. Cuadro 5: Composición del ingreso no laboral para Hogares sin ocupados
# Incluimos factor_defl para llevar los montos a precios constantes
complementario$ing_no_lab_sin_ocup <- ind_long %>%
  filter(grupo_hogar == "Hogares sin ocupados") %>%
  left_join(ipc_trimestral %>% select(periodo, factor_defl), by = "periodo") %>% 
  group_by(periodo) %>%
  summarise(
    total_jub_cont    = round(sum(jub_contributiva * factor_defl * PONDERA, na.rm=TRUE)),
    total_jub_mor     = round(sum(jub_moratoria    * factor_defl * PONDERA, na.rm=TRUE)),
    total_desempleo   = round(sum((V3_M + V4_M) * factor_defl * PONDERA, na.rm=TRUE)),
    total_social      = round(sum(V5_M   * factor_defl * PONDERA, na.rm=TRUE)),
    total_rentas      = round(sum((V8_M + V9_M + V10_M) * factor_defl * PONDERA, na.rm=TRUE)),
    total_otros       = round(sum((V11_M + V12_M + V18_M + V19_AM + V21_M) * factor_defl * PONDERA, na.rm=TRUE)),
    total_nolab       = round(sum(ing_nolab_ind * factor_defl * PONDERA, na.rm=TRUE)),
    hogares_expand    = round(sum(PONDIH[!duplicated(paste(CODUSU, NRO_HOGAR))], na.rm=TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    `Jubilación contributiva`           = total_jub_cont  / hogares_expand,
    `Jubilación moratoria`              = total_jub_mor   / hogares_expand,
    `Seg. desempleo / indemnización`    = total_desempleo / hogares_expand,
    `Transferencias sociales y planes`  = total_social    / hogares_expand,
    `Rentas y ganancias`                = total_rentas    / hogares_expand,
    `Otros (becas, alimentos, etc.)`    = total_otros     / hogares_expand,
    `Total no laboral`                  = total_nolab     / hogares_expand
  ) %>%
  select(periodo, `Jubilación contributiva`, `Jubilación moratoria`, `Seg. desempleo / indemnización`, `Transferencias sociales y planes`, `Rentas y ganancias`, `Otros (becas, alimentos, etc.)`, `Total no laboral`) %>%
  pivot_longer(cols = -periodo, names_to = "Categoría", values_to = "Monto prom.") %>%
  group_by(periodo) %>%
  mutate(
    `Composición %` = round(`Monto prom.` / max(`Monto prom.`, na.rm=TRUE) * 100, 1)
  ) %>%
  ungroup()

saveRDS(complementario, file.path(DATA_DIR, "hogares_complementario.rds"))
cat("Guardado: data/hogares_complementario.rds\n")

# -----------------------------------------------------------------------------
# 9. FINALIZACIÓN
# -----------------------------------------------------------------------------

ultimo_trim <- tail(unique(sort(hogares_long$periodo)), 1)
cat(sprintf("\n=== Pipeline finalizado ===\n"))
cat(sprintf("  Cobertura: %s → %s\n",
            head(unique(sort(hogares_long$periodo)), 1), ultimo_trim))
cat(sprintf("  Deflactor: %s\n", base_label))
cat(sprintf("  Archivos generados en data/:\n"))
cat("    hogares_long.rds\n")
cat("    hogares_indicadores.rds\n")
cat("    hogares_complementario.rds\n")
cat("    ipc_trimestral.rds\n")
