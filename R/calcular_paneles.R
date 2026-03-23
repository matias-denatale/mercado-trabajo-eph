# =============================================================================
# EPH - PIPELINE PANELES DE TRANSICIÓN OCUPACIONAL
#
# Estrategia en dos etapas:
#
#   1. HISTÓRICO (2003–2024): se lee directamente del Excel consolidado
#      "data/transiciones_consolidado.xlsx" — ya calculado, no requiere
#      las bases de panel originales ni tarda nada.
#
#   2. ACTUALIZACIÓN AUTOMÁTICA: para el año más reciente disponible en EPH
#      se descarga y calcula el panel nuevo y se agrega a la serie.
#
# Salidas en data/paneles/:
#   matrices_anual.rds        — lista de matrices absolutas por año
#   tasas_cond_anual.rds      — lista de tasas condicionales por año
#   tasas_norm_anual.rds      — lista de tasas normalizadas por año
#   balance_anual.rds         — df con balance del stock por categoría y año
#   matrices_bloque.rds       — matrices absolutas por bloque plurianual
#   tasas_cond_bloque.rds     — tasas condicionales por bloque
# =============================================================================

library(dplyr)
library(readxl)
library(tidyr)
library(purrr)
library(eph)
library(openxlsx)
library(stringr)

dir.create("data/paneles", showWarnings = FALSE, recursive = TRUE)

EXCEL_CONSOLIDADO <- "data/transiciones_consolidado.xlsx"
LABELS <- c(
  "Patrones +5", "TCP calificados", "Asalariados formales",
  "Microempresarios", "TCP semi o no calificados",
  "Asalariados informales", "Desocupados", "Inactivos"
)

# -----------------------------------------------------------------------------
# FUNCIONES
# -----------------------------------------------------------------------------

tasas_cond <- function(mat) prop.table(mat, margin = 1)
tasas_norm <- function(mat) prop.table(mat)
balance    <- function(mat) colSums(mat) / sum(mat) - rowSums(mat) / sum(mat)

hacer_matriz_desde_df <- function(df) {
  df2 <- df |>
    mutate(across(c(g_ocup_t1, g_ocup_t2), as.character),
           n_pond = as.numeric(n_pond))
  m <- xtabs(n_pond ~ g_ocup_t1 + g_ocup_t2, data = df2)
  m_full <- matrix(0, 8, 8, dimnames = list(LABELS, LABELS))
  m_full[rownames(m), colnames(m)] <- m
  m_full
}

# Construye g_ocup (1–8) desde microdatos crudos EPH.
# Variables requeridas: CAT_OCUP, PP07H, PP04D_COD, PP04C, ESTADO.
#
# Categorías:
#   1 Patrones +5                 CAT_OCUP==1 & PP04C ∈ 6–12
#   2 TCP calificados             CAT_OCUP==2 & dígito CNO ∈ {1,2}
#   3 Asalariados formales        CAT_OCUP==3 & PP07H==1
#   4 Microempresarios            CAT_OCUP==1 & PP04C ∈ 1–5
#   5 TCP semi o no calificados   CAT_OCUP==2 & dígito CNO ∈ {3,4}
#   6 Asalariados informales      CAT_OCUP==3 & PP07H==2
#   7 Desocupados                 ESTADO==2
#   8 Inactivos                   ESTADO==3
#   NA todo lo demás (se filtra aguas abajo)
construir_g_ocup <- function(ind) {
  stopifnot(is.data.frame(ind))
  needed <- c("CAT_OCUP", "PP07H", "PP04D_COD", "PP04C", "ESTADO")
  missing_vars <- setdiff(needed, names(ind))
  if (length(missing_vars) > 0)
    stop("Faltan variables en ind: ", paste(missing_vars, collapse = ", "))

  ind |>
    mutate(
      CAT_OCUP  = suppressWarnings(as.integer(.data[["CAT_OCUP"]])),
      PP07H     = suppressWarnings(as.integer(.data[["PP07H"]])),
      PP04C     = suppressWarnings(as.integer(.data[["PP04C"]])),
      ESTADO    = suppressWarnings(as.integer(.data[["ESTADO"]])),
      PP04D_COD = as.character(.data[["PP04D_COD"]]),
      # Dígito 5 del código CNO: calificación del puesto
      .cno5 = suppressWarnings(
        as.integer(
          str_sub(str_pad(.data[["PP04D_COD"]], 5, side = "left", pad = "0"), 5, 5)
        )
      ),
      g_ocup = case_when(
        # Estado de actividad tiene prioridad sobre categoría ocupacional
        ESTADO == 2L                          ~ 7L,  # Desocupados
        ESTADO == 3L                          ~ 8L,  # Inactivos
        # Ocupados (ESTADO == 1)
        CAT_OCUP == 3L & PP07H == 1L          ~ 3L,  # Asalariados formales
        CAT_OCUP == 3L & PP07H == 2L          ~ 6L,  # Asalariados informales
        CAT_OCUP == 2L & .cno5 %in% c(1L,2L) ~ 2L,  # TCP calificados
        CAT_OCUP == 2L & .cno5 %in% c(3L,4L) ~ 5L,  # TCP semi o no calificados
        CAT_OCUP == 1L & PP04C %in% 1:5      ~ 4L,  # Microempresarios
        CAT_OCUP == 1L & PP04C %in% 6:12     ~ 1L,  # Patrones +5
        TRUE                                  ~ NA_integer_
        # Casos que quedan en NA y se filtran aguas abajo:
        #   CAT_OCUP==1 & PP04C==99  (patrón Ns/Nr de tamaño)
        #   CAT_OCUP==4              (familiar sin remuneración)
        #   ocupados sin PP07H ni PP04D_COD válido
      )
    ) |>
    select(-.cno5)
}

leer_hoja_panel <- function(nombre_hoja) {
  raw <- read_excel(EXCEL_CONSOLIDADO, sheet = nombre_hoja,
                    col_names = FALSE, col_types = "text")

  # Detectar fila de inicio de la matriz absoluta:
  # primera fila donde col 3 es numérico > 1 (valores expandidos, no tasas)
  fila_mat <- NA_integer_
  for (i in seq_len(nrow(raw))) {
    v <- suppressWarnings(as.numeric(raw[[3]][i]))
    if (!is.na(v) && v > 1) { fila_mat <- i; break }
  }
  if (is.na(fila_mat))
    stop(sprintf("No se encontró la matriz absoluta en hoja '%s'", nombre_hoja))

  leer_bloque_8x8 <- function(fila_ini) {
    m <- matrix(0, 8, 8, dimnames = list(LABELS, LABELS))
    for (i in 1:8)
      for (j in 1:8) {
        v <- suppressWarnings(as.numeric(raw[[j + 2]][fila_ini + i - 1]))
        if (!is.na(v)) m[i, j] <- v
      }
    m
  }

  # Buscar siguiente bloque (valores en (0, 1]) después de un offset
  sig_bloque_tasas <- function(desde) {
    for (i in seq(desde + 8, nrow(raw))) {
      v <- suppressWarnings(as.numeric(raw[[3]][i]))
      if (!is.na(v) && v > 0 && v <= 1) return(i)
    }
    NA_integer_
  }

  mat_abs  <- leer_bloque_8x8(fila_mat)
  fila_cond <- sig_bloque_tasas(fila_mat)
  mat_cond  <- if (!is.na(fila_cond)) leer_bloque_8x8(fila_cond) else tasas_cond(mat_abs)
  fila_norm <- sig_bloque_tasas(fila_cond + 7)
  mat_norm  <- if (!is.na(fila_norm)) leer_bloque_8x8(fila_norm) else tasas_norm(mat_abs)

  # Balance: buscar primer valor numérico (puede ser negativo) después de mat_norm
  fila_bal <- NA_integer_
  for (i in seq(fila_norm + 8, nrow(raw))) {
    v <- suppressWarnings(as.numeric(raw[[3]][i]))
    if (!is.na(v)) { fila_bal <- i; break }
  }
  bal_vec <- if (!is.na(fila_bal)) {
    setNames(suppressWarnings(as.numeric(raw[[3]][fila_bal:(fila_bal + 7)])), LABELS)
  } else {
    balance(mat_abs)
  }

  list(abs = mat_abs, cond = mat_cond, norm = mat_norm, balance = bal_vec)
}

# -----------------------------------------------------------------------------
# 1. LEER HISTÓRICO DESDE EXCEL CONSOLIDADO
# -----------------------------------------------------------------------------

if (!file.exists(EXCEL_CONSOLIDADO)) {
  candidatos <- c(
    "transiciones_paneles_anuales_2003_2024_CONSOLIDADO.xlsx",
    list.files(".", pattern = "CONSOLIDADO.*\\.xlsx$",
               full.names = TRUE, recursive = FALSE),
    list.files("Bases transiciones", pattern = "CONSOLIDADO.*\\.xlsx$",
               full.names = TRUE)
  )
  candidato <- na.omit(candidatos[file.exists(candidatos)])[1]
  if (is.na(candidato))
    stop(
      "No se encontró el Excel consolidado.\n",
      "Copiarlo a: data/transiciones_consolidado.xlsx"
    )
  file.copy(candidato, EXCEL_CONSOLIDADO)
  cat(sprintf("Copiado a %s\n", EXCEL_CONSOLIDADO))
}

hojas_paneles <- excel_sheets(EXCEL_CONSOLIDADO)
hojas_paneles <- hojas_paneles[grepl("^\\d{4}-\\d{4}$", hojas_paneles)]
cat(sprintf("Excel consolidado: %d paneles (%s → %s)\n",
            length(hojas_paneles), hojas_paneles[1], tail(hojas_paneles, 1)))

paneles_hist <- map(hojas_paneles, function(h) {
  cat(sprintf("  %s\n", h))
  leer_hoja_panel(h)
})
names(paneles_hist) <- hojas_paneles

# -----------------------------------------------------------------------------
# 2. PANEL MÁS RECIENTE DESDE EPH (actualización trimestral)
#
# En cada ejecución el script:
#   1. Busca el trimestre más alto ya cacheado para el año nuevo.
#   2. Intenta descargar trimestres MÁS RECIENTES (Q4→Q3→Q2→Q1).
#   3. Si encuentra uno, calcula el panel y actualiza la caché.
#   4. Guarda data/paneles/ultimo_panel.rds con el panel + su etiqueta
#      ("2024–2025 T3", etc.) para que el widget lo consuma dinámicamente.
#
# Cache por trimestre: cache_panel_YYYY-YYYY_T{1-4}.rds
# -----------------------------------------------------------------------------

ultimo_anio  <- as.integer(str_extract(tail(hojas_paneles, 1), "\\d{4}$"))
anio_nuevo   <- ultimo_anio + 1L
nombre_nuevo <- sprintf("%d-%d", ultimo_anio, anio_nuevo)

# Migración de caché legacy (sin número de trimestre → T2 por convención)
legacy <- sprintf("data/paneles/cache_panel_%s.rds", nombre_nuevo)
if (file.exists(legacy) &&
    !any(file.exists(sprintf("data/paneles/cache_panel_%s_T%d.rds", nombre_nuevo, 1:4)))) {
  file.rename(legacy, sprintf("data/paneles/cache_panel_%s_T2.rds", nombre_nuevo))
  cat("Cache legacy renombrado a formato trimestral (_T2).\n")
}

# Construye un panel desde dos data frames EPH crudos
construir_panel <- function(i1, i2) {
  prep <- function(ind, sfx) {
    ind |>
      construir_g_ocup() |>
      transmute(
        CODUSU, NRO_HOGAR, COMPONENTE,
        !!paste0("g_ocup_", sfx) := g_ocup,
        !!paste0("estado_", sfx) := suppressWarnings(as.integer(ESTADO)),
        edad    = suppressWarnings(as.integer(CH06)),
        pondera = suppressWarnings(as.numeric(PONDERA))
      )
  }
  panel_df <- prep(i1, "t1") |>
    inner_join(
      prep(i2, "t2") |> select(CODUSU, NRO_HOGAR, COMPONENTE, g_ocup_t2, estado_t2),
      by = c("CODUSU", "NRO_HOGAR", "COMPONENTE")
    ) |>
    filter(edad >= 18, edad <= 64,
           !is.na(g_ocup_t1), !is.na(g_ocup_t2),
           g_ocup_t1 %in% 1:8, g_ocup_t2 %in% 1:8) |>
    mutate(
      g_ocup_t1 = factor(g_ocup_t1, levels = 1:8, labels = LABELS),
      g_ocup_t2 = factor(g_ocup_t2, levels = 1:8, labels = LABELS)
    ) |>
    count(g_ocup_t1, g_ocup_t2, wt = pondera, name = "n_pond")
  mat <- hacer_matriz_desde_df(panel_df)
  list(abs=mat, cond=tasas_cond(mat), norm=tasas_norm(mat), balance=balance(mat))
}

# Descarga el primer par de trimestres disponible para yr1/yr2.
# Prueba en el orden indicado; devuelve list(i1, i2, trimestre) o NULL.
obtener_par_trimestres <- function(yr1, yr2, trimestres = c(4L, 3L, 2L, 1L)) {
  for (per in trimestres) {
    cat(sprintf("  %dT%d / %dT%d... ", yr1, per, yr2, per))
    i1 <- tryCatch(get_microdata(year=yr1, period=per, type="individual", vars="all"),
                   error = function(e) NULL)
    i2 <- tryCatch(get_microdata(year=yr2, period=per, type="individual", vars="all"),
                   error = function(e) NULL)
    if (!is.null(i1) && !is.null(i2)) { cat("OK\n"); return(list(i1=i1, i2=i2, trimestre=per)) }
    cat("no disponible\n")
  }
  NULL
}

panel_nuevo        <- NULL   # se incorpora a la serie anual si se calculó
ultimo_panel       <- NULL   # panel más reciente (cualquier trimestre)
ultimo_panel_label <- NULL

cat(sprintf("\nBuscando panel más reciente para %s...\n", nombre_nuevo))

# Trimestre más alto ya cacheado (0 si ninguno)
caches_ok <- Filter(file.exists,
  sprintf("data/paneles/cache_panel_%s_T%d.rds", nombre_nuevo, 4:1))
trim_cache <- if (length(caches_ok)) {
  as.integer(str_match(caches_ok[[1]], "_T(\\d)\\.rds")[, 2])
} else { 0L }

# Solo probar trimestres SUPERIORES al que ya tenemos en caché
trims_a_probar <- setdiff(c(4L, 3L, 2L, 1L), seq_len(trim_cache))

nuevos <- if (length(trims_a_probar)) {
  if (trim_cache > 0L)
    cat(sprintf("  Caché disponible hasta T%d — verificando si hay trimestres más nuevos...\n",
                trim_cache))
  obtener_par_trimestres(ultimo_anio, anio_nuevo, trimestres = trims_a_probar)
} else {
  cat(sprintf("  Caché al día (T%d) — sin descargas.\n", trim_cache)); NULL
}

if (!is.null(nuevos)) {
  per        <- nuevos$trimestre
  cache_path <- sprintf("data/paneles/cache_panel_%s_T%d.rds", nombre_nuevo, per)
  p          <- construir_panel(nuevos$i1, nuevos$i2)
  saveRDS(p, cache_path)
  cat(sprintf("  Panel %s T%d calculado y guardado.\n", nombre_nuevo, per))
  panel_nuevo        <- p
  ultimo_panel       <- p
  ultimo_panel_label <- sprintf("%d\u2013%d T%d", ultimo_anio, anio_nuevo, per)
} else if (length(caches_ok)) {
  p              <- readRDS(caches_ok[[1]])
  panel_nuevo    <- p
  ultimo_panel   <- p
  ultimo_panel_label <- sprintf("%d\u2013%d T%d", ultimo_anio, anio_nuevo, trim_cache)
  cat(sprintf("  Panel %s T%d cargado desde caché.\n", nombre_nuevo, trim_cache))
} else {
  cat(sprintf("  Panel %s aún no disponible en EPH.\n", nombre_nuevo))
}

# Guardar último panel + etiqueta para el widget de paneles.qmd
if (!is.null(ultimo_panel))
  saveRDS(list(panel=ultimo_panel, label=ultimo_panel_label),
          "data/paneles/ultimo_panel.rds")

# Combinar
paneles_todos <- paneles_hist
if (!is.null(panel_nuevo)) paneles_todos[[nombre_nuevo]] <- panel_nuevo

# -----------------------------------------------------------------------------
# 3. CONSTRUIR .RDS PARA paneles.qmd
# -----------------------------------------------------------------------------

matrices_anual   <- map(paneles_todos, "abs")
tasas_cond_anual <- map(paneles_todos, "cond")
tasas_norm_anual <- map(paneles_todos, "norm")

balance_anual_df <- imap_dfr(paneles_todos, function(p, nm) {
  tibble(categoria = names(p$balance), balance = p$balance, periodo = nm)
}) %>%
  pivot_wider(names_from = periodo, values_from = balance)

periodos_bloque <- list(
  "2003-2007" = c(2003, 2006),
  "2008-2011" = c(2007, 2010),
  "2011-2015" = c(2011, 2014),
  "2016-2019" = c(2016, 2018),
  "2022-2024" = c(2022, 2023)
)

matrices_bloque <- map(names(periodos_bloque), function(nm) {
  rango <- periodos_bloque[[nm]]
  keys  <- names(matrices_anual)[
    as.integer(str_extract(names(matrices_anual), "^\\d{4}")) >= rango[1] &
    as.integer(str_extract(names(matrices_anual), "^\\d{4}")) <= rango[2]
  ]
  Reduce("+", matrices_anual[keys])
})
names(matrices_bloque)    <- names(periodos_bloque)
tasas_cond_bloque <- map(matrices_bloque, tasas_cond)

# -----------------------------------------------------------------------------
# 4. GUARDAR
# -----------------------------------------------------------------------------

saveRDS(matrices_anual,    "data/paneles/matrices_anual.rds")
saveRDS(tasas_cond_anual,  "data/paneles/tasas_cond_anual.rds")
saveRDS(tasas_norm_anual,  "data/paneles/tasas_norm_anual.rds")
saveRDS(balance_anual_df,  "data/paneles/balance_anual.rds")
saveRDS(matrices_bloque,   "data/paneles/matrices_bloque.rds")
saveRDS(tasas_cond_bloque, "data/paneles/tasas_cond_bloque.rds")

anios <- names(matrices_anual)
cat(sprintf("\n=== Guardado en data/paneles/ ===\n"))
cat(sprintf("  %d paneles: %s → %s\n", length(anios), anios[1], tail(anios, 1)))
cat(sprintf("  Bloques: %s\n", paste(names(matrices_bloque), collapse = ", ")))
