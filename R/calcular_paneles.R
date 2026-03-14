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
# 2. PANEL MÁS RECIENTE DESDE EPH (automatico)
# -----------------------------------------------------------------------------

ultimo_anio  <- as.integer(str_extract(tail(hojas_paneles, 1), "\\d{4}$"))
anio_nuevo   <- ultimo_anio + 1
nombre_nuevo <- sprintf("%d-%d", ultimo_anio, anio_nuevo)
cache_nuevo  <- sprintf("data/paneles/cache_panel_%s.rds", nombre_nuevo)

panel_nuevo <- NULL

if (!file.exists(cache_nuevo)) {
  cat(sprintf("\nIntentando panel %s desde EPH...\n", nombre_nuevo))

  obtener_par_trimestres <- function(yr1, yr2, trimestres = c(2, 3, 1)) {
    for (per in trimestres) {
      cat(sprintf("  %dT%d / %dT%d... ", yr1, per, yr2, per))
      i1 <- tryCatch(
        get_microdata(year = yr1, period = per, type = "individual", vars = "all"),
        error = function(e) NULL)
      i2 <- tryCatch(
        get_microdata(year = yr2, period = per, type = "individual", vars = "all"),
        error = function(e) NULL)
      if (!is.null(i1) && !is.null(i2)) { cat("OK\n"); return(list(i1=i1, i2=i2)) }
      cat("no disponible\n")
    }
    NULL
  }

  datos <- obtener_par_trimestres(ultimo_anio, anio_nuevo)

  if (!is.null(datos)) {
    prep <- function(ind, sfx) {
      ind %>%
        transmute(
          CODUSU, NRO_HOGAR, COMPONENTE,
          !!paste0("g_ocup_", sfx) := suppressWarnings(as.integer(CAT_OCUP)),
          !!paste0("estado_", sfx) := suppressWarnings(as.integer(ESTADO)),
          edad    = suppressWarnings(as.integer(CH06)),
          pondera = suppressWarnings(as.numeric(PONDERA))
        )
    }

    panel_df <- prep(datos$i1, "t1") %>%
      inner_join(prep(datos$i2, "t2") %>%
                   select(CODUSU, NRO_HOGAR, COMPONENTE, g_ocup_t2, estado_t2),
                 by = c("CODUSU", "NRO_HOGAR", "COMPONENTE")) %>%
      mutate(
        g_ocup_t1 = case_when(estado_t1 == 2 ~ 7L, estado_t1 == 3 ~ 8L,
                              g_ocup_t1 == 8 & estado_t1 == 2 ~ 7L, TRUE ~ g_ocup_t1),
        g_ocup_t2 = case_when(estado_t2 == 2 ~ 7L, estado_t2 == 3 ~ 8L,
                              g_ocup_t2 == 8 & estado_t2 == 2 ~ 7L, TRUE ~ g_ocup_t2)
      ) %>%
      filter(edad >= 18, edad <= 64,
             !is.na(g_ocup_t1), !is.na(g_ocup_t2),
             g_ocup_t1 %in% 1:8, g_ocup_t2 %in% 1:8) %>%
      mutate(
        g_ocup_t1 = factor(g_ocup_t1, levels = 1:8, labels = LABELS),
        g_ocup_t2 = factor(g_ocup_t2, levels = 1:8, labels = LABELS)
      ) %>%
      count(g_ocup_t1, g_ocup_t2, wt = pondera, name = "n_pond")

    mat <- hacer_matriz_desde_df(panel_df)
    panel_nuevo <- list(abs=mat, cond=tasas_cond(mat),
                        norm=tasas_norm(mat), balance=balance(mat))
    saveRDS(panel_nuevo, cache_nuevo)
    cat(sprintf("  Panel %s calculado y guardado.\n", nombre_nuevo))
  } else {
    cat(sprintf("  Panel %s no disponible aún en EPH — se usa solo el histórico.\n",
                nombre_nuevo))
  }
} else {
  panel_nuevo <- readRDS(cache_nuevo)
  cat(sprintf("Panel %s cargado desde caché.\n", nombre_nuevo))
}

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
