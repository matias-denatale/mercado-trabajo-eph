# Mercado de Trabajo Argentino — EPH

Dashboard interactivo de indicadores del mercado laboral argentino construido con microdatos de la **Encuesta Permanente de Hogares (EPH)** del INDEC.

## 📊 Contenido

| Sección | Descripción |
|---------|-------------|
| **Inicio** | Tasas de empleo, desempleo, informalidad y presión sobre el mercado de trabajo — serie trimestral desde 2T2016. |
| **Hogares** | Pobreza e indigencia por tipo de hogar, ingresos (IPCF, ITF), composición laboral/no laboral, distribución de hogares, política social — serie desde 4T2023. |
| **Paneles** | Matrices de transición ocupacional y flujos interanuales basados en datos longitudinales. |
| **Metodología** | Definiciones, ponderadores y notas técnicas. |

## 🛠 Stack técnico

- **R** (`dplyr`, `tidyr`, `ggplot2`, `plotly`, `eph`, `readxl`, `reactable`)
- **Quarto** para el sitio web estático
- **GitHub Actions** para actualización automática
- **renv** para reproducibilidad del entorno

## 📁 Estructura del proyecto

```
├── index.qmd              # Página principal — indicadores generales
├── hogares.qmd            # Pobreza y hogares por tipo
├── paneles.qmd            # Transiciones ocupacionales
├── metodologia.qmd        # Notas metodológicas
├── _quarto.yml             # Configuración del sitio
├── styles.css              # Estilos visuales customizados
├── R/
│   ├── calcular_indicadores.R   # Pipeline de indicadores laborales
│   ├── calcular_hogares.R       # Pipeline de hogares y pobreza
│   └── calcular_paneles.R       # Pipeline de matrices de transición
├── data/                   # Datos intermedios (RDS)
├── docs/                   # Salida HTML (GitHub Pages)
└── renv/                   # Entorno reproducible
```

## 🚀 Cómo usar

### Requisitos previos

- R ≥ 4.3
- Quarto ≥ 1.3
- Conexión a internet (para descargar microdatos de INDEC)

### Ejecutar los scripts de datos

```r
# 1. Restaurar paquetes
renv::restore()

# 2. Calcular indicadores laborales (2T2016 en adelante)
source("R/calcular_indicadores.R")

# 3. Calcular indicadores de hogares y pobreza (4T2023 en adelante)
source("R/calcular_hogares.R")

# 4. (Opcional) Calcular paneles de transición
source("R/calcular_paneles.R")
```

### Renderizar el sitio

```bash
quarto render
```

El sitio se genera en `docs/` y puede servirse con GitHub Pages.

## 📅 Cobertura temporal

| Indicador | Período |
|-----------|---------|
| Tasas de empleo y desempleo | 2T2016 — 3T2025 |
| Tasa de informalidad laboral | 4T2023 — 3T2025 |
| Hogares por tipo y pobreza | 4T2023 — 3T2025 |
| Transiciones ocupacionales | Según bases de panel disponibles |

## 📝 Fuentes

- **Microdatos EPH**: [INDEC](https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos)
- **IPC Nacional**: serie mensual para deflactación de ingresos
- **Canasta Básica**: CBA/CBT de INDEC para cálculo de pobreza e indigencia

## 👤 Autor

**Matías De Natale** — [LinkedIn](https://www.linkedin.com/in/matías-de-natale-336745181) · [Email](mailto:matias_denatale@hotmail.com)

---

*Elaboración propia en base a EPH-INDEC.*
