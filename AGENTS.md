# Code Review Rules

## General
- This is a Quarto website project with R data pipelines
- Primary languages: R, SCSS, YAML, Markdown/QMD
- JavaScript/TypeScript rules below apply only if JS files are added in the future

## R
- Use tidyverse conventions (snake_case, pipe operator)
- Always use explicit namespaces for non-tidyverse packages (e.g. `eph::get_microdata()`)
- Never hardcode absolute paths — use relative paths from project root

## Quarto / YAML
- Theme extensions go in `styles.scss` using `theme: [lux, styles.scss]`
- Assets referenced in `_quarto.yml` must exist at the project root
- Never use raw HTML in YAML string fields (e.g. `title:`, `logo:`)

## CSS / SCSS
- Follow the existing palette: `--verde-oscuro`, `--verde-medio`, `--verde-claro`
- Add new component styles after the existing blocks, with a section comment
