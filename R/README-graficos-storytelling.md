# Graficos storytelling

Estos helpers se cargan automaticamente con:

```r
source("tema_graficos.R")
```

## Formatos disponibles

- `lm_waffle()`: porcentajes como cuadricula.
- `lm_marimekko()`: composicion de grupos y subgrupos.
- `lm_slopegraph()`: cambio entre periodos.
- `lm_ridgeline()`: distribuciones por grupo.
- `lm_sankey()`: flujos entre dos o tres etapas, requiere `ggalluvial`.
- `lm_dumbbell()`: comparacion antes/despues o dos medidas.
- `lm_story_note()`: ficha visual para acompanar un grafico.

## Ejemplos rapidos

```r
source("tema_graficos.R")

df_waffle <- data.frame(
  grupo = c("Formal", "Informal"),
  valor = c(43, 57)
)

lm_waffle(
  df_waffle,
  category = "grupo",
  value = "valor",
  title = "De cada 100 ocupados",
  subtitle = "Distribucion ilustrativa por condicion laboral",
  caption = lm_caption("ENCFT", cut = "2026")
)
```

```r
df_slope <- data.frame(
  sector = rep(c("Turismo", "Industria", "Comercio"), each = 2),
  periodo = rep(c("2019", "2024"), 3),
  valor = c(100, 132, 100, 108, 100, 116)
)

lm_slopegraph(
  df_slope,
  entity = "sector",
  period = "periodo",
  value = "valor",
  title = "Recuperacion desigual",
  subtitle = "Indice 2019 = 100"
)
```

```r
df_flujo <- data.frame(
  bruto = rep("Ingreso bruto", 5),
  etapa = c("Impuestos", "Ingreso neto", "Ingreso neto", "Ingreso neto", "Ingreso neto"),
  uso = c("Estado", "Vivienda", "Ahorro", "Comida", "Otros"),
  valor = c(22, 28, 18, 14, 18)
)

lm_sankey(
  df_flujo,
  axis1 = "bruto",
  axis2 = "etapa",
  value = "valor",
  axis3 = "uso",
  title = "Del ingreso al gasto",
  subtitle = "Ejemplo de flujo editorial"
)
```

## Regla editorial

Usa estos graficos cuando ayuden a contar una comparacion especifica. Si el
grafico necesita mucha explicacion para entenderse, probablemente conviene una
forma mas simple.
