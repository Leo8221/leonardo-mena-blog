library(tidyverse)
library(readxl)
library(scales)
library(margins)
library(gridExtra)
library(knitr)
library(survey) 
library(srvyr)  
library(ggtext)

# ==============================================================================
# 0. CONFIGURACIÓN Y TEMA
# ==============================================================================
tryCatch({
  source("C:\\Users\\leona\\leonardo-mena-blog\\tema_graficos.R")
}, error = function(e) {
  theme_lm <<- function(...) theme_minimal(...) + theme(legend.position = "bottom")
  pal <<- list(plomo = "#666666", terracota = "#E69F00", oliva = "#009E73", 
               texto_muted = "gray50", texto = "#1a1512", texto_soft = "#4a443e")
})

# ==============================================================================
# 1. CARGA Y PROCESAMIENTO DE DATOS
# ==============================================================================
df_raw <- read_excel("ENIEF_Consolidado.xlsx", sheet = "ENIEF 2023-2019", guess_max = 20000) |> 
  filter(BASE == "ENIEF 2023")

if(!"FACTOR_EXP" %in% names(df_raw)) stop("Error: Falta variable FACTOR_EXP")

# Funciones de limpieza
clean_binary <- function(x) {
  val <- suppressWarnings(as.numeric(as.character(x)))
  case_when(val == 1 ~ 1, val == 2 ~ 0, TRUE ~ NA_real_)
}

clean_payment <- function(x) {
  val <- suppressWarnings(as.numeric(as.character(x)))
  case_when(val == 1 ~ 1, val == 10 ~ NA_real_, !is.na(val) ~ 0, TRUE ~ NA_real_)
}

# Construcción del dataset maestro
df_maestro <- df_raw |> 
  mutate(
    Peso = FACTOR_EXP,
    
    # Educación
    Educ_Cod = as.numeric(rand_P1_6),
    Educacion = case_when(
      Educ_Cod %in% c(1:6) ~ "Superior",
      Educ_Cod %in% c(7:8) ~ "Media",
      Educ_Cod %in% c(9:11) ~ "Básica/Ninguna",
      TRUE ~ NA_character_
    ),
    Educacion = factor(Educacion, levels = c("Básica/Ninguna", "Media", "Superior")),
    
    # Ingreso Logarítmico
    Ing_Cod = as.numeric(P1_11),
    Ingreso_Log = log(case_match(Ing_Cod,
                                 1~10000, 2~16600, 3~21750, 4~27600, 5~46100, 6~66000, 
                                 7~76000, 8~86000, 9~95500, 10~150000, 11~250000, 12~0, 
                                 .default = NA) + 1),
    
    # Variables de interés
    Tiene_Cuenta_Bin = clean_binary(PRODUCTO_CUENTA_NOMINA_AHORRO),
    
    Y_Bajo_Valor   = clean_payment(P9_1_1),
    Y_Alto_Valor   = clean_payment(P9_1_2),
    Y_Alquiler     = clean_payment(P9_1_3),
    Y_Servicios    = clean_payment(P9_1_5),
    Y_Supermercado = clean_payment(P9_1_8),
    Y_Combustible  = clean_payment(P9_1_9),
    
    Edad = as.numeric(rand_P1_3),
    Zona = as.character(Nombre_Zona),
    
    # Variables de Actitudes (P3_18)
    across(starts_with("P3_18_"), clean_binary)
  ) |> 
  rowwise() |> 
  mutate(Indice_Efectivo = mean(c_across(starts_with("Y_")), na.rm = TRUE) * 100) |> 
  ungroup()

# ==============================================================================
# 2. DISEÑO MUESTRAL (SRVYR)
# ==============================================================================
diseno_enief <- df_maestro |> 
  as_survey_design(
    ids = 1,              
    weights = Peso        
  )

cat(sprintf("Población Representada: %.2f Millones\n", sum(df_maestro$Peso, na.rm=TRUE) / 1e6))

# ==============================================================================
# 3. ESTADÍSTICAS DESCRIPTIVAS BÁSICAS
# ==============================================================================
# Bancarización
diseno_enief |> 
  filter(!is.na(Tiene_Cuenta_Bin)) |> 
  summarise(
    Pct = survey_mean(Tiene_Cuenta_Bin, vartype = "ci", level = 0.95),
    Total = survey_total(Tiene_Cuenta_Bin)
  ) |> print()

# Uso efectivo colmados
diseno_enief |> 
  filter(!is.na(Y_Bajo_Valor)) |> 
  summarise(Pct = survey_mean(Y_Bajo_Valor, vartype = "ci")) |> 
  print()

# ==============================================================================
# 4. MODELOS ECONOMÉTRICOS (SVYGLM)
# ==============================================================================
gastos_analizar <- list(
  "Bajo Valor (Colmado)" = "Y_Bajo_Valor",
  "Alto Valor"           = "Y_Alto_Valor",
  "Alquiler"             = "Y_Alquiler",
  "Servicios Públicos"   = "Y_Servicios",
  "Supermercado"         = "Y_Supermercado",
  "Combustible"          = "Y_Combustible"
)

correr_modelo_survey <- function(design, y_var, nombre) {
  sub_design <- design |> 
    filter(!is.na(.data[[y_var]]), !is.na(Ingreso_Log), !is.na(Educacion))
  
  n_muestral <- nrow(sub_design)
  if(n_muestral < 100) return(NULL)
  
  # Modelo Quasibinomial
  f <- as.formula(paste(y_var, "~ Ingreso_Log + Educacion + Edad + Zona + Tiene_Cuenta_Bin"))
  mod <- svyglm(f, design = sub_design, family = quasibinomial())
  
  # Efectos marginales
  m <- margins(mod, design = sub_design) 
  
  summary(m) |> 
    filter(factor == "Tiene_Cuenta_Bin" | grepl("Superior", factor)) |> 
    mutate(Tipo_Gasto = nombre)
}

res_lista <- list()
for(i in seq_along(gastos_analizar)) {
  nom <- names(gastos_analizar)[i]
  var <- gastos_analizar[[i]]
  res_lista[[nom]] <- correr_modelo_survey(diseno_enief, var, nom)
}

tabla_robustez <- bind_rows(res_lista)

# Forest Plot de Robustez
g_forest <- tabla_robustez |> 
  mutate(
    Variable = if_else(grepl("Cuenta", factor), "Cuenta Bancaria", "Educación Superior"),
    Significativo = p < 0.05,
    Tipo_Gasto = fct_reorder(Tipo_Gasto, AME)
  ) |> 
  ggplot(aes(x = Tipo_Gasto, y = AME, color = Variable, shape = Significativo)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 4, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.5)) +
  coord_flip() +
  scale_color_manual(values = c("Cuenta Bancaria" = pal$plomo, "Educación Superior" = pal$terracota)) +
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1)) +
  theme_lm(grid = "x") +
  labs(
    title = "Efectos Marginales (svyglm)",
    subtitle = "Estimación con diseño muestral complejo",
    caption = "Fuente: ENIEF 2023 | Modelos Quasibinomiales Ponderados",
    x = NULL, y = "Efecto Marginal Promedio (AME)"
  )

print(g_forest)
# ggsave("Forest_Plot_Survey.png", g_forest, width = 10, height = 7, bg="white")

# ==============================================================================
# 5. GRÁFICO: HEGEMONÍA DEL EFECTIVO (Versión Blog)
# ==============================================================================
vars_blog <- c("Y_Bajo_Valor", "Y_Alquiler", "Y_Supermercado", 
               "Y_Servicios", "Y_Combustible", "Y_Alto_Valor")

nombres_blog <- c("Colmados y Pequeño Comercio", 
                  "Alquiler de Vivienda", 
                  "Supermercados", 
                  "Servicios Públicos (Luz/Agua)", 
                  "Combustibles", 
                  "Bienes de Alto Valor (Muebles/Joyas)")

df_blog_hegemonia <- map2_dfr(vars_blog, nombres_blog, function(v, n) {
  diseno_enief |> 
    filter(!is.na(.data[[v]])) |> 
    summarise(Pct = survey_mean(.data[[v]])) |> 
    mutate(Rubro = n)
}) |> 
  mutate(
    Pct = Pct * 100,
    Label = paste0(round(Pct, 1), "%"),
    Rubro = fct_reorder(Rubro, Pct)
  )

g_blog_rey <- ggplot(df_blog_hegemonia, aes(x = Rubro, y = Pct)) +
  geom_col(fill = pal$terracota, width = 0.65) +
  geom_text(aes(label = Label), 
            hjust = 1.2, 
            color = "white", 
            fontface = "bold", 
            size = 4.5) +
  coord_flip() + 
  scale_y_continuous(limits = c(0, 100), expand = c(0,0)) +
  labs(
    title = "El efectivo sigue siendo el rey de las transacciones",
    subtitle = "Porcentaje de pagos realizados en efectivo por categoría de gasto",
    caption = "Fuente: Elaboración propia con datos de la ENIEF 2023.",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(color = "#2c2c2c"),
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, color = "gray40", margin = margin(b = 20)),
    plot.caption = element_text(size = 8, color = "gray60", margin = margin(t = 15), hjust = 0),
    axis.text.y = element_text(size = 11, face = "bold", color = "#2c2c2c"),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

# Guardar RDS
saveRDS(g_blog_rey, file = "rds/grafico_hegemonia.rds")

# ==============================================================================
# 6. GRÁFICO: HAMACA DE EDAD
# ==============================================================================
df_hamaca_pro <- diseno_enief |> 
  filter(!is.na(Edad), Edad >= 18, Edad <= 75) |> 
  filter(Ing_Cod >= 4 & Ing_Cod != 12) |> # Filtro Clase Media
  mutate(
    Decada = cut(Edad, breaks = c(18, 28, 38, 48, 58, 80), 
                 labels = c("18-28\n(Gen Z)", "29-38\n(Millennials)", 
                            "39-48\n(Gen X)", "49-58\n(Pre-Boom)", 
                            "59+\n(Boomers)"), 
                 include.lowest = T)
  ) |> 
  group_by(Decada) |> 
  summarise(
    Pct = survey_mean(Indice_Efectivo, vartype = "ci", level = 0.95, na.rm=T)
  )

g_hamaca <- ggplot(df_hamaca_pro, aes(x = Decada, y = Pct, group = 1)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 50, ymax = 100, 
           fill = pal$terracota, alpha = 0.05) +
  geom_ribbon(aes(ymin = Pct_low, ymax = Pct_upp), fill = pal$plomo, alpha = 0.15) +
  geom_line(color = pal$terracota, linewidth = 2) +
  geom_point(size = 5, color = pal$terracota, fill = "white", shape = 21, stroke = 2) +
  geom_text(aes(label = paste0(round(Pct, 1), "%")), 
            vjust = -1.8, fontface = "bold", color = pal$texto, size = 4.5) +
  geom_hline(yintercept = 50, linetype = "dashed", color = pal$texto_muted) +
  annotate("text", x = 1, y = 52, label = "Frontera Digital (50%)", 
           hjust = 0, size = 3, color = pal$texto_muted) +
  scale_y_continuous(limits = c(40, 90), labels = function(x) paste0(x, "%")) +
  labs(
    title = "La 'Hamaca' de la Digitalización",
    subtitle = "Uso de efectivo en Clase Media por edad",
    caption = "Fuente: ENIEF 2023 | Paquete srvyr",
    x = NULL, y = "Intensidad de Uso de Efectivo"
  ) +
  theme_lm()

# Guardar RDS
saveRDS(g_hamaca, "rds/grafico_hamaca.rds")

# ==============================================================================
# 7. GRÁFICO: PERSISTENCIA ESTRUCTURAL (CLASE SOCIAL)
# ==============================================================================
df_clase <- diseno_enief |> 
  filter(!is.na(Ing_Cod), Ing_Cod != 12, !is.na(Indice_Efectivo)) |> 
  mutate(
    Clase = case_when(
      Ing_Cod == 1 ~ "Ingreso Bajo\n(<10k)",
      Ing_Cod %in% c(2,3) ~ "Clase Media Baja\n(10k - 21k)",
      Ing_Cod %in% c(4,5) ~ "Clase Media\n(21k - 46k)",
      Ing_Cod >= 6 ~ "Ingreso Alto\n(>66k)", 
      TRUE ~ NA_character_
    )
  ) |> 
  filter(!is.na(Clase)) |> 
  mutate(Clase = factor(Clase, levels = c("Ingreso Bajo\n(<10k)", 
                                          "Clase Media Baja\n(10k - 21k)", 
                                          "Clase Media\n(21k - 46k)", 
                                          "Ingreso Alto\n(>66k)"))) |> 
  group_by(Clase) |> 
  summarise(
    Media = survey_mean(Indice_Efectivo, na.rm = TRUE)
  )

g_clase <- ggplot(df_clase, aes(x = Clase, y = Media)) +
  geom_segment(aes(xend = Clase, y = 0, yend = Media), 
               color = pal$plomo, linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 18, color = pal$terracota) +
  geom_text(aes(label = paste0(round(Media, 0), "%")), 
            color = "white", fontface = "bold", size = 5, family = "sans") +
  
  # Anotación
  annotate("text", x = 2.8, y = 100, 
           label = "Incluso el segmento más rico\nusa efectivo mayoritariamente", 
           hjust = 0.5, size = 3.8, color = pal$texto_soft, lineheight = 0.9,
           fontface = "italic") +
  annotate("curve", 
           x = 3.2, y = 90, 
           xend = 4, yend = 77, 
           curvature = -0.25, 
           arrow = arrow(length = unit(3, "mm")), 
           color = pal$texto_muted, linewidth = 0.8) +
  
  scale_y_continuous(limits = c(0, 115), expand = c(0,0)) + 
  labs(
    title = "Persistencia Estructural: El ingreso no rompe el hábito",
    subtitle = "Porcentaje promedio de compras pagadas en efectivo según nivel socioeconómico",
    caption = "Fuente: ENIEF 2023 | Cálculos ponderados con srvyr.",
    x = NULL, y = NULL
  ) +
  theme_lm(grid = "n") + 
  theme(
    axis.text.y = element_blank(), 
    axis.text.x = element_text(vjust = 5, size = 11, color = pal$texto_soft),
    plot.margin = margin(20, 20, 20, 20)
  )

# Guardar RDS
saveRDS(g_clase, "rds/grafico_clase.rds")

# ==============================================================================
# 8. GRÁFICO: RAZONES DECLARADAS
# ==============================================================================
mapa_razones <- tibble(
  Var = c("P3_18_1", "P3_18_2", "P3_18_3", "P3_18_4", "P3_18_5", "P3_18_6"),
  Razon_Larga = c("No aceptan tarjeta\n(Restricción)", 
                  "Es más barato\n(Descuentos)", 
                  "Es más seguro\n(Privacidad)", 
                  "Es más rápido/fácil\n(Conveniencia)", 
                  "Mejor control de gastos\n(Disciplina)", 
                  "Son montos bajos\n(Micro-pagos)")
)

df_razones_general <- map2_dfr(mapa_razones$Var, mapa_razones$Razon_Larga, function(v, n) {
  diseno_enief |> 
    filter(!is.na(.data[[v]])) |> 
    summarise(Pct = survey_mean(.data[[v]], na.rm=T)) |> 
    mutate(Razon = n)
}) |> 
  mutate(
    Pct = Pct * 100,
    Label = paste0(round(Pct, 1), "%"),
    Tipo = if_else(grepl("Restricción", Razon), "Estructural", "Preferencia")
  )

g_razones_rank <- ggplot(df_razones_general, aes(x = reorder(Razon, Pct), y = Pct)) +
  geom_col(aes(fill = Tipo), width = 0.7) +
  geom_text(aes(label = Label), hjust = -0.2, fontface = "bold", color = pal$texto) +
  coord_flip() +
  scale_fill_manual(values = c("Estructural" = pal$terracota, "Preferencia" = pal$plomo)) +
  scale_y_continuous(limits = c(0, 60), expand = c(0,0)) +
  labs(
    title = "¿Por qué prefieren el efectivo?",
    subtitle = "Principales razones declaradas",
    caption = "Fuente: ENIEF 2023 | Estimación Nacional.",
    x = NULL, y = NULL, fill = "Tipo de Barrera"
  ) +
  theme_lm(grid = "x")

# Guardar RDS
saveRDS(g_razones_rank, "rds/grafico_razones_rank.rds")

# ==============================================================================
# 9. GRÁFICO: PARADOJA DEL CONTROL (EDUCACIÓN)
# ==============================================================================
df_control <- diseno_enief |> 
  filter(!is.na(Educacion), !is.na(P3_18_5)) |> 
  group_by(Educacion) |> 
  summarise(
    Pct_Control = survey_mean(P3_18_5, na.rm=T)
  ) |> 
  mutate(Pct = Pct_Control * 100)

g_control_educ <- ggplot(df_control, aes(x = Educacion, y = Pct)) +
  geom_segment(aes(x = Educacion, xend = Educacion, y = 0, yend = Pct), 
               color = pal$plomo, linetype = "dotted") +
  geom_point(size = 6, aes(color = Educacion)) +
  geom_text(aes(label = paste0(round(Pct, 1), "%")), 
            vjust = -1.5, fontface = "bold", color = pal$texto) +
  
  # Anotación
  annotate("text", x = 2.5, y = 38, 
           label = "¡La paradoja!\nLos más educados son los que más\nbuscan 'auto-controlarse'\ncon efectivo.", 
           color = pal$texto_soft, size = 3.5, fontface = "italic", hjust = 0.5) +
  
  scale_color_manual(values = c("Básica/Ninguna" = pal$plomo, 
                                "Media" = pal$oliva, 
                                "Superior" = pal$terracota)) +
  scale_y_continuous(limits = c(0, 45)) +
  labs(
    title = "El efectivo como herramienta de disciplina",
    subtitle = "% de personas que usan efectivo para tener 'Mejor Control de Gastos'",
    caption = "Fuente: ENIEF 2023 | Análisis por nivel educativo.",
    x = NULL, y = NULL
  ) +
  theme_lm() +
  theme(legend.position = "none")

# Guardar RDS
saveRDS(g_control_educ, "rds/grafico_control_educ.rds")