# ==============================================================================
# GRAFICOS STORYTELLING - LEONARDO MENA
# ==============================================================================
#
# Este archivo extiende tema_graficos.R con graficos editoriales reutilizables.
# Usa ggplot2 y estructuras simples para que cada grafico sea facil de adaptar.
#
# Convencion:
# - data: data.frame
# - columnas: se pasan como texto, ej. category = "sector"
# - salida: ggplot normal, compatible con + labs(), + theme(), ggsave(), etc.
#
# Requiere que tema_graficos.R ya haya definido:
# - pal
# - theme_lm()
# - scale_color_lm()
# - scale_fill_lm()

lm_stop_missing <- function(package) {
  stop(
    "Falta el paquete '", package, "'. Instala con install.packages('", package, "').",
    call. = FALSE
  )
}

lm_require_columns <- function(data, columns) {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0) {
    stop("Faltan columnas: ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

lm_palette_story <- function(n = 6) {
  base <- c(
    pal$terracota,
    pal$plomo,
    pal$oliva,
    pal$ocre,
    "#4f789f",
    "#8c6f9f",
    "#d9c896",
    "#7fb0b8"
  )
  rep(base, length.out = n)
}

lm_caption <- function(source, cut = NULL, note = NULL) {
  parts <- c(
    if (!is.null(source) && nzchar(source)) paste0("Fuente: ", source),
    if (!is.null(cut) && nzchar(cut)) paste0("Corte: ", cut),
    if (!is.null(note) && nzchar(note)) note
  )
  paste(parts, collapse = " - ")
}

lm_export_plot <- function(plot, path, width = 9, height = 5.4, dpi = 320, bg = pal$crema) {
  ggplot2::ggsave(
    filename = path,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = bg
  )
  invisible(path)
}

# ------------------------------------------------------------------------------
# 1. Waffle / pictograma
# ------------------------------------------------------------------------------

lm_waffle <- function(data,
                      category,
                      value,
                      total = 100,
                      rows = 10,
                      title = NULL,
                      subtitle = NULL,
                      caption = NULL,
                      show_legend = TRUE) {
  lm_require_columns(data, c(category, value))

  d <- data[, c(category, value)]
  names(d) <- c("category", "value")
  d <- d[order(d$value, decreasing = TRUE), ]
  d$value <- as.numeric(d$value)

  if (any(d$value < 0, na.rm = TRUE)) {
    stop("lm_waffle() no acepta valores negativos.", call. = FALSE)
  }

  units <- round(d$value / sum(d$value, na.rm = TRUE) * total)
  diff_total <- total - sum(units)
  if (length(units) > 0 && diff_total != 0) units[which.max(units)] <- units[which.max(units)] + diff_total

  tile_category <- rep(d$category, pmax(units, 0))
  tile <- data.frame(
    id = seq_len(total),
    category = factor(tile_category, levels = d$category)
  )
  tile$x <- ((tile$id - 1) %% ceiling(total / rows)) + 1
  tile$y <- rows - floor((tile$id - 1) / ceiling(total / rows))

  ggplot2::ggplot(tile, ggplot2::aes(x, y, fill = category)) +
    ggplot2::geom_tile(color = pal$crema, linewidth = 0.9, width = 0.92, height = 0.92) +
    ggplot2::coord_equal() +
    ggplot2::scale_fill_manual(values = lm_palette_story(length(levels(tile$category))), name = NULL) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption, x = NULL, y = NULL) +
    theme_lm(grid = "n") +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = pal$crema, color = NA),
      legend.position = if (show_legend) "top" else "none"
    )
}

# ------------------------------------------------------------------------------
# 2. Marimekko / mosaico proporcional
# ------------------------------------------------------------------------------

lm_marimekko <- function(data,
                         x,
                         fill,
                         value,
                         title = NULL,
                         subtitle = NULL,
                         caption = NULL,
                         label_min = 0.08) {
  lm_require_columns(data, c(x, fill, value))

  d <- data[, c(x, fill, value)]
  names(d) <- c("x_group", "fill_group", "value")
  d$value <- as.numeric(d$value)
  d <- d[d$value > 0, ]

  x_totals <- aggregate(value ~ x_group, d, sum)
  x_totals$x_share <- x_totals$value / sum(x_totals$value)
  x_totals$xmin <- c(0, head(cumsum(x_totals$x_share), -1))
  x_totals$xmax <- cumsum(x_totals$x_share)

  d <- merge(d, x_totals[, c("x_group", "xmin", "xmax")], by = "x_group", sort = FALSE)
  d <- d[order(d$xmin, d$fill_group), ]
  d$share_y <- ave(d$value, d$x_group, FUN = function(v) v / sum(v))
  d$ymax <- ave(d$share_y, d$x_group, FUN = cumsum)
  d$ymin <- d$ymax - d$share_y
  d$label <- ifelse(d$share_y >= label_min, scales::percent(d$share_y, accuracy = 1), "")

  ggplot2::ggplot(d) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill_group),
      color = pal$crema,
      linewidth = 0.8
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = label),
      color = pal$texto,
      size = 3.2,
      fontface = "bold"
    ) +
    ggplot2::scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = lm_palette_story(length(unique(d$fill_group))), name = NULL) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption, x = NULL, y = NULL) +
    theme_lm(grid = "n") +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = pal$blanco, color = pal$border_dark))
}

# ------------------------------------------------------------------------------
# 3. Slopegraph
# ------------------------------------------------------------------------------

lm_slopegraph <- function(data,
                          entity,
                          period,
                          value,
                          title = NULL,
                          subtitle = NULL,
                          caption = NULL,
                          label_values = TRUE,
                          label_start = FALSE) {
  lm_require_columns(data, c(entity, period, value))

  d <- data[, c(entity, period, value)]
  names(d) <- c("entity", "period", "value")
  d$value <- as.numeric(d$value)
  d$period <- factor(d$period, levels = unique(d$period))

  label_periods <- tail(levels(d$period), 1)
  if (isTRUE(label_start)) label_periods <- c(levels(d$period)[1], label_periods)
  endpoints <- d[d$period %in% label_periods, ]
  endpoints$hjust <- ifelse(endpoints$period == levels(d$period)[1], 1.08, -0.08)
  endpoints$label <- if (label_values) {
    paste0(endpoints$entity, "  ", scales::comma(endpoints$value))
  } else {
    endpoints$entity
  }

  ggplot2::ggplot(d, ggplot2::aes(period, value, group = entity, color = entity)) +
    ggplot2::geom_line(linewidth = 1.05, alpha = 0.9) +
    ggplot2::geom_point(size = 3.2) +
    ggplot2::geom_text(
      data = endpoints,
      ggplot2::aes(label = label, hjust = hjust),
      color = pal$texto_soft,
      size = 3.3,
      fontface = "bold"
    ) +
    ggplot2::scale_color_manual(values = lm_palette_story(length(unique(d$entity))), guide = "none") +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption, x = NULL, y = NULL) +
    ggplot2::coord_cartesian(clip = "off") +
    theme_lm(grid = "y") +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(15, 65, 15, 65)
    )
}

# ------------------------------------------------------------------------------
# 4. Ridgeline / densidades apiladas sin dependencia externa
# ------------------------------------------------------------------------------

lm_ridgeline <- function(data,
                         group,
                         value,
                         title = NULL,
                         subtitle = NULL,
                         caption = NULL,
                         bandwidth = NULL,
                         scale = 0.85) {
  lm_require_columns(data, c(group, value))

  d <- data[, c(group, value)]
  names(d) <- c("group", "value")
  d$value <- as.numeric(d$value)
  d <- d[is.finite(d$value), ]
  groups <- unique(d$group)

  density_rows <- lapply(seq_along(groups), function(i) {
    vals <- d$value[d$group == groups[i]]
    if (length(unique(vals)) < 2) return(NULL)
    den <- stats::density(vals, bw = bandwidth %||% "nrd0", na.rm = TRUE)
    y_scaled <- den$y / max(den$y, na.rm = TRUE) * scale
    data.frame(
      group = groups[i],
      x = den$x,
      ymin = i,
      ymax = i + y_scaled,
      yline = i + y_scaled
    )
  })
  ridges <- do.call(rbind, density_rows)
  ridges$group <- factor(ridges$group, levels = groups)

  ggplot2::ggplot(ridges, ggplot2::aes(x = x, group = group, fill = group)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = ymin, ymax = ymax), alpha = 0.72, color = NA) +
    ggplot2::geom_line(ggplot2::aes(y = yline), color = pal$texto, linewidth = 0.7) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(groups),
      labels = groups,
      expand = ggplot2::expansion(mult = c(0.02, 0.12))
    ) +
    ggplot2::scale_fill_manual(values = lm_palette_story(length(groups)), guide = "none") +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption, x = NULL, y = NULL) +
    theme_lm(grid = "x") +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = pal$blanco, color = NA),
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = pal$border, linewidth = 0.35)
    )
}

# ------------------------------------------------------------------------------
# 5. Sankey / alluvial
# ------------------------------------------------------------------------------

lm_sankey <- function(data,
                      axis1,
                      axis2,
                      value,
                      axis3 = NULL,
                      title = NULL,
                      subtitle = NULL,
                      caption = NULL) {
  axis_cols <- c(axis1, axis2, axis3)
  axis_cols <- axis_cols[!is.null(axis_cols)]
  lm_require_columns(data, c(axis_cols, value))

  if (!requireNamespace("ggalluvial", quietly = TRUE)) {
    lm_stop_missing("ggalluvial")
  }

  d <- data[, c(axis_cols, value)]
  names(d) <- c(paste0("axis", seq_along(axis_cols)), "value")
  d$value <- as.numeric(d$value)

  mapping <- if (length(axis_cols) == 2) {
    ggplot2::aes(axis1 = axis1, axis2 = axis2, y = value)
  } else {
    ggplot2::aes(axis1 = axis1, axis2 = axis2, axis3 = axis3, y = value)
  }

  ggplot2::ggplot(d, mapping) +
    ggalluvial::geom_alluvium(
      ggplot2::aes(fill = axis1),
      alpha = 0.58,
      width = 0.18,
      discern = TRUE
    ) +
    ggalluvial::geom_stratum(
      width = 0.18,
      fill = pal$blanco,
      color = pal$border_dark,
      discern = TRUE
    ) +
    ggalluvial::stat_stratum(
      geom = "text",
      ggplot2::aes(label = ggplot2::after_stat(stratum)),
      size = 3.2,
      color = pal$texto,
      fontface = "bold",
      discern = TRUE
    ) +
    ggplot2::scale_x_discrete(limits = axis_cols, expand = c(0.08, 0.08)) +
    ggplot2::scale_fill_manual(values = lm_palette_story(length(unique(d$axis1))), guide = "none") +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption, x = NULL, y = NULL) +
    theme_lm(grid = "n") +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = pal$crema, color = NA)
    )
}

# ------------------------------------------------------------------------------
# 6. Dumbbell / comparacion entre dos puntos
# ------------------------------------------------------------------------------

lm_dumbbell <- function(data,
                        entity,
                        start,
                        end,
                        title = NULL,
                        subtitle = NULL,
                        caption = NULL,
                        start_label = "Antes",
                        end_label = "Despues") {
  lm_require_columns(data, c(entity, start, end))

  d <- data[, c(entity, start, end)]
  names(d) <- c("entity", "start", "end")
  d$start <- as.numeric(d$start)
  d$end <- as.numeric(d$end)
  d$entity <- stats::reorder(d$entity, d$end - d$start)

  ggplot2::ggplot(d, ggplot2::aes(y = entity)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = start, xend = end, yend = entity),
      color = pal$border_dark,
      linewidth = 1.3
    ) +
    ggplot2::geom_point(ggplot2::aes(x = start), size = 3.4, color = pal$oliva) +
    ggplot2::geom_point(ggplot2::aes(x = end), size = 3.4, color = pal$terracota) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = NULL,
      y = NULL,
      color = NULL
    ) +
    theme_lm(grid = "x") +
    ggplot2::annotate(
      "text",
      x = min(d$start, d$end, na.rm = TRUE),
      y = Inf,
      label = start_label,
      vjust = -0.7,
      hjust = 0,
      color = pal$oliva,
      fontface = "bold",
      size = 3.2
    ) +
    ggplot2::annotate(
      "text",
      x = max(d$start, d$end, na.rm = TRUE),
      y = Inf,
      label = end_label,
      vjust = -0.7,
      hjust = 1,
      color = pal$terracota,
      fontface = "bold",
      size = 3.2
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(plot.margin = ggplot2::margin(18, 18, 18, 18))
}

# ------------------------------------------------------------------------------
# 7. Layout de ficha: grafico + nota corta
# ------------------------------------------------------------------------------

lm_story_note <- function(title, body, fill = pal$gris_claro, border = pal$terracota) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "rect",
      xmin = 0,
      xmax = 1,
      ymin = 0,
      ymax = 1,
      fill = fill,
      color = border,
      linewidth = 0.5
    ) +
    ggplot2::annotate(
      "text",
      x = 0.06,
      y = 0.72,
      label = title,
      hjust = 0,
      color = pal$texto,
      fontface = "bold",
      size = 5
    ) +
    ggplot2::annotate(
      "text",
      x = 0.06,
      y = 0.43,
      label = body,
      hjust = 0,
      color = pal$texto_soft,
      size = 3.7,
      lineheight = 1.05
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    theme_lm(grid = "n") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = pal$crema, color = NA))
}
