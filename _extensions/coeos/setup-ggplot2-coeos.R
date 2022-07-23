theme_coeos <- function(
  base_size = 11,
  base_family = "",
  base_line_size = base_size / 22,
  base_rect_size = base_size / 22
) {
  bc <- c("#333333", "#7F7F7F", "#FAFAFA")
  half_line <- base_size / 2
  ggplot2::theme(
    line = ggplot2::element_line(
      colour = bc[3],
      size = base_line_size,
      linetype = 1,
      lineend = "butt"
    ),
    rect = ggplot2::element_rect(
      fill = bc[1],
      colour = bc[3],
      size = base_rect_size,
      linetype = 1
    ),
    text = ggplot2::element_text(
      family = base_family,
      face = "plain",
      colour = bc[3],
      size = base_size,
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = ggplot2::margin(),
      debug = FALSE
    ),
    title = NULL,
    aspect.ratio = NULL,

    axis.title = NULL,
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = half_line), vjust = 1),
    axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(b = half_line), vjust = 0),
    axis.title.x.bottom = NULL,
    axis.title.y = ggplot2::element_text(angle = 90, margin = ggplot2::margin(r = half_line), vjust = 1),
    axis.title.y.left = NULL,
    axis.title.y.right = ggplot2::element_text(angle = -90, margin = ggplot2::margin(l = half_line), vjust = 0),
    axis.text = ggplot2::element_text(size = ggplot2::rel(0.8), colour = bc[3]),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top = ggplot2::element_text(margin = ggplot2::margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.x.bottom = NULL,
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.text.y.left = NULL,
    axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks = ggplot2::element_line(colour = bc[3]),
    axis.ticks.x = NULL,
    axis.ticks.x.top = NULL,
    axis.ticks.x.bottom = NULL,
    axis.ticks.y = NULL,
    axis.ticks.y.left = NULL,
    axis.ticks.y.right = NULL,
    axis.ticks.length = ggplot2::unit(half_line / 2, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.line = ggplot2::element_blank(),
    axis.line.x = NULL,
    axis.line.x.top = NULL,
    axis.line.x.bottom = NULL,
    axis.line.y = NULL,
    axis.line.y.left = NULL,
    axis.line.y.right = NULL,

    legend.background = ggplot2::element_rect(fill = bc[1], colour = NA),
    legend.margin = ggplot2::margin(half_line, half_line, half_line, half_line),
    legend.spacing = ggplot2::unit(2 * half_line, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.key = ggplot2::element_rect(fill = bc[1], colour = bc[3]),
    legend.key.size = ggplot2::unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
    legend.text.align = NULL,
    legend.title = ggplot2::element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.just = NULL,
    legend.box.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
    legend.box.background = ggplot2::element_blank(),
    legend.box.spacing = ggplot2::unit(2 * half_line, "pt"),

    panel.background = ggplot2::element_rect(fill = bc[1], colour = NA),
    panel.border = ggplot2::element_rect(fill = NA, colour = bc[3], size = 0.5, linetype = "solid"),
    panel.spacing = ggplot2::unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.grid = ggplot2::element_line(colour = bc[2]),
    panel.grid.major = ggplot2::element_line(colour = bc[2], size = ggplot2::rel(0.60)),
    panel.grid.minor = ggplot2::element_line(colour = bc[2], size = ggplot2::rel(0.30)),
    panel.grid.major.x = NULL,
    panel.grid.major.y = NULL,
    panel.grid.minor.x = NULL,
    panel.grid.minor.y = NULL,
    panel.ontop = FALSE,

    plot.background = ggplot2::element_rect(colour = bc[1]),
    plot.title = ggplot2::element_text(
      size = ggplot2::rel(1.25),
      face = "bold",
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.title.position = "plot",
    plot.subtitle = ggplot2::element_text(
      size = ggplot2::rel(1),
      face = "italic",
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.caption = ggplot2::element_text(
      size = ggplot2::rel(0.75),
      face = "italic",
      hjust = 1,
      vjust = 1,
      margin = ggplot2::margin(t = half_line)
    ),
    plot.caption.position = "plot",
    plot.tag = ggplot2::element_text(size = ggplot2::rel(1.25), hjust = 0.5, vjust = 0.5),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin(half_line, half_line, half_line, half_line),

    strip.background = ggplot2::element_rect(fill = bc[1], colour = bc[3]),
    strip.background.x = NULL,
    strip.background.y = NULL,
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.text = ggplot2::element_text(
      colour = bc[3],
      size = ggplot2::rel(0.8),
      margin = ggplot2::margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)
    ),
    strip.text.x = NULL,
    strip.text.y = ggplot2::element_text(angle = -90),
    strip.switch.pad.grid = ggplot2::unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = ggplot2::unit(half_line / 2, "pt"),

    complete = TRUE
  )
}

ggplot2::theme_set(theme_coeos(base_size = 18, base_family = "Alegreya Sans"))

if (nzchar(system.file(package = "ggtext"))) {
  ggplot2::theme_update(
    plot.title = ggtext::element_markdown(),
    plot.subtitle = ggtext::element_markdown(face = "italic"),
    plot.caption = ggtext::element_markdown(face = "italic"),
    axis.title.x = ggtext::element_markdown(),
    axis.text.x = ggtext::element_markdown(),
    axis.title.y = ggtext::element_markdown(),
    axis.text.y = ggtext::element_markdown()
  )
}
ggplot2::update_geom_defaults("point", list(colour = ggplot2::theme_get()$line$colour))

options(
  ggplot2.discrete.colour = function(...) ggplot2::scale_colour_viridis_d(..., begin = 0.15, end = 0.85),
  ggplot2.discrete.fill = function(...) ggplot2::scale_fill_viridis_d(..., begin = 0.15, end = 0.85),
  ggplot2.continuous.colour = function(...) ggplot2::scale_colour_viridis_c(..., begin = 0.15, end = 0.85),
  ggplot2.continuous.fill = function(...) ggplot2::scale_fill_viridis_c(..., begin = 0.15, end = 0.85)
)
