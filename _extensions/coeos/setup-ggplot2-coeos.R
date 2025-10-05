theme_coeos <- function(
  base_size = 11,
  base_family = "Alegreya Sans",
  header_family = NULL,
  base_line_size = base_size / 22,
  base_rect_size = base_size / 22,
  ink = "#fafafa",
  paper = "#333333",
  accent = "#7f7f7f"
) {
  systemfonts::require_font(base_family)

  half_line <- base_size / 2

  # Use header_family if specified, otherwise use base_family
  if (is.null(header_family)) {
    header_family <- base_family
  }

  ggplot2::theme(
    line = ggplot2::element_line(
      colour = ink,
      linewidth = base_line_size,
      linetype = 1,
      lineend = "butt",
      linejoin = "round"
    ),
    rect = ggplot2::element_rect(
      fill = paper,
      colour = ink,
      linewidth = base_rect_size,
      linetype = 1,
      linejoin = "round"
    ),
    text = ggplot2::element_text(
      family = base_family,
      face = "plain",
      colour = ink,
      size = base_size,
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = ggplot2::margin(),
      debug = FALSE
    ),
    title = ggplot2::element_text(family = header_family),
    spacing = ggplot2::unit(half_line, "pt"),
    margins = ggplot2::margin_auto(half_line),
    point = ggplot2::element_point(
      colour = ink,
      shape = 19,
      fill = paper,
      size = (base_size / 11) * 1.5,
      stroke = base_line_size
    ),
    polygon = ggplot2::element_polygon(
      fill = paper,
      colour = ink,
      linewidth = base_rect_size,
      linetype = 1,
      linejoin = "round"
    ),
    geom = element_geom(
      ink = ink,
      paper = paper,
      accent = accent,
      linewidth = base_line_size,
      borderwidth = base_line_size,
      linetype = 1L,
      bordertype = 1L,
      family = base_family,
      fontsize = base_size,
      pointsize = (base_size / 11) * 1.5,
      pointshape = 19
    ),
    axis.line = ggplot2::element_blank(),
    axis.line.x = NULL,
    axis.line.x.top = NULL,
    axis.line.x.bottom = NULL,
    axis.line.y = NULL,
    axis.line.y.left = NULL,
    axis.line.y.right = NULL,
    axis.text = ggplot2::element_text(size = ggplot2::rel(0.8), colour = ink),
    axis.text.x = ggplot2::element_text(
      margin = ggplot2::margin(t = 0.8 * half_line / 2),
      vjust = 1
    ),
    axis.text.x.top = ggplot2::element_text(
      margin = ggplot2::margin(b = 0.8 * half_line / 2),
      vjust = 0
    ),
    axis.text.x.bottom = NULL,
    axis.text.y = ggplot2::element_text(
      margin = ggplot2::margin(r = 0.8 * half_line / 2),
      hjust = 1
    ),
    axis.text.y.left = NULL,
    axis.text.y.right = ggplot2::element_text(
      margin = ggplot2::margin(l = 0.8 * half_line / 2),
      hjust = 0
    ),
    axis.text.r = ggplot2::element_text(
      margin = ggplot2::margin(l = 0.8 * half_line / 2),
      hjust = 0.5
    ),
    axis.ticks = ggplot2::element_line(colour = ink),
    axis.ticks.x = NULL,
    axis.ticks.x.top = NULL,
    axis.ticks.x.bottom = NULL,
    axis.ticks.y = NULL,
    axis.ticks.y.left = NULL,
    axis.ticks.y.right = NULL,
    axis.ticks.length = ggplot2::rel(0.5),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.minor.ticks.length = ggplot2::rel(0.75),
    axis.title = NULL,
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = half_line),
      vjust = 1
    ),
    axis.title.x.top = ggplot2::element_text(
      margin = ggplot2::margin(b = half_line),
      vjust = 0
    ),
    axis.title.x.bottom = NULL,
    axis.title.y = ggplot2::element_text(
      angle = 90,
      margin = ggplot2::margin(r = half_line),
      vjust = 1
    ),
    axis.title.y.left = NULL,
    axis.title.y.right = ggplot2::element_text(
      angle = -90,
      margin = ggplot2::margin(l = half_line),
      vjust = 0
    ),
    legend.background = ggplot2::element_rect(fill = paper, colour = NA),
    legend.spacing = ggplot2::rel(2),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = ggplot2::margin_auto(half_line),
    legend.key = ggplot2::element_rect(fill = paper, colour = ink),
    legend.key.size = ggplot2::unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.key.spacing = ggplot2::unit(half_line / 4, "pt"),
    legend.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
    legend.text.align = NULL,
    legend.title = ggplot2::element_text(hjust = 0),
    legend.title.align = NULL,
    legend.ticks.length = ggplot2::rel(0.2),
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.just = NULL,
    legend.box.margin = ggplot2::margin_auto(0),
    legend.box.background = ggplot2::element_blank(),
    legend.box.spacing = ggplot2::unit(2 * half_line, "pt"),
    panel.background = ggplot2::element_rect(fill = paper, colour = NA),
    panel.border = ggplot2::element_rect(
      fill = NA,
      colour = ink,
      linewidth = 0.5,
      linetype = "solid"
    ),
    panel.spacing = ggplot2::unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.grid = ggplot2::element_line(colour = accent),
    panel.grid.major = ggplot2::element_line(linewidth = ggplot2::rel(0.60)),
    panel.grid.minor = ggplot2::element_line(linewidth = ggplot2::rel(0.30)),
    panel.grid.major.x = NULL,
    panel.grid.major.y = NULL,
    panel.grid.minor.x = NULL,
    panel.grid.minor.y = NULL,
    panel.ontop = FALSE,
    strip.background = ggplot2::element_rect(fill = paper, colour = ink),
    strip.background.x = NULL,
    strip.background.y = NULL,
    strip.clip = "inherit",
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.text = ggplot2::element_text(
      colour = ink,
      size = ggplot2::rel(0.8),
      margin = ggplot2::margin_auto(0.8 * half_line)
    ),
    strip.text.x = NULL,
    strip.text.y = ggplot2::element_text(angle = -90),
    strip.text.y.left = ggplot2::element_text(angle = 90),
    strip.switch.pad.grid = ggplot2::unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = ggplot2::unit(half_line / 2, "pt"),
    plot.background = ggplot2::element_rect(colour = paper),
    plot.title = ggplot2::element_text(
      family = header_family,
      size = ggplot2::rel(1.25),
      face = "bold",
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.title.position = "plot",
    plot.subtitle = ggplot2::element_text(
      family = header_family,
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
    plot.tag = ggplot2::element_text(
      size = ggplot2::rel(1.25),
      hjust = 0.5,
      vjust = 0.5
    ),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin_auto(half_line),

    palette.colour.discrete = function(...) {
      ggplot2::scale_colour_viridis_d(..., begin = 0.15, end = 0.85)
    },
    palette.fill.discrete = function(...) {
      ggplot2::scale_fill_viridis_d(..., begin = 0.15, end = 0.85)
    },
    palette.colour.continuous = function(...) {
      ggplot2::scale_colour_viridis_c(..., begin = 0.15, end = 0.85)
    },
    palette.fill.continuous = function(...) {
      ggplot2::scale_fill_viridis_c(..., begin = 0.15, end = 0.85)
    },

    complete = TRUE
  )
}

ggplot2::set_theme(theme_coeos(base_size = 18))
if (nzchar(system.file(package = "marquee"))) {
  ggplot2::update_theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = marquee::element_marquee(),
    plot.subtitle = marquee::element_marquee(
      style = marquee::classic_style(italic = TRUE)
    ),
    plot.caption = marquee::element_marquee(
      style = marquee::classic_style(italic = TRUE)
    ),
    axis.title.x = marquee::element_marquee(),
    axis.text.x = marquee::element_marquee(),
    axis.text.x.top = marquee::element_marquee(),
    axis.title.y = marquee::element_marquee(),
    axis.text.y = marquee::element_marquee()
  )
} else if (nzchar(system.file(package = "ggtext"))) {
  ggplot2::update_theme(
    plot.title = ggtext::element_markdown(),
    plot.subtitle = ggtext::element_markdown(face = "italic"),
    plot.caption = ggtext::element_markdown(face = "italic"),
    axis.title.x = ggtext::element_markdown(),
    axis.text.x = ggtext::element_markdown(),
    axis.title.y = ggtext::element_markdown(),
    axis.text.y = ggtext::element_markdown()
  )
}
