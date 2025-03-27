okabe_ito_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "grey80", "grey30", "black")

th <- ggplot2::theme_bw() +
  ggplot2::theme(panel.grid = ggplot2::element_blank())

gs <- function(gg, name, width, height) {
  ggsave(filename = file.path("fig", paste0(name, ".png")), plot = gg, device = "png",
         width = width, height = height, dpi = 300)
}

gp <- function(gg, name, width, height) {
  ggsave(filename = file.path("fig", paste0(name, ".pdf")), plot = gg, device = "pdf",
         width = width, height = height, dpi = 300)
}

scientific_10 <- function(v, limit = 3.01) {
  sc <- map_chr(v, function(x) {
    if (is.na(x)) {
      as.character(x)
    } else if (x == 0 | abs(log10(x)) < limit) {
      format(x, scientific = FALSE)
    } else {
      format(x, scientific = TRUE) |> 
        str_replace("e", "%*%10^") |> 
        str_replace("\\+", "") |> 
        str_replace("\\^0+", "^")
    }
  })
  parse(text = sc)
}


plot_time_course <- function(dat, mdl, log_data = FALSE) {
  if(log_data) {
    dat <- dat |> 
      mutate(value = log2(value))
    ylab <- expression(log[2]~Normalised~intensity)
  } else {
    ylab <- "Normalised intensity"
  }

  curves <- mdl |> 
    select(condition, fit) |> 
    distinct() |> 
    group_split(condition) |> 
    map(function(w) {
      d <- tibble(time_point = seq(0, 8, 0.1))
      y <- predict(w$fit[[1]], d)
      tibble(
        condition = w$condition,
        t = d$time_point,
        y = y
      )
    }) |> 
    list_rbind()

  dm <- dat |> 
    group_by(condition, time_point) |> 
    summarise(M = mean(value))

  w <- 0.5
  dat |> 
    ggplot(aes(x = time_point, y = value)) +
    th +
    geom_beeswarm(aes(shape = bio_rep, colour = tech_rep), size = 3, cex = 2) +
    geom_segment(data = dm, aes(x = time_point - w, xend = time_point + w, y = M, yend = M), linewidth = 1.3, colour = "brown") +
    geom_line(data = curves, aes(x = t, y = y), colour = "grey50") +
    facet_wrap(~ condition, ncol = 1) +
    scale_colour_manual(values = okabe_ito_palette) +
    scale_x_continuous(breaks = c(0, 1, 2, 4, 6, 8)) +
    labs(x = "Time point (h)", y = ylab)
}

plot_tukey <- function(tukey, p_limit = 0.05) {
  tukey |> 
    mutate(sig = if_else(`p adj` < p_limit, "x", " ")) |> 
    ggplot(aes(x = tp_1, y = tp_2, fill = diff)) +
    th +
    geom_tile() +
    geom_text(aes(label = sig)) +
    scale_fill_viridis_c(option = "cividis") +
    facet_wrap(~ condition)
}


plot_compare <- function(cmp, fdr_limit = 0.05) {
  cmp |> 
    mutate(sig = p_adj < fdr_limit) |> 
    ggplot(aes(x = time_point, y = slope, ymin = conf.low, ymax = conf.high, colour = sig)) +
    th +
    theme(legend.position = "none") +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
    geom_point() +
    geom_errorbar(width = 0.2) +
    scale_colour_manual(values = c("grey70", "black")) +
    labs(x = "Time point (h)", y = "Difference")
}