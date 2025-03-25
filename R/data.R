read_sheet <- function(fname, sheet) {
  read_excel(fname, sheet = sheet) |> 
    pivot_longer(-time_point) |> 
    separate_wider_position(name, c(tech_rep = 1, bio_rep = 1)) |>
    add_column(condition = sheet)
}

read_data <- function(fname) {
  bind_rows(
    read_sheet(fname, "WT"),
    read_sheet(fname, "S2513A"),
    read_sheet(fname, "WIBJ2")
  ) |> 
    mutate(value = log2(value))
}

do_anova <- function(dat) {
  conds <- dat$condition |> unique()
  map(conds, function(cond) {
    d <- dat |> 
      filter(condition == cond) |> 
      mutate(time_point = time_point |> as.character() |> as.factor())
    anv <- aov(value ~ time_point, data = d)
    TukeyHSD(anv) |> 
      pluck("time_point") |> 
      as_tibble(rownames = "contrast") |> 
      add_column(condition = cond)
  }) |> 
    list_rbind() |> 
    separate_wider_delim(contrast, delim = "-", names = c("tp_1", "tp_2"))
}


compare_conditions <- function(dat, conds = c("WT", "S2513A")) {
  d <- dat |> 
    filter(condition %in% conds) |>
    select(time_point, condition, value) 

  # Find time poins with both data available
  good_tp <- d |>
    select(time_point, condition) |> 
    distinct() |> 
    count(time_point) |> 
    filter(n == 2) |> 
    pull(time_point)

  d |> 
    filter(time_point %in% good_tp) |> 
    nest(data = c(condition, value)) |> 
    mutate(
      tst = map(data, ~t.test(value ~ condition, data = .x)),
      tidied = map(tst, tidy)
    ) |> 
    select(-c(data, tst)) |> 
    unnest(tidied) |> 
    mutate(p_adj = p.adjust(p.value, method = "BH"))
}

fit_halflife <- function(dat, time_sel = NULL) {
  if(is.null(time_sel)) {
    d <- dat
  } else {
    d <- dat |> 
      filter(time_point %in% time_sel)
  }

  d |> 
    select(time_point, condition, value) |> 
    nest(data = c(time_point, value)) |> 
    mutate(
      fit = map(data, ~lm(value ~ 0 + time_point, data = .x)),
      tidied = map(fit, tidy)
    ) |> 
    select(-c(data, fit)) |> 
    unnest(tidied) |> 
    rename(
      slope = estimate,
      std_slope = std.error
    ) |> 
    mutate(
      t_half = -1 / slope,
      std_t_half = std_slope / (slope^2)
    )
}