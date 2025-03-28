targets_main <- function() {

  main <- tar_plan(
    dat = read_data("data/stats NICD half life.xlsx"),
    tukey = do_anova(dat),
    lin = fit_halflife(dat, time_sel = c(0, 1, 2, 4)),
    mdl = fit_exp(dat, time_sel = c(0, 1, 2, 4)),
    mdl_half = find_half_from_exp(mdl),
    mdl_b = fit_exp_b(dat),
    mdl_b_half = find_half_from_exp_b(mdl_b),
    cmp_wt_s2513a = compare_conditions(dat, conds = c("WT", "S2513A")),
    cmp_wt_wibj2 = compare_conditions(dat, conds = c("WT", "WIBJ2")),

    fig_dat_exp = plot_time_course(dat, mdl, time_limit = c(0, 4)),
    fig_dat_exp_b = plot_time_course(dat, mdl_b),
    fig_dat_lin = plot_time_course(dat, lin, log_data = TRUE),
    fig_tukey = plot_tukey(tukey),
    fig_wt_s2513a = plot_compare(cmp_wt_s2513a),
    fig_wt_wibj2 = plot_compare(cmp_wt_wibj2)
  )

  c(
    main
  )

}