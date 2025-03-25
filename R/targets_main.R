targets_main <- function() {

  main <- tar_plan(
    dat = read_data("data/stats NICD half life.xlsx"),
    tukey = do_anova(dat),
    half = fit_halflife(dat, time_sel = c(0, 1, 2, 4)),
    cmp_wt_s2513a = compare_conditions(dat, conds = c("WT", "S2513A")),
    cmp_wt_wibj2 = compare_conditions(dat, conds = c("WT", "WIBJ2")),

    fig_dat = plot_time_course(dat, half),
    fig_tukey = plot_tukey(tukey),
    fig_wt_s2513a = plot_compare(cmp_wt_s2513a),
    fig_wt_wibj2 = plot_compare(cmp_wt_wibj2)
  )

  c(
    main
  )

}