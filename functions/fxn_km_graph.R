# recheck graph

fxn_km_graph <- function(data,
                         censor_time = days_to_censor_trim,
                         censor_event = rechecked,
                         facet_col = farm) {
  data_surv <- data |>
    mutate(
      surv_object = Surv(
        time = {{ censor_time }},
        event = {{ censor_event }}
      ),
      facet_col = factor({{ facet_col }}),
      # chronicity = fct_relevel(chronicity,
      #                          c("No Lesion",
      #                            "First Lesion",
      #                            "History Lesion"
      #                          )
      #                          )
    )

  # Calculate the number of facets based on 'facet.by'
  num_facets2 <- length(unique(data_surv$facet_col))

  # Define the number of groups per facet
  num_groups <- length(unique(data_surv$chronicity))

  # Generate the linetype vector dynamically
  linetype_vector <- rep(1:5,
    each = num_groups,
    length.out = num_groups * num_facets2
  )

  fit_km <- survfit(surv_object ~ chronicity, data = data_surv)

  fit_km |>
    ggsurvplot(
      # needed data statement as extracts see help and without it doesn't work
      data = data_surv,
      facet.by = "facet_col",
      ncol = 4,
      pval = FALSE,
      conf.int = TRUE,
      censor = FALSE,
      fun = "pct",
      size = 1,
      palette = NULL,
      legend.title = "Lesion History",
      xlab = x_label,
      ylab = y_label,
      xlim = c(0, 180),
      ylim = c(0, 100),
      break.time.by = 30,
      short.panel.labs = TRUE
    ) +
    theme(legend.position = "bottom") +
    guides(linetype = "none") + # Remove the linetype leg
    scale_colour_manual(values = life_colours, breaks = force_order) +
    scale_fill_manual(values = life_colours, breaks = force_order)
}
