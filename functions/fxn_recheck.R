# special function for recheck data
fxn_recheck_data <- function(data = time_to_lame, 
                                 censor_days = censordat,
                                 disease = rechecked,
                                 years,
                                 life_x_dz = life_times_lame) {
  data |> 
    # reduce data
    lazy_dt() |> 
    select(farm, id_animal, year, {{censor_days}}, 
           {{life_x_dz}}, {{disease}}
    ) |>  
    filter(year == {{years}}) |>
    # to filter out lesions this year
    # this allows it function better for graphing function
    mutate (lesion = {{ disease }},
            ## this as.numeric creates NA's not sure why as it works without function
            censor_time = as.numeric({{censor_days}}),
            # create variables to condition on
            life_x_disease = case_when({{ life_x_dz }} == 1 ~ 1,
                                       {{ life_x_dz }} == 2 ~ 2,
                                       {{ life_x_dz }} > 2 ~ 3,
                                       TRUE ~ NA),
            life_x_disease_cat = case_when(life_x_disease == 1 ~ 
                                             "Once",
                                           life_x_disease == 2 ~ 
                                             "Twice",
                                           life_x_disease == 3 ~ 
                                             "3 or more times",
                                           TRUE ~ NA)) |> 
    filter(!is.na(life_x_disease)) |> 
    mutate(life_x_disease_cat = fct_reorder(life_x_disease_cat, 
                                            life_x_disease,
                                            .na_rm = TRUE)
    ) |> 
    # needs to be dataframe due to surv below otherwise factors get messed up
    as.data.frame()
}


# examples
# test_next_lame <- time_to_lame |>
#   # set up data
#   fxn_next_lame_date("noninf") |>
#   # create censor variable
#   fxn_lesion_censor(lesion_var = "noninf") |>
#   fxn_next_lesion_data(disease = noninf,
#                        life_x_dz = lifexnoninf,
#                        years = 2024,
#   )
