source("data-model-prep.R")

# Across f2 = 0, 0.1, ... 1.0:
sd_strength <- seq(0, 1, 0.2) %>% purrr::set_names()
m_fs <- purrr::map(sd_strength, ~ {
  fit_seeiqr(
    daily_diffs,
    sampFrac2_type = "fixed",
    fixed_f_forecast = .x,
    seeiqr_model = seeiqr_model, chains = 8, iter = 2000
  )
})

# fewer for plot:
m_fs_states <- purrr::map(sd_strength, ~ {
  fit_seeiqr(
    daily_diffs,
    sampFrac2_type = "fixed",
    fixed_f_forecast = .x,
    seeiqr_model = seeiqr_model, chains = 8, iter = 300,
    save_state_predictions = TRUE
  )
})
