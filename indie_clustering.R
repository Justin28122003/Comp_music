pop <- read_csv("/Users/justin_v_egmond/Comp_music/dat/indie-pop.csv", col_types = "ccccDnnlcTccnnnnnnnnnnnn")
party <- read_csv("/Users/justin_v_egmond/Comp_music/dat/indie-party.csv", col_types = "ccccDnnlcTccnnnnnnnnnnnn")
workout <- read_csv("/Users/justin_v_egmond/Comp_music/dat/indie-running.csv", col_types = "ccccDnnlcTccnnnnnnnnnnnn")
indie <-
  bind_rows(
    pop |> mutate(Playlist = "Indie Pop") |> slice_head(n = 20),
    party |> mutate(Playlist = "Indie Party") |> slice_head(n = 20),
    workout |> mutate(Playlist = "Indie Running") |> slice_head(n = 20)
  )
indie_recipe <-
  recipe(
    Playlist ~
      Danceability +
      Energy +
      Loudness +
      Speechiness +
      Acousticness +
      Instrumentalness +
      Liveness +
      Valence +
      Tempo +
      `Duration (ms)`,
    data = indie                    # Use the same name as the previous block.
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors())      # Converts to z-scores.
# step_range(all_predictors())    # Sets range to [0, 1].

indie_cv <- indie |> vfold_cv(5)

knn_model <-
  nearest_neighbor(neighbors = 1) |>
  set_mode("classification") |> 
  set_engine("kknn")
indie_knn <- 
  workflow() |> 
  add_recipe(indie_recipe) |> 
  add_model(knn_model) |> 
  fit_resamples(indie_cv, control = control_resamples(save_pred = TRUE))
indie_knn |> get_conf_mat()
indie_knn |> get_conf_mat() |> autoplot(type = "mosaic")
indie_knn |> get_conf_mat() |> autoplot(type = "heatmap")
indie_knn |> get_pr()

forest_model <-
  rand_forest() |>
  set_mode("classification") |> 
  set_engine("ranger", importance = "impurity")
indie_forest <- 
  workflow() |> 
  add_recipe(indie_recipe) |> 
  add_model(forest_model) |> 
  fit_resamples(
    indie_cv, 
    control = control_resamples(save_pred = TRUE)
  )
indie_forest |> get_pr()
workflow() |> 
  add_recipe(indie_recipe) |> 
  add_model(forest_model) |> 
  fit(indie) |> 
  pluck("fit", "fit", "fit") |>
  ranger::importance() |> 
  enframe() |> 
  mutate(name = fct_reorder(name, value)) |> 
  ggplot(aes(name, value)) + 
  geom_col() + 
  coord_flip() +
  theme_minimal() +
  labs(x = NULL, y = "Importance")
indie |>
  ggplot(aes(x = Acousticness, y = Liveness, colour = Playlist, size = Energy)) +
  geom_point(alpha = 0.8) +
  scale_color_viridis_d() +
  labs(
    x = "Acousticness",
    y = "Liveness",
    size = "Energy",
    colour = "Playlist"
  )
