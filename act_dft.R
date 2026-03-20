library(tidyverse)
library(compmus)
pata_pata <- read_csv("/Users/justin_v_egmond/Comp_music/dat/pata-pata-novelty.csv")
pata_pata |>
  ggplot(aes(x = TIME, y = VALUE)) +
  geom_line() +
  xlim(0, 30) +                         # Adjust the limits as desired
  theme_minimal() +
  labs(x = "Time (s)", y = "Novelty")
fools_tempo <- read_csv("/Users/justin_v_egmond/Comp_music/dat/fools_niall_tempo.csv")
fools_tempo |>
  ggplot(aes(x = TIME, y = VALUE)) +
  geom_line() +
  xlim(0, 30) +                         # Adjust the limits as desired
  theme_minimal() +
  labs(x = "Time (s)", y = "Novelty")

graveola_act <- read_csv("/Users/justin_v_egmond/Comp_music/dat/graveola-act.csv")
graveola_dft <- read_csv("/Users/justin_v_egmond/Comp_music/dat/graveola-dft.csv")
graveola_act |> 
  pivot_longer(-TIME, names_to = "tempo") |> 
  mutate(tempo = as.numeric(tempo)) |> 
  ggplot(aes(x = TIME, y = tempo, fill = value)) +
  geom_raster() +
  scale_y_continuous(transform = c("reciprocal", "reverse"), breaks = seq(50, 350, 100)) +    
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()
graveola_dft |> 
  pivot_longer(-TIME, names_to = "tempo") |> 
  mutate(tempo = as.numeric(tempo)) |> 
  ggplot(aes(x = TIME, y = tempo, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()

fools_act <- read_csv("/Users/justin_v_egmond/Comp_music/dat/niall_act.csv")
fools_dft <- read_csv("/Users/justin_v_egmond/Comp_music/dat/niall_dft.csv")
fools_act |> 
  pivot_longer(-TIME, names_to = "tempo") |> 
  mutate(tempo = as.numeric(tempo)) |> 
  ggplot(aes(x = TIME, y = tempo, fill = value)) +
  geom_raster() +
  scale_y_continuous(transform = c("reciprocal", "reverse"), breaks = seq(50, 350, 100)) +    
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()
fools_dft |> 
  pivot_longer(-TIME, names_to = "tempo") |> 
  mutate(tempo = as.numeric(tempo)) |> 
  ggplot(aes(x = TIME, y = tempo, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()
