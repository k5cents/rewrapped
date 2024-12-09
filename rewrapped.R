# load necessary libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(extrafont)
  library(forcats)
  library(fs)
  library(ggplot2)
  library(ggtext)
  library(glue)
  library(httr2)
  library(jsonlite)
  library(lubridate)
  library(patchwork)
  library(purrr)
  library(ragg)
  library(readr)
  library(scales)
  library(stringr)
  library(systemfonts)
  library(tidyr)
})

# fonts -------------------------------------------------------------------

# font_import(
#   paths = "data/",
#   prompt = FALSE
# )

spotify_mix <- filter(
  system_fonts(),
  str_detect(family, "Spotify")
)


# functions ---------------------------------------------------------------

most_common <- function(x, n = 6) {
  as.vector(na.omit(names(sort(table(x), decreasing = TRUE)[1:n])))
}

# ggplot2 -----------------------------------------------------------------

theme_spotify <- function(...) {
  theme_classic() +
    theme(
      ...,
      
    )
}

scale_x_flip <- function(..., limits = rev) {
  scale_x_discrete(..., limits = limits)
}

save_plot <- function(plot, filename, height = 5, width = 9, dpi = 300, ...) {
  ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    ...
  )
}

# objects -----------------------------------------------------------------

spotify_green <- "#1ED760"
spotify_black <- "#121212"
spotify_white <- "#FFFFFF"

# read history ------------------------------------------------------------

# list all files in the downloaded "extended streaming history" data
hist_json <- dir_ls("data/", regexp = "Streaming_History_Audio_.*\\.json")

# read all files into a single data frame
hist_dat <- map_df(hist_json, ~as_tibble(read_json(., simplify = TRUE)))

# extract track IDs from track URI
track_ids <- hist_dat %>% 
  filter(is.na(episode_name)) %>% 
  count(spotify_track_uri, sort = TRUE) %>% 
  filter(n > 1) %>% 
  pull(spotify_track_uri) %>% 
  str_remove("^spotify\\:track\\:") %>% 
  unique()

# request features --------------------------------------------------------

#### NOTE: The audio-features API endpoint was deprecated on 2024-11-27
#### https://developer.spotify.com/blog/2024-11-27-changes-to-the-web-api

# this file has been combined from pre-11/27 queries and public datasets
track_features <- read_csv(
  file = "data/track_features.csv"
)

# # spotify API credentials (replace with your actual credentials)
# client_id <- ""
# client_secret <- ""
# 
# # obtain access token
# access_token <-
#   request("https://accounts.spotify.com/api/token") %>%
#   req_body_form(grant_type = "client_credentials") %>%
#   req_auth_basic(client_id, client_secret) %>%
#   req_perform() %>%
#   resp_body_json() %>%
#   pluck("access_token")
# 
# # directory to store track information locally
# local_data_dir <- dir_create("~/Documents/spotify_track_data")
# 
# # process each track ID
# pb <- txtProgressBar(max = length(track_ids), style = 3)
# for (i in seq_along(track_ids)) {
#   track_id <- track_ids[i]
#   track_path <- path(local_data_dir, path_ext_set(track_id, "json"))
# 
#   # skip if the file already exists
#   if (file_exists(track_path)) {
#     next
#   }
# 
#   # query the API for this track
#   track_req <-
#     request("https://api.spotify.com/v1/audio-features") %>%
#     req_url_query(ids = track_id) %>%
#     req_auth_bearer_token(access_token) %>%
#     req_retry(max_tries = 3) %>%
#     req_error(is_error = function(resp) FALSE)
# 
#   resp <- req_perform(track_req, path = track_path)
# 
#   if (resp_status(resp) == 429) {
#     file_delete(track_path)
#     break
#   }
# 
#   # rate limit: 5 requests per second
#   Sys.sleep(runif(1, 0.5, 1.5))
#   setTxtProgressBar(pb, value = i)
# }
# 
# # combine all results into a single data frame
# track_json <- dir_ls(local_data_dir)
# track_dat <- map_df(track_json, ~read_json(., simplifyVector = TRUE)[[1]])
# 
# track_dat <- track_dat %>%
#   as_tibble() %>%
#   select(
#     -id,
#     -track_href,
#     -analysis_url,
#     -type,
#     -status
#   ) %>%
#   relocate(uri, .before = 1) %>%
#   mutate(
#     duration = duration_ms / 6e4,
#     .keep = "unused"
#   ) %>%
#   distinct()
# 
# # save the combined data frame to a CSV file
# write_csv(track_dat, file = "~/Documents/track_features.csv", na = "")

# clean history -----------------------------------------------------------

# format and select columns
dat <- hist_dat %>% 
  # remove podcast episodes
  filter(
    is.na(episode_name) | is.na(episode_show_name)
  ) %>% 
  mutate(
    ts = as_datetime(ts),
    min = ms_played / 6e4
  ) %>% 
  select(
    ts,
    min,
    platform,
    uri = spotify_track_uri,
    track = master_metadata_track_name,
    album = master_metadata_album_album_name,
    artist = master_metadata_album_artist_name,
    reason_start,
    reason_end
  )

stopifnot(all(complete.cases(dat)))

# clean up platform
dat <- dat %>% 
  mutate(
    platform = platform %>% 
      str_to_lower() %>% 
      str_remove("partner windows_tv microsoft;") %>% 
      str_remove("_") %>% 
      str_remove("[:punct:].*$") %>% 
      str_replace("os x", "macos") %>% 
      word(1) %>% 
      str_replace("xbox.*", "xbox") %>% 
      str_replace("partner", "cast")
  )

dat %>% 
  count(platform, sort = TRUE)

# join the track data
dat <- left_join(
  x = dat,
  y = track_features,
  by = "uri"
)

dat <- dat %>% 
  filter(year(ts) == 2024)

# visualize ---------------------------------------------------------------

plot_dir <- dir_create("plots")

## artist all bar ---------------------------------------------------------

plot_artist_all_bar <- dat %>% 
  group_by(artist = str_to_title(artist)) %>% 
  summarise(
    min = sum(min),
    lbl = round(min)
  ) %>% 
  arrange(desc(min)) %>% 
  head(10) %>% 
  ungroup() %>% 
  mutate(
    artist = as_factor(artist),
    hjust = ifelse(row_number() == 1, 1.25, -0.25)
  ) %>% 
  ggplot(
    mapping = aes(
      x = reorder(artist, min), 
      y = min,
    )
  ) + 
  geom_col(
    color = spotify_black, 
    mapping = aes(
      fill = min
    )
  ) + 
  geom_text(
    family = "Spotify Mix",
    mapping = aes(
      label = lbl,
      hjust = hjust
    )
  ) +
  scale_fill_gradient(
    low = spotify_black,
    high = spotify_green
  ) +
  scale_y_continuous(
    expand = expansion(), 
    n.breaks = 12
  ) +
  coord_flip() + 
  labs(
    title = "Most Played Artists (2024)",
    y = "Minutes Played",
    x = NULL, 
    fill = NULL
  ) + 
  theme_classic() +
  theme(
    text = element_text(family = "Spotify Mix"),
    plot.title = element_text(face = "bold", size = 20),
    legend.position = "none",
    axis.title.x = element_text(margin = margin(t = 10))
  )

plot_artist_all_bar

save_plot(
  plot = plot_artist_all_bar,
  filename = "plots/01_artist__all_bar.png"
)

## album all bar ----------------------------------------------------------

plot_album_all_bar <- dat %>% 
  mutate(
    album = album %>% 
      str_remove("\\((.*)\\)") %>% 
      str_remove("- Deluxe Edition") %>% 
      str_trim() %>% 
      str_to_title() %>% 
      str_replace("^Am$", "AM")
  ) %>% 
  group_by(artist, album) %>% 
  summarise(
    min = sum(min),
    lbl = round(min)
  ) %>% 
  arrange(desc(min)) %>% 
  head(10) %>% 
  ungroup() %>% 
  mutate(
    album = glue("{album}<br>({artist})"),
    album = as_factor(album),
    hjust = ifelse(row_number() <= 1, 1.25, -0.25)
  ) %>% 
  ggplot(
    mapping = aes(
      x = reorder(album, min), 
      y = min,
    )
  ) + 
  geom_col(
    color = spotify_black, 
    mapping = aes(
      fill = min
    )
  ) + 
  geom_text(
    family = "Spotify Mix",
    mapping = aes(
      label = lbl,
      hjust = hjust
    )
  ) +
  scale_fill_gradient(
    low = spotify_black,
    high = spotify_green
  ) +
  scale_y_continuous(
    expand = expansion(add = c(0, 0.5)), 
    n.breaks = 12
  ) +
  coord_flip() + 
  labs(
    title = "Most Played Albums (2024)",
    y = "Minutes Played",
    x = NULL, 
    fill = NULL
  ) + 
  theme_classic() +
  theme(
    text = element_text(family = "Spotify Mix"),
    plot.title = element_text(face = "bold", size = 20),
    legend.position = "none",
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_markdown()
  )

plot_album_all_bar

save_plot(
  plot = plot_album_all_bar,
  filename = "plots/02_album_all_bar.png"
)

## song all bar -----------------------------------------------------------

plot_track_all_bar <- dat %>% 
  group_by(artist, track) %>% 
  summarise(
    n = n(),
    lbl = round(n)
  ) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  ungroup() %>% 
  mutate(
    track = glue("{track}<br>_({artist})_"),
    track = as_factor(track),
    hjust = ifelse(row_number() <= 1, 1.25, -0.25)
  ) %>% 
  ggplot(
    mapping = aes(
      x = reorder(track, n), 
      y = n,
    )
  ) + 
  geom_col(
    color = spotify_black, 
    mapping = aes(
      fill = n
    )
  ) + 
  geom_text(
    family = "Spotify Mix",
    mapping = aes(
      label = lbl,
      hjust = hjust
    )
  ) +
  scale_fill_gradient(
    low = spotify_black,
    high = spotify_green
  ) +
  scale_y_continuous(
    expand = expansion(add = c(0, 0.5)), 
    n.breaks = 12
  ) +
  coord_flip() + 
  labs(
    title = "Most Played Tracks (2024)",
    y = "Times Played",
    x = NULL, 
    fill = NULL
  ) + 
  theme_classic() +
  theme(
    text = element_text(family = "Spotify Mix"),
    plot.title = element_text(face = "bold", size = 20),
    legend.position = "none",
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_markdown()
  )

plot_track_all_bar

save_plot(
  plot = plot_track_all_bar,
  filename = "plots/03_track_all_bar.png"
)

# time bar/polar ----------------------------------------------------------

plot_hour_all_bar <- dat %>%
  mutate(
    ts = with_tz(ts, "EST"),
    hour = hour(ts),
    year = year(ts),
  ) %>%
  group_by(year, hour) %>%
  summarise(totalMin = sum(min)) %>%
  filter(year > 2014) %>%
  complete(hour = 0:23) %>%
  ggplot(aes(x = hour, y = totalMin)) +
  geom_col(
    mapping = aes(fill = totalMin),
    color = "black"
  ) +
  scale_fill_gradient(
    low = spotify_black,
    high = spotify_green,
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = 0:23,
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    labels = label_comma(),
    n.breaks = 10
  ) +
  labs(
    title = "Listening Time By Hour (2024)",
    x = "Hour",
    y = "Minutes"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "Spotify Mix"),
    plot.title = element_text(face = "bold", size = 20),
    legend.position = "none",
    axis.title.x = element_text(margin = margin(t = 10))
  )

plot_hour_all_bar

save_plot(
  plot = plot_hour_all_bar,
  filename = "plots/04_hour_all_bar.png"
)

# -------------------------------------------------------------------------

p <- plot_artist_all_bar / 
  plot_album_all_bar / 
  plot_track_all_bar /
  plot_hour_all_bar

ggsave(
  filename = "plots/combined.png",
  plot = p,
  height = 20,
  width = 9,
  dpi = 300
)
