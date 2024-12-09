## artist month ------------------------------------------------------------

dat %>%
  group_by(month = month(ts)) %>%
  # select top N per month
  filter(
    artist %in% most_common(artist, 3),
    year(ts) == max(year(ts)) - 1
  ) %>%
  # add Y for labels and bars
  group_by(month, artist) %>%
  summarise(
    total_min = sum(min),
    .groups = "drop_last"
  ) %>%
  mutate(
    lbl_y = cumsum(total_min),
    lbl_abb = if_else(
      total_min < 100, "", abbreviate(artist)
    )
  ) %>%
  ggplot(
    mapping = aes(x = month, y = total_min)
  ) +
  geom_col(
    mapping = aes(fill = str_trunc(artist, width = 25)),
    color = "black",
    position = position_stack(reverse = TRUE)
  ) +
  geom_text(
    mapping = aes(y = lbl_y, label = lbl_abb),
    vjust = 1.5,
    color = "black"
  ) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Most listened to artists each month",
    fill = "Artist",
    x = "Month",
    y = "Minutes"
  ) +
  theme_classic()

## album bar ---------------------------------------------------------------

x %>%
  group_by(album, artist) %>%
  filter(album %out% c("")) %>%
  summarise(hours = sum(min) / 60) %>%
  arrange(desc(hours)) %>%
  head(20) %>%
  mutate(
    lbl = paste(
      str_trunc(album, 30, side = "right"),
      str_trunc(artist, 30, side = "right"),
      sep = " | "
    )
  ) %>%
  ggplot(aes(x = reorder(lbl, hours), y = hours)) +
  geom_col(color = "black", aes(fill = hours)) +
  scale_fill_viridis_c(option = "B", guide = "none", end = 0.8) +
  coord_flip() +
  labs(x = "Artist", y = "Hours") +
  theme_classic()

x %>%
  mutate(
    album = str_to_title(str_remove(album, "\\s\\(.*\\)$")),
  ) %>%
  group_by(album, artist) %>%
  filter() %>%
  summarise(min = sum(min), n = n()) %>%
  arrange(desc(n)) %>%
  head(100) %>%
  write_csv("~/Documents/spotify_albums.csv", na = "")

## song bar ----------------------------------------------------------------

x %>%
  group_by(track, artist) %>%
  filter(track %out% c("")) %>%
  summarise(hours = sum(min) / 60) %>%
  arrange(desc(hours)) %>%
  head(20) %>%
  mutate(
    lbl = paste(
      str_trunc(track, 30, side = "right"),
      str_trunc(artist, 30, side = "right"),
      sep = " | "
    )
  ) %>%
  ggplot(aes(x = reorder(lbl, hours), y = hours)) +
  geom_col(color = "black", aes(fill = hours)) +
  scale_fill_viridis_c(option = "B", guide = "none", end = 0.8) +
  coord_flip() +
  labs(x = "Artist", y = "Hours") +
  theme_classic()

x %>%
  mutate(
    album = str_to_title(str_remove(album, "\\s\\(.*\\)$")),
    track = str_to_title(track)
  ) %>%
  group_by(track, album, artist) %>%
  filter() %>%
  summarise(min = sum(min), n = n()) %>%
  arrange(desc(n)) %>%
  head(100) %>%
  write_csv("~/Documents/spotify_songs.csv", na = "")

## platform ----------------------------------------------------------------

dat %>%
  group_by(platform, yr = year(ts)) %>%
  summarise(hours = sum(min) / 60) %>%
  ggplot(aes(x = yr, y = hours)) +
  geom_col(color = "black", aes(fill = platform)) +
  labs(x = "Year", y = "Hours") +
  theme_classic()

x %>%
  group_by(yr = year(ts), platform) %>%
  summarise(hours = sum(min) / 60) %>%
  mutate(p = hours / sum(hours)) %>%
  ggplot(aes(x = yr, y = p)) +
  geom_col(color = "black", aes(fill = platform)) +
  labs(x = "Year", y = "Hours") +
  theme_classic()

## time polar --------------------------------------------------------------

dat %>%
  mutate(
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
  scale_fill_viridis_c(
    end = 0.90,
    option = "B",
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = 0:23,
    minor_breaks = NULL,
    # labels = function(x) {
    #   format(as.POSIXct(as.character(x), format = "%H"), format = "%I %p")
    # }
  ) +
  scale_y_continuous(
    labels = label_comma(),
    n.breaks = 10
  ) +
  # coord_polar(start = 0) +
  labs(
    title = "Listening time by hour of the day",
    x = "Hour",
    y = "Minutes"
  ) +
  theme_classic() +
  theme(axis.text.y = element_blank()) +
  facet_wrap(~year, ncol = 1)

## cumulative time ---------------------------------------------------------

y <- x %>%
  filter(
    artist %in% most_common(artist, 5),
    year(ts) == max(year(ts)) - 1
  ) %>%
  group_by(artist, wk = week(ts)) %>%
  summarise(wk_min = sum(min)) %>%
  mutate(
    cum_min = cumsum(wk_min),
    lbl_abb = abbreviate(artist)
  )

y %>%
  ggplot(
    mapping = aes(x = wk, y = cum_min)
  ) +
  geom_step(
    mapping = aes(color = artist)
  ) +
  geom_text(
    data = filter(y, cum_min == max(cum_min)),
    x = max(y$wk),
    nudge_y = max(y$cum_min) * 0.02,
    mapping = aes(
      label = artist,
      color = artist,
      x = wk
    )
  ) +
  scale_y_continuous(
    labels = comma
  ) +
  scale_color_discrete(
    guide = "none"
  ) +
  labs(
    title = "Cumulative listening time over year",
    color = "Artist",
    x = "Week",
    y = "Minutes"
  ) +
  theme_classic()

## calendar ----------------------------------------------------------------

x %>%
  group_by(date = as_date(ts)) %>%
  summarise(min_total = sum(min)) %>%
  mutate(
    year = year(date),
    month = month(date, label = TRUE, abbr = TRUE),
    wday = fct_relevel(
      .f = wday(date, label = TRUE, abbr = TRUE),
      c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    ),
    day = day(date),
    wk = format(date, "%W"),
    ### cap outlier
    min_total = ifelse(min_total > 500, 500, min_total)
  ) %>%
  filter(year > 2014, year < 2023) %>%
  ggplot(
    mapping = aes(x = wk, y = wday)
  ) +
  geom_tile(
    mapping = aes(fill = min_total),
    color = "black",
    linewidth = 0.5
  ) +
  coord_equal() +
  scale_fill_viridis_c(
    end = 0.95,
    option = "B",
    na.value = "black"
  ) +
  labs(
    title = "Listened time each day of the year",
    x = "Week",
    y = "Weekday",
    fill = "Minutes"
  ) +
  scale_x_discrete(
    labels = if_else(is_even(0:53), "", as.character(0:53))
  ) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "black")) +
  facet_wrap(~year, ncol = 1)
