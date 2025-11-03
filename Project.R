library(jsonlite)
library(readr)

#API endpoint (pulls all Earth close-approaches for NEOs from 1900–2100)
url <- "https://ssd-api.jpl.nasa.gov/cad.api?body=Earth&date-min=1900-01-01&date-max=2100-01-01&neo=true"

#Fetch the data from NASA API
neo_raw <- fromJSON(url)

# Convert JSON to a basic data frame
neo_raw_df <- as.data.frame(neo_raw$data, stringsAsFactors = FALSE)

# Add column names from the API’s field list
colnames(neo_raw_df) <- neo_raw$fields

str(nasa_neo)
nrow(nasa_neo)
ncol(nasa_neo)

library(tidyverse)

# Replace blank and "null" values with NA
nasa_neo <- nasa_neo %>%
  mutate_all(~na_if(., "")) %>%
  mutate_all(~na_if(., "null"))

# Convert numeric columns
nasa_neo <- nasa_neo %>%
  mutate(
    jd = as.numeric(jd),
    dist = as.numeric(dist),
    dist_min = as.numeric(dist_min),
    dist_max = as.numeric(dist_max),
    v_rel = as.numeric(v_rel),
    v_inf = as.numeric(v_inf),
    h = as.numeric(h)
  )

#new column for velocity change
nasa_neo <- nasa_neo %>%
  mutate(vel_change = v_rel - v_inf)

nasa_neo= nasa_neo %>% select(-vel_diff)

# Convert date column
nasa_neo <- nasa_neo %>%
  mutate(cd = as.Date(cd, format = "%Y-%b-%d"))

colSums(is.na(nasa_neo))

nasa_neo %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
  mutate(Missing_Percent = round((Missing_Count / nrow(nasa_neo)) * 100, 2))

summary(nasa_neo)


# Summary statistics for numeric variables
nasa_neo %>%
  select(jd, dist, dist_min, dist_max, v_rel, v_inf, h) %>%
  summary()


library(ggplot2)
library(patchwork)

# Individual plots
p1 <- ggplot(nasa_neo, aes(x = dist)) +
  geom_histogram(bins = 40, fill = "skyblue", color = "white") +
  labs(title = "Asteroid Miss Distance", x = "Distance (AU)", y = "Count")

p2 <- ggplot(nasa_neo, aes(x = dist_min)) +
  geom_histogram(bins = 50, fill = "darkgreen", color = "white") +
  labs(title = "Minimum Miss Distance", x = "Min Distance (AU)", y = "Count")

p3 <- ggplot(nasa_neo, aes(x = h)) +
  geom_histogram(bins = 50, fill = "orange", color = "white") +
  labs(title = "Asteroid Brightness (H)", x = "Absolute Magnitude (H)", y = "Count")

p4 <- ggplot(nasa_neo, aes(x = v_rel)) +
  geom_histogram(bins = 50, fill = "maroon", color = "white") +
  labs(title = "Relative Velocity", x = "Velocity (km/s)", y = "Count")

# Combine all four in a 2x2 grid
(p1 | p2) /
  (p3 | p4)

#outliers
nasa_neo %>%
  select(dist, dist_min, v_rel, v_inf, h) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(outlier.color = "darkblue", outlier.size = 1.5) +
  theme_minimal() +
  labs(title = "Outliers", x = "", y = "Value")


library(GGally)   # for ggcorr

# pick numeric columns
neo_numeric <- nasa_neo %>%
  select(dist, dist_min, dist_max, v_rel, v_inf, h) %>%
  drop_na()
cor_matrix <- cor(neo_numeric)
round(cor_matrix, 2)

nasa_neo <- nasa_neo %>%
  mutate(
    # Extract days (the part before "_", if it exists)
    days = if_else(str_detect(t_sigma_f, "_"),
                   as.numeric(str_extract(t_sigma_f, "^[0-9]+(?=_)")),
                   0))
library(dplyr)
library(ggplot2)
library(lubridate)

# Step 1: Extract year from date column
nasa_neo <- nasa_neo %>%
  mutate(year = year(cd))   # 'cd' is your close-approach date

# Step 2: Count number of asteroids per year
asteroid_yearly <- nasa_neo %>%
  group_by(year) %>%
  summarise(count = n(), .groups = "drop")

# Step 3: Plot line graph
ggplot(asteroid_yearly, aes(x = year, y = count)) +
  geom_line(color = "deepskyblue3", linewidth = 1.3) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    title = "Asteroid approaches by Year",
    subtitle = "Number of recorded near-Earth object encounters per year",
    x = "Year",
    y = "Number of Asteroids"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    panel.grid.minor = element_blank()
  )

library(ggplot2)
library(dplyr)
library(patchwork)  # optional if you want to stack them

# step 1: create observation index + velocity change
nasa_neo <- nasa_neo %>%
  mutate(
    obs_id = row_number(),
    vel_change = v_rel - v_inf
  )

# step 2: far-field velocity (v_inf)
p1 <- ggplot(nasa_neo, aes(x = obs_id, y = v_inf)) +
  geom_line(color = "#0072B2", linewidth = 1) +
  labs(
    title = "Far velocity v_inf",
    x = "Observations",
    y = "Vel (km/s)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank()
  )

# step 3: near-Earth velocity (v_rel)
p2 <- ggplot(nasa_neo, aes(x = obs_id, y = v_rel)) +
  geom_line(color = "#E69F00", linewidth = 1) +
  labs(
    title = "Near-Earth Velocity (v_rel)",
    x = "Observations",
    y = "Vel (km/s)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank()
  )

# step 4: velocity change (v_rel - v_inf)
p3 <- ggplot(nasa_neo, aes(x = obs_id, y = vel_change)) +
  geom_line(color = "#009E73", linewidth = 1) +
  labs(
    title = "Velocity Change (v_rel - v_inf)",
    x = "Observation",
    y = "Vel diff. (km/s)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank()
  )

# step 5 (optional): display all three vertically
p1 / p2 / p3
