library(jsonlite)
library(readr)

#API endpoint (pulls all Earth close-approaches for NEOs from 1900–2100)
url <- "https://ssd-api.jpl.nasa.gov/cad.api?body=Earth&date-min=1900-01-01&date-max=2100-01-01&neo=true"
#Data from NASA API
neo_raw <- fromJSON(url)
# Convert JSON to a basic data frame
neo_raw_df <- as.data.frame(neo_raw$data, stringsAsFactors = FALSE)
# Add column names from the API’s field list
colnames(neo_raw_df) <- neo_raw$fields
str(nasa_neo)
nrow(nasa_neo)
ncol(nasa_neo)

library(tidyverse)

# Replace with NA
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
  mutate(days = if_else(str_detect(t_sigma_f, "_"),
                   as.numeric(str_extract(t_sigma_f, "^[0-9]+(?=_)")),
                   0))

library(dplyr)
library(lubridate)

# 1. date-time
nasa_neo <- nasa_neo %>%
  mutate(
    cd_dt = as.POSIXct(cd,
                       format = "%Y-%b-%d %H:%M",  # e.g., "1900-Jan-04 22:25"
                       tz = "UTC"),
    year = year(cd_dt)
  )

# 2. asteroids per year
asteroid_yearly <- nasa_neo %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarise(count = n(), .groups = "drop")

# graph
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

nasa_neo <- nasa_neo %>%
  mutate(
    obs_id = row_number(),
    vel_change = v_rel - v_inf
  )

# (v_inf)
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

#(v_rel)
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

# (v_rel - v_inf)
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

p1/p2/p3

library(dplyr)
library(ggplot2)

neo_bd <- nasa_neo %>%
  filter(!is.na(dist_min), !is.na(h))
# Check correlation
cor(neo_bd$dist_min, neo_bd$h)

# scatterplot:distance vs brightness
ggplot(neo_bd, aes(x = dist_min, y = h)) +
  geom_point(alpha = 0.15, size = 0.9) +
  labs(
    title = "Relationship Between Minimum Miss Distance and Brightness",
    x = "Minimum Miss Distance (AU)",
    y = "Absolute Magnitude (h)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )

library(dplyr)
library(ggplot2)

# Create distance bands
nasa_neo <- nasa_neo %>%
  mutate(
    dist_band = case_when(
      dist_min <= 0.01              ~ "Very close (≤ 0.01 AU)",
      dist_min > 0.01 & dist_min <= 0.03 ~ "Moderately close (0.01–0.03 AU)",
      dist_min > 0.03 & dist_min <= 0.05 ~ "Outer close range (0.03–0.05 AU)",
      dist_min > 0.05              ~ "Beyond 0.05 AU"
    ),
    dist_band = factor(
      dist_band,
      levels = c("Very close (≤ 0.01 AU)",
                 "Moderately close (0.01–0.03 AU)",
                 "Outer close range (0.03–0.05 AU)",
                 "Beyond 0.05 AU")
    )
  )

# Summary by band
dist_band_summary <- nasa_neo %>%
  group_by(dist_band) %>%
  summarise(
    n          = n(),
    mean_dist  = mean(dist_min, na.rm = TRUE),
    mean_h     = mean(h, na.rm = TRUE),
    mean_vrel  = mean(v_rel, na.rm = TRUE)
  )

dist_band_summary

risk_summary <- nasa_neo %>%
  filter(risk_band %in% c("Hazard-like", "Near-hazard", "Low concern")) %>%
  summarise(
    n                   = n(),
    mean_dist_min       = mean(dist_min, na.rm = TRUE),
    median_dist_min     = median(dist_min, na.rm = TRUE),
    mean_h              = mean(h, na.rm = TRUE),
    median_h            = median(h, na.rm = TRUE),
    mean_v_rel          = mean(v_rel, na.rm = TRUE),
    median_v_rel        = median(v_rel, na.rm = TRUE),
    mean_vel_change     = mean(vel_change, na.rm = TRUE),
    median_vel_change   = median(vel_change, na.rm = TRUE),
    mean_uncert_hours   = mean(uncertainty_hours, na.rm = TRUE),
    median_uncert_hours = median(uncertainty_hours, na.rm = TRUE),
    .by = risk_band
  )

risk_summary

ggplot(
  nasa_neo %>% filter(risk_band %in% c("Hazard-like", "Near-hazard", "Low concern")),
  aes(x = h, y = v_rel, color = risk_band)
) +
  geom_point(alpha = 0.25, size = 0.7, shape = 16) +
  labs(
    title = "Velocity vs Brightness by Hazard-Oriented Group",
    x = "Absolute Magnitude (h)",
    y = "Relative Velocity (km/s)",
    color = "Risk Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )
risk_vel <- nasa_neo %>%
  filter(risk_band %in% c("Hazard-like", "Near-hazard", "Low concern")) %>%
  group_by(risk_band) %>%
  summarise(
    n               = n(),
    mean_v_rel      = mean(v_rel, na.rm = TRUE),
    sd_v_rel        = sd(v_rel, na.rm = TRUE),
    mean_vel_change = mean(vel_change, na.rm = TRUE),
    sd_vel_change   = sd(vel_change, na.rm = TRUE)
  )

risk_vel

risk_uncert <- nasa_neo %>%
  filter(risk_band %in% c("Hazard-like", "Near-hazard", "Low concern")) %>%
  group_by(risk_band) %>%
  summarise(
    n                 = n(),
    mean_uncert_hours = mean(uncertainty_hours, na.rm = TRUE),
    median_uncert_hours = median(uncertainty_hours, na.rm = TRUE),
    sd_uncert_hours   = sd(uncertainty_hours, na.rm = TRUE)
  )

risk_uncert

#Modelling
#Clustering
vars <- c("dist_min", "h", "v_rel", "vel_change", "uncertainty_hours")
clust_data <- nasa_neo %>%
  select(all_of(vars)) %>%
  na.omit()
clust_scaled <- scale(clust_data)

# Elbow plot
set.seed(123)
wss <- sapply(1:10, function(k) {
  kmeans(clust_scaled, centers = k, nstart = 20)$tot.withinss
})
plot(1:10, wss, type = "b",
     xlab = "Number of clusters (k)",
     ylab = "Total within-cluster SS",
     main = "Elbow Plot for k-means")
set.seed(123)
k3 <- kmeans(clust_scaled, centers = 3, nstart = 50)

clust_labels <- factor(k3$cluster)

clust_data_with_labels <- clust_data %>%
  mutate(cluster = clust_labels)

cluster_summary <- clust_data_with_labels %>%
  group_by(cluster) %>%
  summarise(
    n                = n(),
    mean_dist_min    = mean(dist_min),
    mean_h           = mean(h),
    mean_v_rel       = mean(v_rel),
    mean_vel_change  = mean(vel_change),
    mean_uncert      = mean(uncertainty_hours)
  )

cluster_summary

# attach cluster IDs back to original rows via rownames
nasa_neo$cluster_k3 <- NA_integer_
nasa_neo$cluster_k3[as.numeric(rownames(clust_data))] <- k3$cluster

table(nasa_neo$cluster_k3, nasa_neo$risk_band)

library(dplyr)
library(ggplot2)

risk_space <- nasa_neo %>%
  select(dist_min, h) %>%
  na.omit()
risk_scaled <- scale(risk_space)
set.seed(123)
k3_risk <- kmeans(risk_scaled, centers = 3, nstart = 50)

risk_space$cluster_risk <- factor(k3_risk$cluster)

# Quick scatter in risk space
ggplot(risk_space, aes(x = dist_min, y = h, color = cluster_risk)) +
  geom_point(alpha = 0.3, size = 0.6, shape = 16) +
  labs(
    title = "k-means Clusters in Distance–Brightness Space",
    x = "Minimum Miss Distance (AU)",
    y = "Absolute Magnitude (h)",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

hazard_only <- nasa_neo %>%
  filter(risk_band == "Hazard-like") %>%
  select(v_rel, vel_change, uncertainty_hours) %>%
  na.omit()

hazard_scaled <- scale(hazard_only)

set.seed(123)
k2_hazard <- kmeans(hazard_scaled, centers = 2, nstart = 50)

hazard_only_with_labels <- as.data.frame(hazard_scaled) %>%
  mutate(cluster = factor(k2_hazard$cluster))

hazard_cluster_summary <- hazard_only_with_labels %>%
  group_by(cluster) %>%
  summarise(
    n              = n(),
    mean_v_rel     = mean(v_rel),
    mean_vel_change = mean(vel_change),
    mean_uncert    = mean(uncertainty_hours)
  )
hazard_cluster_summary

#Classification:
library(rpart)
library(rpart.plot)
library(dplyr)

# Use only the three risk bands inside 0.05 AU
class_data <- nasa_neo %>%
  filter(risk_band %in% c("Hazard-like", "Near-hazard", "Low concern")) %>%
  select(risk_band, dist_min, h) %>%
  na.omit()
set.seed(123)
tree_risk <- rpart(
  risk_band ~ dist_min + h,
  data   = class_data,
  method = "class",
  control = rpart.control(cp = 0.001, minbucket = 200)  # to avoid over-fragmenting
)

rpart.plot(tree_risk, main = "Classification Tree in Distance–Brightness Space")

# misclassification rate
pred_tree <- predict(tree_risk, type = "class")
mean(pred_tree == class_data$risk_band)
table(Observed = class_data$risk_band, Predicted = pred_tree)

library(dplyr)
library(rpart)
library(rpart.plot)

# hazard-like vs low concern, no near-hazard
class2_data <- nasa_neo %>%
  filter(risk_band %in% c("Hazard-like", "Low concern")) %>%
  mutate(
    class_bin = if_else(risk_band == "Hazard-like", 1, 0)
  ) %>%
  select(class_bin, v_rel, vel_change, uncertainty_hours) %>%
  na.omit()
set.seed(123)
tree_dyn <- rpart(
  factor(class_bin) ~ v_rel + vel_change + uncertainty_hours,
  data = class2_data,
  method = "class",
  control = rpart.control(cp = 0.001, minbucket = 200)
)
rpart.plot(tree_dyn, main = "Classification Tree Using Only Dynamic Features")

pred_dyn <- predict(tree_dyn, type = "class")
mean(pred_dyn == factor(class2_data$class_bin))
table(Observed = class2_data$class_bin, Predicted = pred_dyn)

