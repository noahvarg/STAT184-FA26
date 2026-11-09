# Install packages once in the Console if needed:
# install.packages(c("ggplot2", "dplyr", "tidyr", "readr", "sf"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
dir.create("data", showWarnings = FALSE)
dir.create("plots", showWarnings = FALSE)
vehicles <- ggplot2::mpg # Full dataset
gems <- ggplot2::diamonds # Full dataset

# Problem 1A

p_a <- ggplot(
  vehicles,
  aes(x = displ, y = hwy, color = drv)
) +
  geom_point(alpha = 0.7) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Engine Size and Highway Fuel Economy",
    x = "Engine Displacement (L)",
    y = "Highway MPG",
    color = "Drive Type"
  ) +
  theme_bw()

print(p_a)

ggsave(
  "plots/problem1a.png",
  p_a,
  width = 8,
  height = 5,
  dpi = 300
)

# Three fitted lines appear because drv is mapped to
# color in ggplot(). This grouping is inherited by
# geom_smooth(), producing one line per drive type.


# Problem 1B

# Assumption: Reference B compares drive types by class.

p_b_count <- ggplot(
  vehicles,
  aes(x = class, fill = drv)
) +
  geom_bar(position = "stack") +
  labs(
    title = "Vehicle Counts by Class and Drive Type",
    x = "Vehicle Class",
    y = "Count",
    fill = "Drive Type"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_b_count)

p_b_prop <- ggplot(
  vehicles,
  aes(x = class, fill = drv)
) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of Drive Types by Vehicle Class",
    x = "Vehicle Class",
    y = "Proportion",
    fill = "Drive Type"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_b_prop)

ggsave(
  "plots/problem1b_count.png",
  p_b_count,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "plots/problem1b_proportion.png",
  p_b_prop,
  width = 8,
  height = 5,
  dpi = 300
)

# position = "fill" rescales each bar to 100%.
# Equal bar heights show equal total proportions,
# not equal numbers of observations.


# Problem 1C

# Assumption: Smaller table includes diamonds <= 2 carats.
# Change the filter if the original figure uses another subset.

gems_small <- gems %>%
  filter(!is.na(carat), carat <= 2)

retained <- nrow(gems_small)
excluded <- nrow(gems) - retained

cat("Retained records:", retained, "\n")
cat("Excluded records:", excluded, "\n")

p_fine <- ggplot(
  gems_small,
  aes(x = carat)
) +
  geom_histogram(
    binwidth = 0.01,
    boundary = 0,
    closed = "right",
    fill = "steelblue",
    color = "white"
  ) +
  coord_cartesian(xlim = c(0, 2)) +
  labs(
    title = "Diamond Carat Distribution",
    subtitle = "Bin Width = 0.01",
    x = "Carat",
    y = "Count"
  ) +
  theme_bw()

p_coarse <- ggplot(
  gems_small,
  aes(x = carat)
) +
  geom_histogram(
    binwidth = 0.5,
    boundary = 0,
    closed = "right",
    fill = "darkorange",
    color = "white"
  ) +
  coord_cartesian(xlim = c(0, 2)) +
  labs(
    title = "Diamond Carat Distribution",
    subtitle = "Bin Width = 0.5",
    x = "Carat",
    y = "Count"
  ) +
  theme_bw()

print(p_fine)
print(p_coarse)

ggsave(
  "plots/problem1c_fine.png",
  p_fine,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "plots/problem1c_coarse.png",
  p_coarse,
  width = 8,
  height = 5,
  dpi = 300
)

carat_counts <- gems %>%
  filter(carat %in% c(0.99, 1.00)) %>%
  count(carat, name = "n")

print(carat_counts)

# Narrow bins make clustering at specific carat values
# easier to see, especially near 1.00 carat.
# Wider bins hide some of this variation.


# Problem 2

# Download real Chicago public library and boundary data.

libraries_url <- paste0(
  "https://data.cityofchicago.org/resource/",
  "x8fc-8rcq.geojson?$limit=5000"
)

boundaries_url <- paste0(
  "https://data.cityofchicago.org/resource/",
  "igwz-8jzy.geojson?$limit=5000"
)

download.file(
  libraries_url,
  "data/libraries_raw.geojson",
  mode = "wb"
)

download.file(
  boundaries_url,
  "data/community_areas_raw.geojson",
  mode = "wb"
)

data_sources <- tibble(
  dataset = c(
    "Chicago Public Libraries",
    "Chicago Community Areas"
  ),
  publisher = c(
    "City of Chicago",
    "City of Chicago"
  ),
  source_url = c(
    libraries_url,
    boundaries_url
  ),
  download_date = as.character(Sys.Date()),
  license_note = c(
    "See City of Chicago Data Portal Terms of Use",
    "See City of Chicago Data Portal Terms of Use"
  )
)

write_csv(
  data_sources,
  "data_sources.csv"
)

libraries_raw <- st_read(
  "data/libraries_raw.geojson",
  quiet = TRUE
)

districts_raw <- st_read(
  "data/community_areas_raw.geojson",
  quiet = TRUE
)

cat("Library fields:\n")
print(names(libraries_raw))

cat("Boundary fields:\n")
print(names(districts_raw))

cat("Raw library records:", nrow(libraries_raw), "\n")
cat("Raw boundary records:", nrow(districts_raw), "\n")

name_candidates <- c(
  "branch",
  "branch_",
  "site_name",
  "name"
)

name_field <- intersect(
  name_candidates,
  names(libraries_raw)
)[1]

if (is.na(name_field)) {
  stop("No recognized library name field was found.")
}

if (is.na(st_crs(libraries_raw))) {
  st_crs(libraries_raw) <- 4326
}

libraries_ll <- st_transform(
  libraries_raw,
  4326
)

coords <- st_coordinates(libraries_ll)

libraries_check <- libraries_ll %>%
  mutate(
    raw_id = row_number(),
    facility_name = as.character(
      .data[[name_field]]
    ),
    longitude = coords[, 1],
    latitude = coords[, 2],
    empty_geometry = st_is_empty(geometry),
    valid_geometry = st_is_valid(geometry),
    valid_coords = is.finite(longitude) &
      is.finite(latitude) &
      longitude >= -180 &
      longitude <= 180 &
      latitude >= -90 &
      latitude <= 90,
    in_chicago_region = longitude >= -88.1 &
      longitude <= -87.4 &
      latitude >= 41.6 &
      latitude <= 42.1,
    valid_name = !is.na(facility_name) &
      trimws(facility_name) != "",
    removal_reason = case_when(
      empty_geometry ~ "Empty geometry",
      !valid_geometry ~ "Invalid geometry",
      !valid_coords ~ "Invalid coordinates",
      !in_chicago_region ~ "Outside Chicago region",
      !valid_name ~ "Missing facility name",
      TRUE ~ "Retained"
    )
  )

write_csv(
  st_drop_geometry(libraries_check),
  "data/library_cleaning_audit.csv"
)

print(
  libraries_check %>%
    st_drop_geometry() %>%
    count(removal_reason)
)

duplicate_coordinates <- libraries_check %>%
  st_drop_geometry() %>%
  filter(valid_coords) %>%
  count(longitude, latitude) %>%
  filter(n > 1)

cat(
  "Repeated coordinate locations:",
  nrow(duplicate_coordinates),
  "\n"
)

print(duplicate_coordinates)

sites_clean <- libraries_check %>%
  filter(removal_reason == "Retained") %>%
  mutate(
    site_id = sprintf("LIB%03d", row_number())
  ) %>%
  select(
    site_id,
    facility_name,
    longitude,
    latitude,
    geometry
  )

# EPSG:26916 uses meters.

crs_m <- 26916

sites_m <- st_transform(
  sites_clean,
  crs_m
)

cat("Retained facilities:", nrow(sites_m), "\n")

cat(
  "Removed facilities:",
  nrow(libraries_raw) - nrow(sites_m),
  "\n"
)

stopifnot(nrow(sites_m) >= 20)

write_csv(
  st_drop_geometry(sites_clean),
  "data/cleaned_locations.csv"
)

st_write(
  sites_m,
  "data/sites_m.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)


# Problem 2: Boundaries

if (is.na(st_crs(districts_raw))) {
  st_crs(districts_raw) <- 4326
}

districts_raw <- st_transform(
  districts_raw,
  crs_m
)

districts_valid <- districts_raw %>%
  filter(!st_is_empty(geometry)) %>%
  st_make_valid() %>%
  st_collection_extract("POLYGON")

study_boundary <- st_sf(
  name = "Chicago",
  geometry = st_union(
    st_geometry(districts_valid)
  )
)

source_caption <- paste0(
  "Source: City of Chicago Open Data | Downloaded ",
  Sys.Date()
)


# Problem 2: Facility Map

p_sites <- ggplot() +
  geom_sf(
    data = study_boundary,
    fill = "gray95",
    color = "gray50"
  ) +
  geom_sf(
    data = sites_m,
    aes(color = "Public libraries"),
    size = 1.8
  ) +
  scale_color_manual(
    name = "Facilities",
    values = c("Public libraries" = "blue")
  ) +
  labs(
    title = "Chicago Public Library Locations",
    subtitle = paste(nrow(sites_m), "retained facilities"),
    caption = source_caption
  ) +
  theme_bw()

print(p_sites)

ggsave(
  "plots/p_sites.png",
  p_sites,
  width = 9,
  height = 8,
  dpi = 300
)


# Problem 2: Buffer Analysis

buffer_500 <- st_buffer(
  sites_m,
  dist = 500
)

buffer_1000 <- st_buffer(
  sites_m,
  dist = 1000
)

union_500 <- st_sf(
  radius_m = 500,
  geometry = st_union(
    st_geometry(buffer_500)
  )
)

union_1000 <- st_sf(
  radius_m = 1000,
  geometry = st_union(
    st_geometry(buffer_1000)
  )
)

area_500 <- as.numeric(
  st_area(union_500)
) / 1000000

area_1000 <- as.numeric(
  st_area(union_1000)
) / 1000000

buffer_summary <- tibble(
  radius_m = c(500, 1000),
  union_area_km2 = c(
    area_500,
    area_1000
  )
)

print(buffer_summary)

write_csv(
  buffer_summary,
  "data/buffer_summary.csv"
)

stopifnot(area_1000 >= area_500)

st_write(
  union_500,
  "data/buffer_500m.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

st_write(
  union_1000,
  "data/buffer_1000m.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

p_buffers <- ggplot() +
  geom_sf(
    data = study_boundary,
    fill = "gray95",
    color = "gray60"
  ) +
  geom_sf(
    data = union_500,
    aes(fill = "500 m combined buffer"),
    alpha = 0.55
  ) +
  geom_sf(
    data = sites_m,
    aes(color = "Public libraries"),
    size = 1
  ) +
  scale_fill_manual(
    name = "Buffer",
    values = c(
      "500 m combined buffer" = "lightblue"
    )
  ) +
  scale_color_manual(
    name = "Facilities",
    values = c(
      "Public libraries" = "darkblue"
    )
  ) +
  labs(
    title = "Chicago Libraries: 500-Meter Buffers",
    subtitle = "Overlapping buffers counted only once",
    caption = source_caption
  ) +
  theme_bw()

print(p_buffers)

ggsave(
  "plots/p_buffers.png",
  p_buffers,
  width = 9,
  height = 8,
  dpi = 300
)

# The 500-meter union measures the area covered by at
# least one facility buffer. The 1,000-meter union
# covers a larger area. Overlap is counted only once.
# Buffers are not clipped to the city boundary.


# Problem 3

id_candidates <- c(
  "area_num_1",
  "area_numbe",
  "area_num",
  "community"
)

district_name_candidates <- c(
  "community",
  "community_name",
  "name"
)

district_id_field <- intersect(
  id_candidates,
  names(districts_valid)
)[1]

district_name_field <- intersect(
  district_name_candidates,
  names(districts_valid)
)[1]

if (
  is.na(district_id_field) ||
  is.na(district_name_field)
) {
  stop("Check the community area ID/name column names.")
}

districts_m <- districts_valid %>%
  mutate(
    district_id = as.character(
      .data[[district_id_field]]
    ),
    district_name = as.character(
      .data[[district_name_field]]
    )
  ) %>%
  select(
    district_id,
    district_name,
    geometry
  )

districts_m <- districts_m %>%
  filter(
    !is.na(district_id),
    district_id != "",
    !is.na(district_name)
  ) %>%
  group_by(
    district_id,
    district_name
  ) %>%
  summarise(
    .groups = "drop"
  )

districts_m <- st_make_valid(districts_m)

cat("Community areas:", nrow(districts_m), "\n")

stopifnot(nrow(districts_m) >= 5)

districts_m <- districts_m %>%
  mutate(
    area_km2 = as.numeric(
      st_area(geometry)
    ) / 1000000
  )

stopifnot(all(districts_m$area_km2 > 0))

st_write(
  districts_m,
  "data/districts_m.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)


# Problem 3: Spatial Matching

matches <- st_intersects(
  sites_m,
  districts_m
)

site_match_audit <- tibble(
  site_id = sites_m$site_id,
  facility_name = sites_m$facility_name,
  n_area_matches = lengths(matches),
  matched_district = vapply(
    matches,
    function(i) {
      if (length(i) == 1) {
        as.character(districts_m$district_id[i])
      } else {
        NA_character_
      }
    },
    character(1)
  )
)

write_csv(
  site_match_audit,
  "site_match_audit.csv"
)

cat("Match audit:\n")

print(
  site_match_audit %>%
    count(n_area_matches)
)

cat("Sites without exactly one match:\n")

print(
  site_match_audit %>%
    filter(n_area_matches != 1)
)

unassigned_n <- sum(
  site_match_audit$n_area_matches != 1
)

assigned_sites <- site_match_audit %>%
  filter(n_area_matches == 1)

assigned_counts <- assigned_sites %>%
  count(
    matched_district,
    name = "n_sites"
  )

district_counts <- districts_m %>%
  left_join(
    assigned_counts,
    by = c(
      "district_id" = "matched_district"
    )
  ) %>%
  mutate(
    n_sites = replace_na(n_sites, 0L),
    sites_per_km2 = n_sites / area_km2
  )

stopifnot(
  sum(district_counts$n_sites) ==
    nrow(assigned_sites)
)

stopifnot(
  all(district_counts$area_km2 > 0)
)

stopifnot(
  !anyNA(district_counts$n_sites)
)

district_table <- district_counts %>%
  st_drop_geometry() %>%
  select(
    district_id,
    district_name,
    n_sites,
    area_km2,
    sites_per_km2
  ) %>%
  arrange(desc(n_sites))

print(
  district_table,
  n = Inf
)

write_csv(
  district_table,
  "data/district_counts.csv"
)

st_write(
  district_counts,
  "data/district_counts.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)


# Problem 3: Count Map

map_caption <- paste0(
  "Source: City of Chicago Open Data | Downloaded ",
  Sys.Date(),
  " | Sites without exactly one match: ",
  unassigned_n
)

p_count <- ggplot(
  district_counts
) +
  geom_sf(
    aes(fill = n_sites),
    color = "white",
    linewidth = 0.15
  ) +
  scale_fill_gradient(
    low = "lightyellow",
    high = "darkblue",
    na.value = "gray70",
    name = "Number of\nlibraries"
  ) +
  labs(
    title = "Public Libraries by Chicago Community Area",
    subtitle = "Number of facilities",
    caption = map_caption
  ) +
  theme_bw()

print(p_count)

ggsave(
  "plots/p_count.png",
  p_count,
  width = 9,
  height = 8,
  dpi = 300
)


# Problem 3: Density Map

p_density <- ggplot(
  district_counts
) +
  geom_sf(
    aes(fill = sites_per_km2),
    color = "white",
    linewidth = 0.15
  ) +
  scale_fill_gradient(
    low = "lightyellow",
    high = "darkgreen",
    na.value = "gray70",
    name = "Libraries\nper km²"
  ) +
  labs(
    title = "Library Density by Chicago Community Area",
    subtitle = "Facilities per square kilometer",
    caption = map_caption
  ) +
  theme_bw()

print(p_density)

ggsave(
  "plots/p_density.png",
  p_density,
  width = 9,
  height = 8,
  dpi = 300
)


# Problem 3: Comparison

cat("Top 5 areas by facility count:\n")

top_count <- district_table %>%
  arrange(desc(n_sites)) %>%
  slice_head(n = 5)

print(top_count)

cat("Top 5 areas by facility density:\n")

top_density <- district_table %>%
  arrange(desc(sites_per_km2)) %>%
  slice_head(n = 5)

print(top_density)

# Areas with more facilities do not necessarily have
# higher facility density. A large district can have
# many libraries but a low number per square kilometer.
# Smaller districts may rank higher after adjusting for
# geographic area. Areas with zero facilities are retained.
