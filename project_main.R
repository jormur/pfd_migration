#Term Project Code
library(tidyverse)
library(readxl)
library(sf)
library(plm)
library(splm)
library(mice)

library(spdep)
library(Matrix)
library(spflow)
library(migest)
library(ggplot2)
library(grid)
library(gridExtra)
library(magick)
library(modelsummary)
library(kableExtra)

# library(spatialhelpers)

# Adapted w_data function
w_data <- function(data, id1, id2, flow = NULL, missing_list = FALSE, quietly = FALSE) {
  id1 <- sym(id1)
  id2 <- sym(id2)
  flow <- sym(flow)
  
  if (!missing(flow)) {
    data <- data %>% rename(a = !!id1, b = !!id2, flow = !!flow) %>%
      select(a, b, flow)
    
    data2 <- data %>% rename(b = a, a = b)
    data <- bind_rows(data, data2)
    
    data <- data %>%
      pivot_wider(id_cols = a, names_from = b, values_from = flow, values_fn = sum, values_fill = 0) %>%
      arrange(a)
    
    data <- data %>%
      select(order(colnames(data)))
    
    data <- as.data.frame(data)
    rownames(data) <- data$a
    data$a <- NULL
    data <- as.matrix(data)
    diag(data) <- 0
  } else {
    stop("Flow argument is missing.")
  }
  
  if (sum(is.na(data)) > 0 & !missing_list) {
    warning("There are missing values in your matrix. These may correspond to observations in your data with no dyadic flows in which case you can recode these values to 0 using: my_matrix[is.na(my_matrix)] <- 0. Otherwise, you can rerun this command with argument missing_list = TRUE to return a list of dyads with NA values.")
  } else if (missing_list) {
    data <- apply(data, 1, function(x) { names(x[is.na(x)]) })
  }
  
  return(data)
}

setwd("~/Documents/uni/Year 2/Semester 2/Local Public Finance/term project")

#
##
###
####
##### Migration Data ------------------------------------------------
file_path <- "data/migration_data.xlsx"

sheet_names <- excel_sheets(file_path)

migration_data <- list()

for (sheet in sheet_names) {
  data <- read_excel(file_path, sheet = sheet, skip = 1)
  
  # year from the sheet name
  year <- as.numeric(sheet)
  
  # Coerce to long
  data_long <- data %>%
    pivot_longer(cols = -1, names_to = "origin", values_to = "migration") %>%
    rename(destination = 1) %>%
    filter(!is.na(migration)) %>%
    mutate(year = year)
  
  migration_data[[sheet]] <- data_long
}


combined_data <- bind_rows(migration_data)

#For rows where Destination and Origin are the same, make the Migration value the value of a new population column that applies to all rows with the same destination
#The population is a new column that is the exact value found in that row, that is then applied to all rows with the same destination
population <- combined_data %>%
  filter(destination == origin) %>%
  mutate(population = migration) %>%
  select(-migration)

#Join the population data with the migration data for both origin and destination
combined_data <- combined_data %>%
  group_by(destination,year) %>%
  left_join(population %>% select(destination,year,population), by = c("destination", "year")) %>%
  rename("population_dest" = "population")

combined_data <- combined_data %>%
  group_by(origin,year) %>%
  left_join(population %>% select(origin,year,population), by = c("origin", "year")) %>%
  rename("population_origin" = "population")

#
##
###
####
##### Variable Joining ------------------------------------------------
############### Join PFD payment values
pfd <- read_csv("data/pfd_history.csv") %>%
  rename("pfd" = "Dividend amount (USD)", "pfd_inflation_adj" = "Inflation-adjusted dividend amount (2023 USD)",
         "year" = "Year") %>%
  mutate(year = as.numeric(year))

#join the migration data with the pfd data
combined_data <- combined_data %>%
  left_join(pfd %>% select(year,pfd,pfd_inflation_adj), by = "year")

################ Unemployment Rate
unemployment_rate <- read_csv("data/unemployment.csv") %>%
  rename("unemployment_rate" = "Unemployment Rate", "year" = "Year", "area" = "Area Name")

#Join the unemployment data with the migration data for both origin and destination
combined_data <- combined_data %>%
  group_by(destination, year) %>%
  left_join(unemployment_rate %>% select(area,year,unemployment_rate), by = c("destination" = "area", "year")) %>%
  rename("unemployment_dest" = "unemployment_rate")

combined_data <- combined_data %>%
  group_by(origin,year) %>%
  left_join(unemployment_rate %>% select(area,year,unemployment_rate), by = c("origin" = "area", "year")) %>%
  rename("unemployment_origin" = "unemployment_rate")

################ Average Monthly Income
file_path <- "data/wage.xlsx"

sheets <- lapply(excel_sheets(file_path), function(sheet) {
  read_excel(file_path, sheet = sheet) %>%
    select(AREANAME, YEAR, wage)
})

wage <- do.call(rbind, sheets)

wage <- wage %>%
  mutate(YEAR = as.numeric(YEAR))

combined_data <- combined_data %>%
  group_by(destination, year) %>%
  left_join(wage %>% select(AREANAME,YEAR,wage), by = c("destination" = "AREANAME", "year" = "YEAR")) %>%
  rename("wage_dest" = "wage")

combined_data <- combined_data %>%
  group_by(origin,year) %>%
  left_join(wage %>% select(AREANAME,YEAR,wage), by = c("origin" = "AREANAME", "year" = "YEAR")) %>%
  rename("wage_origin" = "wage")

#
##
###
####
##### Map Setup ------------------------------------------------
# ak_boroughs00 <- st_read("data/boroughs2000/Boroughs2000.shp")
# ak_boroughs10 <- st_read("data/boroughs2010/Boroughs.shp")
ak_boroughs20 <- st_read("data/Boroughs2020/Boroughs2020.shp")

################ Add FIPS to migration data
# Add FIPS (borough ID) from the shp file to the migration data
combined_data <- combined_data %>%
  left_join(ak_boroughs20 %>% select(FIPS, NAME), by = c("origin" = "NAME")) %>%
  rename("id_origin" = "FIPS", "geometry_origin" = "geometry")

combined_data <- combined_data %>%
  left_join(ak_boroughs20 %>% select(FIPS, NAME), by = c("destination" = "NAME")) %>%
  rename("id_dest" = "FIPS", "geometry_dest" = "geometry")


#
##
###
####
##### SPLM Estimation ------------------------------------------------
############# I - Data Preparation
#conbstruction of new identifier variable by combining origin and destination
combined_data <- combined_data %>%
  mutate(id = paste(id_origin, id_dest, sep = "-"))

#aggregate duplicate flows
migration_flows <- combined_data %>%
  group_by(id_origin, id_dest) %>%
  summarise(migration = sum(migration),
            population_origin = first(population_origin),
            population_dest = first(population_dest),
            pfd_inflation_adj = first(pfd_inflation_adj),
            unemployment_origin = first(unemployment_origin),
            unemployment_dest = first(unemployment_dest),
            wage_origin = first(wage_origin),
            wage_dest = first(wage_dest),
            .groups = 'drop')

#ensuring the spatial weights matrix only includes locations present in migration_flows
unique_origins <- unique(migration_flows$id_origin)
unique_dests <- unique(migration_flows$id_dest)
unique_locs <- union(unique_origins, unique_dests)

#filtering combined_data to include only these locations
filtered_data <- combined_data %>%
  filter(id_origin %in% unique_locs & id_dest %in% unique_locs)

#fill NA values
filtered_data <- filtered_data %>%
  group_by(id) %>%
  fill(unemployment_origin, unemployment_dest, population_origin, population_dest, wage_origin, wage_dest,
       .direction = "downup") %>%
  ungroup()

#show IDs with missing values
filtered_data %>%
  filter(is.na(unemployment_origin) | is.na(unemployment_dest) | is.na(population_origin) | is.na(population_dest) | is.na(pfd_inflation_adj) | is.na(migration))

#double check for NAs
filtered_data <- filtered_data %>%
  filter(complete.cases(migration, pfd_inflation_adj, population_origin, population_dest, unemployment_origin, unemployment_dest))

############# II - Spatial Weights Matrix
#create dyadic spatial weights matrix
w_symmetric <- w_data(data = migration_flows, id1 = "id_origin", id2 = "id_dest", flow = "migration")

#recode missing values to 0 if no flows
w_symmetric[is.na(w_symmetric)] <- 0

#verify symmetry
if (!isSymmetric(w_symmetric)) {
  stop("The spatial weights matrix is not symmetric.")
}

filtered_w_symmetric <- w_symmetric[unique_locs, unique_locs]

############# III - Model Setup
W_listw <- mat2listw(as.matrix(filtered_w_symmetric), style = "W")

migration_panel <- pdata.frame(filtered_data, index = c("id_origin", "id_dest", "year"))

formula <- log(1+migration) ~ log(pfd_inflation_adj) + log(population_origin) + log(population_dest) + log(unemployment_origin) + log(unemployment_dest) + log(wage_origin) + log(wage_dest)

#double check for NAs in panel
migration_panel <- migration_panel %>%
  filter(complete.cases(migration, pfd_inflation_adj, population_origin, population_dest, unemployment_origin, unemployment_dest))

#model estimation
sar_model <- spml(formula, data = migration_panel, listw = W_listw, 
                  model = "within", effect = "twoways",
                  lag = TRUE)

summary(sar_model)

results <- summary(sar_model)

# non_spatial_model <- plm(formula, data = migration_panel, model = "within", effect = "twoways")
# summary(non_spatial_model)

# LATEX EXPORT
tidy.spml <- function(model, ...) {
  coefs <- summary(model)$CoefTable
  data.frame(
    term = rownames(coefs),
    estimate = coefs[, "Estimate"],
    std.error = coefs[, "Std. Error"],
    statistic = coefs[, "t-value"],
    p.value = coefs[, "Pr(>|t|)"]
  )
}

glance.spml <- function(model, ...) {
  summary_model <- summary(model)
  data.frame(
    sigma2 = summary_model$sigma2,
    rsqr = summary_model$rsqr,
    tss = summary_model$tss,
    ssr = summary_model$ssr
  )
}

tidy_results <- tidy.spml(sar_model)
glance_results <- glance.spml(sar_model)

model_list <- list(
  tidy = tidy_results,
  glance = glance_results
)
class(model_list) <- "modelsummary_list"

modelsummary(model_list, 
             output = "final presentation/LPF paper/tbls/est.tex", 
             statistic = "std.error",
             fmt = 
             stars = TRUE)

# modelsummary(results, stars = TRUE,
#              title = "SAR Estimation of PFD on Migration Flows",
#              output = "final presentation/LPF Slides/tbls/sar_est.tex")







#
##
###
####
##### Flow Maps ------------------------------------------------
################ Flow analysis setup
# I
# drop_area <- names(ak_boroughs20) != "AREA"
# # plot(ak_boroughs20[drop_area])
# 
# # II
# old_par <- par(mfrow = c(1, 3), mar = c(0,0,1,0))
# 
# mid_points <- suppressWarnings({
#   st_point_on_surface(st_geometry(ak_boroughs20))})
# 
# ak_boroughs_nb <- list(
#   "by_contiguity" = spdep::poly2nb(ak_boroughs20),
#   "by_distance" = spdep::dnearneigh(mid_points,d1 = 0, d2 = 5),
#   "by_knn" = spdep::knn2nb(knearneigh(mid_points,3))
# )

# plot(st_geometry(ak_boroughs20))
# plot(ak_boroughs_nb$by_contiguity, mid_points, add = T, col = rgb(0,0,0,alpha=0.5))
# title("Contiguity") 
# 
# plot(st_geometry(ak_boroughs20))
# plot(ak_boroughs_nb$by_distance,mid_points, add = T, col = rgb(0,0,0,alpha=0.5)) 
# title("Distance") 
# 
# plot(st_geometry(ak_boroughs20))
# plot(ak_boroughs_nb$by_knn, mid_points, add = T, col = rgb(0,0,0,alpha=0.5))
# title("3 Nearest Neighbors") 

# par(old_par)

# I - Create a list of spflow networks for each year
boroughs_net <- spflow_network(
   id_net = "alaska",
   node_neighborhood = nb2mat(ak_boroughs_nb$by_contiguity),
   node_data = ak_boroughs20,
   node_key_column = "FIPS")

# II - Create network pairs
boroughs_net_pairs <- combined_data %>%
  filter(year == 2001) %>%
  # filter(destination != origin) %>% 
  spflow_network_pair(
  id_orig_net = "alaska",
  id_dest_net = "alaska",
  pair_data = .,
  orig_key_column = "id_origin",
  dest_key_column = "id_dest")

# III - Combine objects
# network_multi <- spflow_network_multi(boroughs_net, boroughs_net_pairs)
# 
# # IV - Model Estimation
# # Formula specification
# spflow_formula <- 
#   log(migration + 1) ~ pfd_inflation_adj + 
#   D_(log(unemployment_dest)) + O_(log(unemployment_origin)) +
#   D_(log(population_dest)) + O_(log(population_origin)) +
#   P_(log(1 + distance))
# 
# #fit model
# results <- spflow(spflow_formula, network_multi)
# summary(results)

# MAPS
# plot(ak_boroughs20$geometry) # polygons as background
# spflow_map(
#   network_multi,
#   flow_var = "migration",
#   add = TRUE,          # add to existing background
#   legend_position = "bottomleft",
#   filter_lowest = .95, # concentrate on the 5% largest
#   remove_intra = TRUE,  # intra-municipality flows are too large
#   cex = 1)

# AGGREGATED MAP
# Filter data to aggregate across all years by origin and destination
combined_data_agg <- combined_data %>%
  group_by(id_origin, id_dest) %>%
  summarise(migration = sum(migration)) %>%
  ungroup()

# Check that the data contains all necessary columns and no NA values
summary(combined_data_agg)
head(combined_data_agg)

# Create the matrix with the coordinates
coords_xy <- st_coordinates(st_centroid(ak_boroughs20))
rownames(coords_xy) <- ak_boroughs20[["FIPS"]]

# Ensure that migration, id_origin, and id_dest are numeric vectors
migration_vector <- combined_data_agg$migration
id_origin_vector <- combined_data_agg$id_origin
id_dest_vector <- combined_data_agg$id_dest

# Plot the base map with borough boundaries
plot(ak_boroughs20$geometry)

# Use map_flows to plot the flows
map_flows(
  migration_vector,
  id_origin_vector,
  id_dest_vector,
  coords_s = coords_xy,
  legend_position = "topright",
  color_palette = colors(distinct = TRUE)[10:40],  # Adjust the color palette as needed
  add = TRUE,
  remove_intra = TRUE
)

# PANEL INTERVAL MAPS
# Define the periods and labels
periods <- list(
  combined_data %>% filter(year >= 2001 & year <= 2004),
  combined_data %>% filter(year >= 2005 & year <= 2008),
  combined_data %>% filter(year >= 2009 & year <= 2012),
  combined_data %>% filter(year >= 2013 & year <= 2016),
  combined_data %>% filter(year >= 2017 & year <= 2020),
  combined_data %>% filter(year >= 2021 & year <= 2023)
)
labels <- c("2001-2004", "2005-2008", "2009-2012", "2013-2016", "2017-2020", "2021-2023")

# Create the matrix with the coordinates
coords_xy <- st_coordinates(st_centroid(ak_boroughs20))
rownames(coords_xy) <- ak_boroughs20[["FIPS"]]

# Function to create and save the map for each period
create_map_plot <- function(data, label, period_index) {
  # Aggregate the data for the period by origin and destination
  data_agg <- data %>%
    group_by(id_origin, id_dest) %>%
    summarise(migration = sum(migration), .groups = 'drop')
  
  # Ensure that migration, id_origin, and id_dest are numeric vectors
  migration_vector <- data_agg$migration
  id_origin_vector <- data_agg$id_origin
  id_dest_vector <- data_agg$id_dest
  
  # Create file name for high-resolution image
  file_name <- paste0("migration_map_", label, ".png")
  
  # Create and save the high-resolution plot
  png(file_name, width = 2000, height = 2000, res = 300)
  plot(ak_boroughs20$geometry)
  map_flows(
    migration_vector,
    id_origin_vector,
    id_dest_vector,
    coords_s = coords_xy,
    legend_position = "topright",
    color_palette = colors(distinct = TRUE)[10:40],  # Adjust the color palette as needed
    add = TRUE,
    remove_intra = TRUE
  )
  title(label, cex.main = 2)
  dev.off()
}

# Create and save a map plot for each period
lapply(seq_along(periods), function(i) {
  create_map_plot(periods[[i]], labels[i], i)
})

# Inform the user that the images have been saved
message("High-resolution images for each interval have been saved to your working directory.")









#
##
###
####
##### Chord diagram ------------------------------------------------
#aggregate all migration flows by origin and destination
migration_flows <-
  combined_data %>%
  group_by(origin, destination) %>%
  summarise(migration = sum(migration)) %>%
  filter(destination != origin) %>% 
  #remove "Census Area", "Municipality" and "City and Borough" from the names
  mutate(origin = str_remove_all(origin, "Census Area|City and Borough|Municipality|Borough"),
         destination = str_remove_all(destination, "Census Area|City and Borough|Municipality|Borough")) %>%
  ungroup()

mig_chord(x = migration_flows,
          label_size = 0.8,
          no_axis = T)

# Assuming combined_data is your original dataset
# Filter the data for each 4-year period
periods <- list(
  combined_data %>% filter(year >= 2001 & year <= 2004),
  combined_data %>% filter(year >= 2005 & year <= 2008),
  combined_data %>% filter(year >= 2009 & year <= 2012),
  combined_data %>% filter(year >= 2013 & year <= 2016),
  combined_data %>% filter(year >= 2017 & year <= 2020),
  combined_data %>% filter(year >= 2021 & year <= 2023)
)

# Create labels for each period
labels <- c("2001-2004", "2005-2008", "2009-2012", "2013-2016", "2017-2020", "2021-2023")

# Create a function to process the data and produce a chord diagram for each period
create_chord_plot <- function(data) {
  migration_flows <- data %>%
    group_by(origin, destination) %>%
    summarise(migration = sum(migration), .groups = 'drop') %>%
    filter(destination != origin) %>%
    mutate(origin = str_remove_all(origin, "Census Area|City and Borough|Municipality|Borough"),
           destination = str_remove_all(destination, "Census Area|City and Borough|Municipality|Borough")) %>%
    ungroup()
  
  mig_chord(x = migration_flows, label_size = 1, no_axis = TRUE)
}

# Create a chord diagram for each period and capture it as an image
plots <- lapply(seq_along(periods), function(i) {
  # Create file name for temporary storage
  file_name <- paste0("chord_plot_", i, ".png")
  
  # Create and save the plot
  png(file_name)
  plot <- create_chord_plot(periods[[i]])
  dev.off()
  
  # Read the image using magick
  image <- image_read(file_name)
  image
})

# Convert images to grobs and add labels
grobs <- lapply(seq_along(plots), function(i) {
  image_grob <- rasterGrob(as.raster(plots[[i]]), interpolate = TRUE)
  label_grob <- textGrob(labels[i], gp = gpar(fontsize = 12, fontface = "bold"))
  # Combine image and label
  combined_grob <- arrangeGrob(image_grob, top = label_grob)
  combined_grob
})

# Arrange plots side by side with reduced spacing using gridExtra::grid.arrange
combined_plot <- do.call(grid.arrange, c(grobs, ncol = 2, padding = unit(0.01, "line")))

# Display the combined plot
grid.draw(combined_plot)








