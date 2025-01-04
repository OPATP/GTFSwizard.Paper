rm(list = ls()); gc()

# preamble -----------------------------------------------------------------------------------
library(GTFSwizard)
library(aopdata)
library(parallel)
library(tidyverse)
library(hrbrthemes)
library(sf)
sf_use_s2(FALSE)

rstudioapi::getActiveDocumentContext()$path %>% 
  dirname() %>% 
  setwd(); setwd('..')

# data loading --------------------------------------------------------------------------------
gtfs <- 
  for_bus_gtfs 

for.data <-
  read_grid(city = 'Fortaleza') %>%
  left_join(read_landuse(city = 'Fortaleza'), by = 'id_hex')

hex.origins <-
  for.data %>% 
  mutate(pop = P013 + P014 + P015) %>% # 19 a 69 anos
  filter(R003 <= 6 & !pop == 0) %>% # até o 4 decil de renda
  select(id = id_hex, pop, R001) %>% 
  st_centroid()

stop.origins <- 
  latlon2epsg(hex.origins %>% select(-pop, - R001)) %>%
  st_buffer(500) %>% 
  st_transform(crs = 4326) %>% 
  st_intersects(get_stops_sf(gtfs$stops)) %>% 
  lapply(., function(x){unlist(list(x))}) %>% 
  tibble() %>% 
  setNames('stop_index') %>% 
  bind_cols(hex.origins) %>% 
  unnest('stop_index') %>% 
  mutate(stop_id = as.numeric(get_stops_sf(gtfs$stops)$stop_id[stop_index])) %>% 
  st_as_sf() %>%
  select(id, stop_id)

hex.destinations <-
  for.data %>% 
  mutate(jobs = T002 + T003) %>% 
  filter(jobs > 0) %>% # empregos de baixa e media escolaridade
  select(id = id_hex, jobs) %>% 
  st_centroid()

stop.destinations <- 
  latlon2epsg(hex.destinations %>% select(-jobs)) %>%
  st_buffer(500) %>% 
  st_transform(crs = 4326) %>% 
  st_intersects(get_stops_sf(gtfs$stops)) %>% 
  lapply(., function(x){unlist(list(x))}) %>% 
  tibble() %>% 
  setNames('stop_index') %>% 
  bind_cols(hex.destinations) %>% 
  unnest('stop_index') %>% 
  mutate(stop_id = as.numeric(get_stops_sf(gtfs$stops)$stop_id[stop_index])) %>% 
  st_as_sf() %>%
  select(id, stop_id)

gtfs <-
  filter_stop(gtfs, unique(c(stop.origins$stop_id, stop.destinations$stop_id)))

shp.bairro <-
  read_sf('data/bairros', crs = 4326) %>%
  select(geometry)

tot.pop <- filter(for.data, id_hex %in% stop.origins$id) %>% reframe(pop = sum(P013, P014, P015))
tot.jobs <- filter(for.data, id_hex %in% stop.destinations$id) %>% reframe(jobs = sum(T002, T003))

# day and time --------------------------------------------------------------------------------
plot_calendar(gtfs, facet_by_year = TRUE)
ggsave('figs/plot_calendar.png', width = 14, height = 4)

plot_frequency(gtfs)

gtfs <- 
  filter_time(gtfs, from = '06:00:00', to = '08:00:00') %>% 
  filter_date(dates = '2021-12-13')

summary(for_bus_gtfs)
summary(gtfs)

# corridors -----------------------------------------------------------------------------------
gtfs <- 
  for_bus_gtfs %>% 
  filter_stop(unique(c(stop.origins$stop_id, stop.destinations$stop_id))) %>% 
  filter_date(dates = '2021-12-13') %>% 
  filter_time(from = '6:0:0', '12:0:0')

plot_corridor(gtfs, i = .02, min.length = 1500)
ggsave('figs/plot_corridor.png', height = 4)

corridors <- 
  get_corridor(gtfs, i = .02, min.length = 1500) 

stops <- corridors$stop_id %>% unlist %>% unique
trips <- corridors$trip_id %>% unlist %>% unique

gtfs.speed <-
  edit_speed(gtfs, trips, stops, factor = 1.1)

# hubs ----------------------------------------------------------------------------------------
plot_hubs(gtfs.speed, i = .05)
ggsave('figs/plot_hubs.png', height = 4)

stops <- 
  c(stops,
  get_hubs(gtfs.speed) %>% 
  filter(percent_rank(n_routes) >= .95) %>%
  .$stop_id) %>% 
  unique

factor <- 2/3

gtfs.speed.dwell <- 
  set_dwelltime(gtfs.speed, 30, stops = stops) %>% 
    edit_dwelltime(stops = stops, factor = factor)

# headway -------------------------------------------------------------------------------------
add.trips <- 
  filter_time(gtfs.speed.dwell,
              from = '6:0:0',
              to = '8:0:0') %>% 
  get_frequency() %>% 
  mutate(hourly_frequency  = daily.frequency/2) %>% 
  filter(hourly_frequency < 3) %>% 
  mutate(add = ceiling(3 - hourly_frequency))

newtrips.data <- 
  get_headways(gtfs.speed.dwell, 'by.trip') %>% 
  filter(route_id %in% add.trips$route_id) %>% 
  arrange(route_id, -headway_minutes) %>% 
  left_join(add.trips[, c(1, 2, 7)]) %>% 
  filter(!is.na(add)) %>% 
  group_by(route_id, direction_id) %>% 
  mutate(n = 1:n()) %>% 
  ungroup %>%
  filter(n  <= add) %>% 
  mutate(delay = ifelse(headway_minutes >= 120,
                        -headway_minutes/8,
                        (240 - headway_minutes) / 8)) %>% 
  select(trip_id, delay) %>% 
  mutate(delay = delay * 60)
  
newtrips.gtfs.speed.dwell <- 
  gtfs.speed.dwell %>% 
  filter_trip(trip = newtrips.data$trip_id)

newtrips.gtfs.speed.dwell.lagged <-
  delay_trip(newtrips.gtfs.speed.dwell,
             trip = newtrips.data$trip_id[1],
             duration = newtrips.data$delay[1])

for (i in 2:nrow(newtrips.data)) {
  
  newtrips.gtfs.speed.dwell.lagged <-
    delay_trip(newtrips.gtfs.speed.dwell.lagged,
               trip = newtrips.data$trip_id[i],
               duration = newtrips.data$delay[i])  
  
}

# alternative scenario ------------------------------------------------------------------------
alternative.gtfs <-
  merge_gtfs(gtfs.speed.dwell, newtrips.gtfs.speed.dwell.lagged) %>%
  filter_time(from = '6:00:00', to = '8:00:00') 

gtfs <- 
  set_dwelltime(for_bus_gtfs, 30, stops = stops) %>% 
  filter_stop(unique(c(stop.origins$stop_id, stop.destinations$stop_id))) %>% 
  filter_date(dates = '2021-12-13') %>% 
  filter_time(from = '6:0:0', '8:0:0')

# raptor parlapply before ----------------------------------------------------------------------------
# DONT NEED TO RUN THIS CHUNK. IT TAKES UP TO 1 HOUR TO COMPLETE. 
# GO TO LINE 275
# Define the parameters for tidy_raptor
time_range <- 3600 * 2
max_transfers <- 2
stop_ids <- unique(stop.origins$stop_id)
filter <- FALSE
keep <- 'shortest'

# Function to wrap tidy_raptor
run_tidy_raptor <- function(i) {
  tidy_raptor(
    gtfs = gtfs,
    stop_ids = stop_ids[i],
    time_range = time_range,
    max_transfers = max_transfers,
    arrival = FALSE,
    filter = filter,
    keep = keep
  ) %>% 
    data.table::fwrite(paste0('data/raptor/before/', i, '.csv'))
}

# Detect number of cores
num_cores <- detectCores() - 1  # Reserve 1 core for the system

# Initiate a cluster
cl <- makeCluster(num_cores)

# Export required objects and libraries to the cluster
clusterExport(cl, c("gtfs", 'filter', 'keep',
                    "time_range", "max_transfers", "stop_ids", "run_tidy_raptor"))
# If tidy_raptor is part of a package, load the library on each worker
clusterEvalQ(cl, c(library(GTFSwizard), library(data.table), library(tidyverse)))  # Replace tidytransit with the correct package if needed

# Run parLapply
parLapply(cl, seq_along(stop_ids), run_tidy_raptor)

# Stop the cluster
stopCluster(cl)

rm(cl, filter, keep, max_transfers, num_cores, run_tidy_raptor, time_range)
# processing raptor before ---------------------------------------------------------------------------
files <- list.files('data/raptor/before', '.csv', full.names = T)

dat <-
  lapply(files, data.table::fread) %>% 
  data.table::rbindlist()

dat <-
  dat %>% 
  select(-journey_departure_time, -journey_arrival_time, -transfers) %>% 
  filter(!from_stop_id == to_stop_id)

dat <-
  dat %>% 
  filter(to_stop_id %in% stop.destinations$stop_id)

dat <-
  dat %>% 
  rename(stop_id = from_stop_id)

dat <-
  dat %>% 
  left_join(stop.origins, by = 'stop_id', relationship = "many-to-many") %>% 
  rename(from_stop_id = stop_id,
         stop_id = to_stop_id,
         id_from = id)

gc()

dat <-
  dat %>% 
  left_join(stop.destinations, by = 'stop_id', relationship = "many-to-many")

dat <-
  dat %>% 
  rename(to_stop_id = stop_id,
         id_to = id)

dat <- 
  dat %>% 
  select(id_from, id_to, travel_time)

ttm <- 
  dat %>% 
  filter(!id_from == id_to) %>% 
  group_by(id_from, id_to) %>% 
  reframe(travel_time = min(travel_time))

ttm %>% 
  data.table::fwrite('data/raptor/ttm.before.csv')

# access before -------------------------------------------------------------------------------
ttm <-  
  data.table::fread('data/ttm.before.csv')

sigma <- 50.95931 # 50% dos empregos alcançáveis em 60 minutos

od <- 
  tibble(id_from = hex.origins$id) %>% 
  group_by(id_from) %>% 
  reframe(id_to = hex.destinations$id) %>% 
  filter(!id_from == id_to)

access.before <- 
  ttm %>% 
  right_join(od, by = c('id_from', 'id_to')) %>% 
  mutate(travel_time = if_else(is.na(travel_time), Inf, travel_time)) %>% 
  rename(id = id_from) %>% 
  left_join(tibble(hex.origins) %>% select(-geom)) %>% 
  rename(id_from = id,
         id = id_to) %>% 
  left_join(tibble(hex.destinations) %>% select(-geom)) %>% 
  rename(id_to = id) %>% 
  mutate(dec.jobs = jobs * rvmethod::gaussfunc(travel_time/60, 0, sigma)) %>% 
  filter(!dec.jobs == 0) %>% 
  group_by(id_from, pop) %>% 
  reframe(access = sum(dec.jobs)/tot.jobs$jobs) %>% 
  rename(id = id_from) %>% 
  left_join(for.data %>% select(id = id_hex)) %>% 
  st_as_sf()
  
access.before %>% 
  ggplot +
  geom_sf(data = reframe(shp.bairro, geometry = st_union(geometry)) %>% st_as_sf(), linewidth = 2, color = 'black') +
  geom_sf(aes(fill = access), color = NA) +
  viridis::scale_fill_viridis(option = 'H', direction = -1, breaks = c(.05, .85), labels = c('low\naccess\nlevels', 'high\naccess\nlevels')) +
  theme_minimal() +
  labs(fill = 'Baseline\nAccessibility\n')

critical.access <- 
  rep(access.before$access, access.before$pop) %>%
  quantile(., .5)

critical.access %>% 
  write_lines('data/critical.access.txt')

access.before %>% 
  mutate(critical = if_else(access < critical.access, 'critical', 'not critical')) %>% 
  ggplot +
  geom_sf(data = reframe(shp.bairro, geometry = st_union(geometry)) %>% st_as_sf(), linewidth = 2, color = 'black', fill = 'white') +
  geom_sf(aes(fill = critical), color = NA) +
  scale_fill_manual(values = c('coral', 'palegreen3')) +
  theme_minimal() +
  labs(fill = 'Baseline\nAccessibility')

baseline.prob.mag <- 
  access.before %>% 
    filter(access < critical.access) %>% 
    .$pop %>% 
    sum

# raptor parlapply after ----------------------------------------------------------------------
# DONT NEED TO RUN THIS CHUNK. IT TAKES UP TO 1 HOUR TO COMPLETE. 
# GO TO LINE 431
gtfs <- alternative.gtfs

# Define the parameters for tidy_raptor
time_range <- 3600 * 2
max_transfers <- 2
stop_ids <- as.character(unique(stop.origins$stop_id))
filter <- FALSE
keep <- 'shortest'

# Function to wrap tidy_raptor
run_tidy_raptor <- function(i) {
  tidy_raptor(
    gtfs = gtfs,
    stop_ids = stop_ids[i],
    time_range = time_range,
    max_transfers = max_transfers,
    arrival = FALSE,
    filter = filter,
    keep = keep
  ) %>% 
    data.table::fwrite(paste0('data/raptor/after/', i, '.csv'))
}

# Detect number of cores
num_cores <- detectCores() - 1  # Reserve 1 core for the system

# Initiate a cluster
cl <- makeCluster(num_cores)

# Export required objects and libraries to the cluster
clusterExport(cl, c("gtfs", 'filter', 'keep',
                    "time_range", "max_transfers", "stop_ids", "run_tidy_raptor"))
# If tidy_raptor is part of a package, load the library on each worker
clusterEvalQ(cl, c(library(GTFSwizard), library(data.table), library(tidyverse)))

# Run parLapply
parLapply(cl, seq_along(stop_ids), run_tidy_raptor)

# Stop the cluster
stopCluster(cl)

rm(cl, filter, keep, max_transfers, num_cores, run_tidy_raptor, time_range)
# processing raptor after ---------------------------------------------------------------------------
files <- list.files('data/raptor/after', '.csv', full.names = T)

dat <-
  lapply(files, data.table::fread) %>% 
  data.table::rbindlist() %>% 
  select(-journey_arrival_time, -journey_departure_time, -transfers) %>% 
  group_by(from_stop_id, to_stop_id) %>% 
  reframe(travel_time = min(travel_time))

dat <- 
  dat %>% 
  filter(!from_stop_id == to_stop_id) %>% 
  filter(to_stop_id %in% stop.destinations$stop_id)

dat <- 
  dat %>% 
  rename(stop_id = from_stop_id)

dat <- 
  dat %>% 
  left_join(select(tibble(stop.origins), -geom), by = 'stop_id', relationship = "many-to-many") %>% 
  rename(from_stop_id = stop_id,
         stop_id = to_stop_id,
         id_from = id)

dat <- 
  dat %>% 
  left_join(select(tibble(stop.destinations), -geom), by = 'stop_id', relationship = "many-to-many") %>% 
  rename(to_stop_id = stop_id,
         id_to = id)

(!unique(dat$from_stop_id) %in% stop.origins$stop_id) %>% as.numeric() %>% sum()
(!unique(dat$to_stop_id) %in% stop.destinations$stop_id) %>% as.numeric() %>% sum()

dat <- 
  dat %>% 
  select(id_from, id_to, travel_time)

gc()

ttm <- 
  dat %>% 
  filter(!id_from == id_to) %>% 
  group_by(id_from, id_to) %>% 
  reframe(travel_time = min(travel_time))

ttm %>% 
  data.table::fwrite('data/raptor/ttm.after.csv')


# access after -------------------------------------------------------------------------------
ttm <-  
  data.table::fread('data/ttm.after.csv')

sigma <- 50.95931 # 50% dos empregos alcançáveis em 60 minutos

od <- 
  tibble(id_from = hex.origins$id) %>% 
  group_by(id_from) %>% 
  reframe(id_to = hex.destinations$id) %>% 
  filter(!id_from == id_to)

access.after <- 
  ttm %>% 
  right_join(od, by = c('id_from', 'id_to')) %>% 
  mutate(travel_time = if_else(is.na(travel_time), Inf, travel_time)) %>% 
  rename(id = id_from) %>% 
  left_join(tibble(hex.origins) %>% select(-geom)) %>% 
  rename(id_from = id,
         id = id_to) %>% 
  left_join(tibble(hex.destinations) %>% select(-geom)) %>% 
  rename(id_to = id) %>% 
  mutate(dec.jobs = jobs * rvmethod::gaussfunc(travel_time/60, 0, sigma)) %>% 
  filter(!dec.jobs == 0) %>% 
  group_by(id_from, pop) %>% 
  reframe(access = sum(dec.jobs)/tot.jobs$jobs) %>% 
  rename(id = id_from) %>% 
  left_join(for.data %>% select(id = id_hex)) %>% 
  st_as_sf()

access.after %>% 
  ggplot +
  geom_sf(data = reframe(shp.bairro, geometry = st_union(geometry)) %>% st_as_sf(), linewidth = 2, color = 'black') +
  geom_sf(aes(fill = access), color = NA) +
  viridis::scale_fill_viridis(option = 'H', direction = -1, breaks = c(.05, .85), labels = c('low\naccess\nlevels', 'high\naccess\nlevels')) +
  theme_minimal() +
  labs(fill = 'Baseline\nAccessibility\n')

critical.access <- 
  read_lines('data/critical.access.txt') %>% as.numeric()

access.after %>% 
  mutate(critical = if_else(access < critical.access, 'critical', 'not critical')) %>% 
  ggplot +
  geom_sf(data = reframe(shp.bairro, geometry = st_union(geometry)) %>% st_as_sf(), linewidth = 2, color = 'black', fill = 'white') +
  geom_sf(aes(fill = critical), color = NA) +
  scale_fill_manual(values = c('coral', 'palegreen3')) +
  theme_minimal() +
  labs(fill = 'Baseline\nAccessibility')

alternative.prob.mag <- 
  access.after %>% 
  filter(access < critical.access) %>% 
  .$pop %>% 
  sum


# costs ---------------------------------------------------------------------------------------
gtfs <- 
  set_dwelltime(for_bus_gtfs, 30, stops = stops) %>% 
  filter_stop(unique(c(stop.origins$stop_id, stop.destinations$stop_id))) %>% 
  filter_date(dates = '2021-12-13') %>% 
  filter_time(from = '6:0:0', '8:0:0')

get_distances(gtfs) %>% #.$trips %>% sum
  reframe(sum(trips*average.distance))
get_fleet(gtfs, 'by.hour') %>% .$fleet %>% max()

get_distances(alternative.gtfs) %>% #.$trips %>% sum
  reframe(sum(trips*average.distance))
get_fleet(alternative.gtfs, 'by.hour') %>% .$fleet %>% max()

# impact assessment data ----------------------------------------------------------------------
impact.data <- 
  full_join(tibble(access.before %>% 
            rename(access.before = access)),
          tibble(access.after %>% 
            rename(access.after = access)) %>% 
            select(-geom),
          by = c('id', 'pop')) %>% 
  left_join(select(tibble(hex.origins), id, R001), by = 'id') %>% 
  mutate(access.diff = access.after - access.before,
         crit.before = ifelse(access.before < critical.access, 'Critical', 'Not Critical'),
         crit.after = ifelse(access.after < critical.access, 'Critical', 'Not Critical'),
         crit.impact = ifelse(crit.before == 'Critical' & crit.after == 'Critical', 'Critical',
                              ifelse(crit.before == 'Not Critical' & crit.after == 'Not Critical', 'Not Critical',
                                     ifelse(crit.after == 'Critical', 'New Critical', 'New Not Critical')))) %>% 
  st_as_sf()

impact.data$crit.impact %>% table
# impact assessment maps ----------------------------------------------------------------------
impact.data %>% 
  ggplot +
  geom_sf(data = reframe(shp.bairro, geometry = st_union(geometry)) %>% st_as_sf(), linewidth = 2, color = 'black', fill = 'white') +
  geom_sf(aes(fill = crit.impact), color = NA) +
  scale_fill_manual(values = c('coral1', 'green4', 'palegreen3')) +
  geom_sf(data = filter(get_stops_sf(gtfs$stops), stop_id %in% stops), aes(shape = 'Hubs')) +
  geom_sf(data = corridors, aes(color = 'Corridors'), linewidth = .5) +
  theme_minimal() +
  theme(legend.title = element_blank())
ggsave('figs/impact.class.png', width = 2470, height = 1200, units = 'px')

impact.data %>% 
  ggplot +
  geom_sf(data = reframe(shp.bairro, geometry = st_union(geometry)) %>% st_as_sf(), linewidth = 2, color = 'black', fill = 'white') +
  geom_sf(aes(fill = access.diff), color = NA) +
  geom_sf(data = filter(get_stops_sf(gtfs$stops), stop_id %in% stops), aes(shape = 'Hubs')) +
  geom_sf(data = corridors, aes(color = 'Corridors'), linewidth = .5) +
  #geom_sf(data = get_shapes_sf(newtrips.gtfs.speed.dwell.lagged)$shapes, aes(color = 'Frequency\nimprovement'), linewidth = .5) +
  scale_fill_gradient(low = 'gray99', high = 'green4', labels = scales::percent_format()) +
  theme_minimal() +
  labs(fill = 'Intervention\nimpact',
       colour = element_blank(),
       shape = element_blank())
ggsave('figs/impact.grad.png', width = 2470, height = 1200, units = 'px')

# impact assessment scatter -------------------------------------------------------------------
impact.data %>% 
  ggplot() +
  geom_point(aes(x = access.before, y = access.diff, color = crit.before), alpha = .5) +
  geom_vline(aes(xintercept = critical.access, linetype = '\nCritical\naccess\nlevel\n')) +
  geom_smooth(aes(x = access.before, y = access.diff, linetype = '\nLinear\nregression\n'), method = 'lm', color = 'black') +
  scale_linetype_manual(values = c('dashed', 'solid')) +
  scale_color_manual(values = c('firebrick', 'green4')) +
  theme_minimal() +
  labs(x = 'Baseline Accessibility', y = 'Accessibility Difference') +
  theme(legend.title = element_blank())
ggsave('figs/scatter.diff.png', width = 2470, height = 1200, units = 'px')

# palma ratio ----
tibble(impact.data) %>% 
  pivot_longer(cols = c(3, 5), names_to = 'scenario', values_to = 'access') %>% 
  select(-geom, -id) %>% 
  arrange(scenario, access) %>% 
  group_by(scenario) %>% 
  mutate(cumpop = cumsum(pop),
         percentile = cumpop/sum(pop)) %>% 
  filter(percentile <= .4 | percentile >= .9) %>% 
  mutate(class = if_else(percentile <= .4, 'bt40', 'tp10')) %>% 
  group_by(scenario, class) %>% 
  reframe(access = mean(access)) %>% 
  group_by(scenario) %>% 
  reframe(palma.ratio = access[2] / access[1])

# gini index ----
gini.dat <- 
tibble(impact.data) %>% 
  pivot_longer(cols = c(3, 5), names_to = 'scenario', values_to = 'access') %>% 
  select(-geom, -id) %>% 
  mutate(tot.access = pop * access) %>% 
  arrange(scenario, access) %>% 
  group_by(scenario) %>% 
  mutate(cum.tot.access = cumsum(tot.access), 
         cumpop = cumsum(pop)) %>%
  mutate(cum.tot.access = cum.tot.access/max(cum.tot.access), 
         cumpop = cumpop/max(cumpop))


gini.dat %>% 
  ggplot() +
  geom_abline(aes(intercept = 0, slope = 1, linetype = 'Equality\nline')) +
  geom_vline(aes(xintercept = .5, linetype = 'Critical\naccessibility\nlevel')) +
  geom_line(aes(y = cum.tot.access, x = cumpop, color = scenario)) +
  scale_x_percent() +
  scale_y_percent() +
  scale_linetype_manual(values = c('dashed', 'solid')) +
  labs(title = 'Lorenz curve', y = 'Cumulative accessibility', x = 'Cumulative population', color = '', linetype = '') +
  theme(legend.position = 'bottom')

Ts <- (.5 * .5) / 2
Tns <- .5 * .75

gini.dat %>% 
  group_by(scenario, cum.tot.access) %>% 
  reframe(cumpop = cumpop[n()],
          critical = if_else(cumpop <= .5, 'Bs', 'Bns')) %>%
  arrange(scenario, cumpop) %>% 
  group_by(scenario, critical) %>% 
  mutate(dx = lead(cumpop) - cumpop,
         ybar = (lead(cum.tot.access) + cum.tot.access) / 2,
         area = dx * ybar) %>% 
  group_by(scenario, critical) %>% 
  reframe(area = sum(area, na.rm = T)) %>% 
  pivot_wider(names_from = 'critical', values_from = 'area') %>% 
  group_by(scenario) %>% 
  reframe(B = Bns + Bs,
          As = Ts - Bs,
          Asn = Tns - Bns,
          A = As + Asn,
          gini = A/(A+B),
          severity = As/(A+B),
          severity.perc = severity/gini)
