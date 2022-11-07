library(dplyr)
library(sdmTMB)
library(ggplot2)

# 1.25 hours in meeting
# 0.5 hour this evening
# 1 hour this evening
# 5 hours today

dobs <- readr::read_csv("data-raw/obstrip05.csv")
names(dobs) <- tolower(names(dobs))
plot(dobs$lon, dobs$lat)
plot(dobs$lon5, dobs$lat5)
nrow(dobs)
names(dobs)
## table(dobs$gear) #< exclude... lots
# table(dobs$month) s()?
# table(dobs$season)  or this
table(dobs$light) #< light stick... categorical
table(dobs$hbf) #< hooks between floats - how deep linear - important
# table(dobs$area) #<

# table(dobs$month, dobs$season) #

# 5 x 5 variables

# fleets - could be fleet 1 : US (or all)
# hooks - #
# hook - J vs. circle - include likely?
# hok

# c.BUM - this is the main response
# k.
# w.
# k. and w. as habitat covariates - one is drop Phil an email
# multiply by some constant
# one is relative? one absolute?
# could use as predictor?

nrow(dobs)

plot(dobs$lon, dobs$lat)

map_data <- rnaturalearth::ne_countries(
  scale = "small",
  returnclass = "sf"
)
sf::st_bbox(map_data)

table(sf::st_is_valid(map_data))

map_data_cropped <- sf::st_crop(
  sf::st_make_valid(map_data),
    c(xmin = min(dobs$lon) - 5, ymin = min(dobs$lat) + 5,
      xmax = max(dobs$lon) + 5, ymax = max(dobs$lat) + 5)
)

ggplot() + geom_sf(data = map_data_cropped) +
  geom_point(data = dobs, mapping = aes(x = lon, y = lat),
    size = 0.2, alpha = 0.1)

# dobs <- filter(dobs, fleet == 1)
# dobs <- filter(dobs)
# plot(dobs$lon, dobs$lat)

# dobs <- filter(dobs, lat > -10, lon < -20)
# plot(dobs$lon, dobs$lat)

# Albers
# This projection is best suited for equal-area mapping of land masses in mid-latitudes extending in an east-to-west orientation rather than those extending north to south. It is best practice to place standard parallels at one-sixth of the latitude range below the top and above the bottom of the area to be mapped. After ellipsoidal equations were developed, the projection became standard for equal-area maps of the United States.
# ArcGIS Total range in latitude from north to south should not >exceed 30–35°.

# https://desktop.arcgis.com/en/arcmap/latest/map/projections/lambert-conformal-conic.htm#:~:text=Lambert%20conformal%20conic%20is%20a,away%20from%20the%20standard%20parallels.


# https://desktop.arcgis.com/en/arcmap/latest/map/projections/equidistant-conic.htm
# https://en.wikipedia.org/wiki/Equidistant_conic_projection

# False Easting
# False Northing
# Central Meridian
# Standard Parallel 1
# Standard Parallel 2
# Latitude Of Origin

Albers <- "+proj=aea +lat_0=0 +lon_0=-20 +lat_1=-30 +lat_2=35 +x_0=0 +y_0=0
+datum=NAD83 +units=km +no_defs"

EquidistantConic <- "+proj=eqdc +lat_0=0 +lon_0=-20 +lat_1=-30 +lat_2=35 +x_0=0 +y_0=0
+datum=WGS84 +units=km +no_defs"

.crs <- Albers
.crs <- EquidistantConic
# .crs <- 32626

map_data_albers <- sf::st_transform(map_data_cropped, crs = .crs)
dobs_sf <- sf::st_as_sf(dobs, coords = c("lon", "lat"), crs = "WGS84")
dobs_sf <- sf::st_transform(dobs_sf, crs = .crs)
xy <- sf::st_coordinates(dobs_sf)
xy <- mutate(as.data.frame(xy), X100 = X/100, Y100 = Y/100)
dobs$X <- xy$X100
dobs$Y <- xy$Y100

ggplot() + geom_sf(data = map_data_albers) + geom_sf(data = dobs_sf, alpha = 0.2)

# dobs$X <- NULL
# dobs$Y <- NULL
# dobs <- sdmTMB::add_utm_columns(dobs, ll_names = c("lon", "lat"), utm_crs = 32619)
# # dobs <- sdmTMB::add_utm_columns(dobs, ll_names = c("lon", "lat"), utm_crs = 32626)
dobs$light_f <- as.factor(dobs$light)
dobs$season_f <- as.factor(dobs$season)
dobs$year_f <- as.factor(dobs$year)

# map_data_cropped_utm <- sf::st_transform(map_data_cropped, crs = 32619)

ggplot(dobs, aes(X, Y, colour = log(swo))) +
  geom_point() +
  facet_wrap(~year_f) +
  scale_colour_viridis_c()

ggplot(dobs, aes(X, Y, colour = log(bum))) +
  geom_point() +
  facet_wrap(~year) +
  scale_colour_viridis_c()

mean(dobs$bum == 0)
mean(dobs$swo == 0)

dlog <- readr::read_csv("data-raw/logtrip05.csv")
# dp <- filter(dlog, fleet == 1)
# dp <- filter(dlog)
nrow(dp)

dp_sf <- sf::st_as_sf(dp, coords = c("lon", "lat"), crs = "WGS84")
dp_sf <- sf::st_transform(dp_sf, crs = .crs)
dp_xy <- sf::st_coordinates(dp_sf)
dp_xy <- mutate(as.data.frame(dp_xy), X100 = X/100, Y100 = Y/100)
dp$X <- dp_xy$X100
dp$Y <- dp_xy$Y100

# dp <- filter(dp, lat > -10, lon < -20)
# plot(dp$lon, dp$lat, pch = ".")
# dp <- sdmTMB::add_utm_columns(dp, ll_names = c("lon", "lat"), utm_crs = 32619)
# dp <- sdmTMB::add_utm_columns(dp, ll_names = c("lon", "lat"), utm_crs = 32626)
dp$light_f <- as.factor(dp$light)
dp$season_f <- as.factor(dp$season)
dp$year_f <- as.factor(dp$year)

# just for visualizing maps:
grid <- select(dp, X, Y) |> distinct()
nrow(grid)

# ggplot() + geom_sf(data = map_data_albers) + geom_sf(data = dp_sf, alpha = 0.2)

grid_all <- purrr::map_dfr(sort(unique(dp$year)), function(x) {
  bind_cols(tibble(year = x), grid)
})
grid_all <- left_join(grid_all, distinct(select(dp, X, Y, lon, lat)))
grid_all$month <- 6
grid_all$light_f <- sort(unique(dobs$light_f))[1]
grid_all$season_f <- sort(unique(dobs$season_f))[1]
grid_all$hbf <- mean(dobs$hbf)
grid_all$year_f <- as.factor(grid_all$year)
nrow(grid_all)

# try grid with all; could be just one or the other
dall <- bind_rows(select(dobs, X, Y), select(grid, X, Y)) # FIXME?
mesh_all <- make_mesh(dall, c("X", "Y"), n_knots = 200) # cutoff?
mesh <- make_mesh(dobs, c("X", "Y"), mesh = mesh_all$mesh)

mesh$mesh$n
plot(mesh$mesh, asp = 1, main = NULL)
points(dobs$X ,dobs$Y, pch = 21, col = "red")

ggplot() +
  geom_point(data = dobs, mapping = aes(X, Y), alpha = 0.7) +
  inlabru::gg(mesh$mesh) +
  coord_fixed()

# m <- sdmTMB(c.SWO ~ as.factor(year) + s(month, bs = "cc") + s(hbf, k = 5),

table(dobs$light_f)
table(dobs$year_f)
table(dobs$season_f)

# make sure there aren't years with 100% or 0% observations:
group_by(dobs, year) |>
  summarise(pos_swo = mean(swo > 0), pos_bum = mean(bum > 0), n = n()) |>
  as.data.frame()

table(dobs$year, dobs$season)
table(dobs$year, dobs$light_f)
table(dobs$light_f, dobs$season_f)

# So, need RW/AR1 on year... or can't do binomial on SWO

mean(log(dobs$hooks))

m <- sdmTMB(
  # swo ~ as.factor(year) + season_f + s(hbf, k = 5) + light_f,
  bum ~ 0 + year_f + season_f + log(hbf) + light_f,
  # bum ~ 0 + season_f + log(hbf) + light_f,
  data = dobs,
  mesh = mesh,
  # time_varying = ~ 1, # FIXME: consider including/not
  family = nbinom2(),
  # family = tweedie(),
  # family = delta_gamma(),
  # family = delta_lognormal(),
  time = "year",
  offset = log(dobs$hooks),
  spatial = "on",
  # anisotropy = TRUE, # with and without?
  spatiotemporal = "iid",
  priors = sdmTMBpriors( # to include or not?
    matern_s = pc_matern(range_gt = 10, sigma_lt = 1),
    matern_st = pc_matern(range_gt = 10, sigma_lt = 1)
  ),
  # control = sdmTMBcontrol(newton_loops = 1),
  silent = FALSE
)
m

sanity(m)

# use update()...

# sdmTMB:::plot_anisotropy2(m)

# need custom ggeffects currently:
ggeffects::ggeffect(m, terms = "light_f") |> plot()
ggeffects::ggeffect(m, terms = "season_f") |> plot()
ggeffects::ggeffect(m, terms = "year_f") |> plot()
ggeffects::ggeffect(m, terms = "hbf") |> plot()

# plot_smooth(m, select = 1)
# p <- predict(m, newdata = NULL)
r <- residuals(m)
qqnorm(r)
qqline(r)

# dplyr::distinct()
# dsf <- dobs |>
#   sf::st_as_sf(coords = c("X", "Y"))
#
# dcc <- summarize(dsf) |>
#   concaveman::concaveman(concavity = 1)
#
# plot(dcc)

# grid_all$hooks <- 1
pall <- predict(m, newdata = grid_all)

library(ggplot2)
theme_set(theme_light())

ggplot(pall, aes(lon, lat, fill = exp(est))) +
  geom_raster() +
  facet_wrap(~year) +
  scale_fill_viridis_c(trans = "sqrt") +
  coord_equal()

pall |> filter(year == min(pall$year)) |>
  ggplot(aes(lon, lat, fill = omega_s)) +
  geom_raster() +
  scale_fill_gradient2() +
  coord_equal()

pall |>
  ggplot(aes(lon, lat, fill = epsilon_st)) +
  geom_raster() +
  facet_wrap(~year) +
  scale_fill_gradient2() +
  coord_equal()

dobs$resid <- residuals(m)

dobs |>
  ggplot(aes(lon, lat, colour = resid)) +
  geom_point(size = 0.4) +
  facet_wrap(~year) +
  scale_colour_gradient2() +
  coord_equal()

s <- simulate(m, nsim = 100L)
mean(s == 0)
mean(dobs$swo == 0)
# mean(dobs$bum == 0)

dharma_residuals(s, m)

pred <- predict(m, newdata = grid_all, nsim = 100)
grid_all$cv <- apply(pred, 1, function(x) sd(exp(x)) / mean(exp(x)))

filter(grid_all, year == min(dobs$year)) |>
  ggplot(aes(lon, lat, fill = cv)) +
  geom_raster() +
  scale_fill_viridis_c(option = "B") +
  coord_equal()

pred <- predict(m, newdata = dp, return_tmb_object = TRUE)
ind <- get_index(pred, bias_correct = FALSE)

ggplot(ind, aes(year, est, ymin = lwr, ymax = upr)) +
  geom_ribbon(alpha = 0.5) +
  geom_line() +
  ylab("Predicted c.SOW logbook total") +
  xlab("Year")

p1 <- predict(m, newdata = NULL)
p2 <- predict(m, newdata = dobs)
p3 <- p2$est + log(p2$hooks)
plot(p1$est, p3);abline(0, 1)

p <- predict(m, newdata = dp)
p$pred <- exp(p$est + log(dp$hooks))
# saveRDS(select(p, year, month, lon, lat, pred),
saveRDS(select(p, pred),
  file = "data-generated/sdmTMB-pred-log.rds")

total_true <- readr::read_csv("data-raw/TotalAnnualCatches.csv")

# TODO
# - [ ] do with US and with all data?
# - [ ] look at various predictors
# - [?] figure out the projection that makes most sense
# - [ ] calculate index predicting on all and predicting on only test
# - [ ] which species? BUM? c.SWO? enough data for each?
# - [ ] compare to other models that have been done in some way
# - [ ] write up in some form of R Markdown report
# - [ ] write up summary for overall report
# - [ ] find if dobs are also in dlog

# - thoughts:
# - OK to do trip-level full dataset and set-level  and trip level US? just one for US? just US?
# - can do all families... how important? maybe delta-Gamma, Tweedie, and NB2?
# - skip cross-val? gonna be slow
# - where can I get other model-based estimates for comparison
# - where get truth from?
