library(dplyr)
library(sdmTMB)
library(ggplot2)

# 1.25 hours in meeting
# 0.5 hour this evening
# 1 hour this evening

dobs <- readr::read_csv("data-raw/obsset05.csv")
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

dus <- filter(dobs, fleet == 1)
# dus <- filter(dobs)
plot(dus$lon, dus$lat)

dus <- filter(dus, lat > -10, lon < -20)
plot(dus$lon, dus$lat)

dus$X <- NULL
dus$Y <- NULL
dus <- sdmTMB::add_utm_columns(dus, ll_names = c("lon", "lat"), utm_crs = 32619)
# dus <- sdmTMB::add_utm_columns(dus, ll_names = c("lon", "lat"), utm_crs = 32626)
dus$light_f <- as.factor(dus$light)
dus$season_f <- as.factor(dus$season)

plot(dus$X, dus$Y)

map_data_cropped_utm <- sf::st_transform(map_data_cropped, crs = 32619)

ggplot() + #geom_sf(data = map_data_cropped_utm) +
  geom_point(data = dus, mapping = aes(x = X * 1000, y = Y * 1000), size = 0.2, alpha = 0.1)

nrow(dus)

ggplot(dus, aes(X, Y, colour = log(cpue.BUM))) + geom_point() +
  facet_wrap(~year)

ggplot(dus, aes(X, Y, colour = log(c.BUM))) + geom_point() +
  facet_wrap(~year)

ggplot(filter(dus, c.BUM > 0), aes(X, Y, colour = log(c.BUM))) + geom_point() +
  facet_wrap(~year)

mean(dus$c.BUM == 0)

ggplot(dus, aes(X, Y, colour = log(cpue.SWO))) + geom_point() +
  facet_wrap(~year)

dlog <- readr::read_csv("data-raw/logset05.csv")
dp <- filter(dlog, fleet == 1)
# dp <- filter(dlog)
nrow(dp)

dp <- filter(dp, lat > -10, lon < -20)
plot(dp$lon, dp$lat, pch = ".")
dp <- sdmTMB::add_utm_columns(dp, ll_names = c("lon", "lat"), utm_crs = 32619)
# dp <- sdmTMB::add_utm_columns(dp, ll_names = c("lon", "lat"), utm_crs = 32626)
dp$light_f <- as.factor(dp$light)
dp$season_f <- as.factor(dp$season)

nrow(dp)
grid <- select(dp, X, Y) |> distinct()
nrow(grid)

grid_all <- purrr::map_dfr(sort(unique(dp$year)), function(x) {
  bind_cols(tibble(year = x), grid)
})
grid_all <- left_join(grid_all, distinct(select(dp, X, Y, lon, lat)))
grid_all$month <- 6
grid_all$light_f <- sort(unique(dus$light_f))[1]
grid_all$season_f <- sort(unique(dus$season_f))[1]
grid_all$hbf <- mean(dus$hbf)
nrow(grid_all)

dall <- bind_rows(select(dus, X, Y), select(dp, X, Y))

mesh1 <- make_mesh(dall, c("X", "Y"), cutoff = 150)
mesh <- make_mesh(dus, c("X", "Y"), cutoff = 150, mesh = mesh1$mesh)

mesh$mesh$n
# plot(mesh)
plot(mesh$mesh, asp = 1, main = NULL)
points(dus$X ,dus$Y, pch = 21, col = "red")
# m <- sdmTMB(c.SWO ~ as.factor(year) + s(month, bs = "cc") + s(hbf, k = 5),

m1 <- sdmTMB(
  # c.SWO ~ as.factor(year) + season_f + s(hbf, k = 5) + light_f,
  c.BUM ~ as.factor(year) + season_f + s(hbf, k = 5) + light_f,
  data = dus,
  mesh = mesh,
  family = nbinom2(),
  time = "year",
  offset = log(dus$hooks),
  spatial = "on",
  spatiotemporal = "iid",
  control = sdmTMBcontrol(newton_loops = 1),
  silent = FALSE
)
m

max(m$gradients)

sanity(m)
sanity(m1)

# ggeffects::ggeffect(m, terms = "light_f")
# ggeffects::ggeffect(m, terms = "season_f")

# plot_smooth(m, select = 1)
# p <- predict(m, newdata = NULL)
r <- residuals(m)
qqnorm(r)
qqline(r)

# dplyr::distinct()
# dsf <- dus |>
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

dus$resid <- residuals(m)

dus |>
  ggplot(aes(lon, lat, colour = resid)) +
  geom_point(size = 0.4) +
  facet_wrap(~year) +
  scale_colour_gradient2() +
  coord_equal()

s <- simulate(m, nsim = 100L)
mean(s == 0)
mean(dus$c.SWO == 0)
# mean(dus$c.BUM == 0)

dharma_residuals(s, m)

pred <- predict(m, newdata = grid_all, nsim = 100)
grid_all$cv <- apply(pred, 1, function(x) sd(exp(x)) / mean(exp(x)))

filter(grid_all, year == min(dus$year)) |>
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
p2 <- predict(m, newdata = dus)
p3 <- p2$est + log(p2$hooks)
plot(p1$est, p3);abline(0, 1)

p <- predict(m, newdata = dp)
p$pred <- exp(p$est + log(dp$hooks))
# saveRDS(select(p, year, month, lon, lat, pred),
saveRDS(select(p, pred),
  file = "data-generated/sdmTMB-pred-log.rds")

# TODO
# - [ ] do with US and with all data?
# - [ ] look at various predictors
# - [ ] figure out the projection that makes most sense
# - [ ] calculate index predicting on all and predicting on only test
# - [ ] which species? BUM? c.SWO? enough data for each?
# - [ ] compare to other models that have been done in some way
# - [ ] write up in some form of R Markdown report
# - [ ] write up summary for overall report
