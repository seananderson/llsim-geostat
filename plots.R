library(dplyr)
library(ggplot2)
source("theme_sleek.R")
theme_set(theme_sleek())
dir.create("figs", showWarnings = FALSE)

US <- FALSE
# US <- TRUE

if (!US) {
  ind_st <- readRDS("data-generated/index-all-all-sp-on-st-iid-knots-300-share_range-FALSE.rds")
  ind_sp <- readRDS("data-generated/index-all-all-sp-on-st-off-knots-300-share_range-TRUE.rds")
  ind_no <- readRDS("data-generated/index-all-all-sp-off-st-off-knots-300-share_range-TRUE.rds")
} else {
  ind_st <- readRDS("data-generated/index-us-all-sp-on-st-iid-knots-300-share_range-FALSE.rds")
  ind_sp <- readRDS("data-generated/index-us-all-sp-on-st-off-knots-300-share_range-TRUE.rds")
  ind_no <- readRDS("data-generated/index-us-all-sp-off-st-off-knots-300-share_range-TRUE.rds")
}

ind_st <- ind_st |> mutate(family = model, fields = "Spatial + spatiotemporal fields")
ind_sp <- ind_sp |> mutate(family = model, fields = "Spatial fields")
ind_no <- ind_no |> mutate(family = model, fields = "No fields")

ind <- bind_rows(list(ind_st, ind_sp, ind_no)) |>
  # ind <- bind_rows(list(ind_st)) |>
  # filter(family != "NB2") |>
  mutate(fields = factor(fields, levels = c("No fields", "Spatial fields", "Spatial + spatiotemporal fields")))

ind$fields2 <- gsub(" \\+ ", "\\\n\\+ ", ind$fields)
ind$fields2 <- factor(ind$fields2, levels = c("No fields", "Spatial fields", "Spatial\n+ spatiotemporal fields"))
ind$family <- gsub("Gamma", "gamma", ind$family)

if (US) {
  ind <- ind[!(ind$fields == "Spatial + spatiotemporal fields" & ind$family == "Delta-gamma"), ]
  ind <- ind[!(ind$fields == "Spatial + spatiotemporal fields" & ind$family == "Delta-lognormal"), ]
}

unique(ind$fields2)

# dlog <- readr::read_csv("data-raw/logset05.csv")
# saveRDS(dlog, file = "data-raw/logset05.rds")
dlog <- readRDS("data-raw/logset05.rds")

if (US) {
  dlog <- filter(dlog, fleet %in% 1)
}

names(dlog) <- tolower(names(dlog))
dlog$bum <- dlog$c.bum
dlog$bycatch <- dlog[["bum"]]

true <- group_by(dlog, year) |>
  summarize(est = sum(bum))

ind_table <- ind |>
  left_join(rename(true, true = est)) |>
  group_by(family, fields) |>
  mutate(re = (est - true) / true)
if (!US) saveRDS(ind_table, "data-generated/ind-table.rds")
if (US) saveRDS(ind_table, "data-generated/ind-table-us.rds")

ind_table |>
  summarise(
    mare = median(abs(re)),
    rmse = sqrt(mean(re^2)),
    mre = mean(re), coverage = mean(true < upr & true > lwr)
  ) |>
  ungroup() |>
  arrange(mare) |>
  mutate(fields = gsub(" fields", "", fields)) |>
  mutate(fields = gsub("No", "None", fields)) |>
  knitr::kable(
    digits = 3,
    col.names = c("Family", "Fields", "MARE", "RMSE", "MRE", "Coverage")
  )

cols <- RColorBrewer::brewer.pal(n = 4L, name = "Dark2")[1:4]
# names(cols) <- names(unique(ind$family))

gg_scales <- list(
  scale_color_manual(values = cols),
  scale_fill_manual(values = cols),
  # scale_color_brewer(palette = "Set2"),
  # scale_fill_brewer(palette = "Set2"),
  coord_cartesian(ylim = c(0, max(ind$upr) * 0.95)),
  scale_y_continuous(expand = c(0, NA))
)

gg_labs <- list(
  ylab("Bycatch total"),
  xlab("Year"),
  labs(lty = "Type", colour = "Model", fill = "Model")
)

g <- ind |>
  ggplot(aes(year, est,
    ymin = lwr, ymax = upr,
    colour = fields, fill = fields
    # colour = family, fill = family
  )) +
  geom_ribbon(alpha = 0.5, colour = NA) +
  geom_line() +
  geom_line(
    data = true, lty = 2, colour = "black",
    inherit.aes = FALSE, mapping = aes(year, est)
  ) +
  gg_scales +
  gg_labs +
  facet_grid(vars(family), vars(fields)) +
  # facet_grid(vars(family)) +
  # facet_grid(vars(fields2)) +
  guides(fill = "none", colour = "none") +
  theme_sleek()

if (!US) ggsave("figs/all-ts-comparison.png", width = 8, height = 6)
if (US) ggsave("figs/us-ts-comparison.png", width = 8, height = 6)

fit_dl <- readRDS("data-generated/fit-all-dl-sp-on-st-iid-knots-300-share_range-FALSE.rds")
fit_dg <- readRDS("data-generated/fit-all-dg-sp-on-st-iid-knots-300-share_range-FALSE.rds")
fit_nb1 <- readRDS("data-generated/fit-all-nb1-sp-on-st-iid-knots-300-share_range-FALSE.rds")
fit_nb2 <- readRDS("data-generated/fit-all-nb2-sp-on-st-iid-knots-300-share_range-FALSE.rds")
# fit_nb2 <- readRDS("data-generated/fit-us-nb2-sp-on-st-iid-knots-300-share_range-FALSE.rds")

g1 <- sdmTMB::plot_anisotropy(fit_nb2) +
  theme_sleek() +
  coord_equal(xlim = c(-150, 150), ylim = c(-55, 55)) +
  ggtitle("NB2 model anisotropy")
g2 <- sdmTMB::plot_anisotropy(fit_dl) +
  theme_sleek() +
  coord_equal(xlim = c(-150, 150), ylim = c(-55, 55)) +
  ggtitle("Delta-lognormal model anisotropy")
cowplot::plot_grid(g1, g2, nrow = 2L, align = "v")

ggsave("figs/dl-aniso.png", width = 6, height = 4.2)

mesh <- fit_nb2$spde
dobs <- fit_nb2$data

blue <- RColorBrewer::brewer.pal(5, "Blues")[4]
g <- ggplot() +
  geom_point(data = dobs, mapping = aes(X, Y), alpha = 0.4, size = 0.5, colour = "grey75") +
  inlabru::gg(mesh$mesh, edge.color = blue, ext.color = blue) +
  coord_fixed() +
  theme_sleek()
ggsave("figs/mesh.png", width = 4, height = 4)

theme_set(theme_sleek())


r <- list()
s <- simulate(fit_nb2, nsim = 200)
r[[1]] <- sdmTMBextra::dharma_residuals(s, fit_nb2)

s <- simulate(fit_nb1, nsim = 200)
r[[2]] <- sdmTMBextra::dharma_residuals(s, fit_nb1)

s <- simulate(fit_dl, nsim = 200)
r[[3]] <- sdmTMBextra::dharma_residuals(s, fit_dl)

s <- simulate(fit_dg, nsim = 200)
r[[4]] <- sdmTMBextra::dharma_residuals(s, fit_dg)

names(r) <- c("NB2", "NB1", "Delta-lognormal", "Delta-gamma")
rr <- bind_rows(r, .id = "Family")
ggplot(rr, aes(expected, observed)) +
  geom_point() +
  coord_equal() +
  facet_wrap(vars(Family), nrow = 2) +
  geom_abline(slope = 1, intercept = 0)

r <- list()
set.seed(1)
r[[1]] <- data.frame(y = residuals(fit_nb2))

set.seed(1)
r[[2]] <- data.frame(y = residuals(fit_nb1))

set.seed(1)
r[[3]] <- data.frame(y = residuals(fit_dl)) # first model only

set.seed(1)
r[[4]] <- data.frame(y = residuals(fit_dg)) # first model only

names(r) <- c("NB2", "NB1", "Delta-lognormal", "Delta-gamma")
rr <- bind_rows(r, .id = "Family")

ggplot(rr, aes(sample = y)) +
  stat_qq(alpha = 0.4) +
  stat_qq_line() +
  coord_fixed() +
  facet_wrap(vars(Family), nrow = 2) +
  labs(x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_sleek()
ggsave("figs/qq.png", width = 4.5, height = 4.5)

fit <- fit_nb2
grid_all <- readRDS("data-generated/grid_all.rds")
p_grid <- predict(fit, newdata = grid_all)

p_sims <- predict(fit_nb2, newdata = grid_all, nsim = 200)
grid_all$cv <- apply(p_sims, 1, function(x) sd(exp(x)) / mean(exp(x)))

map_data <- rnaturalearth::ne_countries(
  scale = "small",
  returnclass = "sf"
)

map_data_cropped <- map_data
# sf::st_crop(
# sf::st_make_valid(map_data)
# c(
#   xmin = min(dlog$lon) - 20, ymin = min(dlog$lat) - 20,
#   xmax = max(dlog$lon) + 20, ymax = max(dlog$lat) + 20
# )
# )

gg_coord <- coord_sf(
  expand = FALSE, crs = "WGS84",
  xlim = c(range(dlog$lon) + c(-5, 5)),
  ylim = c(range(dlog$lat) + c(-5, 5))
)


plot_map <- function(dat, column, label = "", nrow = NULL) {
  g <- ggplot() +
    geom_raster(
      data = dat,
      mapping = aes(fill = {{ column }}, x = lon, y = lat)
    ) +
    geom_sf(data = map_data_cropped, colour = "grey70", fill = "grey70") +
    scale_fill_viridis_c(trans = "sqrt", option = "C") +
    gg_coord +
    theme_sleek() +
    theme(legend.position = "bottom") +
    labs(fill = label, x = "Longitude", y = "Latitude") +
    gg_coord

  if (length(unique(dat$year)) > 1) g <- g + facet_wrap(vars(year), nrow = nrow)
  g
}

max_yr <- max(dlog$year)
min_yr <- min(dlog$year)

yrs <- seq(min_yr, max_yr, 2)
length(yrs)

g <- p_grid |>
  filter(year %in% yrs) |>
  plot_map(exp(est), nrow = 3) +
  theme(legend.position = "bottom") +
  labs(fill = "Predicted blue marlin bycatch")
ggsave("figs/dl-all-predicted.png", width = 10, height = 7)

g <- filter(grid_all, year %in% c(2000)) |> # pick one; very similar
  plot_map(column = cv) +
  scale_fill_viridis_c(option = "A", trans = "log10") +
  theme_sleek() +
  labs(fill = "CV")
ggsave("figs/cv.png", width = 5, height = 4)

rev_scale2 <- scale_fill_gradient2(
  low = scales::muted("blue"),
  high = scales::muted("red")
)

po <- p_grid |>
  filter(year %in% 1990) |>
  plot_map(omega_s) + rev_scale2 +
  labs(fill = "Deviation in\nlink space") +
  theme(legend.position = "right")
ggsave("figs/omega.png", width = 6, height = 5)

pe <- p_grid |>
  filter(year %in% yrs) |>
  plot_map(epsilon_st, nrow = 3L) + rev_scale2 +
  theme(legend.position = "bottom") +
  labs(fill = "Deviation in\nlink space")
ggsave("figs/epsilon.png", width = 10, height = 7)


# compare ----------------------

if (Sys.info()[["user"]] == "seananderson") {
  if (US) {
    dl <- readr::read_csv("/Users/seananderson/Library/CloudStorage/Box-Box/LLSim/For comparison with spatial/Output USA 5 percent/Blue marlin bycatch/Blue marlinbycatchTMBdelta-LognormalAnnualSummary.csv")
    tw <- readr::read_csv("/Users/seananderson/Library/CloudStorage/Box-Box/LLSim/For comparison with spatial/Output USA 5 percent/Blue marlin bycatch/Blue marlinbycatchTMBtweedieAnnualSummary.csv")
    nb2 <- readr::read_csv("/Users/seananderson/Library/CloudStorage/Box-Box/LLSim/For comparison with spatial/Output USA 5 percent/Blue marlin bycatch/Blue marlinbycatchTMBnbinom2AnnualSummary.csv")
    nb1 <- readr::read_csv("/Users/seananderson/Library/CloudStorage/Box-Box/LLSim/For comparison with spatial/Output USA 5 percent/Blue marlin bycatch/Blue marlinbycatchTMBnbinom1AnnualSummary.csv")
  }

  # --------

  if (!US) {
    dl <- readr::read_csv("/Users/seananderson/Library/CloudStorage/Box-Box/LLSim/For comparison with spatial/Output All 5 percent/Blue marlin bycatch/Blue marlinbycatchTMBdelta-LognormalAnnualSummary.csv")
    tw <- readr::read_csv("/Users/seananderson/Library/CloudStorage/Box-Box/LLSim/For comparison with spatial/Output All 5 percent/Blue marlin bycatch/Blue marlinbycatchTMBtweedieAnnualSummary.csv")
    nb2 <- readr::read_csv("/Users/seananderson/Library/CloudStorage/Box-Box/LLSim/For comparison with spatial/Output All 5 percent/Blue marlin bycatch/Blue marlinbycatchTMBnbinom2AnnualSummary.csv")
    nb1 <- readr::read_csv("/Users/seananderson/Library/CloudStorage/Box-Box/LLSim/For comparison with spatial/Output All 5 percent/Blue marlin bycatch/Blue marlinbycatchTMBnbinom1AnnualSummary.csv")
  }

  nb2$model <- "NB2"
  dl$model <- "Delta-lognormal"
  nb1$model <- "NB1"
  tw$model <- "Tweedie"

  dd <- bind_rows(list(nb2, dl, nb1, tw))
  # dd <- bind_rows(list(nb2))

  # mult <- 10000
  ggplot(dd, aes(Year, Total, ymin = TotalLCI, ymax = TotalUCI, colour = model, fill = model)) +
    geom_line() +
    geom_ribbon(alpha = 0.5, colour = NA) +
    geom_line(data = true, mapping = aes(year, est), inherit.aes = FALSE, lty = 2)

  dd2 <- select(dd, year = Year, est = Total.mean, lwr = TotalLCI, upr = TotalUCI, model)
  dd2$model <- paste0(dd2$model)

  ind2 <- select(ind, year, est, lwr, upr, model, fields)
  ind2 <- mutate(ind2, model = paste(model, "sdmTMB", fields))
  ind2$fields <- NULL

  ind_table2 <- bind_rows(ind2, dd2) |>
    left_join(rename(true, true = est)) |>
    group_by(model) |>
    mutate(re = (est - true) / true)

  if (!US) saveRDS(ind_table2, "data-generated/ind-table-compare.rds")
  if (US) saveRDS(ind_table2, "data-generated/ind-table-us-compare.rds")

  ind_table2 |>
    summarise(
      mare = median(abs(re)),
      # rmse = sqrt(mean(re^2)),
      mre = mean(re), coverage = mean(true < upr & true > lwr)
    ) |>
    ungroup() |>
    arrange(mare) |>
    knitr::kable(digits = 2)
}
