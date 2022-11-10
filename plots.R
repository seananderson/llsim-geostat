library(dplyr)
library(ggplot2)
source("theme_sleek.R")
theme_set(theme_sleek())

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
  filter(family != "NB2") |>
  mutate(fields = factor(fields, levels = c("No fields", "Spatial fields", "Spatial + spatiotemporal fields")))

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

ind_table <- ind |> left_join(rename(true, true = est)) |>
  group_by(family, fields) |>
  mutate(re = (est - true) / true)
if (!US) saveRDS(ind_table, "data-generated/ind-table.rds")
if (US) saveRDS(ind_table, "data-generated/ind-table-us.rds")

ind_table |> summarise(mare = median(abs(re)),
    rmse = sqrt(mean(re^2)),
    mre = mean(re), coverage = mean(true < upr & true > lwr)) |>
  ungroup() |>
  arrange(mare) |>
  mutate(fields = gsub(" fields", "", fields)) |>
  mutate(fields = gsub("No", "None", fields)) |>
  knitr::kable(
    digits = 3,
    col.names = c("Family", "Fields", "MARE", "RMSE", "MRE", "Coverage")
  )

cols <- RColorBrewer::brewer.pal(n = 4L, name = "Dark2")[1:3]
# names(cols) <- names(unique(ind$family))

gg_scales <- list(
  scale_color_manual(values = cols),
  scale_fill_manual(values = cols),
  # scale_color_brewer(palette = "Set2"),
  # scale_fill_brewer(palette = "Set2"),
  coord_cartesian(ylim = c(0, max(ind$upr) * 0.95)),
  scale_y_continuous(expand = c(0, NA)))

gg_labs <- list(
  ylab("Bycatch total"),
  xlab("Year"),
  labs(lty = "Type", colour = "Model", fill = "Model"))

g <- ggplot(ind, aes(year, est,
  ymin = lwr, ymax = upr,
  colour = fields, fill = fields
)) +
  geom_ribbon(alpha = 0.5, colour = NA) +
  geom_line() +
  geom_line(data = true, lty = 2, colour = "black",
    inherit.aes = FALSE, mapping = aes(year, est)) +
  gg_scales +
  gg_labs +
  facet_grid(vars(family), vars(fields)) +
  # facet_grid(vars(family)) +
  guides(fill = "none", colour = "none") +
  theme_sleek()
print(g)

dir.create("figs", showWarnings = FALSE)

if (!US) ggsave("figs/all-ts-comparison.png", width = 7.2, height = 3.5)
if (US) ggsave("figs/us-ts-comparison.png", width = 7.2, height = 3.5)



fit_dl <- readRDS("data-generated/fit-all-dl-sp-on-st-iid-knots-300-share_range-FALSE.rds")
fit_dg <- readRDS("data-generated/fit-all-dg-sp-on-st-iid-knots-300-share_range-FALSE.rds")

sdmTMB::plot_anisotropy(fit_dl) +
  theme_sleek()
ggsave("figs/dl-aniso.png", width = 6, height = 3.2)


fit_names <- c(
  "fit-all-dl-sp-on-st-iid-knots-300-share_range-FALSE.rds",
  "fit-all-dg-sp-on-st-iid-knots-300-share_range-FALSE.rds",
  "fit-all-dl-sp-on-st-off-knots-300-share_range-TRUE.rds",
  "fit-all-dg-sp-on-st-off-knots-300-share_range-TRUE.rds",
  "fit-all-dl-sp-off-st-off-knots-300-share_range-TRUE.rds",
  "fit-all-dg-sp-off-st-off-knots-300-share_range-TRUE.rds"
)

d <- tidy(fit_dl, effects = "ran_pars", model = 1, conf.int = TRUE)
ggplot(d, aes(estimate, y = term, xmin = conf.low,  xmax = conf.high)) +
  geom_point() +
  geom_linerange()


families <- c(
  "Delta-lognormal",
  "Delta-Gamma",
  "Delta-lognormal",
  "Delta-Gamma",
  "Delta-lognormal",
  "Delta-Gamma"
)

spatial <- c(
  "Spatial",
  "Spatial",
  "Spatial",
  "Spatial",
  "",
  ""
)
spatiotemporal <- c(
  " + spatiotemporal",
  " + spatiotemporal",
  "",
  "",
  "",
  ""
)

# fits <- list()
# for (i in 1:6) {
#   fits[[i]] <- readRDS(paste0("data-generated/", fit_names[i]))
#   # print(fits[[i]])
# }
# dd1 <- purrr::map_dfr(1:6, function(i) {
#   d <- tidy(fits[[i]], "ran_pars", conf.int = TRUE, model = 1)
#   d$component <- "Binomial"
#   d$spatial <- spatial[i]
#   d$spatiotemporal <- spatiotemporal[i]
#   d$family <- families[i]
#   d
# })
# dd2 <- purrr::map_dfr(1:6, function(i) {
#   d <- tidy(fits[[i]], "ran_pars", conf.int = TRUE, model = 2)
#   d$component <- "Positive"
#   d$spatial <- spatial[i]
#   d$spatiotemporal <- spatiotemporal[i]
#   d$family <- families[i]
#   d
# })
#
# dd <- bind_rows(dd1, dd2)
# dd$lab <- paste(dd$spatial, dd$spatiotemporal, sep = "")
# dd$lab[dd$lab == ""] <- "No fields"
# dd <- filter(dd, family == "Delta-lognormal")
#
# ggplot(dd, aes(estimate, y = term, xmin = conf.low,  xmax = conf.high, colour = lab)) +
#   geom_point(position = position_dodge(width = 1)) +
#   geom_linerange(position = position_dodge(width = 1)) +
#   facet_grid(vars(family), vars(component), scales = "free_x") +
#   theme_sleek() +
#   scale_x_log10() +
#   theme(axis.title.y = element_blank()) +
#   labs(x = "Estimate", colour = "Model configuration") +
#   scale_colour_brewer(palette = "Set2")
# ggsave("figs/ran-coefs.png", width  = 6.5, height = 2.5)
#
# d <- tidy(fit_dl, "ran_pars", conf.int = TRUE, model = 1)
# ggplot(d, aes(estimate, y = term, xmin = conf.low,  xmax = conf.high)) +
#   geom_point() +
#   geom_linerange()
#
# d <- tidy(fit_dl, conf.int = TRUE, model = 2)
# ggplot(d, aes(estimate, y = term, xmin = conf.low,  xmax = conf.high)) +
#   geom_point() +
#   geom_linerange()

mesh <- fit_dl$mesh
dobs <- fit_dl$data

blue <- RColorBrewer::brewer.pal(5, "Blues")[4]
ggplot() +
  geom_point(data = dobs, mapping = aes(X, Y), alpha = 0.4, size = 0.5, colour = "grey75") +
  inlabru::gg(mesh$mesh, edge.color = blue, ext.color = blue) +
  coord_fixed()  +
  theme_sleek()
ggsave("figs/mesh.png", width = 4, height = 4)

theme_set(theme_sleek())

set.seed(1)
rdl <- data.frame(y = residuals(fit_dl))
# set.seed(1)
# rdg <- data.frame(y = residuals(fit_dg))

g1 <- ggplot(rdl, aes(sample = y)) + stat_qq(alpha = 0.4) + stat_qq_line() +
  coord_fixed() +
  labs(x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_sleek()
  # ggtitle("Delta-Lognormal")
# g2 <- ggplot(rdg, aes(sample = y)) + stat_qq(alpha = 0.4) + stat_qq_line() +
#   coord_fixed() +
#   labs(x = "Theoretical quantiles", y = "Sample quantiles") +
#   ggtitle("Delta-Gamma")

# cowplot::plot_grid(g1, g2, ncol = 2)
ggsave("figs/qq.png", width = 3.7, height = 3.7)

grid_all <- readRDS("data-generated/grid_all.rds")
p_grid <- predict(fit, newdata = grid_all)

map_data <- rnaturalearth::ne_countries(
  scale = "small",
  returnclass = "sf"
)

map_data_cropped <- sf::st_crop(
  sf::st_make_valid(map_data),
  c(
    xmin = min(dlog$lon) - 20, ymin = min(dlog$lat) - 20,
    xmax = max(dlog$lon) + 20, ymax = max(dlog$lat) + 20
  )
)

gg_coord <- coord_sf(expand = FALSE, crs = "WGS84",
  xlim = c(range(dlog$lon) + c(-5, 5)),
  ylim = c(range(dlog$lat) + c(-5, 5))
)


plot_map <- function(dat, column, label = "", nrow = NULL) {
  g <- ggplot() +
    geom_raster(data = dat,
      mapping = aes(fill = {{ column }}, x = lon, y = lat)) +
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

yrs <- seq(min_yr, max_yr, 5)

p_grid |>
  filter(year %in% yrs) |>
  plot_map(plogis(est1) * exp(est2), nrow = 2) +
  theme(legend.position = "bottom") +
  labs(fill = "Predicted blue marlin bycatch")
ggsave("figs/dl-all-predicted.png", width = 8, height = 6)

rev_scale2 <- scale_fill_gradient2(low = scales::muted("blue"),
  high = scales::muted("red"))

po1 <- p_grid |>
  filter(year %in% 1990) |>
  plot_map(omega_s1) + rev_scale2 +
  ggtitle("Spatial random effects [binomial component]")+
  labs(fill = "Deviation in\nlink space")
po2 <- p_grid |>
  filter(year %in% 1990) |>
  plot_map(omega_s2) + rev_scale2 +
  ggtitle("Spatial random effects [positive component]")+
  labs(fill = "Deviation in\nlink space")
cowplot::plot_grid(po1, po2, ncol = 2L)
ggsave("figs/omega.png", width = 8, height = 6)

yrs <- seq(max_yr - 2, max_yr)
pe1 <- p_grid |>
  filter(year %in% yrs) |>
  plot_map(epsilon_st1, nrow = 1L) + rev_scale2 +
  theme(legend.position = "right") +
  ggtitle("Spatiotemporal random effects [binomial component]") +
  labs(fill = "Deviation in\nlink space")
pe2 <- p_grid |>
  filter(year %in% yrs) |>
  plot_map(epsilon_st2, nrow = 1L) + rev_scale2 +
  theme(legend.position = "right") +
  ggtitle("Spatiotemporal random effects [positive component]")+
  labs(fill = "Deviation in\nlink space")

png("figs/epsilon.png", width = 8, height = 5.5, units = "in", res = 180)
cowplot::plot_grid(pe1, pe2, nrow = 2L)
dev.off()
