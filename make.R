rmarkdown::render("geostat.Rmd",
  params = list(
    use_sets = TRUE,
    fleets = c(1, 2, 3),
    species = "bum",
    spatiotemporal = "iid",
    spatial = "on",
    share_range = FALSE,
    cache_path = "all-sets-bum-iid-share-range-false"
  ),
  output_file = "all-sets-bum-iid-share-range-false.html"
)

rmarkdown::render("geostat.Rmd",
  params = list(
    use_sets = TRUE,
    fleets = c(1, 2, 3),
    species = "bum",
    spatiotemporal = "off",
    spatial = "off",
    share_range = TRUE,
    cache_path = "all-sets-bum-no-fields",
    anisotropy = FALSE
  ),
  output_file = "all-sets-bum-no-fields.html"
)

rmarkdown::render("geostat.Rmd",
  params = list(
    use_sets = TRUE,
    fleets = c(1, 2, 3),
    species = "bum",
    spatiotemporal = "off",
    spatial = "on",
    share_range = TRUE,
    cache_path = "all-sets-bum-spatial-only",
    anisotropy = TRUE
  ),
  output_file = "all-sets-bum-spatial-only.html"
)

rmarkdown::render("geostat.Rmd",
  params = list(
    use_sets = TRUE,
    fleets = c(1),
    species = "bum",
    spatiotemporal = "iid",
    spatial = "on",
    share_range = FALSE,
    cache_path = "us-sets-bum-iid-share-range-false"
  ),
  output_file = "us-sets-bum-iid-share-range-false.html"
)

rmarkdown::render("geostat.Rmd",
  params = list(
    use_sets = TRUE,
    fleets = c(1),
    species = "bum",
    spatiotemporal = "off",
    spatial = "off",
    share_range = TRUE,
    cache_path = "us-sets-bum-no-fields",
    anisotropy = FALSE
  ),
  output_file = "us-sets-bum-no-fields.html"
)

rmarkdown::render("geostat.Rmd",
  params = list(
    use_sets = TRUE,
    fleets = c(1),
    species = "bum",
    spatiotemporal = "off",
    spatial = "on",
    share_range = TRUE,
    cache_path = "us-sets-bum-spatial-only",
    anisotropy = TRUE
  ),
  output_file = "us-sets-bum-spatial-only.html"
)
