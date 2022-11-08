rmarkdown::render("geostat.Rmd",
  params = list(
    use_sets = TRUE,
    fleets = c(1),
    species = "swo",
    spatiotemporal = "iid"
  ),
  output_file = "us-sets-iid-swo.html"
)

rmarkdown::render("geostat.Rmd",
  params = list(
    use_sets = TRUE,
    fleets = c(1),
    species = "bum",
    spatiotemporal = "off"
  ),
  output_file = "us-sets-bum.html"
)

rmarkdown::render("geostat.Rmd",
  params = list(
    use_sets = TRUE,
    fleets = c(1, 2, 3),
    species = "swo",
    spatiotemporal = "off"
  ),
  output_file = "all-sets-swo.html"
)

rmarkdown::render("geostat.Rmd",
  params = list(
    use_sets = TRUE,
    fleets = c(1, 2, 3),
    species = "bum",
    spatiotemporal = "iid",
    spatial = "on"
  ),
  output_file = "all-sets-bum-iid.html"
)

rmarkdown::render("geostat.Rmd",
  params = list(
    use_sets = TRUE,
    fleets = c(1, 2, 3),
    species = "bum",
    spatiotemporal = "iid",
    spatial = "on",
    share_range = FALSE
  ),
  output_file = "all-sets-bum-iid-share-range-false.html"
)
