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
    fleets = c(1, 3, 3),
    species = "bum",
    spatiotemporal = "off"
  ),
  output_file = "all-sets-bum.html"
)
