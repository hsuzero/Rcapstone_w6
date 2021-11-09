
test_data <- eq_clean_data(system.file("extdata", "dataset.txt",
                                       package = "Rcapstone"))

tryCatch({
  context("eq_clean_data")
  expect_that(test_data, is_a("data.frame"))
  expect_that(dim(test_data), is_equivalent_to(c(1223, 8)))
  expect_that(colnames(test_data), is_equivalent_to(c("DATE", "LATITUDE", "LONGITUDE",
                                                      "COUNTRY", "LOCATION", "MAGNITUDE",
                                                      "INTENSITY", "DEATH_COUNT")))
  expect_that(sapply(test_data, typeof), is_equivalent_to(c("double", "double", "double",
                                                            "character", "character", "double",
                                                            "double", "double")))

  context("eq_location_clean")
  test_data <- eq_location_clean(test_data)
  expect_that(dim(test_data), is_equivalent_to(c(1223, 8)))
  expect_that(colnames(test_data), is_equivalent_to(c("DATE", "LATITUDE", "LONGITUDE",
                                                      "COUNTRY", "LOCATION", "MAGNITUDE",
                                                      "INTENSITY", "DEATH_COUNT")))
  expect_that(sapply(test_data, typeof), is_equivalent_to(c("double", "double", "double",
                                                            "character", "character", "double",
                                                            "double", "double")))


  context("geom_timeline")
  data_plot <- ggplot(data = test_data, aes(x = DATE, country = COUNTRY,
                                            label = LOCATION,
                                            magnitude = MAGNITUDE)) +
    geom_timeline(ctry = "MEXICO",
                  xmin = dmy("01/01/2000"),
                  xmax = dmy("01/01/2018")) +
    theme_classic()
  expect_that(length(data_plot$layers), is_equivalent_to(1))
  expect_that(data_plot$labels, is_equivalent_to(c("DATE", "COUNTRY",
                                                   "LOCATION", "MAGNITUDE")))

  context("geom_timeline_label")
  data_plot <- data_plot +
    geom_timeline_label(n_max = 2, ctry = "MEXICO",
                        xmin = dmy("01/01/2000"),
                        xmax = dmy("01/01/2018"))
  expect_that(length(data_plot$layers), is_equivalent_to(2))

  context("eq_map")
  test_data <- test_data %>%
    filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000)
  data_map <- test_data %>%
    eq_map()
  expect_that(class(data_map), is_equivalent_to(c("leaflet", "htmlwidget")))

  context("eq_create_label")
  expect_that(eq_create_label(test_data), is_a("character"))
  expect_that(length(eq_create_label(test_data)), is_equivalent_to(8))

  }, finally = {
    rm(test_data)
    rm(data_plot)
    rm(data_map)
  })

