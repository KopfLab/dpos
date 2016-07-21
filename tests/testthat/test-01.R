context("Basic functionality")

test_that("Data loading", {

  # load_isodat_scan errors
  expect_error(load_isodat_scan("DNE"), "missing file*")
  expect_error(load_isodat_scan(c(system.file("doc", package="dpos"), "DNE2")), "missing file.* DNE2")
  # FIXME: have to use other files, these are not available
  #expect_error(load_isodat_scan(system.file(file.path("R", "package.R"), package = "dpos")), "failed to load.*package.R")
  #expect_error(load_isodat_scan(system.file(file.path("R", c("package.R", "loading.R")), package = "dpos")),
  #             "failed to load.*package.R.*loading.R")

  # expect outcome from load_isodat_scan
  expect_true(file.exists(system.file(file.path("extdata", "peakshapes.scn"), package = "dpos")))
  expect_message(scan <- load_isodat_scan(system.file(file.path("extdata", "peakshapes.scn"), package = "dpos"), quiet = T), NA)
  expect_is(scan, "list")
  expect_is(scan[[1]], "IsodatScanFile")

  # get_mass_data errors
  expect_error(get_mass_data(NULL, format = "NA"), "unknown format")
  expect_error(get_mass_data("NA"), "unsupported file type")

  # expected outcome from get_mass_data
  expect_message(scan %>% get_mass_data(), "aggregated mass data.*7 masses.*1050 measurements")
  expect_message(dt <- scan %>% get_mass_data(quiet = T), NA)
  expect_is(dt, "tbl")
  expect_equal(dt %>% nrow(), 1050)
  expect_equal(dt %>%  names(), c("step", paste0("mass", c(44, 45, 46, 47, 54, 48, 49)), "path", "filename"))
  expect_equal(dt[1,c("step", "mass44", "mass48", "filename")] %>% mutate(mass44 = round(mass44, 5), mass48 = round(mass48, 3)),
               data_frame(step = 61601, mass44 = 0.08439, mass48 = 18.241, filename = "peakshapes.scn"))
  expect_equal(scan %>% get_mass_data(format = "long") %>% names(), c("step", "path", "filename", "mass", "intensity"))
  expect_equal(scan %>% get_mass_data(format = "long") %>% nrow(), 7350)
})
