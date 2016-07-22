context("Basic functionality")

test_that("Data loading", {

  # load_isodat_scan1 errors
  expect_error(load_isodat_scan("DNE"), "missing file*")
  expect_error(load_isodat_scan(c(system.file("doc", package="dpos"), "DNE2")), "missing file.* DNE2")
  # FIXME: have to use other files, these are not available
  #expect_error(load_isodat_scan(system.file(file.path("R", "package.R"), package = "dpos")), "failed to load.*package.R")
  #expect_error(load_isodat_scan(system.file(file.path("R", c("package.R", "loading.R")), package = "dpos")),
  #             "failed to load.*package.R.*loading.R")

  # expect outcome from load_isodat_scan
  expect_true(file.exists(system.file(file.path("extdata", "peakshapes.scn"), package = "dpos")))
  expect_message(scan1 <- load_isodat_scan(system.file(file.path("extdata", "peakshapes.scn"), package = "dpos"), quiet = T), NA)
  expect_is(scan1, "list")
  expect_is(scan1[[1]], "IsodatScanFile")

  # load_range_scan1 errors
  expect_error(load_range_scan("DNE"), "missing file*")
  expect_error(load_range_scan(c(system.file("doc", package="dpos"), "DNE2")), "missing file.* DNE2")
  # FIXME: have to use other files, these are not available
  #expect_error(load_isodat_scan(system.file(file.path("R", "package.R"), package = "dpos")), "failed to load.*package.R")
  #expect_error(load_isodat_scan(system.file(file.path("R", c("package.R", "loading.R")), package = "dpos")),
  #             "failed to load.*package.R.*loading.R")

  # expect outcome from load_range_scan
  expect_true(file.exists(system.file(file.path("extdata", "extraction.bff"), package = "dpos")))
  expect_message(scan2 <- load_range_scan(system.file(file.path("extdata", "extraction.bff"), package = "dpos"), quiet = T), NA)
  expect_is(scan2, "list")
  expect_is(scan2[[1]], "RangeScanFile")

  # get_mass_data errors
  expect_error(get_mass_data(NULL, format = "NA"), "unknown format")
  expect_error(get_mass_data("NA"), "unsupported file type")

  # expected outcome from get_mass_data
  expect_message(scan1 %>% get_mass_data(), "aggregated mass data.*7 masses.*1050 measurements")
  expect_message(scan2 %>% get_mass_data(), "aggregated mass data.*6 masses.*216 measurements")
  expect_message(c(scan1, scan2) %>% get_mass_data(), "aggregated mass data.*8 masses.*1266 measurements")
  expect_message(dt <- c(scan1, scan2) %>% get_mass_data(quiet = T), NA)
  expect_is(dt, "data.frame")
  expect_equal(dt %>% nrow(), 1266)
  expect_equal(length(setdiff(dt %>%  names(), c("scan_type", "scan_value", "tune_param1", "tune_param2", "tune_value1", "tune_value2",
                                  paste0("mass", c(44, 45, 46, 47, 54, 48, 49, 56)), "path", "filename"))), 0)
  expect_equal(dt[1,c("scan_value", "mass44", "mass48", "filename")] %>%
                 mutate(mass44 = round(mass44, 5), mass48 = round(mass48, 3)) %>%
                 as.list(),
               list(scan_value = 61601, mass44 = 0.08439, mass48 = 18.241, filename = "peakshapes.scn"))
  expect_equal(c(scan1, scan2) %>% get_mass_data(format = "long") %>% nrow(), 10128)
})
