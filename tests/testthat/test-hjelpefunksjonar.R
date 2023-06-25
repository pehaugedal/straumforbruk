dag = as.POSIXct("2023-06-22 16:00:00 CEST")
kveld = as.POSIXct("2023-06-22 23:00:00 CEST")
helg = as.POSIXct("2023-06-24 16:00:00 CEST")

test_that("dagtid() gjev forventa resultat", {
  expect_true(dagtid(dag))
  expect_false(dagtid(kveld))
  expect_false(dagtid(helg))
})
