test_that("tissue windows preserve holes and disconnected components", {
  skip_if_not_installed("sf"); skip_if_not_installed("spatstat.geom")
  square <- function(x, y, size) rbind(c(x,y), c(x+size,y), c(x+size,y+size), c(x,y+size), c(x,y))
  geometry <- sf::st_sfc(sf::st_polygon(list(square(0,0,10), square(3,3,4))),
                         sf::st_polygon(list(square(20,0,10))))
  window <- tissue_window(geometry)
  expect_equal(spatstat.geom::area.owin(window), 184)
  expect_false(spatstat.geom::inside.owin(5,5,window))
  expect_false(spatstat.geom::inside.owin(15,5,window))
  expect_true(spatstat.geom::inside.owin(25,5,window))
  cells <- data.frame(cell_id=c("a","b"), x_um=c(1,21), y_um=c(1,1))
  expect_equal(spatstat.geom::npoints(cell_pattern(cells,window)),2)
  cells$x_um[1] <- 5; cells$y_um[1] <- 5
  expect_error(cell_pattern(cells,window), "excluded holes")
  scaled <- tissue_window(geometry, "mm")
  expect_equal(spatstat.geom::area.owin(scaled),184e6)
  expect_error(tissue_window(sf::st_set_crs(geometry,4326)), "no CRS")
})

test_that("tissue windows accept sf tables, union overlaps and reject non-polygons", {
  skip_if_not_installed("sf"); skip_if_not_installed("spatstat.geom")
  square <- function(x, y, size) {
    rbind(c(x, y), c(x + size, y), c(x + size, y + size), c(x, y + size), c(x, y))
  }
  overlapping <- sf::st_sfc(sf::st_polygon(list(square(0, 0, 10))),
                            sf::st_polygon(list(square(5, 0, 10))))
  window <- tissue_window(sf::st_sf(id = 1:2, geometry = overlapping))
  expect_equal(spatstat.geom::area.owin(window), 150)
  expect_identical(attr(window, "bloodspottR_coordinate_unit"), "um")
  clockwise <- sf::st_sfc(sf::st_polygon(list(square(0, 0, 10)[5:1, ])))
  expect_equal(spatstat.geom::area.owin(tissue_window(clockwise)), 100)
  expect_true(spatstat.geom::inside.owin(5, 5, tissue_window(clockwise)))
  expect_error(tissue_window(list(square(0, 0, 10))), "no CRS")
  expect_error(tissue_window(overlapping[0]), "no CRS")
  expect_error(tissue_window(overlapping, "cm"), "should be one of")
  expect_error(tissue_window(sf::st_sfc(sf::st_point(c(1, 1)))), "valid nonempty polygons")
  expect_error(tissue_window(sf::st_sfc(sf::st_polygon())), "valid nonempty polygons")
  bowtie <- sf::st_polygon(list(rbind(c(0, 0), c(10, 10), c(10, 0), c(0, 10), c(0, 0))))
  expect_error(tissue_window(sf::st_sfc(bowtie)), "valid nonempty polygons")
})

test_that("cell patterns need a verified window and unambiguous cells", {
  skip_if_not_installed("sf"); skip_if_not_installed("spatstat.geom")
  ring <- rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0))
  window <- tissue_window(sf::st_sfc(sf::st_polygon(list(ring))))
  cells <- data.frame(cell_id = c("a", "b"), x_um = c(1, 8), y_um = c(1, 9))
  pattern <- cell_pattern(cells, window)
  expect_equal(spatstat.geom::npoints(pattern), 2)
  marks <- as.data.frame(spatstat.geom::marks(pattern))
  expect_identical(as.character(marks[[1]]), c("a", "b"))
  unverified <- spatstat.geom::owin(c(0, 10), c(0, 10))
  expect_error(cell_pattern(cells, unverified), "verified micrometre units")
  expect_error(cell_pattern(as.list(cells), window), "verified micrometre units")
  expect_error(cell_pattern(cells[c("cell_id", "x_um")], window), "verified micrometre units")
  change <- function(column, value) { cells[[column]] <- value; cells }
  for (unknown in list(change("x_um", c(1, NA)), change("y_um", c(Inf, 9)),
                       change("x_um", c("1", "8")), change("cell_id", c("a", NA)),
                       change("cell_id", c("a", "")))) {
    expect_error(cell_pattern(unknown, window), "must be known")
  }
  expect_error(cell_pattern(change("cell_id", c("a", "a")), window), "require review")
  stacked <- change("x_um", c(1, 1)); stacked$y_um <- c(1, 1)
  expect_error(cell_pattern(stacked, window), "require review")
  expect_error(cell_pattern(change("x_um", c(1, 11)), window), "outside evaluable tissue")
})
