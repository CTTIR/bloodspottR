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
