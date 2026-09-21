test_that("relative OD features match hand-calculated study channels", {
  x <- rbind(c(255,255,255),c(255,127,63),c(0,0,0))
  result <- bs_color_features(x)
  expect_equal(unname(result[1,]),c(0,0,0))
  expect_equal(unname(result[2,]),c(log(2),log(4),log(2)))
  expect_equal(unname(result[3,]),c(0,0,log(256)))
  expect_true(all(is.finite(bs_color_features(matrix(0,1,3),white=1e308,pseudocount=1e-308))))
  expect_equal(colnames(result),c("od_g_minus_r","od_b_minus_r","od_mean"))
  expect_equal(bs_color_features(x/255,white=1,pseudocount=1/255),result)
  a <- array(rep(x,2),dim=c(3,2,3))
  expect_equal(dim(bs_color_features(a)),c(3L,2L,3L))
  expect_equal(unname(matrix(bs_color_features(a),ncol=3)),unname(bs_color_features(matrix(a,ncol=3))))
})

test_that("color input errors cannot become silent missing features", {
  for (bad in list(1:3,matrix(1,2,2),array(1,c(1,1,1,3)),matrix(numeric(),0,3),matrix("a",1,3)))
    expect_error(bs_color_features(bad),"RGB matrix")
  for (v in c(NA,NaN,Inf,-1,256)) expect_error(bs_color_features(matrix(c(v,1,2),1,3)),"finite and between")
  for (v in list(NA,0,-1,Inf,c(1,2))) expect_error(bs_color_features(matrix(1,1,3),white=v),"positive scalars")
  expect_error(bs_color_features(matrix(1,1,3),pseudocount=0),"positive scalars")
  expect_error(bs_color_features(matrix(1,1,3),white=1e308,pseudocount=1e308),"must be finite")
})

test_that("profile diameter uses direction-independent geometry and explicit threshold", {
  rectangle <- rbind(c(0,0),c(3,0),c(3,4),c(0,4))
  x <- bs_profile_extent(rectangle,5)
  expect_equal(x$max_diameter_um,5)
  expect_equal(c(x$x_extent_um,x$y_extent_um),c(3,4))
  expect_equal(x$flag,"single_compatible")
  expect_equal(bs_profile_extent(rectangle,4.99)$flag,"exceeds_reference")
  rotated <- rectangle %*% matrix(c(cos(.4),sin(.4),-sin(.4),cos(.4)),2)
  expect_equal(bs_profile_extent(rotated,6)$max_diameter_um,5)
  expect_equal(bs_profile_extent(rbind(rectangle,rectangle),5)$n_unique_points,4)
  expect_equal(bs_profile_extent(rbind(c(0,0),c(1,1),c(3,3)),5)$max_diameter_um,sqrt(18))
  expect_equal(bs_profile_extent(matrix(c(10,20),1),5)$max_diameter_um,0)
  expect_equal(bs_profile_extent(as.data.frame(rectangle),5),x)
})

test_that("calipers agree with independent exhaustive reference on small clouds", {
  set.seed(119)
  for (n in c(3,5,10,50,100)) {
    cloud <- matrix(rnorm(2*n),n,2)
    expect_equal(bs_profile_extent(cloud,5)$max_diameter_um,max(stats::dist(cloud)),tolerance=1e-12)
  }
  theta <- seq(0,2*pi,length.out=10001)[-10001]
  circle <- cbind(cos(theta),sin(theta))
  expect_equal(bs_profile_extent(circle,3)$max_diameter_um,2,tolerance=1e-12)
})

test_that("profile invalid dimensions and nonphysical references fail explicitly", {
  for (bad in list(matrix(numeric(),0,2),matrix(1,2,3),matrix(c(NA,1),1),matrix(c(Inf,1),1),c(1,2),data.frame(x="a",y=1)))
    expect_error(bs_profile_extent(bad,5),"numeric|finite")
  for (bad in list(NA,Inf,0,-1,c(5,6),"5")) expect_error(bs_profile_extent(matrix(c(0,0),1),bad),"positive scalar")
  expect_error(bs_profile_extent(rbind(c(-1e308,0),c(1e308,0)),5),"numerical range")
  expect_equal(bs_profile_extent(rbind(c(0,0),c(1e200,1e200)),1e201)$max_diameter_um,sqrt(2)*1e200)
})
