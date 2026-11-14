vector_burden_fixture <- function() {
 x<-measure_burden(c(1,2),c(2,4),c(10,20),1,1,n_profiles=c(1,1))
 x$physical_slide_id<-'s';x$organ<-'organ';x$physical_area_id<-c('a','b')
 x
}

test_that('calibration counts and dimensions cannot create extra measurements', {
 arguments<-list(n_spots=c(1,2),red_pixels=c(2,4),tissue_pixels=c(10,20),
                  pixel_width_um=c(1,1),pixel_height_um=c(1,1),n_profiles=c(1,1))
 for(name in names(arguments)) {
  for(bad in list(matrix(arguments[[name]]),cbind(arguments[[name]],arguments[[name]]),
                   structure(arguments[[name]],class='unsupported_measurement'))) {
   a<-arguments;a[[name]]<-bad
   # Equal-length matrices reach the explicit vector guard; two-column inputs
   # may first fail the public equal-count-length requirement.
   expect_error(do.call(measure_burden,a),'vector|equal|incompatible')
  }
 }
 expect_equal(nrow(do.call(measure_burden,arguments)),2L)
})

test_that('aggregation rejects matrix metrics before pooling or missing filtering', {
 x<-vector_burden_fixture();before<-x
 for(name in c('n_spots','n_profiles','red_area_mm2','tissue_area_mm2')) {
  for(bad in list(cbind(x[[name]],x[[name]]),matrix(x[[name]]),
                   structure(x[[name]],class='unsupported_measurement'))) {
   z<-x;z[[name]]<-bad;copy<-z
   expect_error(aggregate_burden(z),'plain integer/double vector')
   expect_identical(z,copy)
  }
 }
 z<-x;z$n_profiles<-matrix(rep(NA_real_,4),nrow=2)
 expect_error(aggregate_burden(z),'plain integer/double vector')
 expect_identical(x,before)
 expected<-aggregate_burden(x)
 expect_equal(expected$n_spots,3)
 expect_equal(expected$tissue_area_mm2,3e-5)
 expect_equal(expected$spots_per_mm2,1e5)
})

test_that('group and QC columns preserve one value per measurement', {
 x<-vector_burden_fixture()
 for(name in c('physical_slide_id','organ','physical_area_id')) {
  z<-x;z[[name]]<-cbind(z[[name]],z[[name]])
  expect_error(aggregate_burden(z),'dimensionless identifier vector')
 }
 z<-x;z$qc_flags<-matrix(rep('',4),2)
 expect_error(aggregate_burden(z),'dimensionless character')
 expect_error(aggregate_burden(x,by=matrix(c('physical_slide_id','organ'))),'required columns')
 x$physical_slide_id<-factor(x$physical_slide_id)
 expect_equal(aggregate_burden(x)$n_spots,3)
})

test_that('zero areas and missing profiles retain their explicit meanings', {
 x<-measure_burden(c(1,0),c(2,0),c(10,0),.5,2,n_profiles=c(1,0))
 expect_equal(x$tissue_area_mm2,c(1e-5,0))
 expect_equal(x$profiles_per_mm2,c(1e5,NA_real_))
 x$physical_slide_id<-'s';x$organ<-'organ';x$physical_area_id<-c('a','b')
 x$n_profiles[2]<-NA_real_
 out<-aggregate_burden(x)
 expect_true(is.na(out$n_profiles));expect_true(is.na(out$profiles_per_mm2))
 expect_equal(out$n_measurements,2L)
 expect_equal(out$n_spots,1)
})
