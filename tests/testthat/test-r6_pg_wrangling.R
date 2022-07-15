data("QProMS_dataset")
data("expdesign")

prepare_object <- function(){
  object <- QProMS$new()
  object$data <- QProMS_dataset
  object$input_type <- "MaxQuant"
  object$intensity_type <- "LFQ"
  object$expdesign <- expdesign
  object$make_unique_names_pg()
  object$standardize_pg_data(expdesign)
  return(object)
}


test_that(desc = "check if reverse proteins are correctly removed", {

  test <- prepare_object()

  test$pg_wrangling(rev = TRUE, cont = FALSE, oibs = FALSE, pep_col = "peptides", pep_thr = 0)

  expect_equal(unique(test$pg_filtered_data$reverse), "")
})

test_that(desc = "check if contaminant proteins are correctly removed", {

  test <- prepare_object()

  test$pg_wrangling(rev = FALSE, cont = TRUE, oibs = FALSE, pep_col = "peptides", pep_thr = 0)

  expect_equal(unique(test$pg_filtered_data$potential_contaminant), "")
})

test_that(desc = "check if only identified by site proteins are correctly removed", {

  test <- prepare_object()

  test$pg_wrangling(rev = FALSE, cont = FALSE, oibs = TRUE, pep_col = "peptides", pep_thr = 0)

  expect_equal(unique(test$pg_filtered_data$only_identified_by_site), "")
})
