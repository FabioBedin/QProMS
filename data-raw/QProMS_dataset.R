## code to prepare `QProMS_dataset` dataset goes here
QProMS_dataset <- data.table::fread(input = "C:/Users/ieo4973/Documents/Scripts/proteinGroups.txt") %>%
  tibble::as_tibble(.name_repair = janitor::make_clean_names)

usethis::use_data(QProMS_dataset, overwrite = TRUE)


## create experimental design for now just lfq

lfq_names <- QProMS_dataset %>% dplyr::select(starts_with("lfq_intensity")) %>% colnames()

expdesign <- tibble::tibble(
  key=lfq_names,
  label=lfq_names,
  condition=rep(c("lps_1h", "lps_2h", "lps_30min", "lps_4h", "ut"), each=4),
  replicate=rep(1:4, times = 5)
)

usethis::use_data(expdesign, overwrite = TRUE)
