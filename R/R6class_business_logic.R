#' @importFrom R6 R6Class

QProMS <- R6::R6Class(
  classname = "QProMS",
  public = list(
    data = NULL,
    pg_filtered_data = NULL,
    filtered_data = NULL,
    normalized_data = NULL,
    imputed_data = NULL,
    input_type = NULL,
    intensity_type = NULL,
    expdesign = NULL,
    loading_data = function(data, input_type, intensity_type){
      self$data <- data %>% tibble::as_tibble(.name_repair = janitor::make_clean_names)
      self$input_type <- input_type
      self$intensity_type <- intensity_type
    },
    make_unique_names_pg = function(){
      ## this function remove duplicate and missing gene names in proteinGroups.txt input

      ## Indentify all duplicate gene names and add after __ the protein iD
      data_list <- self$data

      list_unique_gene_names <- data_list %>%
        dplyr::select(protein_i_ds, gene_names, id) %>%
        dplyr::mutate(gene_names = stringr::str_extract(gene_names, "[^;]*")) %>%
        ## every protein gorups now have only 1 gene name associated to it
        dplyr::rename(unique_gene_names = gene_names) %>%
        janitor::get_dupes(unique_gene_names) %>%
        dplyr::mutate(
          unique_gene_names = dplyr::case_when(
            unique_gene_names != "" ~ paste0(
              unique_gene_names,
              "__",
              stringr::str_extract(protein_i_ds, "[^;]*")
            ),
            TRUE ~ stringr::str_extract(protein_i_ds, "[^;]*")
          )
        ) %>%
        dplyr::select(unique_gene_names, id)

      ## update self$data that now don't have dupe or missing spot
      self$data <-
        dplyr::left_join(self$data, list_unique_gene_names, by = "id") %>%
        dplyr::mutate(
          gene_names = dplyr::case_when(unique_gene_names != "" ~ unique_gene_names,
                                        TRUE ~ gene_names)
        ) %>%
        dplyr::select(-unique_gene_names) %>%
        dplyr::mutate(gene_names = stringr::str_extract(gene_names, "[^;]*"))
    },
    standardize_pg_data = function(expdesign){
      self$data <- self$data %>%
        dplyr::select(
          gene_names,
          dplyr::all_of(expdesign$label),
          peptides,
          razor_unique_peptides,
          unique_peptides,
          reverse,
          potential_contaminant,
          only_identified_by_site
        ) %>%
        tidyr::pivot_longer(
          !c(gene_names,
             peptides,
             razor_unique_peptides,
             unique_peptides,
             reverse,
             potential_contaminant,
             only_identified_by_site),
          names_to = "label",
          values_to = "raw_intensity"
        ) %>%
        dplyr::inner_join(., expdesign, by = "label") %>%
        dplyr::mutate(raw_intensity = log2(raw_intensity)) %>%
        dplyr::mutate(raw_intensity = dplyr::na_if(raw_intensity, -Inf))
    },
    pg_wrangling = function(rev = TRUE,
                            cont = TRUE,
                            oibs = TRUE,
                            pep_col = "peptides", ## c("peptides", "unique", "razor")
                            pep_thr = 2) {

      self$pg_filtered_data <- self$data %>%
        ## remove reverse, potentialcontaminant and oibs from data base on user input
        {if(rev)dplyr::filter(., !reverse == "+") else .} %>%
        {if(cont)dplyr::filter(., !potential_contaminant == "+") else .} %>%
        {if(oibs)dplyr::filter(., !only_identified_by_site == "+") else .} %>%
        ## filter on peptides:
        {if(pep_col == "peptides"){dplyr::filter(., peptides >= pep_thr)}
          else if (pep_col == "unique") {dplyr::filter(., unique_peptides >= pep_thr)}
          else {dplyr::filter(., razor_unique_peptides >= pep_thr)}} %>%
        dplyr::select(gene_names, label, condition, replicate, raw_intensity)

      invisible(self)
    },
    filter_valid_val = function(type = "alog", thr){

      self$filtered_data <- self$pg_filtered_data %>%
        dplyr::mutate(bin_intensity = dplyr::if_else(is.na(raw_intensity), 0, 1)) %>%
        ## different type of strategy for filter missing data:
        ## c("alog", "each_grp", "total") alog -> at least one group
        {if(type == "total")dplyr::group_by(., gene_names)
          else dplyr::group_by(., gene_names, condition)} %>%
        dplyr::mutate(miss_val = n() - sum(bin_intensity)) %>%
        dplyr::mutate(n_size = n()) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(gene_names) %>%
        ## rage compreso tra 50% e 100% espresso in valori tra 0.5 e 1
        {if(type == "alog") dplyr::filter(., any(miss_val <= round(n_size * (1 - thr), 0)))
          else dplyr::filter(., all(miss_val <= round(n_size * (1 - thr), 0)))} %>%
        dplyr::select(gene_names, label, condition, replicate, raw_intensity, bin_intensity)
    },
    vsn_normalization = function(){

      ## convert tibble data into a matrix
      raw_matrix <- self$filtered_data %>%
        tidyr::pivot_wider(id_cols = gene_names,
                           names_from = label,
                           values_from = raw_intensity) %>%
        tibble::column_to_rownames("gene_names") %>%
        as.matrix()

      ## Variance stabilization transformation on matrix
      vsn_fit <- vsn::vsn2(2 ^ raw_matrix, verbose = FALSE)
      norm_matrix <- vsn::predict(vsn_fit, 2 ^ raw_matrix)

      ## return a table with QProMS object format
      self$normalized_data <- norm_matrix %>%
        tibble::as_tibble(rownames = "gene_names") %>%
        tidyr::pivot_longer(cols = !gene_names,
                            names_to = "label",
                            values_to = "norm_intensity") %>%
        dplyr::full_join(self$filtered_data, .x, by = c("gene_names", "label")) %>%
        dplyr::relocate(norm_intensity, .after = last_col())
    }
  )
)
