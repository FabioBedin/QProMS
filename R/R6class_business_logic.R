#' @importFrom R6 R6Class

QProMS <- R6::R6Class(
  classname = "QProMS",
  public = list(
    ####################
    # Input parameters #
    data = NULL,
    input_type = NULL,
    intensity_type = NULL,
    expdesign = NULL,
    color_palette = NULL,
    ################################
    # Arguments for MaxQuant input #
    pg_data = NULL,
    pg_filtered_data = NULL,
    ################################
    # Arguments for data wrangling #
    filtered_data = NULL,
    ## plots
    protein_counts_plot = NULL,
    protein_coverage_plot = NULL,
    ###############################
    # Arguments for normalization #
    normalized_data = NULL,
    is_norm = NULL,
    vsn_norm_run_once = FALSE,
    ## plots
    normalization_plot = NULL,
    ############################
    # Arguments for imputation #
    imputed_data = NULL,
    mixed_mean_data = NULL,
    is_mixed = NULL,
    is_imp = NULL,
    ## plots
    imputation_plot = NULL,

    loading_data = function(data_input, input_type, intensity_type){
      self$data <- data.table::fread(input = data_input$datapath) %>%
        tibble::as_tibble(.name_repair = janitor::make_clean_names)
      self$input_type <- input_type
      self$intensity_type <- intensity_type
    },
    make_expdesign = function(){
      ## qui mettere tutti gli if in base all'intensity type
      ## per adesso metto solo lfq
      col_names <- self$data %>%
        dplyr::select(starts_with("lfq_intensity")) %>%
        colnames()

      self$expdesign <- data.frame(
        key = col_names,
        label = col_names,
        condition = rep("", each = length(col_names)),
        replicate = rep("", each = length(col_names))
      )
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
      self$pg_data <-
        dplyr::left_join(self$data, list_unique_gene_names, by = "id") %>%
        dplyr::mutate(
          gene_names = dplyr::case_when(unique_gene_names != "" ~ unique_gene_names,
                                        TRUE ~ gene_names)
        ) %>%
        dplyr::select(-unique_gene_names) %>%
        dplyr::mutate(gene_names = stringr::str_extract(gene_names, "[^;]*"))
    },
    standardize_pg_data = function(expdesign){
      ## cambiare il self data
      self$pg_data <- self$pg_data %>%
        dplyr::select(
          gene_names,
          dplyr::all_of(expdesign$key),
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
          names_to = "key",
          values_to = "raw_intensity"
        ) %>%
        dplyr::inner_join(., expdesign, by = "key") %>%
        dplyr::mutate(raw_intensity = log2(raw_intensity)) %>%
        dplyr::mutate(raw_intensity = dplyr::na_if(raw_intensity, -Inf)) %>%
        dplyr::mutate(bin_intensity = dplyr::if_else(is.na(raw_intensity), 0, 1)) %>%
        dplyr::select(-key)
    },
    pg_wrangling = function(rev = TRUE,
                            cont = TRUE,
                            oibs = TRUE,
                            pep_col = "peptides", ## c("peptides", "unique", "razor")
                            pep_thr = 2) {

      self$pg_filtered_data <- self$pg_data %>%
        ## remove reverse, potentialcontaminant and oibs from data base on user input
        {if(rev)dplyr::filter(., !reverse == "+") else .} %>%
        {if(cont)dplyr::filter(., !potential_contaminant == "+") else .} %>%
        {if(oibs)dplyr::filter(., !only_identified_by_site == "+") else .} %>%
        ## filter on peptides:
        {if(pep_col == "peptides"){dplyr::filter(., peptides >= pep_thr)}
          else if (pep_col == "unique") {dplyr::filter(., unique_peptides >= pep_thr)}
          else {dplyr::filter(., razor_unique_peptides >= pep_thr)}}
    },
    filter_valid_val = function(data, type = "alog", thr){

      self$filtered_data <- data %>%
        # dplyr::mutate(bin_intensity = dplyr::if_else(is.na(raw_intensity), 0, 1)) %>%
        ## different type of strategy for filter missing data:
        ## c("alog", "each_grp", "total") alog -> at least one group
        {if(type == "total")dplyr::group_by(., gene_names)
          else dplyr::group_by(., gene_names, condition)} %>%
        dplyr::mutate(miss_val = dplyr::n() - sum(bin_intensity)) %>%
        dplyr::mutate(n_size = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(gene_names) %>%
        ## rage compreso tra 50% e 100% espresso in valori tra 0.5 e 1
        {if(type == "alog") dplyr::filter(., any(miss_val <= round(n_size * (1 - thr), 0)))
          else dplyr::filter(., all(miss_val <= round(n_size * (1 - thr), 0)))} %>%
        dplyr::ungroup() %>%
        dplyr::select(gene_names, label, condition, replicate, raw_intensity, bin_intensity)
    },
    normalization = function(norm_methods, run_once){

      if(is.null(norm_methods)){
        print("error")# devo ancora fare i test giusti
      }

      if(norm_methods == "None"){
        data <- self$filtered_data
        self$is_norm <- FALSE
        self$normalization_plot <- data %>%
          dplyr::group_by(condition, label) %>%
          echarts4r::e_charts() %>%
          echarts4r::e_title(text = "Intensity distribution", left = "center") %>%
          echarts4r::e_boxplot(
            raw_intensity,
            colorBy = "data",
            layout = 'horizontal',
            itemStyle = list(color = "#DADADA", borderWidth = 2)
          ) %>%
          echarts4r::e_tooltip(trigger = "item") %>%
          echarts4r::e_color(self$color_palette) %>%
          echarts4r::e_theme("QProMS_theme")
      }else{
        if(!run_once){
          self$vsn_norm_run_once <- TRUE
          ## convert tibble data into a matrix
          raw_mat <- self$filtered_data

          raw_matrix <- raw_mat %>%
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
            dplyr::full_join(raw_mat, .x, by = c("gene_names", "label")) %>%
            dplyr::relocate(norm_intensity, .after = last_col())
        }
        data <- self$normalized_data
        self$is_norm <- TRUE
        self$normalization_plot <- data %>%
          dplyr::group_by(condition, label) %>%
          echarts4r::e_charts() %>%
          echarts4r::e_title(text = "Intensity distribution", left = "center") %>%
          echarts4r::e_boxplot(
            norm_intensity,
            colorBy = "data",
            layout = 'horizontal',
            itemStyle = list(color = "#DADADA", borderWidth = 2)
          ) %>%
          echarts4r::e_tooltip(trigger = "item") %>%
          echarts4r::e_color(self$color_palette) %>%
          echarts4r::e_theme("QProMS_theme")
      }

    },
    mean_partial_imputation = function(){

      ## in this function we define row in each group that have more then 75% of valid values
      ## and replace their missing data (MAR) whit the mean of the group

      # define if use normalize or row intensity
      if(self$is_norm){
        data <- self$normalized_data
        data <- data %>% dplyr::mutate(selected_intensity = norm_intensity)
      }else{
        data <- self$filtered_data
        data <- data %>% dplyr::mutate(selected_intensity = raw_intensity)
      }

      # perform mean imputation
      self$mixed_mean_data <- data %>%
        dplyr::group_by(gene_names, condition) %>%
        dplyr::mutate(for_mean_imp = dplyr::if_else((sum(bin_intensity) / dplyr::n()) >= 0.75, TRUE, FALSE)) %>%
        dplyr::mutate(mean_grp = mean(selected_intensity, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(imp_intensity = dplyr::case_when(
          bin_intensity == 0 & for_mean_imp ~ mean_grp,
          TRUE ~ as.numeric(selected_intensity))) %>%
        dplyr::select(-c(for_mean_imp, mean_grp, selected_intensity))
    },
    perseus_imputation = function(shift, scale) {

      # Define if use normalize, mixed or row intensity
      if(!self$is_norm & !self$is_mixed){
        data <- self$filtered_data
        data <- data %>% dplyr::mutate(selected_intensity = raw_intensity)
      }else if(self$is_norm & !self$is_mixed){
        data <- self$normalized_data
        data <- data %>% dplyr::mutate(selected_intensity = norm_intensity)
      }else{
        self$mean_partial_imputation()
        data <- self$mixed_mean_data
        data <- data %>% dplyr::mutate(selected_intensity = imp_intensity)
      }

      ## this funcion perform classical Perseus imputation
      ## sice use random nomral distibution i will set a set.seed()
      set.seed(11)

      self$imputed_data <- data %>%
        dplyr::group_by(label) %>%
        # Define statistic to generate the random distribution relative to sample
        dplyr::mutate(
          mean = mean(selected_intensity, na.rm = TRUE),
          sd = sd(selected_intensity, na.rm = TRUE),
          n = sum(!is.na(selected_intensity)),
          total = nrow(data) - n
        ) %>%
        dplyr::ungroup() %>%
        # Impute missing values by random draws from a distribution
        # which is left-shifted by parameter 'shift' * sd and scaled by parameter 'scale' * sd.
        dplyr::mutate(imp_intensity = dplyr::case_when(
          is.na(selected_intensity) ~ rnorm(total, mean = (mean - shift * sd), sd = sd * scale),
          TRUE ~ selected_intensity
        )) %>%
        dplyr::select(-c(mean, sd, n, total, selected_intensity))
    },
    effect_of_imputation_plot = function(){

      if(!self$is_norm & !self$is_imp){
        data <- self$filtered_data
        data <- data %>% dplyr::mutate(plot_intensity = raw_intensity)
      }else if(self$is_norm & !self$is_imp){
        data <- self$normalized_data
        data <- data %>% dplyr::mutate(plot_intensity = norm_intensity)
      }else{
        data <- self$imputed_data
        data <- data %>% dplyr::mutate(plot_intensity = imp_intensity)
      }

      self$imputation_plot <- data %>%
        dplyr::group_by(condition) %>%
        echarts4r::e_charts() %>%
        echarts4r::e_density(
          plot_intensity,
          smooth = TRUE,
          areaStyle = list(opacity = 0),
          symbol = "none"
        ) %>%
        echarts4r::e_color(self$color_palette) %>%
        echarts4r::e_theme("QProMS_theme")
    }
  )
)
