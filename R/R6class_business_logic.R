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
    pg_data_standard = NULL,
    pg_filtered_data = NULL,
    ################################
    # Arguments for data wrangling #
    filtered_data = NULL,
    ## plots
    protein_counts_plot = NULL,
    protein_coverage_plot = NULL,
    missing_data_plot = NULL,
    ###############################
    # Arguments for normalization #
    normalized_data = NULL,
    is_norm = NULL,
    vsn_norm_run_once = FALSE,
    ## plots
    normalization_plot = NULL,
    correlation_matrix_plot = NULL,
    ############################
    # Arguments for imputation #
    imputed_data = NULL,
    mixed_mean_data = NULL,
    is_mixed = NULL,
    is_imp = NULL,
    ## plots
    effect_of_imputation_plot = NULL,

    loading_data = function(input_path, input_type, intensity_type){

      self$data <- data.table::fread(input = input_path) %>%
        tibble::as_tibble(.name_repair = janitor::make_clean_names)

      self$input_type <- input_type

      self$intensity_type <- intensity_type
    },
    make_expdesign = function(){
      ## qui mettere tutti gli if in base all'intensity type
      col_names <- self$data %>%
        dplyr::select(starts_with(self$intensity_type)) %>%
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
        dplyr::left_join(data_list, list_unique_gene_names, by = "id") %>%
        dplyr::mutate(
          gene_names = dplyr::case_when(unique_gene_names != "" ~ unique_gene_names,
                                        TRUE ~ gene_names)
        ) %>%
        dplyr::select(-unique_gene_names) %>%
        dplyr::mutate(gene_names = stringr::str_extract(gene_names, "[^;]*"))
    },
    standardize_pg_data = function(expdesign){
      ## cambiare il self data
      data <- self$pg_data

      self$pg_data_standard <- data %>%
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
    pg_wrangling = function(rev = TRUE, cont = TRUE, oibs = TRUE,
                            pep_col = "peptides", ## c("peptides", "unique", "razor")
                            pep_thr = 2, rescue_cont = NULL) {

      self$pg_filtered_data <- self$pg_data_standard %>%
        dplyr::mutate(potential_contaminant = dplyr::case_when(
          gene_names %in% rescue_cont ~ "", TRUE ~ potential_contaminant)) %>%
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
    protein_counts = function(data, expdesig){
      p <- data %>%
        dplyr::group_by(label) %>%
        dplyr::summarise(counts = sum(bin_intensity)) %>%
        dplyr::ungroup() %>%
        dplyr::inner_join(., expdesig, by = "label") %>%
        dplyr::mutate(replicate = as.factor(replicate)) %>%
        dplyr::group_by(condition) %>%
        echarts4r::e_charts(replicate) %>%
        # echarts4r::e_title(text = "Protein per sample", left = "center") %>%
        echarts4r::e_bar(counts) %>%
        echarts4r::e_x_axis(name = "Replicates") %>%
        echarts4r::e_y_axis(name = "Counts") %>%
        echarts4r::e_tooltip(trigger = "item") %>%
        echarts4r::e_color(self$color_palette) %>%
        echarts4r::e_theme("QProMS_theme")

      return(p)
    },
    protein_coverage = function(data){
      p <- data %>%
        dplyr::group_by(gene_names) %>%
        dplyr::summarise(counts = sum(bin_intensity)) %>%
        dplyr::ungroup() %>%
        dplyr::select(counts) %>%
        table() %>%
        tibble::as_tibble() %>%
        dplyr::rename(occurrence = n) %>%
        echarts4r::e_charts(counts) %>%
        # echarts4r::e_title(text = "Protein coverage", left = "center") %>%
        echarts4r::e_bar(occurrence) %>%
        echarts4r::e_y_axis(name = "Counts") %>%
        echarts4r::e_tooltip(trigger = "item") %>%
        echarts4r::e_color(self$color_palette) %>%
        echarts4r::e_theme("QProMS_theme")

      return(p)
    },
    missing_data = function(data){
      p <- data %>%
        dplyr::group_by(label) %>%
        dplyr::mutate(bin_intensity = dplyr::if_else(bin_intensity == 1, "Valid", "Missing")) %>%
        dplyr::count(bin_intensity) %>%
        tidyr::pivot_wider(id_cols = label, names_from = bin_intensity, values_from = n) %>%
        {if(ncol(.) == 2) dplyr::mutate(., Missing = 0)else . } %>%
        dplyr::ungroup() %>%
        dplyr::mutate(total = Valid + Missing) %>%
        dplyr::mutate(perc_present = paste0(round(Valid*100/total, 1), "%")) %>%
        dplyr::mutate(perc_missing = paste0(round(Missing*100/total, 1), "%")) %>%
        echarts4r::e_charts(label) %>%
        echarts4r::e_bar(Valid, stack = "grp", bind = perc_present) %>%
        echarts4r::e_bar(Missing, stack = "grp", bind = perc_missing) %>%
        echarts4r::e_x_axis(name = "Samples", axisLabel = list(interval = 0, rotate = 45)) %>%
        echarts4r::e_y_axis(name = "Counts") %>%
        echarts4r::e_tooltip(trigger = "item") %>%
        echarts4r::e_color(c("#21918c", "#440154")) %>%
        echarts4r::e_theme("QProMS_theme")

      return(p)
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
    correlation_matrix = function(){
      # define if use normalize or row intensity
      if(self$is_norm){
        data <- self$normalized_data
        data <- data %>% dplyr::mutate(selected_intensity = norm_intensity)
      }else{
        data <- self$filtered_data
        data <- data %>% dplyr::mutate(selected_intensity = raw_intensity)
      }

      p <- data %>%
        dplyr::select(gene_names, label, selected_intensity) %>%
        tidyr::pivot_wider(names_from = label, values_from = selected_intensity) %>%
        dplyr::filter(dplyr::if_all(.cols = dplyr::everything(), .fns = ~ !is.na(.x))) %>%
        tibble::column_to_rownames("gene_names") %>%
        cor() %>%
        echarts4r::e_charts() %>%
        echarts4r::e_correlations(order = "hclust", visual_map = FALSE) %>%
        echarts4r::e_x_axis(axisLabel = list(interval = 0, rotate = 45)) %>%
        echarts4r::e_y_axis(axisLabel = list(interval = 0, rotate = 0), position = "right") %>%
        echarts4r::e_tooltip() %>%
        echarts4r::e_title("Correlation matrix", subtext = "Pearson correlation") %>%
        echarts4r::e_visual_map(
          min = -1,
          max = 1,
          bottom = 150,
          inRange = list(color = c("#440154", "#31688e", "#35b779"))
        ) %>%
        echarts4r::e_theme("QProMS_theme")

      return(p)
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
    effect_of_imputation = function(){

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

      self$effect_of_imputation_plot <- data %>%
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



### new version in development

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
    # Arguments for data wrangling #
    filtered_data = NULL,
    ###############################
    # Arguments for normalization #
    normalized_data = NULL,
    is_norm = NULL,
    vsn_norm_run_once = FALSE,
    ############################
    # Arguments for imputation #
    imputed_data = NULL,
    is_mixed = NULL,
    is_imp = NULL,
    imp_run_once = FALSE,
    #############
    # Functions #
    loading_data = function(input_path, input_type, intensity_type){

      self$data <- data.table::fread(input = input_path) %>%
        tibble::as_tibble(.name_repair = janitor::make_clean_names)

      self$input_type <- input_type

      self$intensity_type <- intensity_type
    },
    make_expdesign = function(){
      ## qui mettere tutti gli if in base all'intensity type
      col_names <- self$data %>%
        dplyr::select(starts_with(self$intensity_type)) %>%
        colnames()

      self$expdesign <- data.frame(
        key = col_names,
        label = col_names,
        condition = rep("", each = length(col_names)),
        replicate = rep("", each = length(col_names))
      )
    },
    pg_preprocessing = function(expdesign){
      ########################################################################
      #### This function prepare the proteing groups in the QProMS format ####
      #### and remove duplicates.                                         ####
      ########################################################################

      ### this firts part remove duplicate and missing gene names
      ### in proteinGroups.txt input

      ## Indentify all duplicate gene names
      ## and add after __ the protein iD

      data <- self$data

      list_unique_gene_names <- data %>%
        dplyr::select(protein_i_ds, gene_names, id) %>%
        dplyr::mutate(gene_names = stringr::str_extract(gene_names, "[^;]*")) %>%
        ## every protein gorups now have only 1 gene name associated to it
        dplyr::rename(unique_gene_names = gene_names) %>%
        janitor::get_dupes(unique_gene_names) %>%
        dplyr::mutate(unique_gene_names = dplyr::case_when(
          unique_gene_names != "" ~ paste0(unique_gene_names, "__",
                                           stringr::str_extract(protein_i_ds, "[^;]*")),
          TRUE ~ stringr::str_extract(protein_i_ds, "[^;]*"))) %>%
        dplyr::select(unique_gene_names, id)

      ## update data that now don't have dupe or missing spot
      data_unique <- dplyr::left_join(data, list_unique_gene_names, by = "id") %>%
        dplyr::mutate(gene_names = dplyr::case_when(
          unique_gene_names != "" ~ unique_gene_names,
          TRUE ~ gene_names)) %>%
        dplyr::select(-unique_gene_names) %>%
        dplyr::mutate(gene_names = stringr::str_extract(gene_names, "[^;]*"))

      ### this second part standardize the data in the right format

      data_standardized <- data_unique %>%
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

      self$data <- data_standardized
    },
    data_wrangling = function(valid_val_filter = "alog", valid_val_thr = 0.75,
                              pep_filter = "peptides", pep_thr = 2,
                              rev = TRUE, cont = TRUE, oibs = TRUE, rescue_cont = NULL) {

      ##############################################################
      #### this funcion is divided in 2 steps:                  ####
      #### the first apply filer specific to maxquant input.    ####
      #### the second part filer the data base on valid values. ####
      ##############################################################

      # setup object parameters
      self$vsn_norm_run_once <- FALSE
      self$imp_run_once <- FALSE
      data <- self$data

      if (self$input_type == "max_quant"){
        ### pep filter puo essere:
        ## c("peptides", "unique", "razor")

        data_wrang <- data %>%
          dplyr::mutate(potential_contaminant = dplyr::case_when(
            gene_names %in% rescue_cont ~ "", TRUE ~ potential_contaminant)) %>%
          ## remove reverse, potentialcontaminant and oibs from data base on user input
          {if(rev)dplyr::filter(., !reverse == "+") else .} %>%
          {if(cont)dplyr::filter(., !potential_contaminant == "+") else .} %>%
          {if(oibs)dplyr::filter(., !only_identified_by_site == "+") else .} %>%
          ## filter on peptides:
          {if(pep_filter == "peptides"){dplyr::filter(., peptides >= pep_thr)}
            else if (pep_filter == "unique") {dplyr::filter(., unique_peptides >= pep_thr)}
            else {dplyr::filter(., razor_unique_peptides >= pep_thr)}}
      }else{
        data_wrang <- data
      }

      ## different type of strategy for filter missing data:
      ## c("alog", "each_grp", "total") alog -> at least one group

      filtered_data <- data_wrang %>%
        {if(valid_val_filter == "total")dplyr::group_by(., gene_names)
          else dplyr::group_by(., gene_names, condition)} %>%
        dplyr::mutate(miss_val = dplyr::n() - sum(bin_intensity)) %>%
        dplyr::mutate(n_size = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(gene_names) %>%
        ## rage compreso tra 0 e 100% espresso in valori tra 0 e 1
        {if(valid_val_filter == "alog") dplyr::filter(., any(miss_val <= round(n_size * (1 - valid_val_thr), 0)))
          else dplyr::filter(., all(miss_val <= round(n_size * (1 - valid_val_thr), 0)))} %>%
        dplyr::ungroup() %>%
        dplyr::select(gene_names, label, condition, replicate, bin_intensity, raw_intensity) %>%
        dplyr::rename(intensity = raw_intensity)

      self$filtered_data <- filtered_data

    },
    plot_protein_counts = function(){

      # controllare che ci sia il self$filtered_data
      data <- self$filtered_data
      expdes <- self$expdesign

      p <- data %>%
        dplyr::group_by(label) %>%
        dplyr::summarise(counts = sum(bin_intensity)) %>%
        dplyr::ungroup() %>%
        dplyr::inner_join(., expdes, by = "label") %>%
        dplyr::mutate(replicate = as.factor(replicate)) %>%
        dplyr::group_by(condition) %>%
        echarts4r::e_charts(replicate) %>%
        echarts4r::e_bar(counts) %>%
        echarts4r::e_x_axis(name = "Replicates") %>%
        echarts4r::e_y_axis(name = "Counts") %>%
        echarts4r::e_tooltip(trigger = "item") %>%
        echarts4r::e_color(self$color_palette) %>%
        echarts4r::e_theme("QProMS_theme")

      return(p)
    },
    plot_total_counts = function(){

      # controllare che ci sia il self$filtered_data
      data <- self$filtered_data

      counts <- data %>%
        dplyr::count(label) %>%
        dplyr::slice(1L) %>%
        dplyr::pull(n)


      echarts4r::e_chart() %>%
        echarts4r::e_gauge(
          as.numeric(counts),
          "Proteins",
          min = 0,
          max = 10000,
          progress = list(show = TRUE)
        ) %>%
        echarts4r::e_title(text = "Total number of proteins", subtext = "Identify across all dataset") %>%
        echarts4r::e_color("#440154")
    },
    plot_protein_coverage = function(){

      # controllare che ci sia il self$filtered_data
      data <- self$filtered_data

      p <- data %>%
        dplyr::group_by(gene_names) %>%
        dplyr::summarise(counts = sum(bin_intensity)) %>%
        dplyr::ungroup() %>%
        dplyr::select(counts) %>%
        table() %>%
        tibble::as_tibble() %>%
        dplyr::rename(occurrence = n) %>%
        echarts4r::e_charts(counts) %>%
        echarts4r::e_bar(occurrence) %>%
        echarts4r::e_y_axis(name = "Counts") %>%
        echarts4r::e_tooltip(trigger = "item") %>%
        echarts4r::e_color(self$color_palette) %>%
        echarts4r::e_theme("QProMS_theme")

      return(p)
    },
    plot_cv = function(){

      # controllare che ci sia il self$filtered_data
      data <- self$filtered_data

      p <- data %>%
        dplyr::group_by(gene_names, condition) %>%
        dplyr::summarise(
          mean = mean(intensity, na.rm = TRUE),
          sd = sd(intensity, na.rm = TRUE),
          CV = round(sd / mean, 3)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(condition) %>%
        echarts4r::e_chart() %>%
        echarts4r::e_boxplot(
          CV,
          colorBy = "data",
          outliers = FALSE,
          itemStyle = list(color = "#DADADA", borderWidth = 2)
        ) %>%
        echarts4r::e_tooltip(trigger = "axis") %>%
        echarts4r::e_title(text = "Sample CV", subtext = "Coefficient of variation")

      return(p)
    },
    plot_missing_data = function(){

      # controllare che ci sia il self$filtered_data
      data <- self$filtered_data

      p <- data %>%
        dplyr::group_by(label) %>%
        dplyr::mutate(bin_intensity = dplyr::if_else(bin_intensity == 1, "Valid", "Missing")) %>%
        dplyr::count(bin_intensity) %>%
        tidyr::pivot_wider(id_cols = label, names_from = bin_intensity, values_from = n) %>%
        {if(ncol(.) == 2) dplyr::mutate(., Missing = 0)else . } %>%
        dplyr::ungroup() %>%
        dplyr::mutate(total = Valid + Missing) %>%
        dplyr::mutate(perc_present = paste0(round(Valid*100/total, 1), "%")) %>%
        dplyr::mutate(perc_missing = paste0(round(Missing*100/total, 1), "%")) %>%
        echarts4r::e_charts(label) %>%
        echarts4r::e_bar(Valid, stack = "grp", bind = perc_present) %>%
        echarts4r::e_bar(Missing, stack = "grp", bind = perc_missing) %>%
        echarts4r::e_x_axis(name = "Samples", axisLabel = list(interval = 0, rotate = 45)) %>%
        echarts4r::e_y_axis(name = "Counts") %>%
        echarts4r::e_tooltip(trigger = "item") %>%
        echarts4r::e_color(c("#21918c", "#440154")) %>%
        echarts4r::e_theme("QProMS_theme")

      return(p)
    },
    plot_missval_distribution = function(){

      # controllare che ci sia il self$filtered_data
      data <- self$filtered_data

      p <- data %>%
        dplyr::group_by(gene_names) %>%
        dplyr::summarise(mean = mean(intensity, na.rm = TRUE),
                         missval = any(is.na(intensity))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(missing_value = dplyr::if_else(missval, "Missing", "Valid")) %>%
        dplyr::group_by(missing_value) %>%
        echarts4r::e_charts() %>%
        echarts4r::e_density(
          mean,
          smooth = TRUE,
          areaStyle = list(opacity = 0),
          symbol = "none"
        ) %>%
        echarts4r::e_y_axis(
          name = "Densiry",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 20,
            lineHeight = 60
          )
        ) %>%
        echarts4r::e_x_axis(
          name = "log2 Intensity",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 20,
            lineHeight = 60
          )
        )

      return(p)
    },
    normalization = function(norm_methods = "None"){

      if(is.null(self$filtered_data)){
        print("error")
      }

      self$imp_run_once <- FALSE
      data <- self$filtered_data

      if(norm_methods == "None"){
        self$is_norm <- FALSE
      }else{
        if(!self$vsn_norm_run_once){

          self$vsn_norm_run_once <- TRUE

          ## convert tibble data into a matrix
          raw_matrix <- data %>%
            tidyr::pivot_wider(id_cols = gene_names,
                               names_from = label,
                               values_from = intensity) %>%
            tibble::column_to_rownames("gene_names") %>%
            as.matrix()

          ## Variance stabilization transformation on matrix
          vsn_fit <- vsn::vsn2(2 ^ raw_matrix, verbose = FALSE)
          norm_matrix <- vsn::predict(vsn_fit, 2 ^ raw_matrix)

          ## return a table with QProMS object format
          normalized_data <- norm_matrix %>%
            tibble::as_tibble(rownames = "gene_names") %>%
            tidyr::pivot_longer(cols = !gene_names,
                                names_to = "label",
                                values_to = "norm_intensity") %>%
            dplyr::full_join(data, .x, by = c("gene_names", "label")) %>%
            dplyr::mutate(intensity = norm_intensity) %>%
            dplyr::select(-norm_intensity) %>%
            dplyr::relocate(intensity, .after = last_col())

          self$normalized_data <- normalized_data
        }
        self$is_norm <- TRUE
      }

    },
    plot_distribution = function(){

      if(self$is_norm){
        data <- self$normalized_data
      }else{
        data <- self$filtered_data
      }

      p <- data %>%
        dplyr::mutate(intensity = round(intensity, 2)) %>%
        dplyr::group_by(condition, label) %>%
        echarts4r::e_charts() %>%
        echarts4r::e_title(text = "Intensity distribution", left = "center") %>%
        echarts4r::e_boxplot(
          intensity,
          colorBy = "data",
          layout = 'horizontal',
          itemStyle = list(color = "#DADADA", borderWidth = 2)
        ) %>%
        echarts4r::e_tooltip(trigger = "item") %>%
        echarts4r::e_color(self$color_palette) %>%
        echarts4r::e_theme("QProMS_theme")

      return(p)
    },
    plot_correlation_matrix = function(){
      # define if use normalize or row intensity
      if(self$is_norm){
        data <- self$normalized_data
      }else{
        data <- self$filtered_data
      }

      p <- data %>%
        dplyr::select(gene_names, label, intensity) %>%
        tidyr::pivot_wider(names_from = label, values_from = intensity) %>%
        dplyr::filter(dplyr::if_all(.cols = dplyr::everything(), .fns = ~ !is.na(.x))) %>%
        tibble::column_to_rownames("gene_names") %>%
        cor() %>%
        echarts4r::e_charts() %>%
        echarts4r::e_correlations(order = "hclust", visual_map = FALSE) %>%
        echarts4r::e_x_axis(axisLabel = list(interval = 0, rotate = 45)) %>%
        echarts4r::e_y_axis(axisLabel = list(interval = 0, rotate = 0), position = "right") %>%
        echarts4r::e_tooltip(trigger = "item") %>%
        echarts4r::e_title("Correlation matrix", subtext = "Pearson correlation") %>%
        echarts4r::e_visual_map(
          min = -1,
          max = 1,
          bottom = 150,
          inRange = list(color = c("#440154", "#31688e", "#35b779"))
        ) %>%
        echarts4r::e_theme("QProMS_theme")

      return(p)
    },
    imputation = function(imp_methods = "mixed", shift = 1.8, scale = 0.3) {

      # Define if use normalize or row intensity
      if(self$is_norm){
        data <- self$normalized_data
      }else{
        data <- self$filtered_data
      }

      if(imp_methods == "mixed"){
        self$is_mixed <- TRUE
      }else{
        self$is_mixed <- FALSE
      }

      if(imp_methods == "mixed" | imp_methods == "perseus"){
        self$is_imp <- TRUE
        if(!self$imp_run_once){

          self$imp_run_once <- TRUE

          if(self$is_mixed){
            data_mixed <- data %>%
              dplyr::group_by(gene_names, condition) %>%
              dplyr::mutate(for_mean_imp = dplyr::if_else((sum(bin_intensity) / dplyr::n()) >= 0.75, TRUE, FALSE)) %>%
              dplyr::mutate(mean_grp = mean(intensity, na.rm = TRUE)) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(imp_intensity = dplyr::case_when(
                bin_intensity == 0 & for_mean_imp ~ mean_grp,
                TRUE ~ as.numeric(intensity))) %>%
              dplyr::mutate(intensity = imp_intensity) %>%
              dplyr::select(-c(for_mean_imp, mean_grp, imp_intensity))

            data <- data_mixed
          }
          ## this funcion perform classical Perseus imputation
          ## sice use random nomral distibution i will set a set.seed()
          set.seed(11)

          imputed_data <- data %>%
            dplyr::group_by(label) %>%
            # Define statistic to generate the random distribution relative to sample
            dplyr::mutate(
              mean = mean(intensity, na.rm = TRUE),
              sd = sd(intensity, na.rm = TRUE),
              n = sum(!is.na(intensity)),
              total = nrow(data) - n
            ) %>%
            dplyr::ungroup() %>%
            # Impute missing values by random draws from a distribution
            # which is left-shifted by parameter 'shift' * sd and scaled by parameter 'scale' * sd.
            dplyr::mutate(imp_intensity = dplyr::case_when(
              is.na(intensity) ~ rnorm(total, mean = (mean - shift * sd), sd = sd * scale),
              TRUE ~ intensity
            )) %>%
            dplyr::mutate(intensity = imp_intensity) %>%
            dplyr::select(-c(mean, sd, n, total, imp_intensity))

          self$imputed_data <- imputed_data

        }

      }else{
        self$is_imp <- FALSE
      }


    },
    plot_imputation = function(){

      if(!self$is_norm & !self$is_imp){
        data <- self$filtered_data
      }else if(self$is_norm & !self$is_imp){
        data <- self$normalized_data
      }else{
        data <- self$imputed_data
      }

      p <- data %>%
        dplyr::group_by(condition) %>%
        echarts4r::e_charts() %>%
        echarts4r::e_density(
          intensity,
          smooth = TRUE,
          areaStyle = list(opacity = 0),
          symbol = "none"
        ) %>%
        echarts4r::e_color(self$color_palette) %>%
        echarts4r::e_theme("QProMS_theme")

      return(p)
    },
    plot_pca = function(view_3d = FALSE){

      # verificare che ci sia
      data <- self$imputed_data

      ## generate a matrix from imputed intensiy
      mat <- data %>%
        dplyr::select(gene_names, label, intensity) %>%
        tidyr::pivot_wider(id_cols = "gene_names",
                           names_from = "label",
                           values_from = "intensity") %>%
        tibble::column_to_rownames("gene_names") %>%
        as.matrix()

      ## perform PCA

      pca <- prcomp(t(mat), center = TRUE, scale = TRUE)

      ## calculate persentage of each PC
      pca_var <- pca$sdev^2
      pca_var_perc <- round(pca_var/sum(pca_var)*100, 1)

      ## create a data.frame for the first 3 PC
      pca_table <- data.frame(
        label = rownames(pca$x),
        x = pca$x[, 1],
        y = pca$x[, 2],
        z = pca$x[, 3]
      ) %>%
        dplyr::left_join(self$expdesign, by = "label")

      ## generate plot
      if(!view_3d){
        p <- pca_table %>%
          dplyr::group_by(condition) %>%
          echarts4r::e_chart(x) %>%
          echarts4r::e_scatter(y, symbol_size = c(10, 10), bind = replicate) %>%
          echarts4r::e_tooltip(
            trigger = "item",
            formatter = htmlwidgets::JS("
        function(params){
          return('Rep: ' + params.name);
        }
      ")
          ) %>%
          echarts4r::e_title(text = "PCA", subtext = "Principal component analysis") %>%
          echarts4r::e_x_axis(
            name = paste0("PC1 - ", pca_var_perc[1], " %"),
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 15,
              lineHeight = 50
            )
          ) %>%
          echarts4r::e_y_axis(
            name = paste0("PC2 - ", pca_var_perc[2], " %"),
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 15,
              lineHeight = 50
            )
          )
      }else{
        p <- pca_table %>%
          dplyr::group_by(condition) %>%
          echarts4r::e_chart(x) %>%
          echarts4r::e_scatter_3d(y, z, symbol_size = c(10, 10), bind = replicate) %>%
          echarts4r::e_tooltip(
            trigger = "item",
            formatter = htmlwidgets::JS("
        function(params){
          return('Rep: ' + params.name);
        }
      ")
          ) %>%
          echarts4r::e_title(text = "PCA", subtext = "Principal component analysis") %>%
          echarts4r::e_x_axis_3d(
            name = paste0("PC1 - ", pca_var_perc[1], " %"),
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 15,
              lineHeight = 50
            )
          ) %>%
          echarts4r::e_y_axis_3d(
            name = paste0("PC2 - ", pca_var_perc[2], " %"),
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 15,
              lineHeight = 50
            )
          ) %>%
          echarts4r::e_z_axis_3d(
            name = paste0("PC3 - ", pca_var_perc[3], " %"),
            nameLocation = "center",
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 15,
              lineHeight = 50
            )
          ) %>%
          echarts4r::e_legend()
      }

      return(p)
    },
    tidy_vector = function(data, name) {

      tidy_vec <- data %>%
        tibble::as_tibble(rownames = NA) %>%
        tibble::rownames_to_column(var = "gene_names") %>%
        dplyr::rename(!!name := value)

      return(tidy_vec)
    },
    stat_t_test = function(data, cond_1, cond_2, type="student",
                           fc = 1, alpha = 0.05, p_adj_method = "BH", ...){

      switch(type,
             student = .type <- TRUE,
             welch = .type <- FALSE)

      mat <- data %>%
        dplyr::filter(condition == cond_1 | condition == cond_2) %>%
        dplyr::mutate(label_test = paste(condition, replicate, sep = "_")) %>%
        tidyr::pivot_wider(id_cols = "gene_names",
                           names_from = "label_test",
                           values_from = "intensity") %>%
        tibble::column_to_rownames("gene_names") %>%
        dplyr::relocate(dplyr::contains(cond_2), .after = dplyr::last_col()) %>%
        na.omit() %>%
        as.matrix()

      a <- grep(cond_1, colnames(mat))
      b <- grep(cond_2, colnames(mat))

      p_values_vec <- apply(mat, 1, function(x) t.test(x[a], x[b], var.equal=.type, ...)$p.value)

      p_values <- p_values_vec %>%
        self$tidy_vector(name = "p_val")

      fold_change <- apply(mat, 1, function(x) mean(x[a]) - mean(x[b])) %>% #metterlo in log2?
        self$tidy_vector(name = "fold_change")

      p_ajusted <- p.adjust(p_values_vec, method = p_adj_method) %>%
        self$tidy_vector(name = "p_adj")

      stat_data <- data %>%
        dplyr::filter(condition == cond_1 | condition == cond_2) %>%
        tidyr::pivot_wider(id_cols = "gene_names",
                           names_from = "label",
                           values_from = "intensity") %>%
        dplyr::full_join(., fold_change, by = "gene_names") %>%
        dplyr::full_join(., p_values, by = "gene_names") %>%
        dplyr::full_join(., p_ajusted, by = "gene_names") %>%
        dplyr::mutate(significant = dplyr::if_else(abs(fold_change) >= fc & p_adj <= alpha, TRUE, FALSE))

      return(stat_data)
    },
    stat_anova = function(data, alpha = 0.05){

      p_values_vec <- data %>%
        dplyr::group_by(gene_names) %>%
        dplyr::group_modify( ~ broom::tidy(aov(intensity ~ condition, data = .))) %>%
        dplyr::filter(term == "condition") %>%
        dplyr::select(gene_names, p.value) %>%
        dplyr::rename(p_value = p.value) %>%
        tibble::deframe()

      p_values <- p_values_vec %>%
        self$tidy_vector(name = "p_val")

      p_ajusted <- p.adjust(p_values_vec, method = "BH") %>%
        self$tidy_vector(name = "p_adj")

      stat_data <- data %>%
        tidyr::pivot_wider(id_cols = "gene_names", names_from = "label", values_from = "intensity") %>%
        dplyr::full_join(., p_values, by = "gene_names") %>%
        dplyr::full_join(., p_ajusted, by = "gene_names") %>%
        dplyr::mutate(significant = dplyr::if_else(p_adj <= alpha, TRUE, FALSE))

      return(stat_data)
    },
    plot_volcano = function(table, fc = 1, p_val_thr = 0.05,
                            highlights_names = NULL, text_color = "#440154", bg_color = "#fde725") {
      left_line <-
        tibble(p_val = c(-log10(p_val_thr),-log10(p_val_thr), max(-log10(table$p_val))),
               fold_change = c(min(table$fold_change),-fc,-fc))

      right_line <-
        tibble(p_val = c(max(-log10(table$p_val)),-log10(p_val_thr),-log10(p_val_thr)),
               fold_change = c(fc, max(table$fold_change), fc))

      pal <- list(In = "#21918c", Out = "#e9ecef")

      p <- table %>%
        dplyr::mutate(color = dplyr::if_else(abs(fold_change) >= fc &
                                               p_val <= p_val_thr, "In", "Out")) %>%
        dplyr::group_by(color) %>%
        dplyr::mutate(fold_change = round(fold_change, 2)) %>%
        dplyr::mutate(p_val = -log10(p_val)) %>%
        dplyr::mutate(p_val = round(p_val, 3)) %>%
        echarts4r::e_chart(fold_change) %>%
        echarts4r::e_scatter(p_val, legend = FALSE, bind = gene_names) %>%
        echarts4r::e_tooltip(
          formatter = htmlwidgets::JS(
            "
      function(params){
        return('<strong>' + params.name +
                '</strong><br />FC: ' + params.value[0] +
                '<br />P.val: ' + params.value[1])
                }
    "
          )
        ) %>%
        echarts4r::e_color(c(pal$In, pal$Out)) %>%
        echarts4r::e_data(left_line, fold_change) %>%
        echarts4r::e_line(
          p_val,
          legend = FALSE,
          color = "#440154",
          symbol = "none",
          lineStyle = list(type = "dashed", width = .8)
        ) %>%
        echarts4r::e_data(right_line, fold_change) %>%
        echarts4r::e_line(
          p_val,
          legend = FALSE,
          color = "#440154",
          symbol = "none",
          lineStyle = list(type = "dashed", width = .8)
        ) %>%
        echarts4r::e_toolbox() %>%
        echarts4r::e_toolbox_feature(feature = "dataZoom") %>%
        echarts4r::e_toolbox_feature(feature = "saveAsImage") %>%
        echarts4r::e_title(text = "Volcano plot",
                           subtext = "lps_4h vs ut",
                           left = "center") %>%
        echarts4r::e_x_axis(
          name = "Fold_change",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 15,
            lineHeight = 50
          )
        ) %>%
        echarts4r::e_y_axis(
          name = "-log(Pvalue)",
          nameLocation = "center",
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 15,
            lineHeight = 50
          )
        )

      if (!is.null(highlights_names)) {
        for (name in highlights_names) {
          highlights_name <- table %>%
            dplyr::filter(gene_names == name) %>%
            dplyr::mutate(p_val = -log10(p_val)) %>%
            dplyr::select(xAxis = fold_change,
                          yAxis = p_val,
                          value = gene_names) %>% as.list()

          p <- p %>%
            echarts4r::e_mark_point(
              data = highlights_name,
              silent = TRUE,
              label = list(color = text_color),
              itemStyle = list(color = bg_color)
            )
        }
      }

      return(p)
    }
  )
)
