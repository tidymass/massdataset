#' @title convert_mztab2mass_dataset
#' @description Convert mzTab-m format to mass_dataset class.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param file mzTab-m file
#' @param path where is the mzTab-m file.
#' @return A mass_dataset-class object.
#' @importFrom dplyr filter rename
#' @importFrom stats time
#' @export

convert_mztab2mass_dataset <-
  function(file,
           path = ".") {
    options(warn = -1)
    data <-
      read_mztab(file = file, path = path)
    
    ####sample information
    mtd_table <-
      data$mtd_table
    
    group_id <-
      mtd_table %>%
      dplyr::filter(stringr::str_detect(name, "study_variable")) %>%
      pull(name) %>%
      stringr::str_replace_all("-.*", "") %>%
      unique()
    
    group <-
      mtd_table$value[match(group_id, mtd_table$name)]
    
    sample_info <-
      seq_len(length(group)) %>%
      purrr::map(function(i) {
        x <- group_id[i] %>%
          stringr::str_replace("\\[", "\\\\[") %>%
          stringr::str_replace("\\]", "\\\\]")
        temp_sample_id <-
          mtd_table %>%
          dplyr::filter(stringr::str_detect(name, x)) %>%
          dplyr::filter(stringr::str_detect(name, "assay_refs")) %>%
          pull(value) %>%
          stringr::str_split("\\|") %>%
          `[[`(1)
        data.frame(group = group[i],
                   group_id = group_id[i],
                   sample_id = temp_sample_id)
      }) %>%
      dplyr::bind_rows()
    
    sml_table <-
      data$sml_table
    
    expression_data <-
      sml_table %>%
      dplyr::select(dplyr::contains("abundance_assay"))
    
    variable_info <-
      sml_table %>%
      dplyr::select(-colnames(expression_data)) %>%
      dplyr::rename(variable_id = SML_ID) %>%
      dplyr::select(-SMH)
    
    expression_data_group <-
      variable_info %>%
      dplyr::select(dplyr::contains("abundance_study_variable"))
    
    variable_info <-
      variable_info %>%
      dplyr::select(-colnames(expression_data_group))
    
    group_cv <-
      variable_info %>%
      dplyr::select(dplyr::contains("abundance_variation_study_variable"))
    
    variable_info <-
      variable_info %>%
      dplyr::select(-colnames(group_cv))
    
    colnames(expression_data)  <-
      colnames(expression_data) %>%
      stringr::str_replace("abundance_", "")
    
    rownames(expression_data) <- variable_info$variable_id
    
    expression_data <-
      expression_data[, sample_info$sample_id]
    
    colnames(expression_data) <-
      colnames(expression_data) %>%
      stringr::str_replace("\\[", "_") %>%
      stringr::str_replace("\\]", "")
    
    sample_info$sample_id <-
      colnames(expression_data)
    
    sample_info <-
      sample_info %>%
      dplyr::select(sample_id, dplyr::everything()) %>%
      dplyr::mutate(class = "Subject")
    
    mz_rt <-
      purrr::map(variable_info$SMF_ID_REFS, function(x) {
        temp_data <-
          data$smf_table %>%
          dplyr::filter(SMF_ID %in% stringr::str_split(x, "\\|")[[1]]) %>%
          head(1)
        
        temp_data %>%
          dplyr::select(mz = exp_mass_to_charge,
                        rt = retention_time_in_seconds) %>%
          dplyr::mutate(mz = as.numeric(mz),
                        rt = as.numeric(rt))
      }) %>%
      dplyr::bind_rows()
    
    variable_info <-
      variable_info %>%
      dplyr::mutate(mz = mz_rt$mz,
                    rt = mz_rt$rt)
    
    sample_info_note <-
      data.frame(
        name = colnames(sample_info),
        meaning = colnames(sample_info),
        check.names = FALSE
      )
    
    variable_info_note <-
      data.frame(
        name = colnames(variable_info),
        meaning = colnames(variable_info),
        check.names = FALSE
      )
    
    rownames(expression_data) <- variable_info$variable_id
    
    check_result <-
      check_mass_dataset(
        expression_data = expression_data,
        sample_info = sample_info,
        variable_info = variable_info,
        sample_info_note = sample_info_note,
        variable_info_note = variable_info_note
      )
    
    if (stringr::str_detect(check_result, "error")) {
      stop(check_result)
    }
    
    process_info = list()
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "convet_mztabl2mass_dataset()",
      parameter = list("no" = "no"),
      time = Sys.time()
    )
    
    process_info$create_mass_dataset = parameter
    
    object <- new(
      Class = "mass_dataset",
      expression_data = expression_data,
      ms2_data = list(),
      annotation_table = data.frame(),
      sample_info = sample_info,
      variable_info = variable_info,
      sample_info_note = sample_info_note,
      variable_info_note = variable_info_note,
      process_info = process_info,
      other_files = data,
      version = as.character(utils::packageVersion(pkg = "massdataset"))
    )
    object
  }


#' @title Read mzTab-M format data
#' @description Read mzTab-M format data
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param file File name of the mzTab-M data.
#' @param path Where is the file? Default is .
#' @return A list.
#' @importFrom dplyr filter rename
#' @importFrom stats time
#' @importFrom readr read_csv
#' @importFrom purrr map
#' @importFrom stringr str_detect str_split
#' @export

read_mztab <-
  function(file, path = ".") {
    data <-
      readr::read_csv(file.path(path, file),
                      show_col_types = FALSE,
                      col_names = FALSE) %>%
      as.data.frame()
    
    ##Overall structure of an mzTab-M file.
    ##(A) Metadata about the experiment,
    ## describing experimental design (study variables and assays), links
    ###to other files, etc.
    ###(B) The small molecule (SML) table,
    ###capturing “final” results table: i.e., overall calculated quantification
    ###value (and identity where known) of a metabolite.
    ###(C) Quantification value in each (aligned) MS run for MS1 features:
    ###e.g., mapped to individual adducts or charge states of molecule.
    ###(D) Evidence supporting identification (with ambiguity if needed)
    ###for molecules, using CV terms for scores/statistics where available.
    
    ####extract MTD table
    idx <- which(stringr::str_detect(data$X1, "^MTD"))
    
    mtd_table <-
      data[idx, , drop = FALSE]$X1 %>%
      purrr::map(function(x) {
        stringr::str_split(x, "\\\t")[[1]]
      }) %>%
      do.call(rbind, .) %>%
      as.data.frame()
    
    colnames(mtd_table) <- c("MTD", "name", "value")
    
    ####SML table
    idx <- which(stringr::str_detect(data$X1, "^SMH|^SML"))
    
    sml_table <-
      data[idx, , drop = FALSE]$X1 %>%
      purrr::map(function(x) {
        stringr::str_split(x, "\\\t")[[1]]
      }) %>%
      do.call(rbind, .) %>%
      as.data.frame()
    
    colnames(sml_table) <- as.character(sml_table[1,])
    sml_table <- sml_table[-1, , drop = FALSE]
    
    sml_table$theoretical_neutral_mass <-
      as.numeric(sml_table$theoretical_neutral_mass)
    
    idx <-
      which(stringr::str_detect(colnames(sml_table), "abundance"))
    
    for (i in idx) {
      sml_table[, i] <- as.numeric(sml_table[, i])
    }
    
    ####SMF table
    idx <- which(stringr::str_detect(data$X1, "^SMF|^SFH"))
    
    smf_table <-
      data[idx, , drop = FALSE]$X1 %>%
      purrr::map(function(x) {
        stringr::str_split(x, "\\\t")[[1]]
      }) %>%
      do.call(rbind, .) %>%
      as.data.frame()
    
    colnames(smf_table) <- as.character(smf_table[1,])
    smf_table <- smf_table[-1, , drop = FALSE]
    
    idx <-
      which(stringr::str_detect(colnames(smf_table), "abundance"))
    
    for (i in idx) {
      smf_table[, i] <- as.numeric(smf_table[, i])
    }
    
    ####SME table
    idx <- which(stringr::str_detect(data$X1, "^SEH|^SME"))
    
    sme_table <-
      data[idx, , drop = FALSE]$X1 %>%
      purrr::map(function(x) {
        stringr::str_split(x, "\\\t")[[1]]
      }) %>%
      do.call(rbind, .) %>%
      as.data.frame()
    
    colnames(sme_table) <- as.character(sme_table[1,])
    sme_table <- sme_table[-1, , drop = FALSE]
    
    return_result <-
      list(
        mtd_table = mtd_table,
        sml_table = sml_table,
        smf_table = smf_table,
        sme_table = sme_table
      )
    
    return(return_result)
    
  }
