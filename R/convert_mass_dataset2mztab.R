#' Convert mass_dataset to mzTab Format
#'
#' @author Xiaotao Shen <shenxt1990@outlook.com>
#' @description This function converts a `mass_dataset` object to mzTab format. The function creates
#' mzTab-compatible tables including metadata (MTD), small molecule (SML), small molecule feature (SMF),
#' and small molecule evidence (SME) tables.
#'
#' @param object A `mass_dataset` object containing the `expression_data`, `sample_info`, and `other_files`.
#' @param path A string specifying the directory where the mzTab file will be saved. Default is the current directory.
#'
#' @return Writes an mzTab file to the specified directory.
#'
#' @examples
#' \dontrun{
#' # Assuming 'dataset' is a mass_dataset object
#' convert_mass_dataset2mztab(dataset, path = "./mzTabFiles/")
#' }
#'
#' @details
#' The function first checks if the `mass_dataset` object contains the necessary mzTab tables (MTD, SML, SMF, SME).
#' If not, it generates these tables based on the `expression_data` and `sample_info` in the `mass_dataset` object.
#' The function then writes these tables to an mzTab file in the specified directory.
#'
#' @export

convert_mass_dataset2mztab <-
  function(object,
           path = ".") {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    other_files <-
      tryCatch(
        slot(object = object, name = "other_files"),
        error = function(e)
          list()
      )
    
    if (length(other_files) > 0) {
      if (all(c("mtd_table", "sml_table", "smf_table", "sme_table") %in% names(other_files))) {
        other_files <-
          other_files[c("mtd_table", "sml_table", "smf_table", "sme_table")]
      }
    }
    
    if (length(other_files) == 0 |
        any(!c("mtd_table", "sml_table", "smf_table", "sme_table") %in% names(other_files))) {
      object <-
        object %>%
        activate_mass_dataset(what = "sample_info") %>%
        filter(class == "Subject")
      sample_info <-
        object@sample_info
      
      sample_info$ms_run <-
        paste0("ms_run",
               "[",
               seq_len(nrow(sample_info)),
               "]")
      
      sample_info$assay <-
        paste0("assay",
               "[",
               seq_len(nrow(sample_info)),
               "]")
      
      sample_info$study_variable <-
        paste0("study_variable",
               "[",
               length(unique(sample_info$class)),
               "]")
      
      ms_run_info <-
        lapply(sample_info$ms_run,
               function(x) {
                 paste0(
                   x,
                   "-",
                   c(
                     "location",
                     "hash_method",
                     "format",
                     "fragmentation_method[1]",
                     "scan_polarity[1]",
                     "id_format"
                   )
                 )
               }) %>%
        unlist() %>%
        data.frame(MTD = "MTD",
                   name = .,
                   value = "null")
      
      assay_info <-
        data.frame(
          MTD = "MTD",
          name = paste0(sample_info$assay, "-ms_run_ref"),
          value = sample_info$ms_run
        )
      
      study_variable_info <-
        lapply(unique(sample_info$study_variable),
               function(x) {
                 paste0(
                   x,
                   c(
                     "",
                     "-description",
                     "-average_function",
                     "-variation_function",
                     "-assay_refs"
                   )
                 )
               }) %>%
        unlist() %>%
        data.frame(MTD = "MTD",
                   name = .,
                   value = "null")
      
      study_variable_info <- 
      study_variable_info %>% 
        dplyr::left_join(sample_info[,c("class", "study_variable")] %>% 
                           dplyr::distinct(study_variable, .keep_all = TRUE),
                         by = c("name" = "study_variable"))
      
      study_variable_info[is.na(study_variable_info)] <- "null"
      
      study_variable_info <-
        study_variable_info %>% 
        dplyr::mutate(value = dplyr::case_when(
          class != "null" ~ class,
          TRUE ~ value
        )) %>% 
        dplyr::select(-class)
      
      study_variable_info$value <-
      seq_along(study_variable_info$name) %>% 
        purrr::map(function(i){
          x <-
            study_variable_info$name[i]
           
          if(stringr::str_detect(x, "assay_refs")){
            sample_info %>% 
              dplyr::filter(study_variable == stringr::str_replace(x, "-assay_refs", "")) %>% 
              pull(assay) %>% 
              paste(collapse = "|")
          }else{
            study_variable_info$value[i]
          }
        }) %>% 
        unlist()
      
      
      ###mtd table
      mtd_table <-
        rbind(mtd_table_info,
              ms_run_info,
              assay_info,
              study_variable_info)
      
      ###
      variable_info <-
        extract_variable_info(object)
      
      expression_data <-
        extract_expression_data(object)
      
      ###sml table
      sml_table <-
        data.frame(
          SMH = "SML",
          SML_ID = variable_info$variable_id,
          SMF_ID_REFS = variable_info$variable_id,
          database_identifier = "null",
          chemical_formula = "null",
          smiles = "null",
          inchi = "null",
          chemical_name = "null",
          uri = "null",
          theoretical_neutral_mass = "null",
          adduct_ions = "null",
          reliability = "null",
          best_id_confidence_measure = "null",
          best_id_confidence_value = "null"
        )
      
      colnames(expression_data) <-
        paste0("abundance_", sample_info$assay)
      
      sml_table <-
        cbind(sml_table, expression_data)
      
      ####smf table
      smf_table <-
        data.frame(
          SFH = "SMF",
          SMF_ID = variable_info$variable_id,
          SME_ID_REFS = variable_info$variable_id,
          SME_ID_REF_ambiguity_code = "null",
          adduct_ion = "null",
          isotopomer = "null",
          exp_mass_to_charge = variable_info$mz,
          charge = "null",
          retention_time_in_seconds = variable_info$rt,
          retention_time_in_seconds_start = NA,
          retention_time_in_seconds_end = NA
        )
      
      smf_table <-
        cbind(smf_table, expression_data)
      
      ###SME table
      sme_table <-
        data.frame(
          SEH = "SME",
          SME_ID = variable_info$variable_id,
          evidence_input_id = "null",
          database_identifier = "null",
          chemical_formula = "null",
          smiles = "null",
          inchi = "null",
          chemical_name = "null",
          uri = "null",
          derivatized_form = "null",
          adduct_ion = "null",
          exp_mass_to_charge = variable_info$mz,
          charge = "null",
          theoretical_mass_to_charge = variable_info$mz,
          spectra_ref = "null",
          identification_method = "null",
          ms_level = "null",
          "id_confidence_measure[1]" = "null",
          rank = "null",
          check.names = FALSE
        )
      
      other_files <-
        list(
          mtd_table = mtd_table,
          sml_table = sml_table,
          smf_table = smf_table,
          sme_table = sme_table
        )
    }
    
    max_ncol <- c(
      ncol(other_files$mtd_table),
      ncol(other_files$sml_table),
      ncol(other_files$smf_table),
      ncol(other_files$sme_table)
    ) %>%
      max()
    
    other_files <-
      other_files %>%
      purrr::map(function(x) {
        if (ncol(x) < max_ncol) {
          new_info <-
            matrix("", nrow = nrow(x), ncol = max_ncol - ncol(x)) %>%
            as.data.frame()
          x <-
            cbind(x, new_info)
          x
        } else{
          x
        }
      })
    
    colnames(other_files$mtd_table) <- NULL
    
    other_files$sml_table <-
      rbind(rep("", ncol(other_files$sml_table)),
            rbind(colnames(other_files$sml_table),
                  other_files$sml_table))
    colnames(other_files$sml_table) <- NULL
    
    other_files$smf_table <-
      rbind(rep("", ncol(other_files$smf_table)),
            rbind(colnames(other_files$smf_table),
                  other_files$smf_table))
    colnames(other_files$smf_table) <- NULL
    
    other_files$sme_table <-
      rbind(rep("", ncol(other_files$sme_table)),
            rbind(colnames(other_files$sme_table),
                  other_files$sme_table))
    colnames(other_files$sme_table) <- NULL
    
    colnames(other_files[[1]]) <-
      colnames(other_files[[2]]) <-
      colnames(other_files[[3]]) <-
      colnames(other_files[[4]]) <-
      seq_len(max_ncol)
    
    other_files <-
      other_files %>%
      dplyr::bind_rows() %>%
      as.data.frame()
    
    colnames(other_files) <- NULL
    rownames(other_files) <- NULL
    
    which(other_files == "V1", arr.ind = TRUE)
    
    idx1 <-
      which(other_files[, 1, drop = TRUE] == "SMH")
    idx2 <-
      which(stringr::str_detect(as.character(other_files[idx1, ]), "V[0-9]{1,2}"))
    if (length(idx2) > 0) {
      other_files[idx1, idx2] <- ""
    }
    
    idx1 <-
      which(other_files[, 1, drop = TRUE] == "SFH")
    idx2 <-
      which(stringr::str_detect(as.character(other_files[idx1, ]), "V[0-9]{1,2}"))
    if (length(idx2) > 0) {
      other_files[idx1, idx2] <- ""
    }
    
    write.table(
      x = other_files,
      file = file.path(path, "data.mzTab.txt"),
      quote = FALSE,
      sep = "\t",
      row.names = FALSE
    )
    
    
    file.rename(from = file.path(path, "data.mzTab.txt"),
                to = file.path(path, "data.mzTab"))
    
  }








mtd_table_name <-
  c(
    "assay[1]-ms_run_ref",
    "assay[2]-ms_run_ref",
    "assay[3]-ms_run_ref",
    "assay[4]-ms_run_ref",
    "assay[5]-ms_run_ref",
    "assay[6]-ms_run_ref",
    "assay[7]-ms_run_ref",
    "assay[8]-ms_run_ref",
    "assay[9]-ms_run_ref",
    "assay[10]-ms_run_ref",
    "assay[11]-ms_run_ref",
    "assay[12]-ms_run_ref",
    "assay[13]-ms_run_ref",
    "assay[14]-ms_run_ref",
    "assay[15]-ms_run_ref",
    "study_variable[1]",
    "study_variable[1]-description",
    "study_variable[1]-average_function",
    "study_variable[1]-variation_function",
    "study_variable[1]-assay_refs",
    "study_variable[2]",
    "study_variable[2]-description",
    "study_variable[2]-average_function",
    "study_variable[2]-variation_function",
    "study_variable[2]-assay_refs",
    "study_variable[3]",
    "study_variable[3]-description",
    "study_variable[3]-average_function",
    "study_variable[3]-variation_function",
    "study_variable[3]-assay_refs",
    "cv[1]-label",
    "cv[1]-uri",
    "cv[1]-version",
    "cv[1]-full_name",
    "cv[2]-label",
    "cv[2]-uri",
    "cv[2]-version",
    "cv[2]-full_name",
    "cv[3]-label",
    "cv[3]-uri",
    "cv[3]-version",
    "cv[3]-full_name",
    "cv[4]-label",
    "cv[4]-uri",
    "cv[4]-version",
    "cv[4]-full_name",
    "cv[5]-label",
    "cv[5]-uri",
    "cv[5]-version",
    "cv[5]-full_name",
    "cv[6]-label",
    "cv[6]-uri",
    "cv[6]-version",
    "cv[6]-full_name",
    "small_molecule-quantification_unit",
    "small_molecule_feature-quantification_unit",
    "small_molecule-identification_reliability",
    "database[1]",
    "database[1]-prefix",
    "database[1]-uri",
    "database[1]-version"
  )


mtd_table_info <-
  data.frame(
    MTD = "MTD",
    name = c(
      "mzTab-version",
      "mzTab-ID",
      "contact[1]-name",
      "contact[1]-email",
      "contact[1]-affiliation",
      "contact[2]-name",
      "contact[2]-email",
      "contact[2]-affiliation",
      "publication[1]",
      "instrument[1]-name",
      "instrument[1]-source",
      "instrument[1]-analyzer[1]",
      "instrument[1]-detector",
      "quantification_method",
      "sample[1]-species[1]",
      "sample[1]-cell_type[1]",
      "sample[1]-tissue[1]",
      "sample_processing[1]",
      "software[1]",
      "software[1]-setting[1]"
    ),
    value = "null"
  )
