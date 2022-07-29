#' @importFrom rlang ".data"
#' @export
add_insee_metadata2 = function (df) {
  if (any(class(df) %in% c("data.frame"))) {
    if ("IDBANK" %in% names(df)) {
      
      list_idbank_selected_df = dplyr::distinct(.data = df, 
                                                .data$IDBANK)
      
      list_idbank_selected = dplyr::pull(.data = list_idbank_selected_df, 
                                         .data$IDBANK)
      
      idbank_list = suppressMessages(get_idbank_list2())
      idbank_list_short = dplyr::filter(.data = idbank_list, 
                                        .data$idbank %in% list_idbank_selected)
      list_dataset_selected_df = dplyr::distinct(.data = idbank_list_short, 
                                                 .data$nomflow)
      list_dataset_selected = dplyr::pull(.data = list_dataset_selected_df, 
                                          .data$nomflow)
      metadata = get_idbank_list2(list_dataset_selected)
      metadata = dplyr::filter(.data = metadata, .data$idbank %in% 
                                 list_idbank_selected)
      col_to_keep = names(metadata)[!names(metadata) %in% 
                                      c(names(df), paste0("dim", 1:50))]
      metadata = metadata[, col_to_keep]
      metadata = dplyr::mutate(.data = metadata, idbank = as.character(.data$idbank))
      df = dplyr::mutate(.data = df, IDBANK = as.character(.data$IDBANK))
      df = dplyr::left_join(df, metadata, by = c(IDBANK = "idbank"))
      df = insee:::add_type_qual_conf_rev_metadata(df)
    }
  }
  return(df)
}

