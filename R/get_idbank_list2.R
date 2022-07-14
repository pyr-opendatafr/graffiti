#' @importFrom rlang ".data"
#' @export
get_idbank_list2 = function(..., dataset = NULL){
  
  idbank_list_internal = insee:::idbank_list_internal
  
  if(length(list(...)) > 0){
    
    if(length(list(...)) == 1){
      list_dataset = unlist(list(...)[[1]])
    }else{
      list_dataset = unlist(list(...))
    }
    if(is.null(dataset)){
      dataset = list_dataset
    }else{
      dataset = c(dataset, list_dataset)
    }
    idbank_list = dplyr::filter(.data = idbank_list_internal, .data$nomflow %in% dataset)
    
    not_all_na <- function(x) any(!is.na(x))
    
    idbank_list =  dplyr::select(.data = idbank_list, tidyselect:::where(not_all_na))
  }else{
    idbank_list = dplyr::select(.data = insee:::idbank_list_internal, 1:3)
  }
  
  return(idbank_list)
}

# df = get_idbank_list2()

