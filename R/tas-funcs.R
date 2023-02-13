
tas_reverse = function(x) (x-6)*-1


#' @importFrom dplyr across mutate
tas_compute_reversed <- function(data, 
                                 cols = c(tas_04, tas_05, tas_10,tas_18),
                                 keep_all = TRUE){
  tmp <- mutate(data, across({{cols}} , tas_reverse))
  if(!keep_all){
    tmp <- select(tmp, {{cols}} )
  }
  tmp
}



#' Compute the TAS factors
#' 
#' @param data Data containing TAS data
#' @param reverse_cols Columns that need reversing
#' @param identify_cols Columns for the "identify feeling" factor
#' @param describe_cols Columns for the "describing feelings" factor
#' @param thinking_cols Columns for the "externally oriented thinking" factor
#' @template keep_all
#' @template prefix
#' @export
#' @importFrom dplyr transmute bind_cols 
tas_compute <- function(data, 
                        reverse_cols  = c(tas_04, tas_05, tas_10,tas_18),
                        identify_cols = c(tas_01,tas_03,tas_06,tas_07,tas_09,tas_13,tas_14),
                        describe_cols = c(tas_02,tas_04,tas_11,tas_12,tas_17),
                        thinking_cols = c(tas_05,tas_08,tas_10,tas_15,tas_16,tas_18,tas_19,tas_20),
                        prefix = "tas_",
                        keep_all = TRUE){
  
  tmp <- tas_compute_reversed(data, cols = {{reverse_cols}}, keep_all = TRUE)
  tmp <- transmute(tmp,
                   fact1_identify = rowSums(select(tmp, {{identify_cols}} )), 
                   fact2_describe = rowSums(select(tmp, {{describe_cols}} )), 
                   fact3_thinking = rowSums(select(tmp, {{thinking_cols}} )) 
  ) 
  if(!is.null(prefix)){
    tmp <- rename_all(tmp, ~paste0(prefix, .x))
  }
  
  if(keep_all){
    tmp <- bind_cols(data, tmp)
  }
  tmp
}

if(getRversion() >= "2.15.1")  
  utils::globalVariables(sprintf("tas_%02d", 1:20))
