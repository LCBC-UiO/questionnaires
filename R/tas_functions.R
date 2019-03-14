
tas_reverse = function(x) (x-6)*-1


#' @importFrom dplyr enquo vars mutate_at
#' @importFrom readr cols
tas_compute_reversed <- function(data, cols = c(TAS_04, TAS_05, TAS_10,TAS_18),
                                 keep_all = TRUE){
  cols = enquo(cols)
  
  tmp <- mutate_at(data, vars(!!cols), tas_reverse)
  
  if(keep_all){
    tmp
  }else{
    select(tmp, !!cols)
  }
}



#' Compute the TAS factors
#' 
#' @param data Data containing TAS data
#' @param feeling_cols Columns for the "identify feeling" factor
#' @param descr_cols Columns for the "describing feelings" factor
#' @param ext_cols Columns for the "externally oriented thinking" factor
#' @param keep_all logical, append to data.frame
#' @export
#' @importFrom dplyr enquo transmute select bind_cols mutate one_of
tas_compute <- function(data, 
                        feeling_cols = c(TAS_01,TAS_03,TAS_06,TAS_07,TAS_09,TAS_13,TAS_14),
                        descr_cols = c(TAS_02,TAS_04,TAS_11,TAS_12,TAS_17),
                        ext_cols = c(TAS_05,TAS_08,TAS_10,TAS_15,TAS_16,TAS_18,TAS_19,TAS_20),
                        keep_all = TRUE){
  
  feeling_cols = enquo(feeling_cols)
  descr_cols = enquo(descr_cols)
  ext_cols = enquo(ext_cols)
  
  tmp <- mutate(tas_compute_reversed(data))
  
  tmp <- transmute(tmp,
                   TAS_Fact1_Ident.Feelings = rowSums(select(tmp, !!feeling_cols)), 
                   TAS_Fact2_Descr.Feelings = rowSums(select(tmp, !!descr_cols)), 
                   TAS_Fact3_Ext.O.Thinking = rowSums(select(tmp, !!ext_cols)) 
  ) 
  
  if(keep_all){
    bind_cols(select(data, -one_of(names(data)[names(data) %in% names(tmp)])), 
              tmp)
  }else{
    tmp
  }
}

if(getRversion() >= "2.15.1")  utils::globalVariables(paste0("TAS_", stringr::str_pad(1:20, 2, "left", 0)))
