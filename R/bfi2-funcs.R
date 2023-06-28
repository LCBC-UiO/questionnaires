#' @name bfi
#' @title Big 5 Inventory
#' @description {
#' ```{r child="man/fragments/bfi/background.Rmd"}
#' ```
#' 
#' ```{r child="man/fragments/bfi/scoring.Rmd"}
#' ```
#' 
#' ## Data requirements
#' ```{r child="man/fragments/bfi/datareq.Rmd"}
#' ```
#' 
#' ## References
#' ```{r child="man/fragments/bfi/references.Rmd"}
#' ```
#' }
#'
#' @param data data.frame containing bfi data
#' @param type Choose domains or facets. Default is both
#' @template keep_all 
#' @template prefix 
#'
#' @return data.frame with calculated scores
#' @export
#'
#' @examples
#' library(dplyr)
#' # Making some test data
#' test_data <- tibble(
#'   id = rep(1:10, each = 60),
#'   name = rep(sprintf("bfi_%02d", 1:60), 10),
#'   value = lapply(1:10, function(x){
#'     sample(1:5, size = 60, replace = TRUE)
#'   }) %>% unlist()
#' ) %>% 
#'   tidyr::pivot_wider()
#'   
#' bfi_compute(test_data)
#' bfi_compute(test_data, prefix = "bfi_")
bfi_compute <- function(data,
                        type = c("domains", "facets"),
                        keep_all = FALSE,
                        prefix = "bfi_"){
  type <- match.arg(type,
                    c("domains", "facets"),
                    several.ok = TRUE)
  
  tmp <- tibble(.del = 1:nrow(data))
  
  if("domains" %in% type)
    tmp <- cbind(tmp,
                 bfi_compute_domains(data))
  
  if("facets" %in% type)
    tmp <- cbind(tmp,
                 bfi_compute_facets(data))
  
  tmp <- dplyr::select(tmp, -.del)
  
  if(prefix != "" | !is.null(prefix))
    names(tmp) <- paste0(prefix, names(tmp))
  
  if(keep_all)
    tmp <- cbind(data, tmp)
  
  tmp
}

#' Big 5 Inventory - Domains
#' @param domains string vector of domains to compute
#' @export
#' @describeIn bfi Compute BFI-2 domains and return in a data.frame
bfi_compute_domains <- function(
    data,
    domains = c("extraversion", 
                "agreeableness",
                "conscientiousness",
                "negative emotionality",
                "open-mindedness"),
    keep_all = FALSE,
    prefix = "domain_"
){
  domains <- match.arg(domains,
                       c("extraversion", 
                         "agreeableness",
                         "conscientiousness",
                         "negative emotionality",
                         "open-mindedness"),
                       several.ok = TRUE)
  tmp <- tibble(.del = 1:nrow(data))
  
  if("extraversion" %in% domains)
    tmp <- cbind(tmp,
                 extrav = bfi_domain_extravers(data))
  
  if("agreeableness" %in% domains)
    tmp <- cbind(tmp,
                 agree = bfi_domain_agreeable(data))
  
  if("conscientiousness" %in% domains)
    tmp <- cbind(tmp,
                 consc = bfi_domain_conscient(data))
  
  if("negative emotionality" %in% domains)
    tmp <- cbind(tmp,
                 negem = bfi_domain_agreeable(data))
  
  if("open-mindedness" %in% domains)
    tmp <- cbind(tmp,
                 openm = bfi_domain_negemotion(data))
  
  tmp <- dplyr::select(tmp, -.del)
  
  if(prefix != "" | !is.null(prefix))
    names(tmp) <- paste0(prefix, names(tmp))
  
  if(keep_all)
    tmp <- cbind(data, tmp)
  
  tmp
}

#' @param facets string vector of facets to compute
#' @export
#' @describeIn bfi Compute BFI-2 domains and return in a data.frame
bfi_compute_facets <- function(
    data,
    facets =  c("sociability", 
                "assertiveness",
                "energy",
                "compassion",
                "respectful",
                "trust", 
                "organization",
                "productive",
                "responsibility",
                "anxiety",
                "depression", 
                "emotional volatility",
                "intellectual curiosity",
                "aesthetic sensebility",
                "creative imagination"),
    keep_all = FALSE,
    prefix = "facet_"
){
  domains <- match.arg(facets,
                       c("sociability", 
                         "assertiveness",
                         "energy",
                         "compassion",
                         "respectful",
                         "trust", 
                         "organization",
                         "productive",
                         "responsibility",
                         "anxiety",
                         "depression", 
                         "emotional volatility",
                         "intellectual curiosity",
                         "aesthetic sensebility",
                         "creative imagination"),
                       several.ok = TRUE)
  tmp <- tibble(.del = 1:nrow(data))
  
  if("sociability" %in% facets)
    tmp <- cbind(tmp,
                 sociabi = bfi_facet_sociability(data))
  
  if("assertiveness" %in% facets)
    tmp <- cbind(tmp,
                 asserti = bfi_facet_assertive(data))
  
  if("energy" %in% facets)
    tmp <- cbind(tmp,
                 energy = bfi_facet_energy(data))
  
  if("compassion" %in% facets)
    tmp <- cbind(tmp,
                 compass = bfi_facet_compassion(data))
  
  if("respectful" %in% facets)
    tmp <- cbind(tmp,
                 respect = bfi_facet_respectful(data))
  
  if("trust" %in% facets)
    tmp <- cbind(tmp,
                 trust = bfi_facet_trust(data))
  
  if("organization" %in% facets)
    tmp <- cbind(tmp,
                 organiz = bfi_facet_organization(data))
  
  if("productive" %in% facets)
    tmp <- cbind(tmp,
                 product = bfi_facet_productive(data))
  
  if("responsibility" %in% facets)
    tmp <- cbind(tmp,
                 respons = bfi_facet_responsibility(data))
  
  if("anxiety" %in% facets)
    tmp <- cbind(tmp,
                 anxiety = bfi_facet_anxiety(data))
  
  if("depression" %in% domains)
    tmp <- cbind(tmp,
                 depress = bfi_facet_depression(data))
  
  if("emotional volatility" %in% facets)
    tmp <- cbind(tmp,
                 emovola = bfi_facet_emovolatility(data))
  
  if("intellectual curiosity" %in% facets)
    tmp <- cbind(tmp,
                 intcuri = bfi_facet_intcuriosity(data))
  
  if("aesthetic sensebility" %in% facets)
    tmp <- cbind(tmp,
                 aessens = bfi_facet_aestheticsens(data))
  
  if("creative imagination" %in% facets)
    tmp <- cbind(tmp,
                 cretima = bfi_facet_imagination(data))
  
  tmp <- dplyr::select(tmp, -.del)
  
  if(prefix != "" | !is.null(prefix))
    names(tmp) <- paste0(prefix, names(tmp))
  
  if(keep_all)
    tmp <- cbind(data, tmp)
  
  tmp
}

# domains ----
#' @name bfi_domain
#' @title BFI-2 Domain computations
#' @description {
#' Calculate the domains of the BFI-2
#' }
#'
#' @param data data.frame with BFI data in
#' @param cols tidyselector(s) of which columns data are in
#' @param reverse logical. If reversal is needed (default) or not
#' @param ... other arguments to \code{\link{bfi_reversal}}
#'
#' @return a vector with computed domain values.
#' @examples
#' library(dplyr)
#' # Making some test data
#' test_data <- dplyr::tibble(
#'   id = rep(1:10, each = 60),
#'   name = rep(sprintf("bfi_%02d", 1:60), 10),
#'   value = lapply(1:10, function(x){
#'     sample(1:5, size = 60, replace = TRUE)
#'   }) %>% unlist()
#' ) %>% 
#'   tidyr::pivot_wider()
#'   
#' bfi_domain_extravers(test_data)
#' bfi_domain_conscient(test_data)
NULL

#' @export
#' @describeIn bfi_domain Calculate the extraversion domain.
bfi_domain_extravers <- function(data, 
                                 cols = matches("01$|06$|11$|16$|21$|26$|31$|36$|41$|46$|51$|56$"),
                                 reverse = TRUE,
                                 ...){
  bfi_sums(data, cols, reverse, ...)
}

#' @export
#' @describeIn bfi_domain Calculate the agreeableness domain.
bfi_domain_agreeable <- function(data, 
                                 cols = matches("02$|07$|12$|17$|22$|27$|32$|37$|42$|47$|52$|57$"),
                                 reverse = TRUE,
                                 ...){
  bfi_sums(data, cols, reverse, ...)
}

#' @export
#' @describeIn bfi_domain Calculate the conscientiousness domain.
bfi_domain_conscient <- function(data, 
                                 cols = matches("03$|08$|13$|18$|23$|28$|33$|38$|43$|48$|53$|58$"),
                                 reverse = TRUE,
                                 ...){
  bfi_sums(data, cols, reverse, ...)
}

#' @export
#' @describeIn bfi_domain Calculate the negative emotionality domain.
bfi_domain_negemotion <- function(data, 
                                  cols = matches("04$|09$|14$|19$|24$|29$|34$|39$|44$|49$|54$|59$"),
                                  reverse = TRUE,
                                  ...){
  bfi_sums(data, cols, reverse, ...)
}

#' @export
#' @describeIn bfi_domain Calculate the open-minded domain.
bfi_domain_openminded <- function(data, 
                                  cols = matches("05$|10$|15$|20$|25$|30$|35$|40$|45$|50$|55$|60$"),
                                  reverse = TRUE,
                                  ...){
  bfi_sums(data, cols, reverse, ...)
}

# facets ----

#' @name bfi_facet
#' @title BFI-2 Facet computations
#' @description {
#' Calculate the facets of the BFI-2
#' }
#'
#' @param data data.frame with BFI data in
#' @param cols tidyselector(s) of which columns data are in
#' @param reverse logical. If reversal is needed (default) or not
#' @param ... other arguments to \code{\link{bfi_reversal}}
#'
#' @return a vector with computed domain values.
#' @examples
#' library(dplyr)
#' # Making some test data
#' test_data <- tibble(
#'   id = rep(1:10, each = 60),
#'   name = rep(sprintf("bfi_%02d", 1:60), 10),
#'   value = lapply(1:10, function(x){
#'     sample(1:5, size = 60, replace = TRUE)
#'   }) %>% unlist()
#' ) %>% 
#'   tidyr::pivot_wider()
#'   
#' bfi_facet_sociability(test_data)
#' bfi_facet_assertive(test_data)
NULL

#' @export
#' @describeIn bfi_facet Calculate the sociability facet.
bfi_facet_sociability <- function(data, 
                                  cols = matches("01$|16$|31$|46$"),
                                  reverse = TRUE,
                                  ...){
  bfi_sums(data, cols, reverse, ...)
}

#' @export
#' @describeIn bfi_facet Calculate the assertive facet.
bfi_facet_assertive <- function(data, 
                                cols = matches("06$|21$|36$|51$"),
                                reverse = TRUE,
                                ...){
  bfi_sums(data, cols, reverse, ...)
}

#' @export
#' @describeIn bfi_facet Calculate the energy level facet.
bfi_facet_energy <- function(data, 
                             cols = matches("11$|26$|41$|56$"),
                             reverse = TRUE,
                             ...){
  bfi_sums(data, cols, reverse, ...)
}

#' @export
#' @describeIn bfi_facet Calculate the compassion facet.
bfi_facet_compassion <- function(data, 
                                 cols = matches("02$|17$|32$|47$"),
                                 reverse = TRUE,
                                 ...){
  bfi_sums(data, cols, reverse, ...)
}

#' @export
#' @describeIn bfi_facet Calculate the respectfulness facet.
bfi_facet_respectful <- function(data, 
                                 cols = matches("07$|22$|37$|52$"),
                                 reverse = TRUE,
                                 ...){
  bfi_sums(data, cols, reverse, ...)
}

#' @export
#' @describeIn bfi_facet Calculate the trust facet.
bfi_facet_trust <- function(data, 
                            cols = matches("12$|27$|42$|57$"),
                            reverse = TRUE,
                            ...){
  bfi_sums(data, cols, reverse, ...)
}

#' @export
#' @describeIn bfi_facet Calculate the organization facet.
bfi_facet_organization <- function(data, 
                                   cols = matches("03$|18$|33$|48$"),
                                   reverse = TRUE,
                                   ...){
  bfi_sums(data, cols, reverse, ...)
}

#' @export
#' @describeIn bfi_facet Calculate the productive facet.
bfi_facet_productive <- function(data, 
                                 cols = matches("08$|23$|38$|53$"),
                                 reverse = TRUE,
                                 ...){
  bfi_sums(data, cols, reverse, ...)
}

#' @export
#' @describeIn bfi_facet Calculate the responsibility facet.
bfi_facet_responsibility <- function(data, 
                                     cols = matches("13$|28$|43$|58$"),
                                     reverse = TRUE,
                                     ...){
  bfi_sums(data, cols, reverse, ...)
}

#' @export
#' @describeIn bfi_facet Calculate the anxiety facet.
bfi_facet_anxiety <- function(data, 
                              cols = matches("04$|19$|34$|49$"),
                              reverse = TRUE,
                              ...){
  bfi_sums(data, cols, reverse, ...)
}

#' @export
#' @describeIn bfi_facet Calculate the depression facet.
bfi_facet_depression <- function(data, 
                                 cols = matches("09$|24$|39$|54$"),
                                 reverse = TRUE,
                                 ...){
  bfi_sums(data, cols, reverse, ...)
}

#' @export
#' @describeIn bfi_facet Calculate the emotional volatiliity facet.
bfi_facet_emovolatility <- function(data, 
                                    cols = matches("14$|29$|44$|59$"),
                                    reverse = TRUE,
                                    ...){
  bfi_sums(data, cols, reverse, ...)
}

#' @export
#' @describeIn bfi_facet Calculate the intellectual curiosity facet.
bfi_facet_intcuriosity <- function(data, 
                                   cols = matches("10$|25$|40$|55$"),
                                   reverse = TRUE,
                                   ...){
  bfi_sums(data, cols, reverse, ...)
}

#' @export
#' @describeIn bfi_facet Calculate the aesthetic sensibility facet.
bfi_facet_aestheticsens <- function(data, 
                                    cols = matches("05$|20$|35$|50$"),
                                    reverse = TRUE,
                                    ...){
  bfi_sums(data, cols, reverse, ...)
}

#' @export
#' @describeIn bfi_facet Calculate the creative imagination facet.
bfi_facet_imagination <- function(data, 
                                  cols = matches("15$|30$|45$|60$"),
                                  reverse = TRUE,
                                  ...){
  bfi_sums(data, cols, reverse, ...)
}


# utils ----
bfi_sums <- function(data, cols, reverse = TRUE, ...){
  tmp <- select(data, {{cols}})
  
  if(reverse){
    tmp <- bfi_reversal(tmp, ...)
  }
  rowSums(tmp)
}


#' Big-5 Item reversals
#'
#' @param data Data with big-5 columns
#' @param ... Column selection to reverse
#'
#' @return data.frame with specified columns reversed
#' @export
#'
#' @examples
#' data <- dplyr::tibble(
#'  col_01 = c(1:5, 3, 5, 4),
#'  col_02 = c(1:5, 3, 5, 4)
#' )
#' bfi_reversal(data, col_01)
bfi_reversal <- function(data, ...){
  tmp <- dplyr::select(data, ...)
  dt  <- dplyr::select(data, -c(...))
  col <- names(data)
  if(nrow(tmp) < 1){
    cols <- c(3, 4, 5, 8, 9, 11, 12, 16, 17, 
              20, 22, 23, 24, 25, 26, 29, 
              31, 36, 37, 42, 44, 45, 47, 
              48, 49, 50, 51, 55, 58)
    cols <- sprintf("%02d", cols)
    cols <- paste0(cols, collapse="$|")
    cols <- paste0(cols, "$")
    tmp  <- dplyr::select(data, dplyr::matches(cols))
    dt   <- dplyr::select(data, -dplyr::matches(cols))
  }
  tmp <- dplyr::mutate(tmp,
                       dplyr::across(dplyr::everything(),
                                     ~ abs((.x - 5)))
  )
  dt <- cbind(dt, tmp)
  dt[, col]
}

if(getRversion() >= "2.15.1")  
  utils::globalVariables(c(".del"))

