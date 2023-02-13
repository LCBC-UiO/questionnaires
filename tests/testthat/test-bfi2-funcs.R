bfi <- read.delim(test_path("data/bfi.tsv"))

test_that("BFI computations", {
  dt <- bfi_compute(bfi)
  expect_equal(ncol(dt), 20)
  expect_equal(nrow(dt), 10)
  expect_equal(names(dt),
               c("bfi_domain_extrav", "bfi_domain_agree", "bfi_domain_consc", 
                 "bfi_domain_negem", "bfi_domain_openm", "bfi_facet_sociabi", 
                 "bfi_facet_asserti", "bfi_facet_energy", "bfi_facet_compass", 
                 "bfi_facet_respect", "bfi_facet_trust", "bfi_facet_organiz", 
                 "bfi_facet_product", "bfi_facet_respons", "bfi_facet_anxiety", 
                 "bfi_facet_depress", "bfi_facet_emovola", "bfi_facet_intcuri", 
                 "bfi_facet_aessens", "bfi_facet_cretima"))
  
  dt <- bfi_compute(bfi, prefix = "")
  expect_equal(ncol(dt), 20)
  expect_equal(nrow(dt), 10)
  expect_equal(names(dt),
               c("domain_extrav", "domain_agree", "domain_consc", "domain_negem", 
                 "domain_openm", "facet_sociabi", "facet_asserti", "facet_energy", 
                 "facet_compass", "facet_respect", "facet_trust", "facet_organiz", 
                 "facet_product", "facet_respons", "facet_anxiety", "facet_depress", 
                 "facet_emovola", "facet_intcuri", "facet_aessens", "facet_cretima"))
  

  dt <- bfi_compute(bfi, keep_all = TRUE)
  expect_equal(ncol(dt), 81)
  expect_equal(nrow(dt), 10)
  expect_equal(names(dt),
               c("id", "bfi_01", "bfi_02", "bfi_03", "bfi_04", "bfi_05", "bfi_06", 
                 "bfi_07", "bfi_08", "bfi_09", "bfi_10", "bfi_11", "bfi_12", "bfi_13", 
                 "bfi_14", "bfi_15", "bfi_16", "bfi_17", "bfi_18", "bfi_19", "bfi_20", 
                 "bfi_21", "bfi_22", "bfi_23", "bfi_24", "bfi_25", "bfi_26", "bfi_27", 
                 "bfi_28", "bfi_29", "bfi_30", "bfi_31", "bfi_32", "bfi_33", "bfi_34", 
                 "bfi_35", "bfi_36", "bfi_37", "bfi_38", "bfi_39", "bfi_40", "bfi_41", 
                 "bfi_42", "bfi_43", "bfi_44", "bfi_45", "bfi_46", "bfi_47", "bfi_48", 
                 "bfi_49", "bfi_50", "bfi_51", "bfi_52", "bfi_53", "bfi_54", "bfi_55", 
                 "bfi_56", "bfi_57", "bfi_58", "bfi_59", "bfi_60", "bfi_domain_extrav", 
                 "bfi_domain_agree", "bfi_domain_consc", "bfi_domain_negem", "bfi_domain_openm", 
                 "bfi_facet_sociabi", "bfi_facet_asserti", "bfi_facet_energy", 
                 "bfi_facet_compass", "bfi_facet_respect", "bfi_facet_trust", 
                 "bfi_facet_organiz", "bfi_facet_product", "bfi_facet_respons", 
                 "bfi_facet_anxiety", "bfi_facet_depress", "bfi_facet_emovola", 
                 "bfi_facet_intcuri", "bfi_facet_aessens", "bfi_facet_cretima"))
  
  dt <- bfi_compute(bfi, type = "domain")
  expect_equal(ncol(dt), 5)
  expect_equal(nrow(dt), 10)
  
  dt <- bfi_compute(bfi, type = "facet")
  expect_equal(ncol(dt), 15)
  expect_equal(nrow(dt), 10)
  
})


test_that("BFI domain computations", {
  vt <- bfi_domain_extravers(bfi)
  expect_equal(class(vt), "numeric")
  expect_equal(length(vt), 10)
  expect_equal(c(36, 29, 35, 24, 51, 47, 47, 35, 33, 45), vt)
  
  vt <- bfi_domain_agreeable(bfi)
  expect_equal(class(vt), "numeric")
  expect_equal(length(vt), 10)
  expect_equal(c(40, 29, 29, 35, 41, 49, 33, 36, 32, 40), vt)
  
  vt <- bfi_domain_conscient(bfi)
  expect_equal(class(vt), "numeric")
  expect_equal(length(vt), 10)
  expect_equal(c(37, 34, 31, 35, 29, 37, 33, 36, 28, 31), vt)
  
  vt <- bfi_domain_negemotion(bfi)
  expect_equal(class(vt), "numeric")
  expect_equal(length(vt), 10)
  expect_equal(c(36, 25, 35, 38, 35, 36, 32, 29, 31, 35), vt)
  
  vt <- bfi_domain_openminded(bfi)
  expect_equal(class(vt), "numeric")
  expect_equal(length(vt), 10)
  expect_equal(c(36, 46, 42, 39, 38, 45, 37, 42, 39, 39), vt)
})

test_that("BFI facet computations", {
  vt <- bfi_facet_aestheticsens(bfi)
  expect_equal(class(vt), "numeric")
  expect_equal(length(vt), 10)
  expect_equal(c(12, 14, 17, 15, 12, 15, 13, 13, 14, 13), vt)
  
  vt <- bfi_facet_anxiety(bfi)
  expect_equal(class(vt), "numeric")
  expect_equal(length(vt), 10)
  expect_equal(c(14, 7, 11, 11, 10, 14, 14, 11, 8, 9), vt)
  
  vt <- bfi_facet_assertive(bfi)
  expect_equal(class(vt), "numeric")
  expect_equal(length(vt), 10)
  expect_equal(c(15, 10, 12, 8, 18, 14, 18, 13, 8, 15), vt)
  
  vt <- bfi_facet_compassion(bfi)
  expect_equal(class(vt), "numeric")
  expect_equal(length(vt), 10)
  expect_equal(c(14, 11, 12, 12, 11, 13, 18, 13, 13, 15), vt)
  
  vt <- bfi_facet_depression(bfi)
  expect_equal(class(vt), "numeric")
  expect_equal(length(vt), 10)
  expect_equal(c(8, 7, 10, 14, 12, 13, 10, 12, 10, 17), vt)
  
  vt <- bfi_facet_emovolatility(bfi)
  expect_equal(class(vt), "numeric")
  expect_equal(length(vt), 10)
  expect_equal(c(14, 11, 14, 13, 13, 9, 8, 6, 13, 9), vt)
})
