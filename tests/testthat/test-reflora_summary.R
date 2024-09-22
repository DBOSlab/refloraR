test_that("reflora_summary works for full search (herbarium = NULL) or with a
          vector of herbarium acronyms", {

            res_ex <- reflora_summary(verbose = FALSE,
                                      save = FALSE,
                                      dir = "reflora_summary")

            res_ex_some <- reflora_summary(herbarium = c("ALCB", "RB", "HUEFS", "US", "K"),
                                           verbose = FALSE,
                                           save = FALSE,
                                           dir = "reflora_summary")

            expect_equal(class(res_ex), "data.frame")
            expect_equal(class(res_ex[,3]), "logical")
            expect_equal(class(res_ex[,8]), "numeric")
            expect_equal(ncol(res_ex), 9)
            expect_equal(nrow(res_ex) > 0, TRUE)

            expect_equal(class(res_ex_some), "data.frame")
            expect_equal(class(res_ex_some[,3]), "logical")
            expect_equal(class(res_ex_some[,8]), "numeric")
            expect_equal(ncol(res_ex_some), 9)
            expect_equal(nrow(res_ex_some) > 0, TRUE)

            expect_equal(nrow(res_ex) > nrow(res_ex_some), TRUE)
            expect_equal(sum(res_ex[,8]) > sum(res_ex_some[,8]), TRUE)
          })
