test_that("reflora_download works for a search with a vector of herbarium
          acronyms", {

            reflora_download(herbarium = c("ALCB", "HUEFS"),
                             verbose = FALSE,
                             dir = "reflora_download")

            filenames <- list.files("reflora_download")

            alcb <- list.files(paste0("reflora_download/", filenames[1]))
            huefs <- list.files(paste0("reflora_download/", filenames[1]))

            expect_equal(dir.exists("reflora_download"), TRUE)
            expect_equal(length(list.files("reflora_download"))==2, TRUE)
            expect_equal(length(alcb)==6, TRUE)
            expect_equal(length(alcb)==length(huefs), TRUE)
          })
