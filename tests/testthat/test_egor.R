context("test_egor.R")

test_that(
  "egor object's components can be extracted and rebuild to an egor object.",
  {
    e <- make_egor(32, 20)
    test_that("e is egor object", 
              expect_true((class(e) == "egor")[1]))
    
    egos <- dplyr::select(e, -.alts, -.aaties)
    alters <- tidyr::unnest(tibble::as_tibble(e)[, c("egoID",".alts")])
    aaties <- tidyr::unnest(tibble::as_tibble(e)[, c("egoID",".aaties")])
    
    names(alters)[2] <- "alterID"
    names(aaties)[2:3] <- c("Source", "Target")
    
    expect_error(e1 <- egor(alters, egos, aaties), NA)
    expect_true((class(e)[1] == "egor"))
  }
)

test_that(
  "summary.egor works",
  {
    expect_error(summary(make_egor(3,20)), NA)
  }
)

test_that(
  "egor() works with missing alters/ aaties / egos.",
  {
    e <- make_egor(net.count = 32, max.alters = 20)
    alters <- tidyr::unnest(tibble::as_tibble(e)[, c("egoID",".alts")])
    aaties <- tidyr::unnest(tibble::as_tibble(e)[, c("egoID",".aaties")])
    names(alters)[2] <- "alterID"
    names(aaties)[2:3] <- c("Source", "Target")
    alters <- alters[!as.numeric(alters$egoID) %in% 1, ]
    
    egos <- dplyr::select(e, -.alts, -.aaties)
    expect_error(e1 <- egor(alters.df = alters,
                            egos.df = egos, 
                            aaties.df =  aaties), NA)
    
    aaties <- aaties[!aaties$egoID %in% 2, ]
    expect_error(egor(alters, egos, aaties), NA)
    
    egos <- dplyr::select(e, -.alts, -.aaties)
    egos <- egos[as.numeric(sample(egos$egoID, 24)), ]
    expect_error(egor(alters, egos, aaties), NA)
  }
)


test_that(
  "egor(): Non-Unique egoID in ego data should raises warning)",
  {
    e <- make_egor(32, 20)
    alters <- tidyr::unnest(tibble::as_tibble(e)[, c("egoID",".alts")])
    aaties <- tidyr::unnest(tibble::as_tibble(e)[, c("egoID",".aaties")])
    names(alters)[2] <- "alterID"
    names(aaties)[2:3] <- c("Source", "Target")
    egos <- dplyr::select(e, -.alts, -.aaties)
    egos <- egos[as.numeric(sample(egos$egoID, 24)), ]
    egos <- rbind(egos, cbind(egos[1:4, 1], egos[5:8, -1] ))
    
    expect_warning(egor(alters, egos, aaties))
  }
)
