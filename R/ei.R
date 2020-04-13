if(getRversion() >= "2.15.1")
  utils::globalVariables(
    c(
      ".alts",
      ".altID",
      "fact",
      "fact.x",
      "fact.y",
      "homogen",
      "netsize",
      "grp_sizes",
      "poss_ext",
      "poss_int",
      "grp_ei_tab",
      "ei_sc",
      ".aaties",
      "ei_tab",
      "poss"
    )
  )

#' Calculate the EI-Index for the alter-alter ties of an ego object
#'
#' The EI-Index is the division of the intra-group edge density and the outer-group edge
#' density. It is calculated for the whole network and for subgroups. The
#' whole network EI is a metric indicating the tendency of a network to be
#' clustered by the categories of a given factor variable. The EI value of a
#' groups describes the tendency of a group to be connected or not connected
#' to other groups. Additionally, the EI index can be employed as a measurement
#' for egos tendency to homo-/heteorphily - use the \code{comp_ei()} command
#' for that version of EI-Index.
#' @param object An \code{egor} object.
#' @param alt.attr \code{Character} naming grouping variable.
#' @references Krackhardt, D., Stern, R.N., 1988. Informal networks and
#' organizational crises: an experimental simulation. Social Psychology
#' Quarterly 51 (2), 123-140.
#' @references Everett, M. G., & Borgatti, S. P. (2012). Categorical attribute
#' based centrality: E-I and G-F centrality. Social Networks, 34(4), 562-569.
#' @keywords ego-centered network
#' @keywords sna
#' @examples
#' data("egor32")
#' EI(egor32, "sex")
#' @export
#' @import dplyr
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom purrr map_dfr
#' @importFrom purrr map_dbl
#' @importFrom tidyr spread
#' @importFrom tidyr replace_na
#' @importFrom tidyr complete
#' @importFrom tibble as_tibble
EI <- function(object, alt.attr) {
  ei <- function(e, i)
    (e - i) / (e + i)
  
  get_ei_tab <- function(object) {
    object %>%
      mutate(.alts = map(.alts, function(x)
        select(x, .altID, fact = !!alt.attr_enquo))) %>%
      mutate(ei_tab = map2(
        .alts,
        .aaties,
        .f = function(x, y) {
          left_join(y, x, by = c(".srcID" = ".altID")) %>%
            left_join(x, by = c(".tgtID" = ".altID")) %>%
            mutate(homogen =  fact.x == fact.y) %>%
            mutate(homogen = factor(
              homogen,
              levels = c("TRUE", "FALSE"),
              labels = c("I", "E")
            ))
        }
      ))
  }
  
  calc_grp_sizes <- function(x) {
    x %>%
      mutate(netsize = map_dbl(.alts, nrow)) %>%
      mutate(grp_sizes = map(.alts, function(x) {
        as_tibble(x) %>%
          count(fact)
      })) %>%
      mutate(poss_ext = map2(netsize, grp_sizes, function(x, y) {
        mutate(y, poss_ext = (x - n) * n)
      })) %>%
      mutate(poss_int = map(grp_sizes, function(x) {
        mutate(x, poss_int = (n ^ 2 - n) / 2)
      })) %>%
      mutate(poss = map2(poss_ext, poss_int, function(x, y)
        full_join(x, y, by = c("fact", "n"))))
  }
  
  calc_grp_ei_tab <- function(ei_tab, fact) {
    ei_tab %>%
      filter(fact.x == fact | fact.y == fact) %>%
      count(homogen) %>%
      complete(homogen) %>%
      replace_na(list(n = 0)) %>%
      spread(homogen, n)
  }
  
  alt.attr_enquo <- enquo(alt.attr)
  
  object2 <- as_nested_egor(object)
  
  obj <-
    object2 %>%
    as_tibble() %>%
    select(.alts, .aaties) %>%
    get_ei_tab() %>%
    calc_grp_sizes() %>%
    mutate(grp_ei_tab = map2(ei_tab, grp_sizes, function(x, y) {
      if (nrow(y) < 1 | nrow(x) < 1) {
        if (is.factor(y$fact))
          res <-
            tibble(fact = factor(NA, levels = levels(y$fact)),
                   E = NA_integer_,
                   I = NA_integer_)
        else if (is.numeric(y$fact))
          res <-
            tibble(fact = NA_integer_, E = NA_integer_, I = NA_integer_)
        else if (is.character(y$fact))
          res <-
            tibble(fact = NA_character_, E = NA_integer_, I = NA_integer_)
        else if (is.logical(y$fact))
          res <-
            tibble(fact = NA, E = NA_integer_, I = NA_integer_)
        return(res)
      }
      
      map(y$fact, function(z) {
        calc_grp_ei_tab(x, z)
      }) %>%
        bind_rows() %>%
        bind_cols(fact = y$fact)
    })) %>%
    mutate(grp_ei_tab = map2(grp_ei_tab, poss, function(x, y)
      full_join(x, y, by = "fact")))
  
  a <- map_dfr(obj$grp_ei_tab, function(x)
    x %>%
      summarise_if(is.numeric, sum) %>%
      mutate(
        ei = ei(E, I),
        ei_sc = ei(E / poss_ext, I / poss_int)
      ) %>%
      select(ei_sc))
  
  b <- map_dfr(obj$grp_ei_tab, function(x) {
    x %>%
      mutate(ei_sc = ei(E / poss_ext, I / poss_int)) %>%
      select(fact, ei_sc) %>%
      spread(fact, ei_sc)
  })
  
  cat(paste0("EI-Index: " , substitute(alt.attr), "\n"))
  bind_cols(a, b)
}
