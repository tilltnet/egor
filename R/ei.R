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

#' Calculate EI-Index of ego networks
#'
#' The EI-Index is the division of the surplus count intra-group edges over inter-group edges,
#' divided by total count of all edges.
#' This implementation uses the intra-group and inter-group density instead
#' of edge counts, when `rescale` is set to `TRUE` (default). The EI-Index is calculated for 
#' the whole network and for subgroups. Alternatively, the EI index can be employed as a measurement
#' for egos tendency to homo-/heterophily - use [egor::comp_ei()]. 
#' for that variant of the EI-Index.
#' @param object An \code{egor} object.
#' @param alt.attr \code{Character} naming grouping variable.
#' @param include.ego `Logical`. Include or exclude ego from EI calculation.
#' @param ego.attr `Character`, naming the ego variable corresponding to `ego.attr`. Defaults to `ego.attr`.
#' @param rescale `Logical`. If `TRUE`, the EI index calculation is re-scaled, 
#' so that the EI is not distorted by differing group sizes.
#' @return Returns `tibble` with the following columns:
#'  - ego ID (".egoID")
#'  - network EI-Index ("ei")
#'  - subgroup EI-Index values (named by value levels of `alt.attr`/`ego.attr`)
#' @details The
#' whole network EI is a metric indicating the tendency of a network to be
#' clustered by the categories of a given factor variable (`alt.attr`). The EI value of a
#' group describes the tendency of that group within a network to be connected 
#' (if between 0 and 1) or not connected (if between -1 and 0)
#' to other groups. Differing group sizes can lead to a distortion of EI values
#' i.e. the ability of a big group A to form relationships to much smaller group B
#' is limited by the size of B. Even when all possible edges between A and B exist,
#' the EI value for group A might still be negative, classifying it as _homophile_.
#' The `re-scaled` EI-Index values provided by this implementation substitutes absolute
#' edge counts by inter- and intra-group edge densities in order to avoid the
#' distortion of the EI-Index values. These values express the extend of homo- or heterophily 
#' of the network and its subgroups, _as made possible by subgroup sizes_.
#' @seealso [comp_ei()], for an ego level homophily measure.
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
EI <- function(object, 
               alt.attr,
               include.ego = FALSE,
               ego.attr = alt.attr,
               rescale = TRUE) {
  object_original <- object
  
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
      tidyr::replace_na(list(n = 0)) %>%
      tidyr::spread(homogen, n)
  }
  
  alt.attr_enquo <- enquo(alt.attr)
  
  object <- 
    purrr::map(object, ungroup)
  
  if (include.ego) {
    object$aatie <-
      object$alter %>%
      select(.srcID = .altID) %>%
      mutate(.tgtID = NA) %>%
      bind_rows(object$aatie)
    
    object$alter <-
      object$ego[c(".egoID", ego.attr)] %>%
      mutate(.altID = NA) %>%
      bind_rows(object$alter)
  }
  
  class(object) <- c("egor", class(object))
  
  object2 <- strip_ego_design(as_nested_egor(object))

  obj <-
    object2 %>%
    as_tibble() %>%
    ungroup() %>% 
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
      
      purrr::map(y$fact, function(z) {
        calc_grp_ei_tab(x, z)
      }) %>%
        bind_rows() %>%
        bind_cols(fact = y$fact)
    })) %>%
    mutate(grp_ei_tab = map2(grp_ei_tab, poss, function(x, y)
      full_join(x, y, by = "fact")))

  E <- igraph::E

  a <- map_dfr(obj$grp_ei_tab, function(x)
    x %>%
      summarise_if(is.numeric, sum) %>%
      mutate(
        ei = if(rescale) ei(E / poss_ext, I / poss_int) else ei(E, I)) %>% 
      select(ei))
  
  b <- map_dfr(obj$grp_ei_tab, function(x) {
    x %>%
      mutate(ei = if(rescale) ei(E / poss_ext, I / poss_int) else ei(E, I)) %>%
      select(fact, ei) %>%
      tidyr::spread(fact, ei)
  })
  
  if(has_ego_design(object_original)) {
    res <- bind_cols(.egoID = object$ego$variables$.egoID, ei = a, b)
  } else {
    res <- bind_cols(.egoID = object$ego$.egoID, ei = a, b)
  }
  
  return_results(object_original, res)
}
