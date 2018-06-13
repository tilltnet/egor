#' Calculate the EI-Index
#'
#' The EI-Index is the division of the intra-group edge density and the outer-group edge 
#' density. It can be calculated for the whole network and for subgroups. The
#' whole network EI is a metric indicating the tendency of a network to be 
#' clustered by the categories of a given factor variable. The EI value of a 
#' groups describes the tendency of a group to be connected or not connected 
#' to other groups. Additionally, the EI index can be employed as a measurment
#' for egos tendendy to homo-/heterphily - use the \code{composition} command
#' for individual EI-Index.
#' @param object An \code{egor} object.
#' @param var_name \code{Character} naming grouping variable.
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
#' @importFrom purrr map_df
#' @importFrom purrr map_dbl
#' @importFrom tidyr spread
#' @importFrom tidyr replace_na
#' @importFrom tidyr complete
#' @importFrom tibble as_tibble
EI <- function(object, var_name) {
  ei <- function(e, i)
    (e - i) / (e + i)
  
  get_ei_tab <- function(object) {
    object %>%
      mutate(.alts = map(.alts, function(x)
        select(x, .altID, var = !!var_name_enquo))) %>%
      mutate(ei_tab = map2(
        .alts,
        .aaties,
        .f = function(x, y) {
          left_join(y, x, by = c(".srcID" = ".altID")) %>%
            left_join(x, by = c(".tgtID" = ".altID")) %>%
            mutate(homogen =  var.x == var.y) %>%
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
          count(var)
      })) %>%
      mutate(poss_ext = map2(netsize, grp_sizes, function(x, y) {
        mutate(y, poss_ext = (x - n) * n)
      })) %>%
      mutate(poss_int = map(grp_sizes, function(x) {
        mutate(x, poss_int = (n ^ 2 - n) / 2)
      })) %>%
      mutate(poss = map2(poss_ext, poss_int, function(x, y)
        full_join(x, y, by = c("var", "n"))))
  }
  
  calc_grp_ei_tab <- function(ei_tab, fact) {
    ei_tab %>%
      filter(var.x == fact | var.y == fact) %>%
      count(homogen) %>%
      complete(homogen) %>%
      replace_na(list(n = 0)) %>%
      spread(homogen, n)
  }
  
  var_name_enquo <- enquo(var_name)
  
  obj <- object %>%
    select(.alts, .aaties) %>%
    get_ei_tab() %>%
    calc_grp_sizes() %>%
    mutate(grp_ei_tab = map2(ei_tab, grp_sizes, function(x, y) {
      
      if(nrow(y) < 1) return(
        mutate_all(tibble(fact = NA, E = NA, I = NA), as.numeric))
      
      map(y$var, function(z) {
        calc_grp_ei_tab(x, z)
      }) %>%
        bind_rows() %>%
        bind_cols(fact = y$var)
    })) %>%
    mutate(grp_ei_tab = map2(grp_ei_tab, poss, function(x, y)
      full_join(x, y, by = c("fact" = "var"))))
  
  a <- map_df(obj$grp_ei_tab, function(x)
    x %>%
      summarise_if(is.numeric, sum) %>%
      mutate(
        ei = ei(E, I),
        ei_sc = ei(E / poss_ext, I / poss_int)
      ) %>%
      select(ei_sc))
  
  b <- map_df(obj$grp_ei_tab, function(x) {
    x %>%
      mutate(ei_sc = ei(E / poss_ext, I / poss_int)) %>%
      select(fact, ei_sc) %>%
      spread(fact, ei_sc)
  })
  
  cat(paste0("EI-Index: " , substitute(var_name), "\n"))
  bind_cols(a, b)
}





