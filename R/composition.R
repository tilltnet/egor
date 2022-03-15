if(getRversion() >= "2.15.1") utils::globalVariables(c(
  "tmp",
  "prop",
  "fact",
  "fact.x",
  "fact.y",
  "homogen",
  "netsize",
  "grp_sizes",
  "poss_ext",
  "poss_int",
  "grp_ei_tab",
  "ei_sc"))

#' Calculate the composition of alter attributes in an `egor` object
#'
#' `composition()` calculates the proportional or absolute composition of 
#' alters for a given attribute/variable. 
#' @param object An `egor` object.
#' @param alt.attr A `character` naming the variable containing the alter-attribute.
#' @param absolute `Logical` indicating if the results should be absolute. 
#' @return A `tibble` with the ego ID and values per category of `alt.attr` as `numeric` columns.
#' @keywords ego-centered network analysis
#' @examples
#' data("egor32")
#' composition(egor32, "sex")
#' @export
composition <- function(object, alt.attr, absolute = FALSE) {
  alt.attr_enquo <- enquo(alt.attr)
  
  if (absolute) {
    comp <- function(x) select(x, .egoID, tmp = !!alt.attr_enquo) %>%
      count(.egoID, tmp) %>%
      tidyr::spread(tmp, n) 
  } else {
    comp <- function(x) select(x, .egoID, tmp = !!alt.attr_enquo) %>%
      count(.egoID, tmp) %>%
      mutate(prop = n / sum(n)) %>%
      select(.egoID, tmp, prop) %>%
      tidyr::spread(tmp, prop)
  }
  res <- ungroup(comp(group_by(object$alter, .egoID)))
  
  return_results(object, res)
}



#' Calculate custom compositional measures on an `egor` object
#'
#' `comp_ply()` applies a function, that uses an alter attribute to calculate
#' a compositional measurement, on all networks in an `egor` object and returns a
#' `numeric vector`.
#' @param object An `egor` object.
#' @param alt.attr A `character` naming the variable containing the alter-attribute.
#' @param .f A `function` that returns a numeric.
#' @param ... Optional arguments to `.f`.
#' @param ego.attr Optional  `character` naming an ego attribute.
#' @param result.name Optional `character` naming the result column.
#' @return A `tibble` with the ego ID and a `numeric` result vector.
#' @details When an ego attribute is used the `.f` is called like this: 
#' `.f(alt.attr, ego.attr, ...)`. `.f` must return a single numeric value.
#' @keywords ego-centered network analysis
#' @examples
#' df <- make_egor(10, 32)
#' comp_ply(df, "age.years", sd, na.rm = TRUE)
#' @author Michał Bojanowski, \email{michal2992@gmail.com}
#' @author Till Krenz, \email{egor@tillt.net}
#' @importFrom purrr map2_dbl
#' @export
comp_ply <-
  function(object,
           alt.attr,
           .f,
           ...,
           ego.attr = NULL,
           result.name = "result") {
    alt.attr_enquo <- enquo(alt.attr)
    alter_l <- alters_by_ego(object)
    
    if (!is.null(ego.attr)) {
      res <-
        purrr::map2_dbl(alter_l, as_tibble(object$ego)[[ego.attr]], function(x, y)
          pull(x,!!alt.attr_enquo) %>% .f(y, ...))
    } else {
      res <- purrr::map_dbl(alter_l, function(x)
        pull(x,!!alt.attr_enquo) %>% .f(...))
    }
    
    ego_id <- if(any(class(object$ego) == "tbl_svy")) {
      object$ego$variables$.egoID
    } else {
      object$ego$.egoID
    }
    
    res <- tibble(.egoID = ego_id,
                  result.name = res)
    names(res) <- c(".egoID", result.name)
    
    return_results(x = object, results = res)
    
  }

#' Calculate diversity measures on an `egor` object.
#'
#' `alts_diversity_count()` counts the categories of a variable present in the
#' networks of an `egor` object. `alts_diversity_entropy()` calculates the Shannon
#' entropy as a measurement for diversity of an alter attribute.
#' @param object An `egor` object.
#' @param alt.attr A `character` naming the variable containing the alter-attribute.
#' @param base `Numeric`, base value of logarithm for entropy calculation.
#' @return A `tibble` with the ego ID and a `numeric` result vector.
#' @keywords ego-centered network analysis
#' @examples
#' data("egor32")
#' alts_diversity_count(egor32, "age")
#' alts_diversity_entropy(egor32, "age")
#' @author Michał Bojanowski, \email{michal2992@gmail.com}
#' @author Till Krenz, \email{egor@tillt.net}
#' @export
alts_diversity_count <- function(object, alt.attr) 
  comp_ply(object, alt.attr, .f = fun_alts_diversity, result.name = "diversity")

fun_alts_diversity <- function(x, var_name) {
  na.omit(x) %>% factor() %>% unique() %>% length()
}

#' @rdname alts_diversity_count
#' @export
alts_diversity_entropy <- function(object, alt.attr, base = 2) 
  comp_ply(object, alt.attr, .f = fun_entropy, base = base, result.name = "entropy")

fun_entropy <- function(x, base = 2) {
  ptab <- prop.table(table(factor(x)))
  sum(ptab * log(1/ptab, base=base))
}

#' Calculate the EI-Indices of an `egor` object as a measurement of ego-alter homophily
#'
#' `comp_ei()` calculates the EI-Index values as a measurement for ego-alter homo-/heterophily.
#' @param object An `egor` object.
#' @param alt.attr A `character` naming the variable containing the alter-attribute.
#' @param ego.attr A `character` naming an ego attribute.
#' @return A `tibble` with the ego ID and a `numeric` result vector.
#' @keywords ego-centered network analysis
#' @examples
#' data("egor32")
#' comp_ei(egor32, "age", "age")
#' @export
comp_ei <- function(object, alt.attr, ego.attr) 
  comp_ply(object, alt.attr, .f = fun_comp_ei, ego.attr = ego.attr, result.name = "ei")

fun_comp_ei <- function (x, ego_var) 
{
  x <- as.character(x)
  ego_var <- as.character(ego_var)
  
  homogen <- x == ego_var
  
  suppressWarnings({
    tibble(homogen) %>%
      mutate(homogen = factor(homogen, c("TRUE", "FALSE"), c("I", "E"))) %>%
      count(homogen) %>%
      tidyr::complete(homogen, fill = list(n = 0)) %>%
      tidyr::spread(homogen, n) %>% {
        (.$E - .$I) / (.$E + .$I)
      }
  })
}
