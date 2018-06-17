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
#' @return A `tibble` with the values per category in the columns.
#' @keywords ego-centered network analysis
#' @examples
#' data("egor32")
#' composition(egor32, "sex")
#' @export
composition <- function(object, alt.attr, absolute = FALSE) {
  alt.attr_enquo <- enquo(alt.attr)
  
  if(absolute) {
    comp <- function(x) select(x, tmp = !!alt.attr_enquo) %>%
      count(tmp) %>%
      spread(tmp, n) 
  } else {
    comp <- function(x) select(x, tmp = !!alt.attr_enquo) %>%
      count(tmp) %>%
      mutate(prop = n / sum(n)) %>%
      select(tmp, prop) %>%
      spread(tmp, prop)
  }
  map_dfr(object$.alts, comp)
}



#' Calculate third-party compositional measures on an `egor` object
#'
#' `comp_ply()` applies a function, that uses an alter attribute to calculate
#' a compositional measurement, on all networks in an `egor` object and returns a
#' `numeric vector`.
#' @param object An `egor` object.
#' @param alt.attr A `character` naming the variable containing the alter-attribute.
#' @param .f A `function` that returns a numeric.
#' @param ... Optional arguments to `.f`.
#' @param ego.attr Optional  `character` naming an ego attribute.
#' @return A `numeric` vector.
#' @details When an ego attribute is used the `.f` is called like this: 
#' `.f(alt.attr, ego.attr, ...)`. `.f` must return a single numeric value.
#' @keywords ego-centered network analysis
#' @examples
#' df <- make_egor(10, 32)
#' comp_ply(df, "age.years", sd, na.rm = TRUE)
#' @author Michał Bojanowski, \email{m.bojanowski@uw.edu.pl}
#' @author Till Krenz, \email{public@tillt.net}
#' @importFrom purrr map2_dbl
#' @export
comp_ply <- function(object, alt.attr, .f, ..., ego.attr = NULL) {
  alt.attr_enquo <- enquo(alt.attr)
  if (!is.null(ego.attr)) {
    map2_dbl(object$.alts, object[[ego.attr]], function(x, y)
      pull(x, !!alt.attr_enquo) %>% .f(y, ...))
  } else {
    map_dbl(object$.alts, function(x)
      pull(x, !!alt.attr_enquo) %>% .f(...))
  }
}

#' Calculate diversity measures on an `egor` object.
#'
#' `alts_diversity_count()` counts the categories of a variable present in the
#' networks of an `egor` object. `alts_diversity_entropy()` calculates the Shannon
#' entropy as a measurement for diversity of an alter attribute.
#' @param object An `egor` object.
#' @param alt.attr A `character` naming the variable containing the alter-attribute.
#' @param base `Numeric`, base value of logarithm for entropy calculation.
#' @return A `numeric` vector.
#' @keywords ego-centered network analysis
#' @examples
#' data("egor32")
#' alts_diversity_count(egor32, "age")
#' alts_diversity_entropy(egor32, "age")
#' @author Michał Bojanowski, \email{m.bojanowski@uw.edu.pl}
#' @author Till Krenz, \email{public@tillt.net}
#' @export
alts_diversity_count <- function(object, alt.attr) 
  comp_ply(object, alt.attr, .f = fun_alts_diversity)

fun_alts_diversity <- function(x, var_name) {
  factor(x) %>% unique() %>% length()
}

#' @rdname alts_diversity_count
#' @export
alts_diversity_entropy <- function(object, alt.attr, base) 
  comp_ply(object, alt.attr, .f = fun_entropy, base = base)

fun_entropy <- function(x, base = 2) {
  ptab <- prop.table(table(factor(x)))
  sum(ptab * log(1/ptab, base=base))
}

#' Calculate the EI-Indices of an `egor` object as a measurement of ego-alter homophily
#'
#' `comp_ei()` calculates the EI-Index values as a measurement for ego-alter homophily.
#' @param object An `egor` object.
#' @param alt.attr A `character` naming the variable containing the alter-attribute.
#' @param ego.attr A `character` naming an ego attribute.
#' @return A `numeric` vector.
#' @keywords ego-centered network analysis
#' @examples
#' data("egor32")
#' comp_ei(egor32, "age", "age")
#' @export
comp_ei <- function(object, alt.attr, ego.attr) 
  comp_ply(object, alt.attr, .f = fun_comp_ei, ego.attr = ego.attr)

fun_comp_ei <- function(x, ego_var) {
  homogen <- x == ego_var
  tibble(homogen) %>%
    mutate(homogen = factor(homogen, c("TRUE", "FALSE"), c("I", "E"))) %>%
    count(homogen) %>%
    complete(homogen) %>%
    spread(homogen, n) %>%
    {(.$E - .$I) / (.$E + .$I)}
}












