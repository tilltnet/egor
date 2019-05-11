activate <- function(obj, what) {
  what <- match.arg(what, UNITS)
  attr(obj, "active") <- what
  obj
}
