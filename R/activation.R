activate <- function(obj, what) {
  if(what %in% c("egos", "aaties", "alters")) 
    attr(obj, "active") <- what
  else
    stop("<what> needs to be 'egos', 'aaties' or 'alters'.")
  obj
}