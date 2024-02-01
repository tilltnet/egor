#' Rotate a matrix of x and y coordinates by a radian
#' 
#' @param xy_mat A two column matrix with x and y coordinates.
#' @param rad `Numeric.` Rotation angle in radian.
#' @param direction `Numeric.` Rotation direction, `1` for clockwise `-1` for counter-clockwise.
#' @keywords internal
rotate_xy <- function(xy_mat, rad, direction = 1) {

  cos_angle <- cos(rad)
  sin_angle <- sin(rad)
  
  rot_mat <- direction * matrix(c(cos_angle, sin_angle, -sin_angle, cos_angle), 2,2)
  
  xy_mat %*% rot_mat
}

#' Rotate circle or star graph layout so that it 'stands' on two bottom nodes.
#' 
#' @param xy_mat A two column matrix with x and y coordinates containing the graph layout.
#' @keywords internal
rotate_to_equilibrium <- 
  function(xy_mat) {
    
    if (all(dim(xy_mat) == 2)) {
      xy_mat[, 2] <- 0
      return(xy_mat)
    }
    
    two_lowest_y <- utils::head(sort(xy_mat[,2]), 2)
    
    if (two_lowest_y[1] == two_lowest_y[2]) return(xy_mat)

    two_lowest_xy <- xy_mat[xy_mat[,2] %in% two_lowest_y, ]
    
    dupl_y_lgl <- duplicated(two_lowest_xy[, 2])
    if (any(dupl_y_lgl)) {
      dupl_y <- two_lowest_xy[, 2][dupl_y_lgl]
      non_dupl_y_xy <- two_lowest_xy[two_lowest_xy[,2] != dupl_y, ]
      
      dupl_y_xy <- two_lowest_xy[two_lowest_xy[,2] == dupl_y, ]
      two_lowest_xy <- rbind(
        dupl_y_xy[dupl_y_xy[,1] == max(dupl_y_xy[,1]), ],
        non_dupl_y_xy)
    }
    max_y <- max(two_lowest_xy[, 2])
    
    low_x <- two_lowest_xy[two_lowest_xy[, 2] != max_y, 1]
    high_x <- two_lowest_xy[two_lowest_xy[, 2] == max_y, 1]
    
    rot_dir <- ifelse(abs(low_x) > abs(high_x), -1, 1)
    adj_opp <- two_lowest_xy[1, ] - two_lowest_xy[2, ]
    
    rotate_xy(xy_mat, atan(adj_opp[2] / adj_opp[1]), rot_dir)
  }

