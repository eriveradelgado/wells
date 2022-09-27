#' Title
#'
#' @param plate_size
#'
#' @return
#' @export
#'
#' @examples
col_size <- function(plate_size){
 plate_size <-  as.character(plate_size)
switch(plate_size,
       "6" = 3,
       "12" = 4,
       "24" = 6,
       "48" = 8,
       "96" = 12,
       "384" = 24)
}

#' Title
#'
#' @param plate_size
#'
#' @return
#' @export
#'
#' @examples
row_size <- function(plate_size){
plate_size <- as.character(plate_size)
switch(plate_size,
       "6" = 2,
       "12" = 3,
       "24" = 4,
       "48" = 6,
       "96" = 8,
       "384" = 16)
}

#' Title
#'
#' @param position
#' @param direction
#' @param plate_size
#'
#' @return
#' @export
#'
#' @examples
#'
position_column <- function(position, direction, plate_size){


  plate_size <-  as.character(plate_size)

  cols <- col_size(plate_size)

  rows <- row_size(plate_size)

  if(!direction %in% c("left_right", "top_bottom")){
    direction = "left_right"
  }
  if(direction %in% "left_right"){
    columns <- rep(1:cols, times = rows)
  }
  if(direction %in% "top_bottom"){
    columns <-  rep(1:cols, each = rows)
  }

columns[position]

}

#' Title
#'
#' @param position
#' @param direction
#' @param plate_size
#'
#' @return
#' @export
#'
#' @examples
#'
position_row <- function(position, direction, plate_size){


  plate_size <-  as.character(plate_size)

  cols <- col_size(plate_size)

  rows <- row_size(plate_size)

  if(!direction %in% c("left_right", "top_bottom")){
    direction = "left_right"
  }
  if(direction %in% "left_right"){
    rows <- rep(LETTERS[1:rows], each = cols)

  }
  if(direction %in% "top_bottom"){
    rows <- rep(LETTERS[1:rows], times = cols)

  }

rows[position]

}


#' Title
#'
#' @param position
#' @param direction
#' @param plate_size
#'
#' @return
#' @export
#'
#' @examples
#'
position_well <- function(position, direction, plate_size){

  row <- position_row(position, direction, plate_size)

  column <- position_column(position, direction, plate_size)

  paste0(row, column)

}


#' Title
#'
#' @param well
#' @param direction
#' @param plate_size
#'
#' @return
#' @export
#'
#' @examples
position <- function(well, direction, plate_size){

plate_size <- as.character(plate_size)

stopifnot(plate_size %in% c("6", "12", "24", "48", "96", "384"))

wells <- position_well(position = 1:plate_size, direction = direction, plate_size = plate_size)

location <- 1:length(wells)

names(location) <- wells

return(as.vector(location[well]))


}





