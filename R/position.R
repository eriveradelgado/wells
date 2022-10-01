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
position_to_column <- function(position, direction, plate_size){


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
position_to_row <- function(position, direction, plate_size){


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
position_to_well <- function(position, direction, plate_size){

  row <- position_to_row(position, direction, plate_size)

  column <- position_to_column(position, direction, plate_size)

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
well_to_position <- function(well, direction, plate_size){

  plate_size <- as.character(plate_size)

  stopifnot(plate_size %in% c("6", "12", "24", "48", "96", "384"))

  wells <- position_to_well(position = 1:plate_size, direction = direction, plate_size = plate_size)

  location <- 1:length(wells)

  names(location) <- wells

  return(as.vector(location[well]))


}

example_range <- c("A1:B2", "A1:C2")
# Should return vector of the kind A1, A2, B1, B2

sequence_to_wells <- function(range){

  well_start_end <- strsplit(example_range, split = ":")

  position_start_end <- purrr::map(start_end, ~well_to_position(.x, direction = "left_right", plate_size = 96))

  purrr::map(position_start_end, ~seq(.x[[1]], .x[[2]]))

}

range_to_well(example_range)





