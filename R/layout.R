
platemap_range <- function(..., direction = NULL, plate_size = NULL){

list_range <- list(...)
names_list_range <- names(list_range)

# Initialize the vector
range <- vector(mode = "list", length = length(list_range))
value <- vector(mode = "list", length = length(list_range))

for(i in 1:length(names_list_range)){

range[[i]] <- range_to_wells(names_list_range[[i]],
                             direction = direction,
                             plate_size = plate_size
                             )
value[[i]] <- rep(list_range[[i]], times = length(range[[i]]))

}

range <- as.vector(unlist(range))
value <- as.vector(unlist(value))

tibble(well  = range,
       value = value)

}
# usage layout_range("A1:C5" = 1)

platemap_column <- function(range,...){

}

platemap_row <- function(range,...){

}

layout_sequence <- function(range,...){

}
