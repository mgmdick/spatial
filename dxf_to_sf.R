
dxf_to_sf <- function(path) {

dxf = readLines(path)

crs_string <- "+init=epsg:28355"

#find the section breaks
section_breaks <- which(dxf == 1001)

#get the index of the lines for the polygon names
section_names <- which(stringr::str_detect(dxf, "VulcanName="))

#strip extra characters for nicer names
poly_names <- str_replace_all( dxf[section_names], "VulcanName=", "")

#create a table of section start and end lines to pull the coordinates
sections <- data.frame(id_start = section_names, id_end = data.table::shift(section_names, type = "lead") - 1, names = poly_names)

#end of last section is given by length of file
sections$id_end[nrow(sections)] <- length(dxf)

sections <- sections[sections$names != "",]

#for each row in the section
sf_polygons <- lapply(1:nrow(sections), function(i) {

  #get the lines relevant to the section
  section_lines <- dxf[sections[i,1] : sections[i,2]]
  
  #get the indices of the " 10" and " 20" rows, and add one to the index to drop down a line
  x_lines <- which(section_lines == " 10") + 1
  y_lines <- which(section_lines == " 20") + 1
  
  
  x_lines <- x_lines[section_lines[x_lines - 6] == "VERTEX"]
  y_lines <- y_lines[section_lines[y_lines - 8] == "VERTEX"]
  
  #get the values and cast to numeric
  y <- as.numeric(section_lines[y_lines])
  x <- as.numeric(section_lines[x_lines])
  
  #append the first value to the end of the vector to produce a closed shape
  y <- c(y,y[1])
  x <- c(x,x[1])
  
  #combine into a st_sf data type with a "name" variable
  tmp <- st_sf(data.frame(name = sections[i,3], st_sfc(st_polygon(list(as.matrix(data.frame(x,y))))), crs = crs_string ))

  tmp
})


all_shapes <- do.call(rbind, sf_polygons)
all_shapes 

}

