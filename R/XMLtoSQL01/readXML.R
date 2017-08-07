library("xml2")

input <- read_xml("example.xml")

#find all sources
source <- xml_find_all(input, ".//SOURCE")

part_select <- "SELECT"
part_from <- "FROM"
for (si in 1:length(source)) {
  s_name <- xml_attr(source[si], "NAME")
  print(s_name)
  
  s_children <- xml_children(source[si])
  for (ci in 1:length(s_children)) {
    c_name <- xml_attr(s_children[ci], "NAME")
    #check for ',' at last position in SELECT part
  }
  
  if (si == 1) {  #from
    part_from <- paste(part_from, s_name, paste0("s", si))
  } else {        #join
    part_from <- paste(part_from, s_name, paste0("s", si))
  }
}




print(part_from)