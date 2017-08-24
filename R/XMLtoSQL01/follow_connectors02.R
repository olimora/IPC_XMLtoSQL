library("xml2")

#read xml
xml_input <- read_xml("example.xml")

#################################################
##
#get all connectors
connectors_all <- xml_find_all(xml_input, ".//CONNECTOR")
all_objects <- c()
from_instance <- c()
to_instance <- c()
from_field <- c()
to_field <- c()
for (con_i in 1:length(connectors_all)) {
  from_instance <- append(from_instance, xml_attr(connectors_all[con_i], "FROMINSTANCE"))
  to_instance <- append(to_instance, xml_attr(connectors_all[con_i], "TOINSTANCE"))
  from_field <- append(from_field, xml_attr(connectors_all[con_i], "FROMFIELD"))
  to_field <- append(to_field, xml_attr(connectors_all[con_i], "TOFIELD"))
} 
connectors_all_df <- data.frame(from_instance, to_instance, from_field, to_field)
connectors_unique_df <- unique(data.frame(from_instance, to_instance))
all_objects <- unique(append(from_instance,to_instance))
rm("from_instance", "to_instance", "from_field", "to_field")

#################################################

branch_starts <- c()
branch_ends <- c()
for (br_i in 1:length(all_objects)) {
  if (sum(connectors_unique_df$to_instance == all_objects[br_i]) != 1) { #pocet vstupnych je rozny od 1 ... 0 ak je to source, 2+ ak join/union. spocitat tolko krat sa nachadza v uniq connectors, stlpec TO, teda kolko donho ide
    branch_starts <- append(branch_starts, all_objects[br_i])
  }
  if (sum(connectors_unique_df$from_instance == all_objects[br_i]) != 1) { 
    branch_ends <- append(branch_ends, all_objects[br_i])
  }
}

#branches <- 


#find all sources
sources <- xml_find_all(xml_input, ".//SOURCE")

