###############################################################
#
# functions used while managing branching
# params: 
#   xml_input - original XML doc with mapping.
#
##################

## returns dataframes with connectors between objects ########
get_connectors_dfs <- function(xml_input) {
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
  connectors <- list()
  connectors$all <- data.frame(from_instance, to_instance, from_field, to_field, stringsAsFactors = F)
  connectors$unique <- unique(data.frame(from_instance, to_instance, stringsAsFactors = F))
  connectors$objects <- unique(append(from_instance,to_instance))
  #rm("from_instance", "to_instance", "from_field", "to_field", "con_i")
  return(connectors)
}

