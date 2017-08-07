library("xml2")

#############
first_column_fl <- 1

#############

xml_input <- read_xml("example.xml")

#find all sources
sources <- xml_find_all(xml_input, ".//SOURCE")

part_select <- "SELECT"
part_from <- "FROM"
part_where <- "WHERE 1=1"
for (si in 1:length(sources)) {

  source_columns <- xml_children(sources[si])
  for (ci in 1:length(source_columns)) {
    column_name <- xml_attr(source_columns[ci], "NAME")
    if (first_column_fl == 1) { 
      first_column_fl <- 0
      part_select <- paste(part_select, column_name)
    } else { 
      part_select <- paste0(part_select, ', ', column_name)
    }
  }
  
  source_name <- xml_attr(sources[si], "NAME")
  source_alias <- paste0("s", si)
  if (si == 1) {  #from
    part_from <- paste(part_from, source_name, source_alias)
  } else {        #join
    part_from <- paste(part_from, source_name, source_alias)
  }
  
  sq_xpath <- paste0(".//TRANSFORMATION[@NAME='SQ_", source_name, "']")
  source_qualifier <- xml_find_all(xml_input, sq_xpath)
  condition_node <- xml_find_all(source_qualifier, "//TABLEATTRIBUTE[@NAME='Source Filter']")
  if (length(condition_node) == 1) {
    condition <- xml_attr(condition_node, "VALUE")
    condition <- gsub("\\$\\$", "&", condition)
    part_where <- paste0(part_where, " and ", source_alias, ".", condition)
  } else {
    #error nenasiel SQ alebo ich nasiel viac
    print("error nenasiel SQ alebo ich nasiel viac")
  }
  
  
}

####prepojit podla konektorov - najdem si source, spracujem, kuknem konektory, ktore idu z neho, kuknem kam idu, a z nich len uniqe, .. to ked idu viacere stlpce do jednoho stvorceka

print(part_select)
print(part_from)
print(part_where)