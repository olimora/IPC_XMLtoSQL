library("xml2")

#read xml
xml_input <- read_xml("example.xml")
#get all connectors
connectors_all <- xml_find_all(xml_input, ".//CONNECTOR")

all_objects <- c()
from <- c()
to <- c()
for (con_i in 1:length(connectors_all)) {
  from <- append(from, xml_attr(connectors_all[con_i], "FROMINSTANCE"))
  to <- append(to, xml_attr(connectors_all[con_i], "TOINSTANCE"))
} 

connectors_all_df <- unique(data.frame(from, to))
all_objects <- unique(append(from,to))
  

#treba vyskladavat od targetu asi
#get target
target_node <- xml_find_all(xml_input, ".//TARGET")

#spravit funkciu hlavnu, ktora bude vediet vyskladat subselect pre kazdy typ objektu
#v nej ify na typ objektu a to uz mozu vlastne funkcie riesit. 
#mohlo by sa tak dat rekurzivne vyskladat, od targetu po source selekt, expressns budu tiez selecty so subselectami

#alebo aj od zaciatku... vyskladat kazdy source, so svojim SQ, filtrom, a pozret im v tabulke connectors_all_df kde sa stretaju, robit len potial, potom ich mergnut
#to je asi lepsie
#spravit zoznam - queue - vetvy, ktore treba spravit:
#od kazdeho source az po joiner/union - univerzalne po hocijaky objekt, do ktoreho idu viac ako 1 unique connectory, connectory z roznych FROMINSTANCE
#od takeho objektu zas az pokial tiez nie je v ceste taky na spajanie
#program vytiahne zo zoznamu vetvu, spravi ju. musi byt pre kazdu vetvu spraveny kazdy nutny predchodca - teda zoznam tych objektov, z ktorych tam idu konektory, a pre kazdy objekt si pamatat ci uz bol navstiveny/spraveny/spracovany.
#zo zoznamu teda tahat na spravenie len tie vetvy, ktore maju splnene nutne predpoklady
#tabulka vsetkych objektov - z konektorov - a pri kazdom ci uz je "done"
#tabulka/zoznam vsetkych vetiev zo zoznamom nutnych predpokladov

#treba prechadzat aj po jednotlivych connectoroch, lebo  