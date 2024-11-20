library(xml2)

# Warn as you go, not at the end
  options(warn=1)

# Read and parse xml file
  dat <- read_xml("WGOA_EwE_Files/WGOA_14Nov2024.eiixml")

# Create a list of Tables (main eiixml output data structure is "Table")
  tables <- xml_find_all(dat,".//Table")   

# Loop through the nodes (Tables) and read into data frames
for (node in tables){
  # how to select a named node from the list of tables:
  # node <- tables[xml_attr(tables,"Name")=="EcopathCatch"]
  #node <- tables[xml_attr(tables,"Name")=="EcopathGroup"]
  node_name <- xml_attr(node,"Name")
  #cat(node_name,""); #flush.console()
  # Parse the Columns attribute of each Table which is var name/var type.
  # This messy command splits column names by , and by : then discarding
  # the : which is variable type.  I'm not really sure how the `[[` works but
  # it's choosing the 1st element from each list.
  cols <- unlist(lapply(strsplit(unlist(strsplit(xml_attr(node,"Columns"),",")),":"), `[[`,1))

  # Get rows of data and split into a data frame with that data
  rawdat <- xml_text(xml_find_all(node,"Row"))
  if (length(rawdat)>0){
    cat(node_name,"")
     #dtable <- data.frame(matrix(as.numeric(unlist(strsplit(rawdat,","))), ncol=length(cols), byrow=T))
    dtable <- type.convert(data.frame(matrix((unlist(strsplit(rawdat,","))), ncol=length(cols), byrow=T)), as.is=T)
    cat(length(rawdat),"rows\n")
    names(dtable) <- cols
    assign(paste("ewe_",node_name,sep="") , dtable)
  } else {
    dtable <- data.frame(matrix(ncol=length(cols),byrow=T))[-1,]
    #cat("0\n")
  }
  # Use these here if you want to make empty tables where there's no data
  #names(dtable) <- cols
  #assign(paste("ewe_",node_name,sep="") , dtable)
} # end node loop

# Not all tables work - some give warnings, and some have gaps in data
# entry because of ',' and ':' in descriptive texts, need to fix those.

library(Rpath)

# Function to replace negative values with NAs
  vec_na <- function(vec){return(ifelse(vec < (-0.1), NA, vec))}

# Order of groups on spreadsheets/in Rpath is listed in the
# Sequence column.  This is different than the GroupID order
# so a lookup is needed.  GroupID is the key used for From and To
# flows so a lookup table is needed
  seq_look  <- ewe_EcopathGroup$Sequence 
  ord       <- order(ewe_EcopathGroup$Sequence)
  ford      <- order(ewe_EcopathFleet$Sequence)
  
# Sort data tables by sequence order
  ordgroups <- ewe_EcopathGroup[ord, ]
  ordfleets <- ewe_EcopathFleet[ford,]

  
# Create an unbalanced model object with appropriate names
  g_names <- c(ordgroups$GroupName, ordfleets$FleetName)
  g_types <- c(ordgroups$Type, rep(3,length(ordfleets$FleetName)))
  unbal <- create.rpath.params(g_names,g_types)

# Fill group vectors  
  gear_na <- rep(NA,length(ordfleets$FleetName))
  unbal$model$Biomass  <- c(vec_na(ordgroups$Biomass), gear_na)
  unbal$model$PB       <- c(vec_na(ordgroups$ProdBiom), gear_na)
  unbal$model$QB       <- c(vec_na(ordgroups$ConsBiom), gear_na)
  unbal$model$EE       <- c(vec_na(ordgroups$EcoEfficiency), gear_na)
  unbal$model$ProdCons <- c(vec_na(ordgroups$ProdCons), gear_na)
  unbal$model$BioAcc   <- c(vec_na(ordgroups$BiomAcc), gear_na)
  unbal$model$Unassim  <- c(vec_na(ordgroups$Unassim), gear_na)
  
  n_seq <- ordgroups$Sequence; names(n_seq)<-ordgroups$GroupID
  
  pplook <-matrix(
    c(as.integer(n_seq[as.character(ewe_EcopathDietComp$PreyID)]),
      as.integer(n_seq[as.character(ewe_EcopathDietComp$PredID)])),
    ncol=2)
  
  
  
 vec_na(ordgroups$Biomass)




