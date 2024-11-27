library(xml2)
library(janitor)
library(Rpath)

# Warn as you go, not at the end
  options(warn=1)

# Read and parse xml file
  dat <- read_xml(eiifile) #"WGOA_EwE_Files/WGOA_Nov2024.eiixml"

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

## ------ ALL RAW DATA from XML should be in R data frames at this point -------

# Extract and order group, gear and stanza names to create Rpath object---------
  
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

# Get and clean names and Types for living groups and gear
  bio_names   <- make_clean_names(ordgroups$GroupName)
  gear_names  <- make_clean_names(ordfleets$FleetName)
  if (sum(gear_names%in%bio_names)>0){
    warning("Fleet ",gear_names[gear_names %in% bio_names]," has same name as a bio group, appending fleet to name")
  }
  gear_names  <- ifelse(gear_names %in% bio_names, paste(gear_names,"fleet",sep='_'),gear_names)
  det_names   <- make_clean_names(ordgroups$GroupName[ordgroups$Type==2])
  live_names  <- make_clean_names(ordgroups$GroupName[ordgroups$Type!=2])  
  
  g_names <- c(bio_names, gear_names)
  g_types <- c(ordgroups$Type, rep(3,length(gear_names)))
  row.names(ordgroups) <- bio_names

# Also make a couple of named vectors associating names with groupID  
# (lookups for species and gear, using ID# as a character index)
  pnames <- bio_names
  names(pnames) <- ordgroups$GroupID
  gnames <- gear_names
  names(gnames) <- ordfleets$FleetID  
  
# Get and clean stanza names, order stanza table  
# TODO: check for models without stanzas
  sord         <- order(ewe_Stanza$StanzaID)
  ordstanzas   <- ewe_Stanza[sord,]
  ordstanzas$stanza_names <- make_clean_names(ordstanzas$StanzaName)
  row.names(ordstanzas) <- as.character(ordstanzas$StanzaID)

# Combine the life stages and stanza info into one table (making some duplication
# as stanza parameters will have multiple copies).
  ordstages <- cbind(ewe_StanzaLifeStage,ordstanzas[as.character(ewe_StanzaLifeStage$StanzaID),])
  
  ordstages$gname <- pnames[as.character(ordstages$GroupID)]
  
  stanza_column <- rep(NA, length(pnames))
  names(stanza_column)<-names(pnames)
  stanza_column[as.character(ordstages$GroupID)] <- ordstages$stanza_names
  stanza_name_only<- c(as.character(stanza_column), rep(NA,length(gnames)))

#-------------------------------------------------------------------------------    

# Create the Unbalanced Rpath object here    
  unbal <- create.rpath.params(group=g_names, type=g_types, stgroup=stanza_name_only)

# ------------------------------------------------------------------------------
# Populate unbal Rpath object with values
  
# Fill group vectors  
  gear_na <- rep(NA,length(gear_names))
  unbal$model$Biomass  <- c(vec_na(ordgroups$Biomass), gear_na)
  unbal$model$PB       <- c(vec_na(ordgroups$ProdBiom), gear_na)
  unbal$model$QB       <- c(vec_na(ordgroups$ConsBiom), gear_na)
  unbal$model$EE       <- c(vec_na(ordgroups$EcoEfficiency), gear_na)
  unbal$model$ProdCons <- c(vec_na(ordgroups$ProdCons), gear_na)
  unbal$model$BioAcc   <- c(vec_na(ordgroups$BiomAcc), gear_na)
  unbal$model$Unassim  <- c(vec_na(ordgroups$Unassim), gear_na)
  unbal$model$DetInput <- c(vec_na(ordgroups$DtImports), gear_na)
  
  #EwE output seems to have a few 0s or other numbers saved that should
  #be NAs, specifically with detritus.  Ensuring those don't sneak through.
  unbal$model$DetInput[ordgroups$Type!=2] <- NA
  unbal$model$EE[ordgroups$Type==2] <- NA
  


# DIET TABLE---------------------------------------
# TODO: where are diet imports in EwE XML data?
  diet_table <- ewe_EcopathDietComp
  diet_table$pred_name <- pnames[as.character(ewe_EcopathDietComp$PredID)]
  diet_table$prey_name <- pnames[as.character(ewe_EcopathDietComp$PreyID)]
  
  ppmat <- data.frame(unbal$diet[,-1])
  row.names(ppmat) <- unbal$diet$Group
  for(i in 1:length(diet_table$Diet)){
    #cat(i,diet_table$prey_name[i], diet_table$pred_name[i],"\n")
    if (diet_table$prey_name[i] %in% rownames(ppmat) & diet_table$pred_name[i] %in% colnames(ppmat)){
       ppmat[diet_table$prey_name[i], diet_table$pred_name[i]] <- diet_table$Diet[i]
     }
    
  }
  # need to convert matrices to data frames or data.table is unhappy
  unbal$diet[,2:ncol(unbal$diet)] <- ppmat

# DETRITUS FATE FOR NON-GEAR -------------------------------------------------    
  detframe <- data.frame(unbal$model)[,det_names]
  row.names(detframe) <- g_names
  for(i in 1:length(diet_table$Diet)){
    if (diet_table$prey_name[i] %in% det_names){
      detframe[diet_table$pred_name[i],diet_table$prey_name[i]] <- diet_table$DetritusFate[i]
    }
  }
  unbal$model[,det_names] <- detframe 
  
# CATCH AND DISCARDS -----------------------------------------------------------
  catch_table <- ewe_EcopathCatch
  catch_table$gear_name  <- gnames[as.character(ewe_EcopathCatch$FleetID)]
  catch_table$group_name <- pnames[as.character(ewe_EcopathCatch$GroupID)]
  
  discard_names <- paste(gear_names,"disc",sep='.')
  
  fishframe    <- data.frame(unbal$model)[,gear_names]
  row.names(fishframe) <- unbal$model$Group
  discardframe <- data.frame(unbal$model)[,discard_names]
  row.names(discardframe) <- unbal$model$Group 
  for (i in 1:length(catch_table$Landing)){
    if(catch_table$gear_name[i] %in% colnames(fishframe) & catch_table$group_name[i] %in% rownames(fishframe)){
      fishframe[catch_table$group_name[i],catch_table$gear_name[i]] <- catch_table$Landing[i]
      discardframe[catch_table$group_name[i],paste(catch_table$gear_name[i],"disc",sep='.')] <- catch_table$Discards[i]
    }
  }
  
  unbal$model[,gear_names] <- fishframe
  unbal$model[,discard_names] <- discardframe
  
# DETRITUS FATE FOR GEAR -------------------------------------------------------
  fate_table <- ewe_EcopathDiscardFate
  fate_table$gear_name  <- gnames[as.character(ewe_EcopathDiscardFate$FleetID)]
  fate_table$group_name <- pnames[as.character(ewe_EcopathDiscardFate$GroupID)]
  
  fateframe <- data.frame(unbal$model)[unbal$model$Group %in% gnames,det_names]
  row.names(fateframe) <- gnames
  for (i in 1:length(fate_table$DiscardFate)){
    if(fate_table$gear_name[i] %in% rownames(fateframe) & fate_table$group_name[i] %in% colnames(fateframe)){
      fateframe[fate_table$gear_name[i], fate_table$group_name[i]] <- fate_table$DiscardFate[i]
    }
  }

  unbal$model[(length(bio_names)+1):(length(bio_names)+length(gear_names)), det_names] <- fateframe

  
# STANZAS ----------------------------------------------------------------------
# Rename ordered tables to use cleaned names, not GroupIDs
row.names(ordstanzas) <- ordstanzas$stanza_names
  
  unbal$stanzas$stgroups$Wmat     <-ordstanzas[unbal$stanzas$stgroups$StanzaGroup,"WmatWinf"]  
  unbal$stanzas$stgroups$BAB      <-ordstanzas[unbal$stanzas$stgroups$StanzaGroup,"BABsplit"] 
  unbal$stanzas$stgroups$RecPower <-ordstanzas[unbal$stanzas$stgroups$StanzaGroup,"RecPower"]
  
row.names(ordstages) <- ordstages$gname
unbal$stanzas$stindiv$StanzaNum <- ordstages[unbal$stanzas$stindiv$Group,"Sequence"]
unbal$stanzas$stindiv$Leading   <- ifelse(
  ordstages[unbal$stanzas$stindiv$Group,"LeadingLifeStage"]==
  ordstages[unbal$stanzas$stindiv$Group,"Sequence"], TRUE, FALSE)
unbal$stanzas$stindiv$First <- ordstages[unbal$stanzas$stindiv$Group,"AgeStart"]

# Not sure how Mort is stored outside of PB, or if it's different.
pb <- unbal$model$PB; names(pb) <- unbal$model$Group
unbal$stanzas$stindiv$Z         <- pb[unbal$stanzas$stindiv$Group]
cat("Created unbal (unbalanced ecopath) from ",eiifile,"\n")

# Looping through in this weird way doesn't change the data.tables data type
# (default logical) for the Last column.  (more foolishness)
unbal$stanzas$stindiv[ , Last := as.numeric(Last)] 
unbal$stanzas$stgroups[, VBGF_Ksp := as.numeric(VBGF_Ksp)]

for (gg in 1:max(unbal$stanzas$stindiv$StGroupNum)){
  # Finding vonBK is a mess involving all three tables (stored on group table)
  # Look up Leading Group and Stanza Group Number to get name on main table.
  unbal$stanzas$stgroups[StGroupNum==gg,"VBGF_Ksp"] <-
    ordgroups[as.character(unbal$stanzas$stindiv[StGroupNum==gg & Leading==T,"Group"]),]$vbK
  
  # Now loop through to add Last Month to first month
  szs <- unbal$stanzas$stindiv[StGroupNum==gg,]
  for (ss in 1:(max(szs$StanzaNum)-1)){
    unbal$stanzas$stindiv[StGroupNum==gg & szs$StanzaNum==ss,"Last"] = szs[szs$StanzaNum==ss+1,"First"] - 1
  }
  ss <- ss+1
  unbal$stanzas$stindiv[StGroupNum==gg & szs$StanzaNum==ss,"Last"] <- 999
}

# TODO RPATH - why does order matter here?
unbal$stanzas$stindiv <- unbal$stanzas$stindiv[order(StGroupNum,StanzaNum)]
#Not sure why Ksp is stored on the indiv table not the main group table
#assuming the value for the leading stanzas is correct.

