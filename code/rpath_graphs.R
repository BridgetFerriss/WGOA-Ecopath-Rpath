require(plotly)

effort_to_F<-function(bal,scene){
   
}

start_biomass <- function(rsim){
      return(rsim$out_Biomass[1, 2:(dim(rsim$out_Biomass)[2])])
}

end_biomass <- function(rsim){
      return(rsim$out_Biomass[dim(rsim$out_Biomass)[1], 2:(dim(rsim$out_Biomass)[2])])
}

start_catch <- function(rsim){
      return(rsim$out_Catch[1, 2:(dim(rsim$out_Catch)[2])])
}

end_catch <- function(rsim){
      return(rsim$out_Catch[dim(rsim$out_Catch)[1], 2:(dim(rsim$out_Catch)[2])])
}

living_groups   <- function(bal){return(bal$Group[1:bal$NUM_LIVING])}
detrital_groups <- function(bal){return(bal$Group[(bal$NUM_LIVING+1):(bal$NUM_LIVING+bal$NUM_DEAD)])}
gear_groups     <- function(bal){return(bal$Group[(bal$NUM_LIVING+bal$NUM_DEAD+1):(bal$NUM_GROUPS)])}

rsim.plotly<-function(rsim,species){
  ptxt<-NULL
  for (c in species){
    if (is.null(ptxt)){
           ptxt <- paste("p <- plot_ly(x=as.numeric(rownames(rsim$annual_Biomass)), y=rsim$annual_Biomass[,'",c,"'], name='",c,"', type = 'scatter', mode = 'lines') %>%", sep="") 
    }else {ptxt <- paste(ptxt," add_trace(y=rsim$annual_Biomass[,'",c,"'], name='",c,"', type = 'scatter', mode = 'lines') %>%",sep="")}
  }
  ptxt<-substr(ptxt,1,nchar(ptxt)-4)
  eval(parse(text = ptxt))
  return(p)       
}

