
hello <- function() {
  print("Hello, world!")
}


streamline <- function(counts,glengths){
    raw <- DGEList(counts=counts, genes=rownames(counts))
    raw <- calcNormFactors(raw)
    raw <- as.matrix(raw)

    #Transform to log RPKM/FPKM
    FPKM <- rpkm(raw,log=TRUE,gene.length = glengths)

    #Now read in the list of genes needed by the Classifier
    load("data/topGeneNames.Rda")
    FPKM[topgenes,]
    return(FPKM)
}

classify <- function(FPKM,thresh=c(0.25,0.5,0.5,0.75)){
    load("data/RF_model.Rda") #Load in random forest

    RFpred <- predict(RF2,data.frame(t(FPKM)))
    RFprob <- predict(RF2,data.frame(t(FPKM)),type="prob")

    
    out = as.data.frame(RFprob)
    out$Classified = ifelse(out$ERG > thresh[1],"ERG",
                            ifelse(out$ETV>thresh[2],"ETV",
                                ifelse(out$Other > thresh[3],"Other",
                                     ifelse(out$Phlike > thresh[4],"Phlike","Unclassifed"))))

    classed = out
    return(classed)
}

#Quick function to visualise the classification on an MDS plot and Heat Map
visualise <- function(sfpkm,classed){
    library(RColorBrewer)
    pal = brewer.pal(5,"Set1")
    #Construct an MDS plot, coloured by Class
    pmds = plotMDS(sfpkm,gene.selection="common",col=pal[classed$class],pch=16)
    legend('topleft',legend=unique(levels(classed$class)),col=pal,pch=16)
    return(pmds)

}
