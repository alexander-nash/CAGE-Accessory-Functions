dropCageSetSamples<-function(cage_set, labels){
        require(rebus)
        index<-grep(paste(literal(labels), collapse="|"), cage_set@sampleLabels)
        out<-cage_set
        out@inputFiles<-out@inputFiles[-index]
        out@sampleLabels<-out@sampleLabels[-index]
        out@librarySizes<-out@librarySizes[-index]
        sampletags<-rowSums(out@tagCountMatrix[, index])
        alltags<-rowSums(out@tagCountMatrix)
        diff<-alltags-sampletags
        dropthese<-which(diff==0)
        out@CTSScoordinates<-out@CTSScoordinates[-dropthese,]
        out@tagCountMatrix<-out@tagCountMatrix[-dropthese,]
        out@normalizedTpmMatrix<-out@normalizedTpmMatrix[-dropthese,]
        out@filteredCTSSidx<-out@filteredCTSSidx[-dropthese]
        names(out@filteredCTSSidx)<-as.character(1:length(out@filteredCTSSidx))
        out@tagClusters[index]<-NULL
        out@CTSScumulativesTagClusters[index]<-NULL
        out@tagClustersQuantileLow[index]<-NULL
        out@tagClustersQuantileUp[index]<-NULL
        message("BE WARY THIS IS NOT PERFECT")
        message("You should rerun aggregateTagClusters and getExpressionProfiles")
        return(out)
}