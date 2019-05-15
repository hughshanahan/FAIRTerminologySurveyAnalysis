

library(ggplot2)
library(knitr)

# Switch off showing code...
knitr::opts_chunk$set(echo=FALSE)

# Function to to column of data where each entry can have 
# more than one piece of text separated by a comma
# and return a vector with all the terms
collectTerms <- function(entries){
  unlist(strsplit(entries,"[,]")) -> r
  
  # Strings may have a leading space which we'll remove
  roles <- sapply(r,function(s){
    l <- unlist(strsplit(s,"^ "))
    if ( length(l) == 1 ){
      return(l)
    }
    else{
      return(l[length(l)])
    }
  })
  return(as.vector(roles))
}


# Function to remove blank entries from a column
# and then print them out nicely
printComments <- function(entries){
  r <- sapply(entries,function(e){
    if ( e == ""){
      NULL
    }
    else { e }
 })
  kable(as.vector(unlist(r)))
}

# Convert columns of data on individual themes and recast it
# as a single vector
turnIntoOneColumn <- function(data,cols){
  colNames <- names(data)[cols]
  nRows <- dim(data)[1]
  labels <- rep(colNames,each=nRows)
  results <- c()
  for ( label in colNames ){
    results <- c(results,data[,label])
  }
  return(data.frame(theme=labels,score=results))
}

# Remove rows where the theme scores have zero variance
removeZeroVarRows <- function(data,cols){
  nRows <- dim(data)[1]
  keep <- c()
  for ( i in 1:nRows ){
    if (var(as.numeric(data[i,cols])) > 1e-6){
      keep <- c(keep,i)
    }
  }
  return(data[keep,])
}

survey <- read.csv("Questionnaire.csv",stringsAsFactors=FALSE)

ggplot(data=survey,aes(x=Attending)) + geom_bar() + scale_y_continuous(breaks=c(2,4,6,8,10))

ggplot(data=survey,aes(x=Organisation)) + geom_bar() + scale_y_continuous(breaks=c(2,4,6,8,10))


x <- data.frame(roles=collectTerms(survey$Role))
ggplot(data=x,aes(x=roles)) + geom_histogram(stat="count") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

printComments(survey$OtherRoles)

x <- data.frame(ResearchDomains=collectTerms(survey$ResearchDomain))
ggplot(data=x,aes(x=ResearchDomains)) + geom_histogram(stat="count") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

printComments(survey$OtherResearchDomain)

ggplot(data=survey,aes(x=CourseMaterialsSearch)) + geom_bar() + scale_y_continuous(breaks=c(4,8,12,16,20)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
printComments(survey$CourseMaterialsSearchReasonNo)

ggplot(data=survey,aes(x=FAIRJobAssessment)) + geom_bar() + scale_y_continuous(breaks=c(4,8,12,16,20)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data=survey,aes(x=ArticleQualityControl)) + geom_bar() + scale_y_continuous(breaks=c(4,8,12,16,20)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data=survey,aes(x=DataRepositoryUploaderSkillsCheck)) + geom_bar() + scale_y_continuous(breaks=c(4,8,12,16,20)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
printComments(survey$ReasonsNo)

ggplot(data=survey,aes(x=CurriculumDesign)) + geom_bar() + scale_y_continuous(breaks=c(4,8,12,16,20)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
printComments(survey$CurriculumDesignReasonsNo)

printComments(survey$OtherUseCases)

x <- data.frame(ResearchDomains=collectTerms(survey$UseCasesWouldEmploy))
ggplot(data=x,aes(x=ResearchDomains)) + geom_histogram(stat="count") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data=survey,aes(x=TerminologySelection)) + geom_histogram(stat="count") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
printComments(survey$ExplanationTerminologySection)
kable(survey[which(survey$TerminologySelection=="Controlled Vocabulary"),"ExplanationTerminologySection"])
kable(survey[which(survey$TerminologySelection=="Thesaurus"),"ExplanationTerminologySection"])
kable(survey[which(survey$TerminologySelection=="Ontology"),"ExplanationTerminologySection"])


scoreData <- turnIntoOneColumn(survey,c(21:29))
ggplot(data=scoreData,aes(x=theme,y=score)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(breaks=c(1:10))

highVarData <- removeZeroVarRows(survey,c(21:29))
ggplot(data=turnIntoOneColumn(highVarData,c(21:29)),aes(x=theme,y=score)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(breaks=c(1:10))

ggplot(data=turnIntoOneColumn(survey[which(survey$Attending=="Yes"),c(21:29)]),aes(x=theme,y=score)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(breaks=c(1:10))
printComments(survey$OtherTopLevelConcepts)
