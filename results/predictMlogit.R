#!/usr/bin/env Rscript
library(nnet)
#setwd('~/Predictive-model-for-the-voting-system-in-SE-network/results')

args <- commandArgs(trailingOnly = TRUE)
if (length(args)==0){
  stop("At least one argument must be supplied.n",call.=FALSE)
} else if (length(args)!=3){
  stop("Arguments: train_set test_set predict_out_filename",call.=FALSE)
}
qtpostrain <- read.csv(args[1])
qtpostest <- read.csv(args[2])

modvanilla <- multinom(AnsRank~ReScore+Norm_Pos,qtpostrain[,c("AnsRank","ReScore","Norm_Pos")])
moddrank <- multinom(AnsRank~ReScore+Norm_Pos+Norm_DRank,qtpostrain[,c("AnsRank","ReScore","Norm_Pos","Norm_DRank")])

predictMNL <- function(model, newdata) {
  # Only works for neural network models
  if (is.element("nnet",class(model))) {
    # Calculate the individual and cumulative probabilities
    probs <- predict(model,newdata,"probs")
    if (is.vector(probs)) {
      probs <- cbind(1-probs,probs)
    }
    cum.probs <- t(apply(probs,1,cumsum))
    # Draw random values
    vals <- runif(nrow(newdata))
    # Join cumulative probabilities and random draws
    tmp <- cbind(cum.probs,vals)
    # For each row, get choice index.
    k <- ncol(probs)
    ids <- 1 + apply(tmp,1,function(x) length(which(x[1:k] < x[k+1])))
    # Return the values
    return(ids)
  }
}

nb_choices <- (qtpostrain$Ans_count)[1]

yvanilla <- predictMNL(modvanilla,qtpostest[,c("ReScore","Norm_Pos")])
ydrank <- predictMNL(moddrank,qtpostest[,c("ReScore","Norm_Pos","Norm_DRank")])
yrand <- sample(1:nb_choices,length(yvanilla),replace=TRUE)
ytest <- qtpostest$AnsRank

outname <- args[3]
sink(outname)
cat(c('','RSS_vanilla','RSS_drank','RSS_random'))
cat('\n')
cat(c(nb_choices,sqrt(sum((yvanilla-ytest)^2)),sqrt(sum((ydrank-ytest)^2)),sqrt(sum((yrand-ytest)^2))))

sink()
