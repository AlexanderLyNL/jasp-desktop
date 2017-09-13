#
# Copyright (C) 2013-2015 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

BainTTestBayesianPairedSamples <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {
    
    all.variables <- unique(unlist(options$pairs))
    all.variables <- all.variables[all.variables != ""]
    
    if (is.null(dataset))
    {
        if (perform == "run") {
            
            if (options$missingValues == "excludeListwise") {
                
                dataset <- .readDataSetToEnd(columns.as.numeric=all.variables, exclude.na.listwise=all.variables)
                
            } else {
                
                dataset <- .readDataSetToEnd(columns.as.numeric=all.variables)
            }
            
        } else {
            
            dataset <- .readDataSetHeader(columns.as.numeric=all.variables)
        }
    }
    
    results <- list()
    
    
    meta <- list()
    
    meta[[1]] <- list(name="ttest", type="table")
    meta[[2]] <- list(name="descriptives", type="table")
    meta[[3]] <- list(name="BFplots", type="collection", meta = "image")
    meta[[4]] <- list(name = "descriptivesPlots", type = "collection", meta = "image")
    
    results[[".meta"]] <- meta
    results[["title"]] <- "Bayesian Informative Hypothesis T-Test"
    
    ttest <- list()
    
    ttest[["title"]] <- "Bayesian Inormative Hypothesis Paired T-Test"
    
    ttest[["citation"]] <- list(
        "Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
        "Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225-237.")
    
    bf.type <- options$bayesFactorType
    
    if(options$hypothesis == "biggerSmaller"){
        
        if (bf.type == "LogBF10") {
            
            BFH1H0 <- TRUE
            
            bf.title <- "Log(BF bigger vs - )"
            
        } else if (bf.type == "BF01") {
            
            BFH1H0 <- FALSE
            
            bf.title <- "BF bigger vs - "
        }
        
    } else {
        
        if (bf.type == "LogBF10") {
            
            BFH1H0 <- TRUE
            
            bf.title <- "Log(BF equal vs - )"
            
        } else if (bf.type == "BF01") {
            
            BFH1H0 <- FALSE
            
            bf.title <- "BF equal vs - "
        }
        
    }
    
    footnotes <- .newFootnotes()
    
    if (options$hypothesis == "groupsNotEqual") {
        type <- 1
        note <- "For all tests, H\u2080: mu1 = mu2 is tested against H\u2081:mu1 \u2260 mu2"
        .addFootnote(footnotes, symbol="<em>Note.</em>", text=note)
    } else if (options$hypothesis == "groupOneGreater") {
        type <- 2
        note <- "For all tests, H\u2080: mu1 = mu2 is tested against H\u2081: mu1 > mu2"
        .addFootnote(footnotes, symbol="<em>Note.</em>", text=note)
    } else if (options$hypothesis == "groupTwoGreater") {
        type <- 3
        note <- "For all tests, H\u2080: mu1 = mu2 is tested against H\u2081: mu1 < mu2"
        .addFootnote(footnotes, symbol="<em>Note.</em>", text=note)
    } else if (options$hypothesis == "biggerSmaller"){
        type <- 4
        note <- paste("For all tests, H\u2080: mu1 > mu2 is tested against H\u2081: mu1 < mu2", sep = "")
        .addFootnote(footnotes, symbol="<em>Note.</em>", text=note)
    } else if (options$hypothesis == "allTypes"){
        type <- 5
        note <- "For all tests, H\u2081: mu1 > mu2 and H\u2082: mu1 < mu2 are compared to H\u2080: mu1 = mu2"
        .addFootnote(footnotes, symbol="<em>Note.</em>", text=note)
    }
    
    if (options$hypothesis == "groupsNotEqual" | 
        options$hypothesis == "groupOneGreater" | 
        options$hypothesis == 'groupTwoGreater' |
        options$hypothesis == "biggerSmaller") {
        
        
        fields <- list(
            list(name="Variable", type="string", title=""),
            list(name = "hypothesis[type1]", type = "string", title = "Hypothesis"),
            list(name="BF[type1]", type="number", format="dp:4", title=bf.title),
            list(name="pmp[type1]", type="number", format="dp:4", title="Posterior probability"),
            list(name = "hypothesis[type2]", type = "string", title = "Hypothesis"),
            list(name="BF[type2]", type="string", format="dp:4", title=bf.title),
            list(name="pmp[type2]", type="number", format="dp:4", title="Posterior probability"))
        
        
    }
    
    if(options$hypothesis == "allTypes"){
        
        type <- 5
        
        fields <- list(
            list(name="Variable", type="string", title=""),
            list(name = "type[greater]", type = "string", title = "Hypothesis"),
            list(name="BF[greater]", type="number", format="dp:0", title=bf.title),
            list(name="pmp[greater]", type="number", format="dp:4", title="Posterior probability"),
            list(name = "type[less]", type = "string", title = "Hypothesis"),
            list(name="BF[less]", type="number", format="dp:4", title="bf.title"),
            list(name="pmp[less]", type="number", format="dp:4", title="Posterior probability"),
            list(name = "type[equal]", type = "string", title = "Hypothesis"),
            list(name = "BF[equal]", type = "number", format="dp:4", title = bf.title),
            list(name="pmp[equal]", type="number", format="dp:4", title="Posterior probability"))
        
    }
    
    ttest[["schema"]] <- list(fields=fields)
    
    ttest.rows <- list()
    plotGroups <- list()
    plots.ttest <- list()
    plotTypes <- list()
    plotPairs <- list()
    descriptivesPlots <- list()
    descriptPlotPairs <- list()
    tablePairs <- list()
    errorFootnotes <- rep("no", length(options$pairs))
    
    state <- .retrieveState()
    
    diff <- NULL
    
    if (!is.null(state)) {
        
        diff <- .diff(options, state$options)
        
    }
    
    if (options$descriptives || options$descriptivesPlots)
        results[["descriptives"]] <- list(title="Descriptives")
    
    footnotes2 <- .newFootnotes()
    
    i <- 1
    
    if(length(options$pairs) == 0){
        currentPair <- ""
    }
    
    for (pair in options$pairs)
    {
        
        currentPair <- paste(pair, collapse=" - ")
        
        if (options$descriptivesPlots) {
            
            if (!is.null(state) && currentPair %in% state$descriptPlotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$missingValues == FALSE && diff$plotWidth == FALSE &&
                                                                                                                                                            diff$plotHeight == FALSE && diff$descriptivesPlotsCredibleInterval == FALSE))) && options$descriptivesPlots) {
                
                # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
                # then, if the requested plot already exists, use it
                
                index <- which(state$descriptPlotPairs == currentPair)
                
                descriptivesPlots[[length(descriptivesPlots)+1]] <- state$descriptivesPlots[[index]]
                
                
            } else if (!is.null(state) && currentPair %in% state$descriptPlotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$missingValues == FALSE && diff$plotWidth == FALSE &&
                                                                                                                                                                   diff$plotHeight == FALSE && diff$descriptivesPlotsCredibleInterval == FALSE))) && options$descriptivesPlots) {
                
                # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
                # then, if the requested plot already exists, use it
                
                index <- which(state$descriptPlotPairs == currentPair)
                
                descriptivesPlots[[length(descriptivesPlots)+1]] <- state$descriptivesPlots[[index]]
                
                
            } else {
                
                descriptivesPlot <- list()
                
                descriptivesPlot[["title"]] <- currentPair
                descriptivesPlot[["status"]] <- "waiting"
                descriptivesPlot[["data"]] <- ""
                
                descriptivesPlots[[length(descriptivesPlots)+1]] <- descriptivesPlot
                
            }
            
            descriptPlotPairs[[length(descriptPlotPairs)+1]] <- currentPair
        }
        
        if (options$plotPriorAndPosterior) {
            
            
            if (!is.null(state) && currentPair %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$hypothesis == FALSE
                                                                                                                                                    && diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE)))) {
                
                # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
                # then, if the requested plot already exists, use it
                
                stateIndex <- which(state$plotPairs == currentPair)
                
                plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[stateIndex]]
                
            } else if (!is.null(state) && currentPair %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$hypothesis == FALSE
                                                                                                                                                           && diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE)))) {
                
                # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
                # if the requested plot already exists use it
                
                stateIndex <- which(state$plotPairs == currentPair)
                
                plots.ttest[[length(plots.ttest)+1]] <- state$plotsTtest[[stateIndex]]
                
            } else {
                
                plot <- list()
                
                plot[["title"]] <- currentPair
                plot[["status"]] <- "waiting"
                
                plots.ttest[[length(plots.ttest)+1]] <- plot
            }
            
            plots.ttest[[i]] <- plots.ttest[[length(plots.ttest)]]
            
            plotPairs[[length(plotPairs)+1]] <- paste(pair, collapse=" - ")
            
        }
        
        i <- i + 1
        
    }
    
    if (options$descriptivesPlots)
        results[["descriptivesPlots"]] <- list(title=ifelse(length(options$pairs) > 1, "Descriptives Plots", "Descriptives Plot"), collection=descriptivesPlots)
    if (options$plotPriorAndPosterior)
        results[["BFplots"]] <- list(title=ifelse(length(options$pairs) > 1, "Bayes factor Plots", "Bayes factor Plot"), collection=plots.ttest)
    
    BF10post <- numeric(length(options$pairs))
    
    for (i in .indices(options$pairs))
    {
        pair <- options$pairs[[i]]
        
        currentPair <- paste(pair, collapse=" - ")
        
        tablePairs[[length(tablePairs)+1]] <- paste(pair, collapse=" - ")
        
        if (pair[[1]] == "" || pair[[2]] == "") {
            
            p1 <- ifelse(pair[[1]] != "", pair[[1]], "...")
            p2 <- ifelse(pair[[2]] != "", pair[[2]], "...")
            
            if(options$hypothesis == "groupsNotEqual"){
                result_test <- list(Variable=currentPair, "hypothesis[type1]" = "Equal","BF[type1]"=".", "pmp[type1]" = ".",
                                    "hypothesis[type2]" = "Not equal", "BF[type2]" = ".", "pmp[type2]" = ".")
            } 
            if(options$hypothesis == "groupTwoGreater"){
                result_test <-list(Variable=currentPair, "hypothesis[type1]" = "Equal","BF[type1]"= ".", "pmp[type1]" = ".",
                                   "hypothesis[type2]" = "Smaller", "BF[type2]" = ".", "pmp[type2]" = ".")
            }
            if(options$hypothesis == "groupOneGreater"){
                result_test <-list(Variable=currentPair, "hypothesis[type1]" = "Equal", "BF[type1]"= ".", "pmp[type1]" = ".",
                                   "hypothesis[type2]" = "Bigger", "BF[type2]" = ".", "pmp[type2]" = ".")
            }
            if(options$hypothesis == "biggerSmaller"){
                result_test <-list(Variable=currentPair, "hypothesis[type1]" = "Bigger", "BF[type1]"= ".", "pmp[type1]" = ".",
                                   "hypothesis[type2]" = "Smaller", "BF[type2]" = ".", "pmp[type2]" = ".")
            }
            if(options$hypothesis == "allTypes"){
                result_test <-list(Variable=currentPair, 
                                   "type[greater]" = "Equal",
                                   "BF[greater]"= ".", 
                                   "pmp[greater]" = ".",
                                   "type[less]"= "Smaller",
                                   "BF[less]" = ".", 
                                   "pmp[less]" = ".",
                                   "type[equal]" = "Bigger",
                                   "BF[equal]" = ".",
                                   "pmp[equal]" = ".") 
            }
            
        } else {
            
            # init perform
            if (perform == "init") {
                
                if (!is.null(state) && tablePairs[[i]] %in% state$tablePairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$hypothesis == FALSE
                                                                                                                                                             && diff$bayesFactorType == FALSE && diff$missingValues == FALSE)))) {
                    
                    stateIndex <- which(state$tablePairs == paste(pair, collapse=" - "))[1]
                    
                    if (state$errorFootnotes[stateIndex] == "no") {
                        
                        result_test <- state$results$ttest$data[[stateIndex]]
                        
                    } else {
                        
                        index2 <- .addFootnote(footnotes2, state$errorFootnotes[stateIndex])
                        
                        errorFootnotes[i] <- state$errorFootnotes[stateIndex]
                        
                        if(options$hypothesis == "groupsNotEqual"){
                            result_test <- list(Variable=currentPair, "hypothesis[type1]" = "Equal","BF[type1]"=.clean(NaN), "pmp[type1]" = .clean(NaN),
                                                "hypothesis[type2]" = "Not equal", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN))
                        } 
                        if(options$hypothesis == "groupTwoGreater"){
                            result_test <-list(Variable=currentPair, "hypothesis[type1]" = "Equal","BF[type1]"= .clean(NaN), "pmp[type1]" = .clean(NaN),
                                               "hypothesis[type2]" = "Smaller", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN))
                        }
                        if(options$hypothesis == "groupOneGreater"){
                            result_test <-list(Variable=currentPair, "hypothesis[type1]" = "Equal", "BF[type1]"= .clean(NaN), "pmp[type1]" = .clean(NaN),
                                               "hypothesis[type2]" = "Bigger", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN))
                        }
                        if(options$hypothesis == "biggerSmaller"){
                            result_test <-list(Variable=currentPair, "hypothesis[type1]" = "Bigger", "BF[type1]"= .clean(NaN), "pmp[type1]" = .clean(NaN),
                                               "hypothesis[type2]" = "Smaller", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN))
                        }
                        if(options$hypothesis == "allTypes"){
                            result_test <-list(Variable=currentPair, 
                                               "type[greater]" = "Equal",
                                               "BF[greater]"= .clean(NaN), 
                                               "pmp[greater]" = .clean(NaN),
                                               "type[less]"= "Smaller",
                                               "BF[less]" = .clean(NaN), 
                                               "pmp[less]" = .clean(NaN),
                                               "type[equal]" = "Bigger",
                                               "BF[equal]" = .clean(NaN),
                                               "pmp[equal]" = .clean(NaN)) 
                        }
                    }
                    
                } else {
                    
                    if(options$hypothesis == "groupsNotEqual"){
                        result_test <- list(Variable=currentPair, "hypothesis[type1]" = "Equal","BF[type1]"=".", "pmp[type1]" = ".",
                                            "hypothesis[type2]" = "Not equal", "BF[type2]" = ".", "pmp[type2]" = ".")
                    } 
                    if(options$hypothesis == "groupTwoGreater"){
                        result_test <-list(Variable=currentPair, "hypothesis[type1]" = "Equal","BF[type1]"= ".", "pmp[type1]" = ".",
                                           "hypothesis[type2]" = "Smaller", "BF[type2]" = ".", "pmp[type2]" = ".")
                    }
                    if(options$hypothesis == "groupOneGreater"){
                        result_test <-list(Variable=currentPair, "hypothesis[type1]" = "Equal", "BF[type1]"= ".", "pmp[type1]" = ".",
                                           "hypothesis[type2]" = "Bigger", "BF[type2]" = ".", "pmp[type2]" = ".")
                    }
                    if(options$hypothesis == "biggerSmaller"){
                        result_test <-list(Variable=currentPair, "hypothesis[type1]" = "Bigger", "BF[type1]"= ".", "pmp[type1]" = ".",
                                           "hypothesis[type2]" = "Smaller", "BF[type2]" = ".", "pmp[type2]" = ".")
                    }
                    if(options$hypothesis == "allTypes"){
                        result_test <-list(Variable=currentPair, 
                                           "type[greater]" = "Equal",
                                           "BF[greater]"= ".", 
                                           "pmp[greater]" = ".",
                                           "type[less]"= "Smaller",
                                           "BF[less]" = ".", 
                                           "pmp[less]" = ".",
                                           "type[equal]" = "Bigger",
                                           "BF[equal]" = ".",
                                           "pmp[equal]" = ".") 
                    }
                }
                
            } else {
                
                unplotable <- FALSE
                unplotableMessage <- NULL
                
                if (!is.null(state) && tablePairs[[i]] %in% state$tablePairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$hypothesis == FALSE
                                                                                                                                                             && diff$bayesFactorType == FALSE && diff$missingValues == FALSE)))) {
                    
                    stateIndex <- which(state$tablePairs == paste(pair, collapse=" - "))[1]
                    
                    if (state$errorFootnotes[stateIndex] == "no") {
                        
                        result_test <- state$results$ttest$data[[stateIndex]]
                        
                    } else {
                        
                        index2 <- .addFootnote(footnotes2, state$errorFootnotes[stateIndex])
                        
                        errorFootnotes[i] <- state$errorFootnotes[stateIndex]
                        
                        if(options$hypothesis == "groupsNotEqual"){
                            result_test <- list(Variable=currentPair, "hypothesis[type1]" = "Equal","BF[type1]"=".", "pmp[type1]" = ".",
                                                "hypothesis[type2]" = "Not equal", "BF[type2]" = ".", "pmp[type2]" = ".", .footnotes=list(BF=list(index2)))
                        } 
                        if(options$hypothesis == "groupTwoGreater"){
                            result_test <-list(Variable=currentPair, "hypothesis[type1]" = "Equal","BF[type1]"= ".", "pmp[type1]" = ".",
                                               "hypothesis[type2]" = "Smaller", "BF[type2]" = ".", "pmp[type2]" = ".", .footnotes=list(BF=list(index2)))
                        }
                        if(options$hypothesis == "groupOneGreater"){
                            result_test <-list(Variable=currentPair, "hypothesis[type1]" = "Equal", "BF[type1]"= ".", "pmp[type1]" = ".",
                                               "hypothesis[type2]" = "Bigger", "BF[type2]" = ".", "pmp[type2]" = ".", .footnotes=list(BF=list(index2)))
                        }
                        if(options$hypothesis == "biggerSmaller"){
                            result_test <-list(Variable=currentPair, "hypothesis[type1]" = "Bigger", "BF[type1]"= ".", "pmp[type1]" = ".",
                                               "hypothesis[type2]" = "Smaller", "BF[type2]" = ".", "pmp[type2]" = ".",.footnotes=list(BF=list(index2)))
                        }
                        if(options$hypothesis == "allTypes"){
                            result_test <-list(Variable=currentPair, 
                                               "type[greater]" = "Equal",
                                               "BF[greater]"= ".", 
                                               "pmp[greater]" = ".",
                                               "type[less]"= "Smaller",
                                               "BF[less]" = ".", 
                                               "pmp[less]" = ".",
                                               "type[equal]" = "Bigger",
                                               "BF[equal]" = ".",
                                               "pmp[equal]" = ".", .footnotes=list(BF=list(index2))) 
                        }
                    }
                    
                    BF10post[i] <- state$BF10post[stateIndex]
                    
                    # if pair is not in state 
                } else {
                    
                    # if there is a pair
                    if(length(pair) > 0){
                        
                        subDataSet <- subset(dataset, select=c(.v(pair[[1]]), .v(pair[[2]])) )
                        subDataSet <- na.omit(subDataSet)
                        
                        c1 <- subDataSet[[ .v(pair[[1]]) ]]
                        c2 <- subDataSet[[ .v(pair[[2]]) ]]
                        
                        r <- Bain::Bain_ttestData(c1, c2, paired = TRUE,type = type)
                        
                        informationFootnote <- FALSE
                        
                        if(any(is.na(r))){
                            informationFootnote <- TRUE
                            note <- "Bayes factor could not be calculated, there are no samples drawn from the constrained area under the curve."
                            index <- .addFootnote(footnotes, note, symbol = "<em>NaN: </em>")
                            row.footnotes <- list(t = list(index))
                        }
                        
                        if(options[["bayesFactorType"]] == "LogBF10"){
                            makeLog <- TRUE
                        } else {
                            makeLog <- FALSE
                        }
                        
                        if(type == 1){
                            
                            if(makeLog){
                                BF_0u <- log(r$BF_0u)
                            } else {
                                BF_0u <- r$BF_0u
                            }
                            
                            PMP_u <- r$PMP_u
                            PMP_0 <- r$PMP_0
                        } else if(type == 2){
                            
                            if(makeLog){
                                BF_01 <- log(r$BF_01)
                            } else{
                                BF_01 <- r$BF_01
                            }
                            
                            PMP_1 <- r$PMP_1
                            PMP_0 <- r$PMP_0
                        } else if(type == 3){
                            
                            if(makeLog){
                                BF_01 <- log(r$BF_01)
                            } else{
                                BF_01 <- r$BF_01
                            }
                            
                            PMP_0 <- r$PMP_0
                            PMP_1 <- r$PMP_1
                        } else if (type == 4){
                            
                            if(makeLog){
                                BF_12 <- log(r$BF_12)
                            } else {
                                BF_12 <- r$BF_12
                            }
                            
                            PMP_1 <- r$PMP_1
                            PMP_2 <- r$PMP_2
                            
                        } else if(type == 5) {
                            
                            if(makeLog){
                                BF_01 <- log(r$BF_01)
                                BF_02 <- log(r$BF_02)
                                BF_12 <- log(r$BF_12)
                                c <- log(1)
                            } else {
                                BF_01 <- r$BF_01
                                BF_02 <- r$BF_02
                                BF_12 <- r$BF_12
                                c <- 1
                            }
                            
                            PMP_0 <- r$PMP_0
                            PMP_1 <- r$PMP_1
                            PMP_2 <- r$PMP_2
                        }
                        
                        if(options$hypothesis == "groupsNotEqual"){
                            result_test <- list(Variable=currentPair, "hypothesis[type1]" = "Equal","BF[type1]"=BF_0u, "pmp[type1]" = PMP_0,
                                                "hypothesis[type2]" = "Not equal", "BF[type2]" = "", "pmp[type2]" = PMP_u)
                        } 
                        if(options$hypothesis == "groupTwoGreater"){
                            result_test <-list(Variable=currentPair, "hypothesis[type1]" = "Equal","BF[type1]"= BF_01, "pmp[type1]" = PMP_0,
                                               "hypothesis[type2]" = "Smaller", "BF[type2]" = "", "pmp[type2]" = PMP_1)
                        }
                        if(options$hypothesis == "groupOneGreater"){
                            result_test <-list(Variable=currentPair, "hypothesis[type1]" = "Equal", "BF[type1]"= BF_01, "pmp[type1]" = PMP_0,
                                               "hypothesis[type2]" = "Bigger", "BF[type2]" = "", "pmp[type2]" = PMP_1)
                        }
                        if(options$hypothesis == "biggerSmaller"){
                            result_test <-list(Variable=currentPair, "hypothesis[type1]" = "Bigger", "BF[type1]"=.clean(BF_12), "pmp[type1]" = .clean(PMP_1),
                                               "hypothesis[type2]" = "Smaller", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_2)) 
                        }
                        if(options$hypothesis == "allTypes"){
                            result_test <-list(Variable=currentPair, 
                                               "type[greater]" = "Equal",
                                               "BF[greater]"= .clean(c), 
                                               "pmp[greater]" = .clean(PMP_0),
                                               "type[less]"= "Smaller",
                                               "BF[less]" = .clean(BF_02), 
                                               "pmp[less]" = .clean(PMP_1),
                                               "type[equal]" = "Bigger",
                                               "BF[equal]" = .clean(BF_01),
                                               "pmp[equal]" = .clean(PMP_2)) 
                        }
                        
                        if(informationFootnote){
                            result_test[[".footnotes"]] <- row.footnotes
                        }
                        
                    }
                }
            }
        }
        
        ttest.rows[[length(ttest.rows)+1]] <- result_test
        
    }
    
    # if there is no pair
    if (length(ttest.rows) == 0){
        if(options$hypothesis == "groupsNotEqual"){
            ttest.rows <- list(list(Variable=currentPair, "hypothesis[type1]" = "Equal","BF[type1]"=".", "pmp[type1]" = ".",
                                    "hypothesis[type2]" = "Not equal", "BF[type2]" = ".", "pmp[type2]" = "."))
        } 
        if(options$hypothesis == "groupTwoGreater"){
            ttest.rows <-list(list(Variable=currentPair, "hypothesis[type1]" = "Equal","BF[type1]"= ".", "pmp[type1]" = ".",
                                   "hypothesis[type2]" = "Smaller", "BF[type2]" = ".", "pmp[type2]" = "."))
        }
        if(options$hypothesis == "groupOneGreater"){
            ttest.rows <-list(list(Variable=currentPair, "hypothesis[type1]" = "Equal", "BF[type1]"= ".", "pmp[type1]" = ".",
                                   "hypothesis[type2]" = "Bigger", "BF[type2]" = ".", "pmp[type2]" = "."))
        }
        if(options$hypothesis == "biggerSmaller"){
            result_test <-list(Variable=currentPair, "hypothesis[type1]" = "Bigger", "BF[type1]"= ".", "pmp[type1]" = ".",
                               "hypothesis[type2]" = "Smaller", "BF[type2]" = ".", "pmp[type2]" = ".")
        }
        if(options$hypothesis == "allTypes"){
            ttest.rows <-list(list(Variable=currentPair, 
                                   "type[greater]" = "Equal",
                                   "BF[greater]"= ".", 
                                   "pmp[greater]" = ".",
                                   "type[less]"= "Smaller",
                                   "BF[less]" = ".", 
                                   "pmp[less]" = ".",
                                   "type[equal]" = "Bigger",
                                   "BF[equal]" = ".",
                                   "pmp[equal]" = ".")) 
        }
    }
    
    ttest[["data"]] <- ttest.rows
    ttest[["footnotes"]] <- as.list(footnotes)
    
    
    if (perform == "run")
        ttest[["status"]] <- "complete"
    
    results[["ttest"]] <- ttest
    
    descriptives <- NULL
    
    if (options$descriptives) {
        
        descriptives <- list()
        
        descriptives[["title"]] <- "Descriptives"
        
        fields <- list(
            list(name="v", type="string", title=""),
            list(name="N",                  type="integer"),
            list(name="mean", title="Mean", type="number", format="sf:4;dp:3"),
            list(name="sd",   title="SD",   type="number", format="sf:4;dp:3"),
            list(name="se",   title="SE",   type="number", format="sf:4;dp:3"))
        
        ## add credible interval values if asked for in plot
        if (options$descriptivesPlots) {
            interval <- 100 * options$descriptivesPlotsCredibleInterval
            title <- paste0(interval, "% Credible Interval")
            fields[[length(fields) + 1]] <- list(name = "lowerCI", type = "number",
                                                 format = "sf:4;dp:3", title = "Lower",
                                                 overTitle = title)
            fields[[length(fields) + 1]] <- list(name = "upperCI", type = "number",
                                                 format = "sf:4;dp:3", title = "Upper",
                                                 overTitle = title)
        }
        
        descriptives[["schema"]] <- list(fields=fields)
        
        descriptives.results <- list()
        
        variables <- unlist(options$pairs)
        variables <- unique(variables)
        variables <- variables[variables != ""]
        
        for (variable in variables) {
            
            if (perform == "run") {
                
                result <- try (silent = TRUE, expr = {
                    
                    variableData <- dataset[[.v(variable)]]
                    variableDataOm <- na.omit(variableData)
                    
                    posteriorSummary <- .posteriorSummaryGroupMean(variable=variableDataOm, descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
                    ciLower <- .clean(posteriorSummary$ciLower)
                    ciUpper <- .clean(posteriorSummary$ciUpper)
                    
                    n <- .clean(as.numeric(length(variableDataOm)))
                    m <- .clean(as.numeric(mean(variableDataOm)))
                    std <- .clean(as.numeric(sd(variableDataOm)))
                    if(is.numeric(std)){
                        se <- .clean(as.numeric(std/sqrt(n)))}
                    else
                        se <- .clean(NaN)
                    
                    list(v=variable, N=n, mean=m, sd=std, se=se, lowerCI=ciLower, upperCI=ciUpper)
                })
                
                if (class(result) == "try-error") {
                    
                    result <- list(v=variable, N="", mean="", sd="", se="", lowerCI="", upperCI="")
                }
                
            } else {
                
                result <- list(v=variable, N=".", mean=".", sd=".", se=".", lowerCI=".", upperCI=".")
            }
            
            descriptives.results[[length(descriptives.results)+1]] <- result
        }
        
        descriptives[["data"]] <- descriptives.results
        descriptives[["status"]] <- "complete"
        
        
    }
    
    results[["descriptives"]] <- descriptives
    
    
    # PLOTS
    
    if (perform == "run" && length(options$pairs) > 0 && (options$plotPriorAndPosterior || options$descriptivesPlots)) {
        
        if ( ! .shouldContinue(callback(results)))
            return()
        
        j <- 1
        descriptInd <- 1
        
        for (i in .indices(options$pairs)) {
            
            pair <- options$pairs[[i]]
            
            status <- list(unplotable = FALSE)
            
            p1 <- ifelse(pair[[1]] != "", pair[[1]], "...")
            p2 <- ifelse(pair[[2]] != "", pair[[2]], "...")
            
            if (perform == "run" && status$unplotable == FALSE && p2 != "..." && p1 != "...") {
                
                subDataSet <- subset(dataset, select=c(.v(pair[[1]]), .v(pair[[2]])) )
                subDataSet <- na.omit(subDataSet)
                
                c1 <- subDataSet[[ .v(pair[[1]]) ]]
                c2 <- subDataSet[[ .v(pair[[2]]) ]]
                
                ind <- which(c1 == c1[1])
                idData <- sum((ind+1)-(1:(length(ind))) == 1)
                
                ind2 <- which(c2 == c2[1])
                idData2 <- sum((ind2+1)-(1:(length(ind2))) == 1)
            }
            else
            {
                c1 <- NULL
                c2 <- NULL
            }
            
            if (options$descriptivesPlots) {
                
                if (!is.null(state) && tablePairs[[i]] %in% state$descriptPlotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$missingValues == FALSE &&
                                                                                                                                                                    diff$plotWidth == FALSE && diff$plotHeight == FALSE && diff$descriptivesPlotsCredibleInterval == FALSE))) && options$descriptivesPlots) {
                    # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
                    # then, if the requested plot already exists, use it
                    
                    index <- which(state$descriptPlotPairs == tablePairs[[i]])
                    
                    descriptivesPlots[[descriptInd]] <- state$descriptivesPlots[[index]]
                    
                    
                } else {
                    
                    results[["descriptivesPlots"]][["collection"]][[descriptInd]][["status"]] <- "running"
                    
                    if ( ! .shouldContinue(callback(results)))
                        return()
                    
                    plot <- descriptivesPlots[[descriptInd]]
                    
                    p <- try(silent= FALSE, expr= {
                        
                        figure <- .plot2GroupMeansBayesPairedTtest(v1 = c1, v2 = c2, nameV1 = pair[[1]], nameV2 = pair[[2]], descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
                        
                        content <- .writeImage(width = options$plotWidth, height = options$plotHeight, plot = figure, obj = TRUE)
                        plot[["convertible"]] <- TRUE
                        plot[["obj"]] <- content[["obj"]]
                        plot[["data"]] <- content[["png"]]
                        
                    })
                    
                    if (class(p) == "try-error") {

                        errorMessageTmp <- .extractErrorMessage(p)
                        errorMessage <- paste0("Plotting not possible: ", errorMessageTmp)
                        plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
                    }
                    
                    plot[["status"]] <- "complete"
                    descriptivesPlots[[descriptInd]] <- plot
                    
                }
                
                results[["descriptivesPlots"]][["collection"]] <- descriptivesPlots
                
                if ( ! .shouldContinue(callback(results)))
                    return()
                
                descriptInd <- descriptInd + 1
                
            }
            
            if (options$plotPriorAndPosterior) {
                
                
                if (!is.null(state) && tablePairs[[i]] %in% state$plotPairs && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$hypothesis == FALSE
                                                                                                                                                            && diff$bayesFactorType == FALSE && diff$missingValues == FALSE && diff$plotWidth == FALSE && diff$plotHeight == FALSE)))) {
                    
                    # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
                    # if the requested plot already exists use it
                    
                    stateIndex <- which(state$plotPairs == tablePairs[[i]])
                    
                    plots.ttest[[j]] <- state$plotsTtest[[stateIndex]]
                    
                } else {
                    
                    #results[["BFplots"]][["collection"]][[i]][["status"]] <- "running"
                    
                    if ( ! .shouldContinue(callback(results)))
                        return()
                    
                    plot <- plots.ttest[[j]]
                    
                    
                    if (status$unplotable == FALSE) {
                        
                        
                        p <- try(silent= FALSE, expr= {
                            
                            .plotFunc2 <- function() {
                                Bain::plot.BainT(Bain::Bain_ttestData(x = c1, y = c2,type = type, paired = TRUE))
                            }
                            content2 <- .writeImage(width = options$plotWidth, height = options$plotHeight, plot = .plotFunc2, obj = TRUE)
                            plot[["convertible"]] <- TRUE
                            plot[["obj"]] <- content2[["obj"]]
                            plot[["data"]] <- content2[["png"]]
                            
                        })
                        
                        
                        if (class(p) == "try-error") {
                            
                            errorMessage <- .extractErrorMessage(p)
                            
                            if (errorMessage == "not enough data") {
                                
                                errorMessage <- "Plotting is not possible: The Bayes factor is too small"
                            } else if (errorMessage == "'from' cannot be NA, NaN or infinite") {
                                
                                errorMessage <- "Plotting is not possible: The Bayes factor is too small"
                            }
                            
                            plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
                        }
                        
                    } else if (status$unplotable && "unplotableMessage" %in% names(status)) {
                        
                        message <- paste("Plotting is not possible:", status$unplotableMessage)
                        plot[["error"]] <- list(error="badData", errorMessage=message)
                    }
                    
                    plot[["status"]] <- "complete"
                    
                    plots.ttest[[j]] <- plot
                }
                
                results[["BFplots"]][["collection"]] <- plots.ttest
                
                if ( ! .shouldContinue(callback(results)))
                    return()
                
                j <- j + 1
            }
            
        }
    }
    
    results[["ttest"]] <- ttest
    
    keep <- NULL
    
    for (plot in plots.ttest)
        keep <- c(keep, plot$data)
    
    for (plot in descriptivesPlots)
        keep <- c(keep, plot$data)
    
    if (perform == "init") {
        
        return(list(results=results, status="inited", state=state, keep=keep))
        
    } else {
        
        return(list(results=results, status="complete", state=list(options=options, results=results, plotsTtest=plots.ttest, plotTypes=plotTypes, plotPairs=plotPairs,
                                                                   descriptPlotPairs=descriptPlotPairs, descriptivesPlots=descriptivesPlots, BF10post=BF10post, tablePairs=tablePairs,
                                                                   errorFootnotes=errorFootnotes), keep=keep))
    }
    
}