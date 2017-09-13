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

BainTTestBayesianIndependentSamples <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {

	dependents <- unlist(options$variables)

	grouping   <- options$groupingVariable

	if (grouping == "")
		grouping <- NULL

	if (is.null(dataset))
	{
		if (perform == "run") {

			if (options$missingValues == "excludeListwise") {

				dataset <- .readDataSetToEnd(columns.as.numeric=dependents, columns.as.factor=grouping, exclude.na.listwise=c(dependents, grouping))

			} else {

				dataset <- .readDataSetToEnd(columns.as.numeric=dependents, columns.as.factor=grouping, exclude.na.listwise=grouping)
			}

		} else {

			dataset <- .readDataSetHeader(columns.as.numeric=dependents, columns.as.factor=grouping)
		}
	}

	.hasErrors(dataset=dataset, perform=perform, type="factorLevels",
	           factorLevels.target=grouping, factorLevels.amount = "!= 2",
	           exitAnalysisIfErrors = TRUE)

	results <- list()

	meta <- list()

	meta[[1]] <- list(name="ttest", type="table")
	meta[[2]] <- list(name="descriptives", type="table")
	meta[[3]] <- list(name="BFplots", type="collection", meta="image")
	meta[[4]] <- list(name = 'descriptivesPlots', type = "collection", meta = "image")

	results[[".meta"]] <- meta
	results[["title"]] <- "Bayesian Informative Hypothesis T-Test"


	state <- .retrieveState()

	diff <- NULL

	if (!is.null(state)) {

		diff <- .diff(options, state$options)

	}

	ttest.results <- .BainttestBayesianIndependentSamplesTTest(dataset, options, perform, state=state, diff=diff)

	results[["ttest"]] <- ttest.results[[1]]
	status <- ttest.results[[2]]
	g1 <- ttest.results[[3]]
	g2 <- ttest.results[[4]]
	BFH1H0 <- ttest.results[[5]]
	plottingError <- ttest.results[[6]]
	BF10post <- ttest.results[[7]]
	errorFootnotes <- ttest.results[[8]]
	plotres <- ttest.results[[9]]
	Bainresult <- ttest.results[[10]]
	plotVariables <- ttest.results[[11]]

	descriptivesTable <- .BainttestBayesianIndependentSamplesDescriptives(dataset, options, perform)
	results[["descriptives"]] <- descriptivesTable


	plotGroups <- list()
	#plots.ttest <- list()
	descriptPlotVariables <- list()
	descriptivesPlots <- list()
	plotTypes <- list()
	plotVariables <- list()
	BFplots <- list()
	BFplotvariables <- list()


	if (options$descriptivesPlots | options$plotPriorAndPosterior) {

		iint <- 1
		q <- 1
		descriptInd <- 1
		BFind <- 1

		for (variable in options[["variables"]]){

			plotGroups[[iint]] <- list()
			plotGroups[[iint]][["title"]] <- variable
			plotGroups[[iint]][["name"]] <- variable

			if (options$descriptivesPlots) {

				if (!is.null(state) && variable %in% state$descriptPlotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$groupingVariable == FALSE &&
					diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE && diff$descriptivesPlotsCredibleInterval == FALSE))) && options$descriptivesPlots) {


					# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
					# then, if the requested plot already exists, use it

					index <- which(state$descriptPlotVariables == variable)

					descriptivesPlots[[descriptInd]] <- state$descriptivesPlots[[index]]


				} else {

					descriptivesPlot <- list()

					descriptivesPlot[["title"]] <- variable
					descriptivesPlot[["width"]] <- options$plotWidth
					descriptivesPlot[["height"]] <- options$plotHeight
					descriptivesPlot[["custom"]] <- list(width="plotWidth", height="plotHeight")
					descriptivesPlot[["status"]] <- "waiting"
					descriptivesPlot[["data"]] <- ""

					descriptivesPlots[[descriptInd]] <- descriptivesPlot
				}


				descriptPlotVariables[[length(descriptPlotVariables)+1]] <- variable

				descriptInd <- descriptInd + 1
			}

			if (options$plotPriorAndPosterior){

			    if (!is.null(state) && variable %in% state$BFplotvariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$groupingVariable == FALSE && diff$hypothesis == FALSE &&
			                                                                                                                                                     diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE))) && options$plotPriorAndPosterior) {


			        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
			        # then, if the requested plot already exists, use it

			        index <- which(state$BFplotvariables == variable)

			        BFplots[[BFind]] <- state$BFplots[[index]]


			    } else {

			        BFplot <- list()

			        BFplot[["title"]] <- variable
			        BFplot[["width"]] <- options$plotWidth
			        BFplot[["height"]] <- options$plotHeight
			        BFplot[["custom"]] <- list(width="plotWidth", height="plotHeight")
			        BFplot[["status"]] <- "waiting"
			        BFplot[["data"]] <- ""

			        BFplots[[BFind]] <- BFplot
			    }


			    BFplotvariables[[length(BFplotvariables)+1]] <- variable

			    BFind <- BFind + 1

			}

			iint <- iint + 1

		}


		if (options$plotPriorAndPosterior)
			results[["BFplots"]] <- list(title=ifelse(length(options[["variables"]]) > 1,
				"Bayes Factors plots", "Bayes Factor plot"), collection=BFplots)

		if (options$descriptivesPlots)
			results[["descriptivesPlots"]] <- list(title=ifelse(length(options[["variables"]]) > 1, "Descriptives Plots", "Descriptives Plot"), collection=descriptivesPlots)


		if (perform == "run" && length(options$variables) > 0 && !is.null(grouping)) {

			if ( ! .shouldContinue(callback(results)))
				return()

			statusInd <- 1
			i <- 1
			z <- 1
			descriptInd <- 1
			BFind <- 1


			for (variable in options[["variables"]]) {


				subDataSet <- subset(dataset, select=c(.v(variable), .v(options$groupingVariable)))
				subDataSet <- na.omit(subDataSet)

				group2 <- subDataSet[subDataSet[[.v(options$groupingVariable)]]== g1,.v(variable)]
				group1 <- subDataSet[subDataSet[[.v(options$groupingVariable)]]== g2,.v(variable)]


				if (options$descriptivesPlots) {


					if (!is.null(state) && variable %in% state$descriptPlotVariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$groupingVariable == FALSE &&
						diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE && diff$descriptivesPlotsCredibleInterval == FALSE))) && options$descriptivesPlots) {

						# if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
						# then, if the requested plot already exists, use it

						index <- which(state$descriptPlotVariables == variable)

						descriptivesPlots[[descriptInd]] <- state$descriptivesPlots[[index]]


					} else {

						if ( ! .shouldContinue(callback(results))){
							return()
						}

						plot <- descriptivesPlots[[descriptInd]]

						if(Bainresult[[i]] == "error"){

						    plot[["data"]] <- ""
						    plot[["error"]] <- list(error="badData", errorMessage=errorFootnotes[[i]])

						} else {

						p <- try(silent= FALSE, expr= {

						    figure <- .plot2GroupMeansBayesIndTtest(v1 = group2, v2 = group1, nameV1 = g1, nameV2 = g2, groupingName = options$groupingVariable, dependentName = variable, descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)

						    content <- .writeImage(width = options$plotWidth, height = options$plotHeight, plot = figure, obj = TRUE)
						    plot[["convertible"]] <- TRUE
						    plot[["obj"]] <- content[["obj"]]
						    plot[["data"]] <- content[["png"]]

							})

						}

						plot[["status"]] <- "complete"

						descriptivesPlots[[descriptInd]] <- plot

					}

					results[["descriptivesPlots"]][["collection"]] <- descriptivesPlots

					descriptInd <- descriptInd + 1

					if ( ! .shouldContinue(callback(results)))
						return()

				}

				if (options$plotPriorAndPosterior) {

				    if (!is.null(state) && variable %in% state$BFplotvariables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$groupingVariable == FALSE && diff$hypothesis == FALSE &&
				                                                                                                                                                     diff$missingValues == FALSE && diff$plotHeight == FALSE && diff$plotWidth == FALSE))) && options$plotPriorAndPosterior) {

				        # if there is state and the variable has been plotted before and there is either no difference or only the variables or requested plot types have changed
				        # then, if the requested plot already exists, use it

				        index <- which(state$BFplotvariables == variable)

				        BFplots[[BFind]] <- state$BFplots[[index]]


				    } else {

				        if ( ! .shouldContinue(callback(results)))
				            return()

				        plot <- BFplots[[BFind]]

				        if(Bainresult[[i]] == "error"){

				            plot[["data"]] <- ""
				            plot[["error"]] <- list(error="badData", errorMessage=errorFootnotes[[i]])

				        } else {

				        p <- try(silent= FALSE, expr= {

				            .plotFuncBF <- function() {
				                Bain::plot.BainT(plotres[[BFind]])
				            }
				            content2 <- .writeImage(width = options$plotWidth, height = options$plotHeight, plot = .plotFuncBF, obj = TRUE)
				            plot[["convertible"]] <- TRUE
				            plot[["obj"]] <- content2[["obj"]]
				            plot[["data"]] <- content2[["png"]]

				        })

				        }

				        plot[["status"]] <- "complete"

				        BFplots[[BFind]] <- plot
				    }

				    results[["BFplots"]][["collection"]] <- BFplots

				    BFind <- BFind + 1

				    if ( ! .shouldContinue(callback(results)))
				        return()

				}

				statusInd <- statusInd + 1
				i <- i + 1
			}
		}
	}


	keep <- NULL

	for(plot in BFplots)
	    keep <- c(keep, plot$data)

	for (plot in descriptivesPlots)
	    keep <- c(keep, plot$data)

	if (perform == "init") {

		return(list(results=results, status="inited", state=state, keep=keep))

	} else {

		return(list(results=results, status="complete", state=list(options=options, results=results, plotTypes=plotTypes, plotVariables=plotVariables,
		descriptPlotVariables=descriptPlotVariables, descriptivesPlots=descriptivesPlots, status=status, plottingError=plottingError, BF10post=BF10post, errorFootnotes=errorFootnotes,
		BFplotvariables = BFplotvariables, BFplots = BFplots, plotres = plotres, Bainresult = Bainresult),
		keep=keep))
	}

}

.BainttestBayesianIndependentSamplesTTest <- function(dataset, options, perform, state, diff) {

    plotres <- list()
    plotVariables <- list()

	g1 <- NULL
	g2 <- NULL

	ttest <- list()

	ttest[["title"]] <- "Bayesian Informative Hypothesis Independent Samples T-Test"

	ttest[["citation"]] <- list(
		"Morey, R. D., & Rouder, J. N. (2015). BayesFactor (Version 0.9.11-3)[Computer software].",
		"Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225–237.")

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

	# Make the fields for the t-test table

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
	        list(name="BF[type2]", type="number", format="dp:4", title=bf.title),
	        list(name="pmp[type2]", type="number", format="dp:4", title="Posterior probability"))
	}

	if(options$hypothesis == "allTypes"){

	    fields <- list(
	        list(name="Variable", type="string", title=""),
	        list(name = "type[greater]", type = "string", title = "Hypothesis"),
	        list(name="BF[greater]", type="number", format="dp:0", title=bf.title),
	        list(name="pmp[greater]", type="number", format="dp:4", title="Posterior probability"),
	        list(name = "type[less]", type = "string", title = "Hypothesis"),
	        list(name="BF[less]", type="number", format="dp:4", title=bf.title),
	        list(name="pmp[less]", type="number", format="dp:4", title="Posterior probability"),
	        list(name = "type[equal]", type = "string", title = "Hypothesis"),
	        list(name = "BF[equal]", type="number", format="dp:4", title = bf.title),
	        list(name="pmp[equal]", type="number", format="dp:4", title="Posterior probability"))
	}

	ttest[["schema"]] <- list(fields=fields)

	footnotes <- .newFootnotes()

	levels <- base::levels(dataset[[ .v(options$groupingVariable) ]])

	if (length(levels) != 2) {

		g1 <- "1"
		g2 <- "2"

	} else {

		g1 <- levels[1]
		g2 <- levels[2]
	}

	if (options$hypothesis == "groupOneGreater") {

	    type <- 3

		message <- paste("For all tests, H\u2080: group ", g1," = group ", g2, " is tested against H\u2081: group ", g1, " < group ", g2, ".", sep = "")
		.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)

	} else if (options$hypothesis == "groupTwoGreater") {

	    type <- 2

		message <- paste("For all tests, H\u2080: group ", g1, " = group ", g2, " is tested against H\u2081: group ", g1, " > group ", g2, ".", sep = "")
		.addFootnote(footnotes, symbol="<em>Note.</em>", text=message)

	} else if (options$hypothesis == "allTypes"){

	    type <- 5

	   message <- paste("For all tests, H\u2081: group ", g1, " > group ", g2, " and H\u2082: group ", g1, " < group ", g2, " are compared to H\u2080: group ", g1, " = group ", g2, ".", sep = "")
	    .addFootnote(footnotes, symbol="<em>Note.</em>", text=message)

	} else if (options$hypothesis == "biggerSmaller"){
	    
	    type <- 4
	    
	    note <- paste("For all tests, H\u2080: group ", g1," > group ", g2," is tested against H\u2081: group ", g1, " < group ", g2, ".", sep = "")
	    .addFootnote(footnotes, symbol="<em>Note.</em>", text=note)
	    
	} else if (options$hypothesis == "groupsNotEqual"){

	    type <- 1

	    message <- paste("For all tests, H\u2080: group ", g1, " = group ", g2, " is tested against H\u2081: group ", g1, " \u2260 group ", g2, ".", sep = "")
	    .addFootnote(footnotes, symbol="<em>Note.</em>", text=message)

	}


	ttest.rows <- list()

	status <- rep("ok", length(options$variables))
	BF10post <- numeric(length(options$variables))
	plottingError <- rep("error", length(options$variables))
	errorFootnotes <- rep("no", length(options$variables))

	Bainresult <- list()

	for (variable in options[["variables"]]) {

		if (!is.null(state) && variable %in% state$options$variables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$hypothesis == FALSE
			&& diff$groupingVariable == FALSE && diff$missingValues == FALSE)))) {

				index <- which(state$options$variables == variable)

				if (state$Bainresult[index] != "error") {

					ttest.rows[[length(ttest.rows)+1]] <- state$results$ttest$data[[index]]

				} else {

					index2 <- .addFootnote(footnotes, state$errorFootnotes[[index]])

					if(options$hypothesis == "groupsNotEqual"){
					    result_test <- list(Variable=variable, "hypothesis[type1]" = "Equal","BF[type1]"=.clean(NaN), "pmp[type1]" = .clean(NaN),
					                        "hypothesis[type2]" = "Not equal", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN),.footnotes = list(BF=list(index2)))
					}
					if(options$hypothesis == "groupTwoGreater"){
					    result_test <-list(Variable=variable, "hypothesis[type1]" = "Equal","BF[type1]"= .clean(NaN), "pmp[type1]" = .clean(NaN),
					                       "hypothesis[type2]" = "Bigger", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN),.footnotes = list(BF=list(index2)))
					}
					if(options$hypothesis == "groupOneGreater"){
					    result_test <-list(Variable=variable, "hypothesis[type1]" = "Equal", "BF[type1]"= .clean(NaN), "pmp[type2]" = .clean(NaN),
					    "hypothesis[type2]" = "Smaller", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN),.footnotes = list(BF=list(index2)))
					}
					if(options$hypothesis == "biggerSmaller"){
					    result_test <-list(Variable=variable, "hypothesis[type1]" = "Bigger", "BF[type1]"= .clean(NaN), "pmp[type2]" = .clean(NaN),
					                       "hypothesis[type2]" = "Smaller", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN),.footnotes = list(BF=list(index2)))
					}
					if(options$hypothesis == "allTypes"){
					    result_test <-list(Variable=variable,
					                       "type[greater]" = "Equal",
					                       "BF[greater]"= .clean(NaN),
					                       "pmp[greater]" = .clean(NaN),
					                       "type[less]"= "Smaller",
					                       "BF[less]" = .clean(NaN),
					                       "pmp[less]" = .clean(NaN),
					                       "type[equal]" = "Bigger",
					                       "BF[equal]" = .clean(NaN),
					                       "pmp[equal]" = .clean(NaN),
					                       .footnotes = list(BF=list(index2)))
					}

					ttest.rows[[length(ttest.rows)+1]] <- result_test

				}

			} else {

				ttest.rows[[length(ttest.rows)+1]] <- list(Variable=variable)
			}
	}

	rowCompleted <- logical(length(ttest.rows))

	for (i in seq_along(ttest.rows))
		rowCompleted[i] <- ifelse(length(ttest.rows[[i]]) > 1, TRUE, FALSE)

	if (!is.null(state) && all(options[["variables"]] %in% state$options$variables) && options$groupingVariable == state$options$groupingVariable && all(rowCompleted))
		ttest[["status"]] <- "complete"

	if (perform == "run" && length(options$variables) != 0 && options$groupingVariable != "") {

		if (length(levels) != 2) {

			ttest[["error"]] <- list(errorType="badData", errorMessage="The Grouping Variable must have 2 levels")

			status <- rep("error", length(options$variables))
			plottingError <- rep("Plotting is not possible: The Grouping Variable must have 2 levels", length(options$variables))

		} else {

			rowNo <- 1

			i <- 1

			errorFootnotes <- list()

			for (variable in options[["variables"]]) {

				subDataSet <- subset(dataset, select=c(.v(variable), .v(options$groupingVariable)))
				subDataSet <- na.omit(subDataSet)

				gs <- base::levels(levels)

				group2 <- subDataSet[subDataSet[[.v(options$groupingVariable)]]== g1,.v(variable)]
				group1 <- subDataSet[subDataSet[[.v(options$groupingVariable)]]== g2,.v(variable)]


				if (!is.null(state) && variable %in% state$options$variables && !is.null(diff) && ((is.logical(diff) && diff == FALSE) || (is.list(diff) && (diff$hypothesis == FALSE
				&& diff$groupingVariable == FALSE && diff$bayesFactorType == FALSE && diff$missingValues == FALSE)))) {

					index <- which(state$options$variables == variable)

					if (state$Bainresult[[index]] != "error") {

						ttest.rows[[i]] <- state$results$ttest$data[[index]]

					} else if (state$Bainresult[[index]] == "error") {

					    index2 <- .addFootnote(footnotes, state$errorFootnotes[index])

					    errorFootnotes[i] <- state$errorFootnotes[index]

					    if(options$hypothesis == "groupsNotEqual"){
					        result_test <- list(Variable=variable, "hypothesis[type1]" = "Equal","BF[type1]"=.clean(NaN), "pmp[type1]" = .clean(NaN),
					                            "hypothesis[type2]" = "Not equal", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN),.footnotes = list(BF=list(index2)))
					    }
					    if(options$hypothesis == "groupTwoGreater"){
					        result_test <-list(Variable=variable, "hypothesis[type1]" = "Equal","BF[type1]"= .clean(NaN), "pmp[type1]" = .clean(NaN),
					                           "hypothesis[type2]" = "Bigger", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN),.footnotes = list(BF=list(index2)))
					    }
					    if(options$hypothesis == "groupOneGreater"){
					        result_test <-list(Variable=variable, "hypothesis[type1]" = "mu1 = mu2", "BF[type1]"= .clean(NaN), "pmp[type2]" = .clean(NaN),
					        "hypothesis[type2]" = "Smaller", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN),.footnotes = list(BF=list(index2)))
					    }
					    if(options$hypothesis == "biggerSmaller"){
					        result_test <-list(Variable=variable, "hypothesis[type1]" = "Bigger", "BF[type1]"= .clean(NaN), "pmp[type2]" = .clean(NaN),
					                           "hypothesis[type2]" = "Smaller", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN),.footnotes = list(BF=list(index2)))
					    }
					    if(options$hypothesis == "allTypes"){
					        result_test <-list(Variable=variable,
					                           "type[greater]" = "Equal",
					                           "BF[greater]"= .clean(NaN),
					                           "pmp[greater]" = .clean(NaN),
					                           "type[less]"= "Smaller",
					                           "BF[less]" = .clean(NaN),
					                           "pmp[less]" = .clean(NaN),
					                           "type[equal]" = "Bigger",
					                           "BF[equal]" = .clean(NaN),
					                           "pmp[equal]" = .clean(NaN),
					                           .footnotes = list(BF=list(index2)))
					    }

					    ttest.rows[[i]] <- result_test

					}

					BF10post[rowNo] <- ttest.rows[[rowNo]] # state$BF10post[index]  # changed from ttest.rows[[rowNo]]$BF
					status[rowNo] <- state$status[index]
					plottingError[rowNo] <- state$plottingError[index]
					plotres[[i]] <- state$plotres[[index]]
					Bainresult[[i]] <- state$Bainresult[[index]]
					#plotVariables[[i]] <- state$plotVariables[[index]]

				} else {

				    errors <- .hasErrors(dataset=dataset, perform=perform, type=c('observations', 'variance', "infinity"),
				                         all.target=variable, observations.amount = "< 2", message = "short")

				    errorMessage <- NULL

				    if (!identical(errors, FALSE)) {
				        errorMessage <- errors$message
				    }

				    if (!is.null(errorMessage)) {

				        ## log the error in a footnote
				        index <- .addFootnote(footnotes, errorMessage)
				        row.footnotes <- list(t = list(index))

				        if(options$hypothesis == "groupsNotEqual"){
				            result_test <- list(Variable=variable, "hypothesis[type1]" = "Equal","BF[type1]"=.clean(NaN), "pmp[type1]" = .clean(NaN),
				                                "hypothesis[type2]" = "Not equal", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN),.footnotes = row.footnotes)
				        }
				        if(options$hypothesis == "groupTwoGreater"){
				            result_test <-list(Variable=variable, "hypothesis[type1]" = "Equal","BF[type1]"= .clean(NaN), "pmp[type1]" = .clean(NaN),
				                               "hypothesis[type2]" = "Bigger", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN),.footnotes = row.footnotes)
				        }
				        if(options$hypothesis == "groupOneGreater"){
				            result_test <-list(Variable=variable, "hypothesis[type1]" = "Equal", "BF[type1]"= .clean(NaN), "pmp[type2]" = .clean(NaN),
				            "hypothesis[type2]" = "Smaller", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN),.footnotes = row.footnotes)
				        }
				        if(options$hypothesis == "biggerSmaller"){
				            result_test <-list(Variable=variable, "hypothesis[type1]" = "Bigger", "BF[type1]"= .clean(NaN), "pmp[type2]" = .clean(NaN),
				                               "hypothesis[type2]" = "Smaller", "BF[type2]" = .clean(NaN), "pmp[type2]" = .clean(NaN),.footnotes = row.footnotes)
				        }
				        if(options$hypothesis == "allTypes"){
				            result_test <-list(Variable=variable,
				                               "type[greater]" = "Equal",
				                               "BF[greater]"= .clean(NaN),
				                               "pmp[greater]" = .clean(NaN),
				                               "type[less]"= "Smaller",
				                               "BF[less]" = .clean(NaN),
				                               "pmp[less]" = .clean(NaN),
				                               "type[equal]" = "Bigger",
				                               "BF[equal]" = .clean(NaN),
				                               "pmp[equal]" = .clean(NaN),
				                               .footnotes = row.footnotes)
				        }


				        Bainresult[[i]] <- "error"
				        plotVariables[[i]] <- variable
				        errorFootnotes[i] <- errorMessage
				        plotres[[i]] <- "error"

				    } else {

						r <- Bain::Bain_ttestData(group1, group2, type = type)
						
						informationFootnote <- FALSE
						
						if(any(is.na(r))){
						    informationFootnote <- TRUE
						    note <- "Bayes factor could not be calculated, there are no samples drawn from the constrained area under the curve."
						    index <- .addFootnote(footnotes, note, symbol = "<em>NaN: </em>")
						    row.footnotes <- list(t = list(index))
						}
						
						Bainresult[[i]] <- "no error"

						plotres[[i]] <- r

						if(options[["bayesFactorType"]] == "LogBF10"){
						    makeLog <- TRUE
						} else {
						    makeLog <- FALSE
						}
						
						if(type == 1){
						    
						    if(makeLog){
						        BF_0u <- log(r$BF_0u)
						    } else{
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
						    
						} else if (type == 5) {
						    
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
						    result_test <- list(Variable=variable, "hypothesis[type1]" = "Equal","BF[type1]"=.clean(BF_0u), "pmp[type1]" = .clean(PMP_0),
						    "hypothesis[type2]" = "Not equal", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_u))
						}
						if(options$hypothesis == "groupTwoGreater"){
						    result_test <-list(Variable=variable, "hypothesis[type1]" = "Equal","BF[type1]"= .clean(BF_01), "pmp[type1]" = .clean(PMP_0),
						                       "hypothesis[type2]" = "Smaller", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_1))
						}
						if(options$hypothesis == "groupOneGreater"){
						    result_test <-list(Variable=variable, "hypothesis[type1]" = "Equal", "BF[type1]"= .clean(BF_01), "pmp[type1]" = .clean(PMP_0),
						                       "hypothesis[type2]" = "Bigger", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_1))
						}
						if(options$hypothesis == "biggerSmaller"){
						    result_test <-list(Variable=variable, "hypothesis[type1]" = "Bigger", "BF[type1]"=.clean(BF_12), "pmp[type1]" = .clean(PMP_1),
						                       "hypothesis[type2]" = "Smaller", "BF[type2]" = "", "pmp[type2]" = .clean(PMP_2)) 
						}
						if(options$hypothesis == "allTypes"){
						    result_test <-list(Variable=variable,
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

					ttest.rows[[i]] <- result_test

				}

				rowNo <- rowNo + 1
				i <- i + 1
			}
		}

		ttest[["status"]] <- "complete"
	}

	ttest[["footnotes"]] <- as.list(footnotes)
	ttest[["data"]] <- ttest.rows


	list(ttest, status, g1, g2, BFH1H0, plottingError, BF10post, errorFootnotes, plotres, Bainresult, plotVariables)
}

.BainttestBayesianIndependentSamplesDescriptives <- function(dataset, options, perform,
												 state = NULL, diff = NULL) {
	if (options$descriptives == FALSE) return(NULL)

	descriptives = list("title" = "Group Descriptives")

	## sets up the table for the descriptives
	fields <- list(
		list(name = "variable", title = "", type = "string", combine = TRUE),
		list(name = "group", title = "Group", type = "string"),
		list(name = "N", title = "N", type = "number"),
		list(name = "mean", title = "Mean", type = "number", format = "sf:4;dp:3"),
		list(name = "sd", title = "SD", type = "number", format = "sf:4;dp:3"),
		list(name = "se", title = "SE", type = "number", format = "sf:4;dp:3")
	)

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

	descriptives[["schema"]] <- list(fields = fields)
	data <- list()


	## function to check if everything is alright with the options
	isAllright <- function(variable, options, state = NULL, diff = NULL) {

		# check if the variable is in the state variables
		cond1 <- !is.null(state) && variable %in% state$options$variables

		# check if either diff is true, or it's a list and descriptives,
		# and groupingVariable, missingValues are FALSE
		cond2 <- (!is.null(diff) && (is.logical(diff) && diff == FALSE) || (is.list(diff)
				 && !any(diff$descriptives,diff$groupingVariable, diff$missingValues)))

		cond1 && cond2
	}

	variables <- options$variables
	if (length(variables) == 0) variables <- "."

	for (variable in variables) {

		if (isAllright(variable, options, state, diff)) {

			stateDat <- state$results$descriptives$data
			descriptivesVariables <- as.character(length(stateDat))

			for (i in seq_along(stateDat))
				descriptivesVariables[i] <- stateDat[[i]]$variable

			indices <- which(descriptivesVariables == variable)
			data[[length(data) + 1]] <- stateDat[[indices[1]]]
			data[[length(data) + 1]] <- stateDat[[indices[2]]]

		} else {
			data[[length(data) + 1]] <- list(variable = variable, .isNewGroup = TRUE)
			data[[length(data) + 1]] <- list(variable = variable)
		}
	}

	## check if we are done with all this crap
	done <- (!is.null(state) &&
			 state$options$descriptives &&
			 all(variables %in% state$options$variables))

	if (done) descriptives[["status"]] <- "complete"

	groups <- options$groupingVariable

	## if we actually have to do the test, and we have a grouping variable
	if (perform == "run" && groups != "") {
		levels <- base::levels(dataset[[ .v(groups) ]])

		## if people don't know what a t-test is...
		if (length(levels) != 2) {
			descriptives[["error"]] <- list(errorType = "badData")

		} else {

			rowNo <- 1
			groupingData <- dataset[[.v(groups)]]

			## do the whole loop as above again
			for (variable in variables) {

				# if everything is alright, add stuff to data
				if (isAllright(variable, options, state, diff)) {

					stateDat <- state$results$descriptives$data
					descriptivesVariables <- as.character(length(stateDat))

					for (i in seq_along(stateDat))
						descriptivesVariables[i] <- stateDat[[i]]$variable

					indices <- which(descriptivesVariables == variable)

					data[[rowNo]] <- stateDat[[indices[1]]]
					data[[rowNo]] <- stateDat[[indices[2]]]

					rowNo <- rowNo + 2

				} else {

					for (i in 1:2) {

					  level <- levels[i]
					  variableData <- dataset[[.v(variable)]]

					  groupData <- variableData[groupingData == level]
					  groupDataOm <- na.omit(groupData)

					  if (class(groupDataOm) != "factor") {

							posteriorSummary <- .posteriorSummaryGroupMean(variable=groupDataOm, descriptivesPlotsCredibleInterval=options$descriptivesPlotsCredibleInterval)
							ciLower <- .clean(posteriorSummary$ciLower)
							ciUpper <- .clean(posteriorSummary$ciUpper)

						  n <- .clean(length(groupDataOm))
						  mean <- .clean(mean(groupDataOm))
						  std <- .clean(sd(groupDataOm))
						  sem <- .clean(sd(groupDataOm) / sqrt(length(groupDataOm)))

						  result <- list(variable = variable, group = level,
										 N = n, mean = mean, sd = std, se = sem, lowerCI = ciLower,
										 upperCI = ciUpper)

					  } else {

						n <- .clean(length(groupDataOm))
						result <- list(variable = variable, group = "",
									   N = n, mean = "", sd = "", se = "", lowerCI = "",
										 upperCI = "")
					}

					if (i == 1) {
						result[[".isNewGroup"]] <- TRUE
					}

					data[[rowNo]] <- result
					rowNo <- rowNo + 1
					}
				}
			}
		}
		descriptives[["status"]] <- "complete"
	}

	descriptives[["data"]] <- data
	descriptives
}
