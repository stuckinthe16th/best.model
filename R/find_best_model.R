#' Best Linear Model
#'
#' This function helps you select the best linear model given your dataset.  Allows you to define a denpendent variable, any variables you wish to have in your final model and which type of optomized liner model you would like returned.  Provides both Adjusted \eqn{R^{2}}{R^2} and p-value optimized models.
#' @aliases best_lm_model
#' @param data A \code{data.frame()} object which contains the data you wish to model.
#' @param dep_var A \code{string} containing the dependent variable you wish your model to measure.  Defaults to the first column of \code{data}.
#' @param keep_vars A \code{string}, or \code{list()} of \code{strings}, which represents the variables you want to keep in your final model regardless of their effects on the final model's optimization.
#' @param ignore_vars A \code{string}, or \code{list()} of \code{strings}, which represents the variables you want to exclude in your final model regardless of their effects on the final model's optimization.
#' @param return A \code{string} object that represents the type of model you would like to return.
#' \itemize{
##'  \item{"base" returns a linear model of all of the variables through the obect \code{model_base} and a summary table of that model.}
##'  \item{"r2" returns an Adjusted \eqn{R^{2}}{R^2} optimized linear model of the data through the obect \code{model_r2} and a summary table of that model.}
##'  \item{"p" returns a p-value optimized (95\% signifigance) linear model of the data through the obect \code{model_p} and a summary table of that model.}
##'  \item{"all" returns all of the above \code{model} objects and summary tables.  \code{return} defaults to "all".}
##'  }
#' @examples
#' best_lm_model(swiss, dep_var="Fertility", keep_var="Catholic", return="p")
#' @export



best_lm_model <- function(data, dep_var="", keep_vars=list(), ignore_vars=list(), return="all"){

          ##LOAD LIBRARIES

          suppressMessages(suppressWarnings(library(statsr)))
          suppressMessages(suppressWarnings(library(dplyr)))
          suppressMessages(suppressWarnings(library(ggplot2)))
          suppressMessages(suppressWarnings(library(GGally)))

          ##SET-UP

                    #Load in Data
                    data_w <- data
                    data_w <- data_w[, !(names(data_w) %in% ignore_vars)]

                    #Define Dependent Variable or Reorder Dataframe
                    moveMe <- function(data, tomove, where = "last", ba = NULL) {
                              temp <- setdiff(names(data), tomove)
                              x <- switch(
                                        where,
                                        first = data[c(tomove, temp)],
                                        last = data[c(temp, tomove)],
                                        before = {
                                                  data[append(temp, values = tomove, after = (match(ba, temp)-1))]
                                        },
                                        after = {
                                                  data[append(temp, values = tomove, after = (match(ba, temp)))]
                                        })
                              x
                    }
                    dep_var_check <- 0
                    if(dep_var==""){
                              dep_var <- names(data_w)[1]
                              dep_var_check <- 1
                    } else{
                              data_w <- moveMe(data_w, dep_var, where="first")
                    }

                    #Create Base Model
                    base_model <- lm(as.formula(paste(dep_var, "~ .")), data = data_w)
                    model_base <<- base_model
                    if(return=="all"| return=="base"){
                              formula <- paste(dep_var, " ~ ", names(data_w)[2])
                              if(length(data_w)>2){
                                        for(x in 3:length(data_w)){
                                                  formula <- paste(formula, " + ", names(data_w)[x], sep="")
                                        }
                              }
                              wid <- options()$width+2
                              for(x in 1:wid){cat("#")}
                              cat("\n")
                              cat("Base Model: \n")
                              cat(formula, "\n")
                              for(x in 1:wid){cat("#")}
                              cat("\n\n")
                              model_df_3 <- data.frame(summary(model_base)$coef)
                              model_df_3$S <- ""
                              model_df_3$S[model_df_3$Pr...t..<0.1] <- "."
                              model_df_3$S[model_df_3$Pr...t..<0.05] <- "*"
                              model_df_3$S[model_df_3$Pr...t..<0.01] <- "**"
                              model_df_3$S[model_df_3$Pr...t..<0.001] <- "***"
                              names(model_df_3)[5] <- ""
                              print(format(data.frame(model_df_3, fix.empty.names=FALSE), justify='left'))
                              cat("---\n")
                              cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
                              cat("\n")
                              cat(paste("Residual standard error: ", round(summary(model_base)$sigma, 4), " on ", summary(model_base)$df[2]," degrees of freedom", sep=""), "\n")
                              cat(paste("Multiple R-squared:  ", round(summary(model_base)$r.squared, 4), ", Adjusted R-squared: ", round(summary(model_base)$adj.r.squared, 4), sep=""), "\n")
                              f_3 <- summary(model_base)$fstatistic
                              p_3 <- pf(f_3[1],f_3[2],f_3[3],lower.tail=F)
                              attributes(p_3) <- NULL
                              cat(paste("F-statistic: ", round(f_3[1], 3), " on ", f_3[2], " and ", f_3[3], " DF,  p-value: ", signif(p_3, digits=3), sep=""))
                              cat("\n\n")
                    }
                    base_r2 <- summary(base_model)$adj.r.squared
                    data_w_o <- data_w


                    #Set-up First Iteration
                    num_dropped <- 0
                    prev_r2 <- base_r2

          ##R2 OPTIMIZED

                    if(return=="all"| return=="r2"){
                              #Set Stop Variable
                              stop <- 0

                              #Loop Until Highest R2
                              while(stop==0){
                                        level_r2 <- 0
                                        for(x in 2:length(data_w)){
                                                  if(names(data_w)[x] %in% keep_vars){
                                                  }else{
                                                            working_model <- lm(as.formula(paste(dep_var, "~ .")), data = subset(data_w, select = -x))
                                                            if(summary(working_model)$adj.r.squared > prev_r2 && summary(working_model)$adj.r.squared > level_r2){
                                                                      level_r2 <- summary(working_model)$adj.r.squared
                                                                      drop_col <- x
                                                            }
                                                  }
                                        }
                                        if(level_r2>0){
                                                  num_dropped <- num_dropped + 1
                                                  assign(paste("r2_", num_dropped, sep=""), level_r2)
                                                  assign(paste("var_", num_dropped, sep=""), names(data_w)[drop_col])
                                                  data_w <- subset(data_w, select=-drop_col)
                                                  prev_r2 <- level_r2
                                        } else{
                                                  stop <- 1
                                        }
                              }

                              #Print Results
                              formula <- paste(dep_var, " ~ ", names(data_w)[2])
                              if(length(data_w)>2){
                                        for(x in 3:length(data_w)){
                                                  formula <- paste(formula, " + ", names(data_w)[x], sep="")
                                        }
                              }
                              wid <- options()$width+2
                              for(x in 1:wid){cat("#")}
                              cat("\n")
                              cat("Adj-R2 Optimized Model: \n")
                              cat(formula, "\n")
                              for(x in 1:wid){cat("#")}
                              cat("\n\n")
                              cat("Model R2: ", prev_r2, "\n")
                              cat("Base R2: ", base_r2, "\n\n")
                              if(num_dropped>0){
                                        for(x in 1:num_dropped){
                                                  cat(paste("Drop ", x, ": ", eval(parse(text = paste("var_", x, sep=""))), sep=""), "\n")
                                                  cat(paste("        adj-r2 ---> ", eval(parse(text = paste("r2_", x, sep=""))), sep=""), "\n")


                                        }
                              }

                              #Return Model
                              cat("\n\n")
                              model_r2 <<- lm(as.formula(paste(dep_var, "~ .")),data = data_w)
                              model_df <- data.frame(summary(model_r2)$coef)
                              model_df$S <- ""
                              model_df$S[model_df$Pr...t..<0.1] <- "."
                              model_df$S[model_df$Pr...t..<0.05] <- "*"
                              model_df$S[model_df$Pr...t..<0.01] <- "**"
                              model_df$S[model_df$Pr...t..<0.001] <- "***"
                              names(model_df)[5] <- ""
                              print(format(data.frame(model_df, fix.empty.names=FALSE), justify='left'))
                              cat("---\n")
                              cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
                              cat("\n")
                              cat(paste("Residual standard error: ", round(summary(model_r2)$sigma, 4), " on ", summary(model_r2)$df[2]," degrees of freedom", sep=""), "\n")
                              cat(paste("Multiple R-squared:  ", round(summary(model_r2)$r.squared, 4), ", Adjusted R-squared: ", round(summary(model_r2)$adj.r.squared, 4), sep=""), "\n")
                              f <- summary(model_r2)$fstatistic
                              p <- pf(f[1],f[2],f[3],lower.tail=F)
                              attributes(p) <- NULL
                              cat(paste("F-statistic: ", round(f[1], 3), " on ", f[2], " and ", f[3], " DF,  p-value: ", signif(p, digits=3), sep=""))
                              cat("\n\n")
                    }

          ##P-VALUE (95% SIG.) OPTIMIZED

                    if(return=="all"| return=="p"){
                              #Re-set-up
                              data_w_2 <- data_w_o
                              if(dep_var_check==1){
                                        dep_var <- names(data_w_2)[1]
                              } else{
                                        data_w_2 <- moveMe(data_w, dep_var, where="first")
                              }

                              #Set-up First Iteration
                              num_dropped_2 <- 0

                              #Iterate Until All Significant
                              length_f <- length(data_w_2)
                              for(a in 1:length_f){
                                        #create model
                                        working_model_2 <- lm(as.formula(paste(dep_var, "~ .")), data=data_w_2)
                                        model_df_2 <- data.frame(summary(working_model_2)$coef)
                                        if(row.names(model_df_2)[1]=="(Intercept)"){
                                                  model_df_2 <- model_df_2[-1,]
                                        }
                                        #add variable names
                                        model_df_2$var_name <- ""
                                        len_df <- nrow(model_df_2)
                                        for(x in 1:len_df){
                                                  for(y in names(data_w_2)){
                                                            pos <- regexpr(y, rownames(model_df_2)[x])[1]
                                                            if(length(pos)>0){
                                                                      if(pos==1){
                                                                                model_df_2$var_name[x] <- y
                                                                      }
                                                            }

                                                  }
                                        }
                                        for(keep_name in keep_vars){
                                                  model_df_2 <- model_df_2[model_df_2$var_name!=keep_name,]
                                        }
                                        #find variable min and max
                                        model_df_2$var_max <- NA
                                        model_df_2$var_min <- NA
                                        for(y in names(data_w_2)){
                                                  suppressMessages(suppressWarnings(model_df_2$var_max[which(model_df_2$var_name==y)] <- max(model_df_2$Pr...t..[which(model_df_2$var_name==y)])))
                                                  suppressMessages(suppressWarnings(model_df_2$var_min[which(model_df_2$var_name==y)] <- min(model_df_2$Pr...t..[which(model_df_2$var_name==y)])))

                                        }
                                        #find insignificant variables
                                        model_df_2$var_sig <- 0
                                        model_df_2$var_sig[which(model_df_2$var_min<=0.05)] <- 1
                                        if(nrow(model_df_2[model_df_2$var_sig==0,])!=0){
                                                  model_df_2 <- model_df_2[model_df_2$var_sig==0, ]
                                                  model_df_2 <- model_df_2[,c("var_name","var_max")]
                                                  model_df_2 <- unique(model_df_2)
                                                  #find highest p_value
                                                  model_df_2 <- model_df_2[with(model_df_2, order(-var_max)), ]
                                                  drop_name <- model_df_2$var_name[1]
                                                            num_dropped_2 <- num_dropped_2 + 1
                                                            assign(paste("drop_name_", num_dropped_2, sep=""), drop_name)
                                                            assign(paste("drop_prt_", num_dropped_2, sep=""), model_df_2$var_max[1])
                                                            drop_col <- which(colnames(data_w_2)==drop_name)
                                                            data_w_2 <- subset(data_w_2, select=-drop_col)
                                        }
                              }

                              #Print Results
                              formula_2 <- paste(dep_var, " ~ ", names(data_w_2)[2])
                              if(length(data_w)>2){
                                        for(x in 3:length(data_w_2)){
                                                  formula_2 <- paste(formula_2, " + ", names(data_w_2)[x], sep="")
                                        }
                              }
                              wid <- options()$width+2
                              for(x in 1:wid){cat("#")}
                              cat("\n")
                              cat("P-Value Optimized Model: \n")
                              cat(formula_2, "\n")
                              for(x in 1:wid){cat("#")}
                              cat("\n\n")
                              cat("Variables Dropped: ", num_dropped_2, "\n\n")
                              if(num_dropped_2>0){
                                        for(x in 1:num_dropped_2){
                                                  cat(paste("Drop ", x, ": ", eval(parse(text = paste("drop_name_", x, sep=""))), sep=""))
                                                  cat(paste(" (p-value= ", eval(parse(text = paste("drop_prt_", x, sep=""))), sep=""), ")\n", sep="")
                                        }
                              }

                              #Return Model
                              cat("\n\n")
                              model_p <<- lm(as.formula(paste(dep_var, "~ .")), data = data_w_2)
                              model_df_2 <- data.frame(summary(model_p)$coef)
                              model_df_2$S <- ""
                              model_df_2$S[model_df_2$Pr...t..<0.1] <- "."
                              model_df_2$S[model_df_2$Pr...t..<0.05] <- "*"
                              model_df_2$S[model_df_2$Pr...t..<0.01] <- "**"
                              model_df_2$S[model_df_2$Pr...t..<0.001] <- "***"
                              names(model_df_2)[5] <- ""
                              print(format(data.frame(model_df_2, fix.empty.names=FALSE), justify='left'))
                              cat("---\n")
                              cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
                              cat("\n")
                              cat(paste("Residual standard error: ", round(summary(model_p)$sigma, 4), " on ", summary(model_p)$df[2]," degrees of freedom", sep=""), "\n")
                              cat(paste("Multiple R-squared:  ", round(summary(model_p)$r.squared, 4), ", Adjusted R-squared: ", round(summary(model_p)$adj.r.squared, 4), sep=""), "\n")
                              f_2 <- summary(model_p)$fstatistic
                              p_2 <- pf(f_2[1],f_2[2],f_2[3],lower.tail=F)
                              attributes(p_2) <- NULL
                              cat(paste("F-statistic: ", round(f_2[1], 3), " on ", f_2[2], " and ", f_2[3], " DF,  p-value: ", signif(p_2, digits=3), sep=""))
                    }
}
