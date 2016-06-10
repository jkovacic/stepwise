# Copyright 2015, Jernej Kovacic
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


#
# Implementation of criteria functions for selection of models
#


.crit.criterion <- function(critf, mdl, msef)
{
  # Evaluates the selected criterion function.
  #
  # The following criteria are currently supported:
  # - adjusted R^2 (crit="adjR2")
  # - Mallows' Cp (crit="mallowsCp")
  # - Aikake's Information Criterion (crit="aic")
  # - Bayesian Information Criterion (crit="bic")
  #
  # Args:
  #   critf: name of the criterion function
  #   mdl: model as returned by the function 'lm'
  #   msef: mean squared error of the full model (only applicable for the Mallows' Cp)
  #
  # Returns:
  #   value of the selected criterion for the given model
  
  return( switch( critf, 
                  "adjR2" = .crit.adjR2(mdl), 
                  "mallowsCp" = .crit.mallowsCp(mdl, msef),
                  "aic" = .crit.aic(mdl),
                  "bic" = .crit.bic(mdl) ) )
}



.crit.msef <- function(dframe=NULL, resp=NULL, full.mdl=NULL)
{
  # Mean squared error of the full model of the given data frame
  #
  # Args:
  #   dframe: desired data frame
  #   resp: name of the response variable
  #   full.mdl: fitted full model (if not NULL, 'dframe' and 'resp' will be ignored)
  #
  # Returns:
  #   mean squared error of the full model 'full.mdl' (if not NULL) or 'dframe$resp ~ .'
  
  if ( FALSE == is.null(full.mdl) )
  {
    mdl <- full.mdl
  }
  else
  {
    mdl <- lm( as.formula(paste0(resp, " ~ .")), data=dframe)
  }

  # The MSE_full with 'p' predictors and 'n' observations
  # is evaluated as:
  #
  #                n
  #              -----
  #              \           ^   2
  #               >   (y_i - y_i)
  #              /
  #              -----
  #               i=1
  #   MSE_f = -----------------------
  #                  n - p - 1
  #

  return( sum( mdl$residuals^2 ) / mdl$df.residual )
}



.crit.adjR2 <- function(mdl)
{
  # Adjusted R^2 of the given model
  #
  # Args:
  #   mdl - model as returned by the function 'lm'
  #
  # Returns:
  #   adjusted R^2 of 'mdl'

  return( summary(mdl)$adj.r.squared )
}


#
# Definitions of the remaining criteria functions are not firm
# and the functions below will apply the definitions from:
# http://www.stat.purdue.edu/~ghobbs/STAT_512/Lecture_Notes/Regression/Topic_15.pdf
#

.crit.mallowsCp <- function(mdl, msef)
{
  # Mallows' Cp of the given model
  #
  # Args:
  #   mdl - model as returned by the function 'lm'
  #   msef: mean squared error of the full model
  #
  # Returns:
  #   Mallows' Cp of 'mdl'
  
  
  # Mallows' Cp is evaluated as:
  #
  #           SSE
  #   Cp = ---------- - (n - 2*p)
  #         MSE_full
  #
  
  sse <- sum( mdl$residuals^2 )
  n <- nrow( mdl$model )
  p <- ncol( mdl$model ) - 1
  
  return( sse/msef - (n-2*p))
}


.crit.aic <- function(mdl)
{
  # Aikake's Information Criterion (AIC) of the given model
  #
  # Args:
  #   mdl - model as returned by the function 'lm'
  #
  # Returns:
  #   AIC of 'mdl'
  
  
  # AIC is evaluated as:
  #
  #                / SSE  \
  #   AIC = n * ln |----- | + 2 * p
  #                \  n   /
  #
  
  sse <- sum( mdl$residuals^2 )
  n <- nrow( mdl$model )
  p <- ncol( mdl$model ) - 1
  
  return( n*log(sse/n) + 2*p )
}


.crit.bic <- function(mdl)
{
  # Bayesian Information Criterion (BIC) a.k.a.
  # Schwarz Bayesian Criterion (SBC) of the given model
  #
  # Args:
  #   mdl - model as returned by the function 'lm'
  #
  # Returns:
  #   BIC of 'mdl'
  
  
  # BIC is evaluated as:
  #
  #                / SSE  \
  #   BIC = n * ln |----- | + p * ln(n)
  #                \  n   /
  #
  
  sse <- sum( mdl$residuals^2 )
  n <- nrow( mdl$model )
  p <- ncol( mdl$model ) - 1
  
  return( n*log(sse/n) + p*log(n) )
}
