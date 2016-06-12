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



.crit.msef <- function(dframe_mdl, resp=NULL)
{
  # Mean squared error of the full model of the given data frame
  #
  # Args:
  #   dframe_mdl: either desired data frame or full linear model
  #   resp: name of the response variable (ignored if 'dframe_mdl' is a model)
  #
  # Returns:
  #   mean squared error of the full model
  
  if ( FALSE == is.data.frame(dframe_mdl) )
  {
    # Nothing to do if 'dframe_mdl' is already a linear model
    mdl <- dframe_mdl
  }
  else
  {
    mdl <- lm( as.formula(paste0(resp, " ~ .")), data=dframe_mdl)
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



.crit.func.factory <- function(critf, dframe_mdl, resp=NULL)
{
  # a "factory" that returns a properly initialized function (where applicable)
  # that evaluates the criterion of the model. The returned function
  # accepts a single argument and is further used by stepwise algorithms.
  #
  # Args:
  #   critf: reference to the desired criteria function
  #   dframe_mdl: data frame or a full model
  #   resp: name of the response variable (ignored if 'dframe_mdl' is a model)
  #  
  # Returns:
  #   criteria function that accepts a single argument
  
  
  # "Initialization" of a closure is only necessary when the requested 
  # criterion is Mallows' Cp. In all other cases, 'critf' is returned.
  if ( TRUE==identical(.crit.mallowsCp, critf ) )
  {
    retf <- .crit.mallowsCp(.crit.msef(dframe_mdl, resp))
  }
  else
  {
    retf <- critf
  }
  
  return( retf )
}



.crit.adjR2 <- function(mdl, msef)
{
  # Adjusted R^2 of the given model
  #
  # Args:
  #   mdl: model as returned by the function 'lm'
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

.crit.mallowsCp <- function(msef)
{
  # A closure that "initializes" the function that evaluates
  # model's Mallows' Cp
  #
  # Args:
  #   msef: mean squared error of the full model
  #
  # Returns:
  #   properly initialized function that evaluates model's Mallows' Cp
  
  return(
    function(mdl)
    {
      # Mallows' Cp of the given model
      #
      # Args:
      #   mdl: model as returned by the function 'lm'
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
    } )
}



.crit.aic <- function(mdl, msef)
{
  # Aikake's Information Criterion (AIC) of the given model
  #
  # Args:
  #   mdl: model as returned by the function 'lm'
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


.crit.bic <- function(mdl, msef)
{
  # Bayesian Information Criterion (BIC) a.k.a.
  # Schwarz Bayesian Criterion (SBC) of the given model
  #
  # Args:
  #   mdl: model as returned by the function 'lm'
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
