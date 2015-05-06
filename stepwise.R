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


.check.validity <- function(d.frame, resp.var, alpha=0.05, inc.vars=NULL)
{
  # Checks validity of all input arguments.
  # This function should be called by all "public" functions first.
  #
  # Args:
  #   d.frame: a data frame
  #   resp.var: name of the response variable. Must be a character and among d.frame's variables
  #   alpha: significance level for p-values. Only applicable if 'adj.R'
  #          is FALSE. Must be a numeric value between 0 and 1.
  #   inc.vars: a list of predictors that must be included into a model. If not
  #             NULL, it must be a list of variable names as character values. All values
  #             must be valid d.frame's variables and none must be equal to 'res'.
  #
  # Returns:
  #   Nothing if all arguments are valid. If not, a stop message is thrown.
  
  
  # 'd.frame' must be a valid data frame:
  if ( FALSE == is.data.frame(d.frame) )
  {
    stop("\'d.frame\' is not a data frame");
  }

  
  # 'alpha' is a p-value and must be a numeric value:
  if ( FALSE == is.numeric(alpha) )
  {
    stop("\'alpha\' is not a numeric value");
  }
  
  # additionally it must be a scalar:
  if ( 1 != length(alpha) )
  {
    stop("\'alpha\' is not a scalar");
  }
  
  # it must be in the range between 0.0 and 1.0:
  if ( alpha <= 0.0 || alpha >= 1.0 )
  {
    stop("\'alpha\' is out of the valid range");
  }
  
  
  # 'resp.var' is a variable's name and must be a character value
  if ( FALSE == is.character(resp.var) )
  {
    stop("\'resp.var\' is not a character string");
  }
  
  
  # A list of all d.frame's variables:
  all.vars <- names(d.frame);
  
  # 'resp.var' must aldo be one of d.frame's variable names:
  if ( FALSE == (resp.var %in% all.vars) )
  {
    stop("The data frame does not contain the variable \'resp.var\'s");
  }
  
  
  # 'resp.var' is expected to be a numeric vector
  if ( FALSE == is.numeric(d.frame[, resp.var]) )
  {
    stop("\'resp.var\' is not a numeric vector");
  }
  
  
  # 'inc.vars' can be either equal to NULL...
  if ( FALSE == is.null(inc.vars) )
  {
    # if it is not NULL, it must be a list...
    if ( FALSE == is.list(inc.vars) )
    {
      stop("\'inc.vars\' is not a list");
    }
    
    # the list should not be empty...
    if ( 0 == length(inc.vars) )
    {
      stop("A non-null list \'inc.vars\' is empty");
    }
    
    # each list's element must be checked....
    for ( var in inc.vars )
    {
      # first it must be a character value...
      if ( FALSE == is.character(var) )
      {
        stop("\'incp\' contains non-character elements");
      }
      
      # It must be a valid d.frame's variable name...
      if ( FALSE == (var %in% all.vars) )
      {
        stop("\'inc.vars\' contains invalid predictors");
      }
      
      # and it must not be equal to the response variable name.
      if ( var == resp.var )
      {
        stop("At least one explanatory variable is equal to the response variable");
      }
    }
    
    # Finally check that there are no duplicates in the list:
    if ( 0 != anyDuplicated(inc.vars) )
    {
      stop("Duplicated variable names in \'inc.vars\'");
    }
  }
}


.create.lm.formula <- function(resp.var, vars)
{
  # Accepts the response variable and a list of explanatory variables
  # and creates the 'formula' that can be passed to 'lm'.
  #
  # Args:
  #   resp.var: response variable as a character
  #   vars: explanatory variables as a list of character values
  #
  # Returns:
  #   a formula to be passed to the function 'lm'
  
  
  # Sanity check:
  if ( TRUE==is.null(vars) || 0==length(vars) )
  {
    stop("Invalid list of explanatory variables");
  }
  
  # Collapse the list into a single string,
  # concatenate it to the response variable
  mdl.str <- paste(resp.var, " ~ ", paste(vars, collapse=" + "), sep="");
  
  return (as.formula(mdl.str));
}



stepwise.fwd.adjR2 <- function(dframe, resp, inc=NULL, ret.expl.vars=TRUE)
{
  # Performs the stepwise regression algorithm, based on forward selection 
  # of predictors and using the adjusted R^2 as criteria.
  #
  # Possibly the data frame should be prepocessed prior to passing to this
  # function. For instance, undesired variables are recommended to be removed
  # from the data frame, values of factor variables must be converted to strings,
  # desired interactions should be done beforehand and appended to the data frame, etc.
  #
  # Args:
  #   dframe: data frame
  #   resp: name of the response variable as a character value
  #   inc: an optional list of names of explanatory variables that 
  #        must be included into the final model
  #   ret.expl.vars: if TRUE, the function will return a list of selected
  #                  exlanatory variables, otherwise a model including
  #                  the selected variables
  #
  # Returns:
  #   see 'ret.expl.vars'


  
  # Short description of the algorithm:
  #
  # - start with an empty model (or with the required expl. variables)
  # - fit all models with all possible single expl. variables
  # - pick the model with the highest adjusted R^2
  # - add remaining variables to the existing model, one at a time,
  #   and pick the model with the highest adjusted R^2
  # - repeat the procedure until addition of any of the remaining variables
  #   does not increase the adjusted R^2
  
    
  # sanity check
  .check.validity(d.frame=dframe, resp.var=resp, inc.vars=inc);

  # Extract all variables' names
  df.vars <- as.list(names(dframe));
  
  # And remove the response variable
  df.vars <- df.vars[ df.vars != resp ];
  
  # List of selected explanatory variables - initially empty
  expl.vars <- list();
  
  # Current adjusted R^2, initially set to the model with the interceptor only
  mdl <- lm( .create.lm.formula(resp.var=resp, vars="-1"), data=dframe );
  adj.R2 <- summary(mdl)$adj.r.squared;
  
  # Only applicable if 'inc' is not empty
  if ( !is.null(inc) )
  {
    # Append required variables to 'expl.vars'
    expl.vars <- c(expl.vars, inc);
    
    # And remove all required variables
    df.vars <- df.vars[ !(df.vars %in% inc) ];
    
    # Update the initial adjusted R2:
    mdl <- lm ( .create.lm.formula(resp.var=resp, vars=expl.vars), data=dframe);
    adj.R2 <- summary(mdl)$adj.r.squared;
  }
  
  # Iterate the loop until 'df.vars' is empty or it is interrupted beforehand
  # due to no increase of 'adj.R2'
  while ( length(df.vars) > 0 )
  {
    # a vector of all models' adjusted R^2's
    r2s <- sapply( df.vars, function(v) 
    {
      mdl <- lm(.create.lm.formula(resp, c(expl.vars, v)), data=dframe );
      return( summary(mdl)$adj.r.squared );
    } );
    
    # find the model with the highest adjusted R^2 and check if this
    # value has increased w.r.t. the current model
    idx <- which.max(r2s);
    r2.max <- r2s[idx];
    if ( r2.max > adj.R2 )
    {
      # Update the maximum adjusted R^2
      adj.R2 <- r2.max;

      # Append the variable to 'expl.vars'
      expl.vars <- c(expl.vars, df.vars[idx]);
      
      # and remove it from 'df.vars'
      df.vars[ idx ] <- NULL;
    }
    else
    {
      # Adjusted R^2 has not increased, terminate the while loop
      break;  # out of while
    }
  }  # while
  
  # Finally return the value requested by 'ret.expl.vars'
  if ( TRUE==ret.expl.vars )
  {
    return(expl.vars);
  }
  else
  {
    formula <- .create.lm.formula(resp, expl.vars);
    return( lm(formula, data=dframe) );
  }
}



stepwise.bck.adjR2 <- function(dframe, resp, inc=NULL, ret.expl.vars=TRUE)
{
  # Performs the stepwise regression algorithm, based on backwards elimination 
  # of predictors and using the adjusted R^2 as criteria.
  #
  # Possibly the data frame should be prepocessed prior to passing to this
  # function. For instance, undesired variables are recommended to be removed
  # from the data frame, values of factor variables must be converted to strings,
  # desired interactions should be done beforehand and appended to the data frame, etc.
  #
  # Args:
  #   dframe: data frame
  #   resp: name of the response variable as a character value
  #   inc: an optional list of names of explanatory variables that 
  #        must be included into the final model
  #   ret.expl.vars: if TRUE, the function will return a list of selected
  #                  exlanatory variables, otherwise a model including
  #                  the selected variables
  #
  # Returns:
  #   see 'ret.expl.vars'
  
  
  
  # Short description of the algorithm:
  #
  # - start with a full model (with all variables included)
  # - drop one variable at a time, fit a model, record the adjusted R^2 for each one
  # - pick the model with the highest adjusted R^2
  # - repeat the procedure until the adjusted R^2 does not increase anymore
  
  
  # sanity check
  .check.validity(d.frame=dframe, resp.var=resp, inc.vars=inc);
  
  # Extract all variables' names
  expl.vars <- as.list(names(dframe));
  
  # And remove the response variable
  expl.vars <- expl.vars[ expl.vars != resp ];
  
  # additionally remove all 'inc' variables if given
  if ( !is.null(inc) )
  {
    expl.vars <- expl.vars[ !(expl.vars %in% inc) ];
  }
  
  # Current adjusted R^2, initially set to 0
  adj.R2 <- 0.0;
  
  # Iterate the loop until 'df.vars' is empty or it is interrupted beforehand
  # due to no increase of 'adj.R2'
  while ( length(expl.vars) > 0 )
  {
    r2s <- sapply(expl.vars, function(v)
    {
      # A temporary list of predictors w/o 'v':
      pred <- expl.vars[ expl.vars != v ];
      mdl <- lm( .create.lm.formula(resp, c(pred, inc)), data=dframe );
      return( summary(mdl)$adj.r.squared );
    } );
    
    # find the model with the highest adjusted R^2 and check if this
    # value has increased w.r.t. the current model
    idx <- which.max(r2s);
    r2.max <- r2s[idx];
    if ( r2.max > adj.R2 )
    {
      # Update the maximum adjusted R^2
      adj.R2 <- r2.max;
    
      # Remove the variable from 'df.vars'
      expl.vars[ idx ] <- NULL;
    }
    else
    {
      # Adjusted R^2 has not increased, terminate the while loop
      break;  # out of while
    }
  }  # while
  
  # Finally return the value requested by 'ret.expl.vars'
  if ( TRUE==ret.expl.vars )
  {
    return( c(expl.vars, inc) );
  }
  else
  {
    formula <- .create.lm.formula(resp, c(expl.vars, inc));
    return( lm(formula, data=dframe) );
  }
}
