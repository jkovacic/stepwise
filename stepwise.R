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


source('critfunc.R');

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
    }  # for var
    
    # Finally check that there are no duplicates in the list:
    if ( 0 != anyDuplicated(inc.vars) )
    {
      stop("Duplicated variable names in \'inc.vars\'");
    }
  }  # if !is.null(inc.vars)
  
  # Finally check that all factors (non-numeric variables)
  # have at least 2 levels
  for ( var in names(d.frame) )
  {
    if ( FALSE == is.numeric(d.frame[, var]) &&
         length( levels(factor(d.frame[, var])) ) < 2 )
    {
      stop("One of factor variables does not contain at least 2 levels");
    }
  }  # for var
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
  
  
  var.list <- vars;
  
  # If 'vars' is empty, return a call of an empty model
  if ( TRUE==is.null(vars) || 0==length(vars) )
  {
    var.list <- "1";
  }
  
  # Collapse the list into a single string,
  # concatenate it to the response variable
  mdl.str <- paste0(resp.var, " ~ ", paste(var.list, collapse=" + "));
  
  return (as.formula(mdl.str));
}


.pval.count.levels <- function(d.frame)
{
  # Counts the number of coefficients, the model will contain for
  # each variable.
  #
  # For numeric variables, the result will always be 1.
  # For factor (i.e. non-numeric) varibles, the result will be
  # the number of levels, decreased by 1.
  #
  # Args:
  #   d.frame - the data frame with variables of interest
  #
  # Returns:
  #   a named integer vector with counts for each d.frame's variable
  
  # Start with an empty vector of integers
  r <- integer();
  
  for ( var in names(d.frame) )
  {
    # default value for numeric variables
    val <- 1L;
    
    # for factor (non-numeric) variables, adjust the 'val'
    # to the number of levels, substracted by 1.
    if ( FALSE == is.numeric(d.frame[, var]) )
    {
      val <- length( levels(factor(d.frame[, var])) ) - 1L;
    }
    
    # Append it to 'r'
    r <- c(r, val);
    
    # And finally give a name to the element
    names(r)[length(r)] <- var;
  }
  
  return(r);
}


.pval.getmin <- function(smdl, l=1)
{
  # Minimum p-value of the coefficients that belong to the same
  # (factor) variable.
  #
  # Args:
  #   smdl: summarized linear model (as returned by summary(lm(..)))
  #   l: number of coefficients per variable of interest
  #
  # Returns:
  #   minimum p-value of the variable's coefficients
  
  # coefficients' p-values are stored in the 4th column of mdls$coefficients
  PVAL.COL <- 4L;
  
  # Starting index of the desired variable's coefficient
  OFFSET <- 2L;
  
  # When a model is fitted, the desired variable will always be given first.
  # Since the first coefficient is reseved for the intercept, the coefficents
  # of interest start at idx=2
  
  return( min( smdl$coefficients[ OFFSET : (OFFSET+l-1L), PVAL.COL]) );
}


.stepwise.fwd <- function(dframe, resp, inc, ret.expl.vars, critf, minimize)
{
  # Performs the stepwise regression algorithm, based on forward selection 
  # of predictors that optimizes the selected criterion.
  #
  # The following criteria are currently supported:
  # - adjusted R^2 (crit="adjR2")
  # - Mallows' Cp (crit="mallowsCp")
  # - Aikake's Information Criterion (crit="aic")
  # - Bayesian Information Criterion (crit="bic")
  #
  # Args:
  #   dframe: data frame
  #   resp: name of the response variable as a character value
  #   inc: an optional list of names of explanatory variables that 
  #        must be included into the final model
  #   ret.expl.vars: if TRUE, the function will return a list of selected
  #                  exlanatory variables, otherwise a model including
  #                  the selected variables
  #   crit: criteria function (see above for more details)
  #   minimize: a logical value indicating whether the selected criterion
  #             should be minimized (TRUE) or maximized (FALSE)
  #
  # Returns:
  #   see 'ret.expl.vars'


  
  # Short description of the algorithm:
  #
  # - start with an empty model (or with the required expl. variables)
  # - fit all models with all possible single expl. variables
  # - pick the model with the best value of the criterion if it has improved
  # - add remaining variables to the existing model, one at a time,
  #   and pick the model with the best value of the criterion
  # - repeat the procedure until addition of any of the remaining variables
  #   does not improve the criterion
  
    
  # sanity check
  .check.validity(d.frame=dframe, resp.var=resp, inc.vars=inc);

  # Extract all variables' names
  df.vars <- as.list(names(dframe));
  
  # And remove the response variable
  df.vars <- df.vars[ df.vars != resp ];
  
  # List of selected explanatory variables - initially empty
  expl.vars <- list();
  
  # If the selected criteria is Mallows' Cp, the MSE of the full model is needed
  msef <- 0.0;
  if ( "mallowsCp" == critf )
  {
    msef <- .crit.msef(dframe, resp);
  }
  
  # Only applicable if 'inc' is not empty
  if ( !is.null(inc) )
  {
    # Append required variables to 'expl.vars'
    expl.vars <- c(expl.vars, inc);
    
    # And remove all required variables
    df.vars <- df.vars[ !(df.vars %in% inc) ];
    
    # Update the initial value of the criterion:
    mdl <- lm ( .create.lm.formula(resp.var=resp, vars=expl.vars), data=dframe);
  }
  else
  {
    # Set the current value of criterion to an empty model (with the interceptor only)
    mdl <- lm( .create.lm.formula(resp.var=resp, vars="1"), data=dframe );
  }
  
  crit <- .crit.criterion( critf, mdl, msef );
  
  # Iterate the loop until 'df.vars' is empty or it is interrupted beforehand
  # due to no improved of the criterion
  while ( length(df.vars) > 0 )
  {
    # a vector of all models' criteria
    cs <- sapply( df.vars, function(v) 
    {
      mdl <- lm(.create.lm.formula(resp, c(expl.vars, v)), data=dframe );
      return( .crit.criterion( critf, mdl, msef ) );
    } );
    
    # find the model with the best criterion and check if this
    # value has improved w.r.t. the current model
    if ( TRUE==minimize )
    {
      idx <- which.min(cs);
    }
    else
    {
      idx <- which.max(cs);
    }
    
    crit.best <- cs[idx];
    if ( (TRUE==minimize && crit.best<crit) ||
         (FALSE==minimize && crit.best>crit) )
    {
      # Update the current criterion
      crit <- crit.best;

      # Append the variable to 'expl.vars'
      expl.vars <- c(expl.vars, df.vars[idx]);
      
      # and remove it from 'df.vars'
      df.vars[ idx ] <- NULL;
    }
    else
    {
      # The criterion has not improved, terminate the while loop
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



.stepwise.bck <- function(dframe, resp, inc, ret.expl.vars, critf, minimize)
{
  # Performs the stepwise regression algorithm, based on backwards elimination 
  # of predictors that optimizes the selected criterion.
  #
  # The following criteria are currently supported:
  # - adjusted R^2 (crit="adjR2")
  # - Mallows' Cp (crit="mallowsCp")
  # - Aikake's Information Criterion (crit="aic")
  # - Bayesian Information Criterion (crit="bic")
  #
  # Args:
  #   dframe: data frame
  #   resp: name of the response variable as a character value
  #   inc: an optional list of names of explanatory variables that 
  #        must be included into the final model
  #   ret.expl.vars: if TRUE, the function will return a list of selected
  #                  exlanatory variables, otherwise a model including
  #                  the selected variables
  #   crit: criteria function (see above for more details)
  #   minimize: a logical value indicating whether the selected criterion
  #             should be minimized (TRUE) or maximized (FALSE)
  #
  # Returns:
  #   see 'ret.expl.vars'
  
  
  
  # Short description of the algorithm:
  #
  # - start with a full model (with all variables included)
  # - drop one variable at a time, fit a model, record the criterion value for each one
  # - pick the model with the best value of the criterion
  # - repeat the procedure until the criterion does not improve anymore
  
  
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
  
  # Current value of the criterion, initially set to the value of the full model
  mdl <- lm( .create.lm.formula( resp.var=resp, vars="."), data=dframe);
  
  # If the selected criteria is Mallows' Cp, MSE of the full model will be needed:
  msef <- 0.0
  if ( "mallowsCp"==critf )
  {
    msef <- .crit.msef(full.mdl=mdl); 
  }
  
  crit <- .crit.criterion( critf, mdl, msef );
  
  # Iterate the loop until 'df.vars' is empty or it is interrupted beforehand
  # due to no improvement of criterion
  while ( length(expl.vars) > 0 )
  {
    cs <- sapply(expl.vars, function(v)
    {
      # A temporary list of predictors w/o 'v':
      pred <- expl.vars[ expl.vars != v ];
      mdl <- lm( .create.lm.formula(resp, c(pred, inc)), data=dframe );
      return( .crit.criterion( critf, mdl, msef ) ); 
    } );
    
    # find the model with the best value of criterion and check if this
    # value has improved w.r.t. the current model
    if ( TRUE == minimize )
    {
      idx <- which.min(cs)
    }
    else
    {
      idx <- which.max(cs);
    }
    crit.best <- cs[idx];
    if ( (TRUE==minimize && crit.best<crit) ||
         (FALSE==minimize && crit.best>crit) )
    {
      # Update the current criterion
      crit <- crit.best;
    
      # Remove the variable from 'df.vars'
      expl.vars[ idx ] <- NULL;
    }
    else
    {
      # The criterion has not improved, terminate the while loop
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



stepwise.fwd.adjR2 <- function(dframe, resp, inc=NULL, ret.expl.vars=TRUE)
{
  # Performs the stepwise regression algorithm, based on forward selection 
  # of predictors and using the adjusted R^2 as criterion.
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
  
  return( .stepwise.fwd(
        dframe = dframe, 
        resp = resp, 
        inc = inc, 
        ret.expl.vars = ret.expl.vars, 
        critf = "adjR2",
        minimize = FALSE ) );
}



stepwise.bck.adjR2 <- function(dframe, resp, inc=NULL, ret.expl.vars=TRUE)
{
  # Performs the stepwise regression algorithm, based on backwards elimination 
  # of predictors and using the adjusted R^2 as criterion.
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
  
  return( .stepwise.bck(
        dframe = dframe, 
        resp = resp, 
        inc = inc, 
        ret.expl.vars = ret.expl.vars, 
        critf = "adjR2",
        minimize = FALSE ) );
}



stepwise.fwd.mallowsCp <- function(dframe, resp, inc=NULL, ret.expl.vars=TRUE)
{
  # Performs the stepwise regression algorithm, based on forward selection 
  # of predictors and using the Mallows' Cp as criterion.
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
  
  return( .stepwise.fwd(
    dframe = dframe, 
    resp = resp, 
    inc = inc, 
    ret.expl.vars = ret.expl.vars, 
    critf = "mallowsCp",
    minimize = TRUE ) );
}



stepwise.bck.mallowsCp <- function(dframe, resp, inc=NULL, ret.expl.vars=TRUE)
{
  # Performs the stepwise regression algorithm, based on backwards elimination 
  # of predictors and using the Mallows' Cp as criterion.
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
  
  return( .stepwise.bck(
    dframe = dframe, 
    resp = resp, 
    inc = inc, 
    ret.expl.vars = ret.expl.vars, 
    critf = "mallowsCp",
    minimize = TRUE ) );
}



stepwise.fwd.aic <- function(dframe, resp, inc=NULL, ret.expl.vars=TRUE)
{
  # Performs the stepwise regression algorithm, based on forward selection 
  # of predictors and using the Aikake's Information Criterion (AIC) as criterion.
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
  
  return( .stepwise.fwd(
    dframe = dframe, 
    resp = resp, 
    inc = inc, 
    ret.expl.vars = ret.expl.vars, 
    critf = "aic",
    minimize = TRUE ) );
}



stepwise.bck.aic <- function(dframe, resp, inc=NULL, ret.expl.vars=TRUE)
{
  # Performs the stepwise regression algorithm, based on backwards elimination 
  # of predictors and using the Aikake's Information Criterion (AIC) as criterion.
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
  
  return( .stepwise.bck(
    dframe = dframe, 
    resp = resp, 
    inc = inc, 
    ret.expl.vars = ret.expl.vars, 
    critf = "aic",
    minimize = TRUE ) );
}



stepwise.fwd.bic <- function(dframe, resp, inc=NULL, ret.expl.vars=TRUE)
{
  # Performs the stepwise regression algorithm, based on forward selection 
  # of predictors and using the Bayesian Information Criterion (BIC) as criterion.
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
  
  return( .stepwise.fwd(
    dframe = dframe, 
    resp = resp, 
    inc = inc, 
    ret.expl.vars = ret.expl.vars, 
    critf = "bic",
    minimize = TRUE ) );
}



stepwise.bck.bic <- function(dframe, resp, inc=NULL, ret.expl.vars=TRUE)
{
  # Performs the stepwise regression algorithm, based on backwards elimination 
  # of predictors and using the Bayesian Information Criterion (BIC) as criterion.
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
  
  return( .stepwise.bck(
    dframe = dframe, 
    resp = resp, 
    inc = inc, 
    ret.expl.vars = ret.expl.vars, 
    critf = "bic",
    minimize = TRUE ) );
}



stepwise.fwd.pval <- function(dframe, resp, alpha=0.05, inc=NULL, ret.expl.vars=TRUE)
{
  # Performs the stepwise regression algorithm, based on forward selection 
  # of predictors and using coefficients' significance as criterion.
  #
  # Possibly the data frame should be prepocessed prior to passing to this
  # function. For instance, undesired variables are recommended to be removed
  # from the data frame, values of factor variables must be converted to strings,
  # desired interactions should be done beforehand and appended to the data frame, etc.
  #
  # Args:
  #   dframe: data frame
  #   resp: name of the response variable as a character value
  #   alpha: maximum significance level of coefficients' p-values
  #   inc: an optional list of names of explanatory variables that 
  #        must be included into the final model
  #   ret.expl.vars: if TRUE, the function will return a list of selected
  #                  exlanatory variables, otherwise a model including
  #                  the selected variables
  #
  # Returns:
  #   see 'ret.expl.vars'
  
  # sanity check
  .check.validity(d.frame=dframe, resp.var=resp, alpha=alpha, inc.vars=inc);
  
  # nr. of levels for each variable
  var.levels <- .pval.count.levels(dframe);
  
  # Extract all variables' names
  df.vars <- as.list(names(dframe));
  
  # And remove the response variable
  df.vars <- df.vars[ df.vars != resp ];
  
  # Also remove any inc. variables
  if ( !is.null(inc) )
  {
    df.vars <- df.vars[ !(df.vars %in% inc) ];
  }
  
  # List of selected explanatory variables - initially empty
  expl.vars <- list();
  
  # Append 'inc' to 'expl.vars'
  expl.vars <- c(expl.vars, inc);
  
  while ( length(df.vars) > 0 )
  {
    # a vector of coefficients' p-values
    p.vals <- sapply(df.vars, function(v)
    {
      # The function '.pval.getmin' requires that the variable of interest ('v')
      # is the first in the list of variables to fit a model
      mdl <- summary(
          lm ( .create.lm.formula( resp, c(v, expl.vars) ), data=dframe) );
      
      return( .pval.getmin(mdl, var.levels[v]) );
    } );
    
    # Find the variable with the minimum p-value
    idx <- which.min(p.vals);
    
    # And make sure it is statistically significant
    if ( p.vals[idx] < alpha )
    {
      # If it is signidficant, append it to 'expl.vars'
      expl.vars <- c(expl.vars, df.vars[idx]);
      
      # and remove it from 'df.vars'
      df.vars[ idx ] <- NULL;
    }
    else
    {
      # Otherwise quit the loop
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
