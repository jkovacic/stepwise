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
# A short demo of all "public" functions in the file 'stepwise.R'
#

path <- ""              # Append a path to 'rfile' if necessary
rfile <- "stepwise.R"   # The actual name of the file

# Load the "library" file
source( paste(path, rfile, sep="") )

# Load the data frame
data(mtcars)
dtframe <- mtcars
rsp <- "mpg"
incl <- list("drat", "vs")

cat("= = = Forward selection, adjusted R^2 as criterion = = =\n")

cat("\n= A list of variables that maximize the criterion, with \'drat\' and \'vs\' required: =\n\n")
mdl <- stepwise.fwd.adjR2(dtframe, rsp, incl)
print(mdl)

cat("= A fitted model with no required expl. variables: =\n")
mdl <- stepwise.fwd.adjR2(dtframe, rsp, ret.expl.vars=FALSE)
print(mdl)


cat("\n= = = Backwards elimination, adjusted R^2 as criterion = = =\n")

cat("\n= A list of variables that maximize the criterion, with \'drat\' and \'vs\' required: =\n\n")
mdl <- stepwise.bck.adjR2(dtframe, rsp, incl)
print(mdl)

cat("= A fitted model with no required expl. variables: =\n")
mdl <- stepwise.bck.adjR2(dtframe, rsp, ret.expl.vars=FALSE)
print(mdl)


cat("\n= = = Forward selection, AIC as criterion = = =\n\n")

cat("= A fitted model with no required expl. variables: =\n")
mdl <- stepwise.fwd.aic(dtframe, rsp, ret.expl.vars=FALSE)
print(mdl)


cat("\n= = = Backwards elimination, AIC as criterion = = =\n\n")

cat("= A fitted model with no required expl. variables: =\n")
mdl <- stepwise.bck.aic(dtframe, rsp, ret.expl.vars=FALSE)
print(mdl)


cat("\n= = = Forward selection, BIC as criterion = = =\n\n")

cat("= A fitted model with no required expl. variables: =\n")
mdl <- stepwise.fwd.bic(dtframe, rsp, ret.expl.vars=FALSE)
print(mdl)


cat("\n= = = Backwards elimination, BIC as criterion = = =\n\n")

cat("= A fitted model with no required expl. variables: =\n")
mdl <- stepwise.bck.bic(dtframe, rsp, ret.expl.vars=FALSE)
print(mdl)


cat("\n= = = Forward selection, Mallows' Cp as criterion = = =\n\n")

cat("= A fitted model with no required expl. variables: =\n")
mdl <- stepwise.fwd.mallowsCp(dtframe, rsp, ret.expl.vars=FALSE)
print(mdl)


cat("\n= = = Backwards elimination, Mallows' Cp as criterion = = =\n\n")

cat("= A fitted model with no required expl. variables: =\n")
mdl <- stepwise.bck.mallowsCp(dtframe, rsp, ret.expl.vars=FALSE)
print(mdl)


cat("\n= = =  Forward selection, p-values as criteria = = =\n\n")

cat("= A list of significant variables, with \'drat\' and \'vs\' required: =\n\n")
mdl <- stepwise.fwd.pval(dtframe, rsp, incl, alpha=0.05, ret.expl.vars=TRUE)
print(mdl)

cat("= A fitted model with no required expl. variables: =\n")
mdl <- stepwise.fwd.pval(dtframe, rsp, alpha=0.05, ret.expl.vars=FALSE)
print(mdl)


cat("\n= = = Backwards elimination, p-values as criteria = = =\n\n")

cat("= A list of significant variables, with \'drat\' and \'vs\' required: =\n\n")
mdl <- stepwise.bck.pval(dtframe, rsp, alpha=0.05, inc=incl, ret.expl.vars=TRUE)
print(mdl)

cat("= A fitted model with no required expl. variables: =\n")
mdl <- stepwise.bck.pval(dtframe, rsp, alpha=0.05, ret.expl.vars=FALSE)
print(mdl)
