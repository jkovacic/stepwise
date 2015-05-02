##About
A simple R library for stepwise selection of the "best" linear model,
depending on the specified criteria, e.g. the adjusted R^2 or p-values.

##Supported methods
* forward selection, based on adjusted R^2 (function `stepwise.fwd.adjR2`)
* backwards elimination, based on adjusted R^2 (function `stepwise.bck.adjR2`)
* forward selection, based on coefficients' p-values (_not implemented yet_)
* backwards elimination, based on coefficients' p-values (_not implemented yet_)

##Usage
Just call the command `source("stepwise.R")` and then call any function
enumerated in the previous section. For more info, see "documentation"
comments in headers of those functions.

Alternatively see the file `test.R` that performs basic unit tests.

##License
All files are are licensed
under the [Apache 2.0 license](http://www.apache.org/licenses/LICENSE-2.0).

##Author
The author of the library is Jernej Kova&#x010d;i&#x010d;.
