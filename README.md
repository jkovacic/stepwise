##About
A simple R library for stepwise selection of the "best" linear model,
depending on the selected criteria, such as adjusted R<sup>2</sup>,
p-values et al.

##Supported methods
* forward selection, based on adjusted R<sup>2</sup> (function `stepwise.fwd.adjR2`)
* backwards elimination, based on adjusted R<sup>2</sup> (function `stepwise.bck.adjR2`)
* forward selection, based on coefficients' p-values (_not implemented yet_)
* backwards elimination, based on coefficients' p-values (_not implemented yet_)

##Usage
Just call the command `source('stepwise.R')` and then call any function
mentioned in the previous section. For more info, see "documentation"
comments in headers of those functions.

Alternatively see the file `test.R` that performs basic unit tests.

##About data frames
The functions do not (pre)process provided data frames in any way. In other
words, the data frame must be preprocessed beforehand if required. This
includes all desired transformations of variables, columns with factor
variables must be provided as vectors of strings, additional columns (such
as powers, interactions, etc.) must be added. It is also strongly recommended
that undesired variables are removed from the data frame.

##License
All files are are licensed
under the [Apache 2.0 license](http://www.apache.org/licenses/LICENSE-2.0).

##Author
The author of the library is Jernej Kova&#x010d;i&#x010d;.
