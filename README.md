[![Travis-CI Build Status](https://travis-ci.org/poissonconsulting/creelr.png?branch=master)](https://travis-ci.org/poissonconsulting/creel)
[![DOI](https://zenodo.org/badge/10250/poissonconsulting/creelr.svg)](https://zenodo.org/badge/latestdoi/10250/poissonconsulting/creelr)

# creelr
 
A prototype R package to Analyse Angler Creel Survey Data. Currently only the
Traditional Access Design for One Access Site is implemented.

## Installation

To install and load the latest version of creelr:

```
install.packages("drat")
drat::addRepo("poissonconsulting")
install.packages("creelr", dependencies = TRUE)
library(creelr)
```

Note if you get the error
`Line starting 'The page you're look ...' is malformed!`
then you will have to temporarily disable [secure package downloads](https://support.rstudio.com/hc/en-us/articles/206827897-Secure-Package-Downloads-for-R).

## Information

For more information type `?creelr` at the R console after installing and
loading the package.
    
## Contributions

You are welcome to:

* submit suggestions and bug-reports at: https://github.com/poissonconsulting/creelr/issues
* send a pull request on: https://github.com/poissonconsulting/creelr/
