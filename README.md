# prescribeR

## Introduction
prescribeR is an R package developed as part of my PhD work, the main aim of which was to create a set of 
flexible, reusable functions for generating common drug exposure variables based on routinely collected 
prescribing data for use in pharmacoepidemiological and pharmacovigilance research, with a view to providing 
structured and standardised methods for quantifying drug exposure and reporting on how data were prepared to
aid in the reproducibility and transparency of research.

The package was developed primarily for use on Scottish prescribing data, but is largely content neutral,
and uses functions from the tidyverse family of packages to manipulate the data. At present, the package 
contains functions for deriving variables describing ever use vs. never use, use at specified time points or
within time periods, prescription durations based on dosage instructions or assumptions and persistence calculated
using the refill gap method.

## Documentation
At present, the main form of documentation for this package can be found within the R manual files,
which provide descriptions of the individual functions, their arguments and their outputs. A wiki is 
currently under construction to provide more detail and examples for each function.

### Citing prescribeR
[![DOI](https://zenodo.org/badge/175052538.svg)](https://zenodo.org/badge/latestdoi/175052538)
