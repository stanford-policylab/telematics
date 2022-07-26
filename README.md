# Measuring Racial and Ethnic Disparities in Traffic Enforcment with Large Scale Telematics Data
###### William Cai, Johann Gaebler, Justin Kaashoek, Lisa Pinals, Samuel Madden, Sharad Goel
---
## Description

This repo contains the code used to clean, process, and analyze the data used
in "Measuring Racial and Ethnic Disparities in Traffic Enforcement with Large
Scale Telematics Data." All figures and tables appearing in the paper were
produced by knitting the `main.Rmd` R-markdown file.

## Replication

Due to privacy concerns we are unable to release telematics data from Cambridge
Mobile Telematics, which are used in `main.Rmd`. However, the TomTom telematics
data included in this repo are sufficient to replicate our main results.

To facilitate replication, this repository uses the `renv` package to manage
`R` package versioning.

First, from this directory, run
```bash
Rscript -e "renv::restore(rebuild = TRUE)"
```
to install the required packages. If installation fails, you may need to
install the following additional libraries:

* [udunits](https://www.unidata.ucar.edu/software/udunits/),
* [GDAL](https://gdal.org).

Finally, knit the `rep.Rmd` R markdown file as follows:
```bash
Rscript -e "knitr::knit('rep.Rmd')"
```
