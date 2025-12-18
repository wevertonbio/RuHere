# <img src="man/figures/logo.png" align="right" width="30%" height="30%"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/wevertonbio/RuHere/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wevertonbio/RuHere/actions/workflows/R-CMD-check.yaml)
[![License](https://img.shields.io/badge/license-GPL%20(%3E=%203)-lightgrey.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)
<!-- badges: end -->


**RuHere: Are your species records really where they say they are?**
================
> *Check them using metadata and specialists’ range information!*

Authors: Weverton C. F. Trindade and Fernanda S. Caron

## **Package overview**

Primary biodiversity data documenting species distributions are central to understand and conserve biodiversity. A major challenge in using these data is the presence of erroneous or overly imprecise geographic coordinates associated with occurrence records. Here, we present **RuHere**, an R package designed to manage species occurrence data, flag potential errors, and support the iterative exploration of problematic records. The package supports robust preparation of occurrence datasets for ecological and conservation applications such as ecological niche modelling, with its main strength being the flexibility and control it provides to deal with and explore potentially erroneous records.

## **Workflow in RuHere**

The RuHere package facilitates several key steps in species occurrence data preparation and validation:

 - **Data Acquisition**: Download species occurrences from multiple global databases.
 - **Standardization**: Merge and standardize disparate datasets into a unified format.
 - **Metadata Flagging**: Identify problematic records using associated metadata information.
 - **Expert Validation**: Flag records using specialist range information sourced from external databases.
 - **Bias Mitigation**: Reduce spatial sampling bias through record thinning.
 - **Exploration**: Visualize and investigate flagged issues within the final datasets.

The main functions of the package are presented in the figure below:

<div class="figure" style="text-align: center">

<img src="man/figures/workflow.png" alt="Figure 1. Overview of the RuHere workflow for species occurrence data preparation and validation" width="761" />
<p class="caption">

Figure 1. Overview of the RuHere workflow for species occurrence data preparation and validation
</p>

</div>

## **Package website**

See the package website (<https://wevertonbio.github.io/RuHere/>) for
further functions explanation and vignettes.

## **Installing the package**

Note: Internet connection is required to install the package.

To install the latest release of RuHere use the following line of code:

``` r
# Installing from CRAN 
#install.packages("RuHere")  # in progress
```

The development version of RuHere can be installed using the code below.

``` r
# Installing and loading packages
if(!require(remotes)){
  install.packages("remotes")
}

# To install the package use
remotes::install_github("wevertonbio/RuHere")

# To install the package and its vignettes use (if needed use: force = TRUE)  
# remotes::install_github("wevertonbio/RuHere", build_vignettes = TRUE) # in progress
```
