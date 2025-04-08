<!-- README.md is generated from README.Rmd. Please edit that file -->

# GAPsurvey <a href={https://afsc-gap-products.github.io/GAPsurvey}><img src="man/figures/logo.png" align="right" width=139 height=139 alt="logo."/>

> This code is always in development

## This code is primarally maintained by:

**Emily Markowitz** (Emily.Markowitz AT noaa.gov; @EmilyMarkowitz-NOAA)

**Sean Rohan** (Sean.Rohan AT noaa.gov; @MargaretSiple-NOAA)

Alaska Fisheries Science Center,

National Marine Fisheries Service,

National Oceanic and Atmospheric Administration,

Seattle, WA 98195

## Table of contents

>      - [**At-sea data management tools for RACE GAP surveys**](#*at-sea-data-management-tools-for-race-gap-surveys*)
>
> -   [*Make sure the necessary packages are
>     installed*](#make-sure-the-necessary-packages-are-installed)
> -   [*Or*](#or)
>     -   [*User Resources*](#user-resources)
>     -   [*Cite this data*](#cite-this-data)
> -   [*Relevant publications*](#relevant-publications)
> -   [*Suggestions and Comments*](#suggestions-and-comments)
>     -   [*R Version Metadata*](#r-version-metadata)
>     -   [*NOAA README*](#noaa-readme)
>     -   [*NOAA License*](#noaa-license)

### *At-sea data management tools for RACE GAP surveys*

    #> Warning: `git_branch_default()` was deprecated in usethis 2.1.0.
    #> ℹ Please use `git_default_branch()` instead.
    #> ℹ The deprecated feature was likely used in the badger package.
    #>   Please report the issue at <https://github.com/GuangchuangYu/badger/issues>.
    #> This warning is displayed once every 8 hours.
    #> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

[![](https://img.shields.io/badge/devel%20version-2024.04.05-blue.svg)](https://github.com/afsc-gap-products/GAPsurvey)
[![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![](https://img.shields.io/github/last-commit/afsc-gap-products/GAPsurvey.svg)](https://github.com/afsc-gap-products/GAPsurvey/commits/main)

## Make sure the necessary packages are installed

    library(devtools)

    devtools::install_github("afsc-gap-products/GAPsurvey")
    # Or
    remotes::install_github("afsc-gap-products/GAPsurvey@main")

    library(GAPsurvey)

## User Resources

-   [GitHub
    repository](https://github.com/afsc-gap-products/gap_products).

-   [Access Tips and Documentation for All Production
    Data](https://afsc-gap-products.github.io/gap_products/)

-   [Fisheries One Stop Shop
    (FOSS)](https://www.fisheries.noaa.gov/foss)

-   [Groundfish Assessment Program Bottom Trawl
    Surveys](https://www.fisheries.noaa.gov/alaska/science-data/groundfish-assessment-program-bottom-trawl-surveys)

-   [AFSC’s Resource Assessment and Conservation Engineering
    Division](https://www.fisheries.noaa.gov/about/resource-assessment-and-conservation-engineering-division)

-   [Survey code
    books](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual)

-   [Publications and Data
    Reports](https://repository.library.noaa.gov/)

-   [Research Surveys conducted at
    AFSC](https://www.fisheries.noaa.gov/alaska/ecosystems/alaska-fish-research-surveys)

## Cite this data

Use the below [bibtext
citations](%22https://afsc-gap-products.github.io/GAPsurvey/blob/main/code/CITATION.bib%22)
for citing the package created and maintained in this repo. Add “note =
{Accessed: mm/dd/yyyy}” to append the day this data was accessed.

    #> @misc{GAPsurvey,
    #>   author = {{NOAA Fisheries Alaska Fisheries Science Center, Goundfish Assessment Program}},
    #>   year = {2024},
    #>   title = {AFSC Goundfish Assessment Program at-Sea data management tools for RACE GAP surveys},
    #>   howpublished = {https://www.fisheries.noaa.gov/alaska/science-data/groundfish-assessment-program-bottom-trawl-surveys},
    #>   publisher = {{U.S. Dep. Commer.}},
    #>   copyright = {Public Domain}
    #> }

# Relevant publications

    source("https://raw.githubusercontent.com/afsc-gap-products/citations/main/cite/current_data_tm.r") # srvy_cite 

**Learn more about these surveys** \[@2023NEBS; @2023NEBS; @GOA2023;
@AI2022; @RN979; @SAPcrab2024\].

# Suggestions and Comments

If you see that the data, product, or metadata can be improved, you are
invited to create a [pull
request](https://github.com/afsc-gap-products/GAPsurvey/pulls), [submit
an issue to the GitHub
organization](https://github.com/afsc-gap-products/data-requests/issues),
or [submit an issue to the code’s
repository](https://github.com/afsc-gap-products/GAPsurvey/issues).

## R Version Metadata

    sessionInfo()
    #> R version 4.4.3 (2025-02-28 ucrt)
    #> Platform: x86_64-w64-mingw32/x64
    #> Running under: Windows 10 x64 (build 19045)
    #> 
    #> Matrix products: default
    #> 
    #> 
    #> locale:
    #> [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                           LC_TIME=English_United States.utf8    
    #> 
    #> time zone: America/Los_Angeles
    #> tzcode source: internal
    #> 
    #> attached base packages:
    #> [1] stats4    stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #>  [1] badger_0.2.4      fontawesome_0.5.3 ggspatial_1.1.9   pkgdown_2.1.1     roxygen2_7.3.2    RODBC_1.3-26      sp_2.2-0          httr_1.4.7        jsonlite_2.0.0    gapindex_3.0.2    gapctd_2.1.8      plotly_4.10.4     interp_1.1-6      bbmle_1.0.25.1    oce_1.8-3         gsw_1.2-0        
    #> [17] coldpool_3.4-3    stringr_1.5.1     reshape2_1.4.4    lubridate_1.9.4   fields_16.3.1     spam_2.11-1       gstat_2.1-3       ggthemes_5.1.0    akgfmaps_4.0.3    terra_1.8-42      stars_0.6-8       abind_1.4-8       sf_1.0-20         here_1.0.1        data.table_1.17.0 janitor_2.2.1    
    #> [33] tibble_3.2.1      ggplot2_3.5.1     readr_2.1.5       viridis_0.6.5     viridisLite_0.4.2 readxl_1.4.5      tidyr_1.3.1       magrittr_2.0.3    dplyr_1.1.4       plyr_1.8.9        remotes_2.5.0     devtools_2.4.5    usethis_3.1.0    
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] RColorBrewer_1.1-3  sys_3.4.3           rstudioapi_0.17.1   dlstats_0.1.7       rmarkdown_2.29      fs_1.6.5            vctrs_0.6.5         memoise_2.0.1       askpass_1.2.1       gh_1.4.1            htmltools_0.5.8.1   curl_6.2.2          raster_3.6-32       cellranger_1.1.0    KernSmooth_2.23-26 
    #> [16] desc_1.4.3          htmlwidgets_1.6.4   httr2_1.1.2         zoo_1.8-13          cachem_1.1.0        mime_0.13           lifecycle_1.0.4     pkgconfig_2.0.3     Matrix_1.7-2        R6_2.6.1            fastmap_1.2.0       shiny_1.10.0        snakecase_0.11.1    digest_0.6.37       numDeriv_2016.8-1.1
    #> [31] colorspace_2.1-1    rprojroot_2.0.4     pkgload_1.4.0       timechange_0.3.0    compiler_4.4.3      proxy_0.4-27        intervals_0.15.5    withr_3.0.2         DBI_1.2.3           pkgbuild_1.4.7      maps_3.4.2.1        MASS_7.3-64         openssl_2.3.2       rappdirs_0.3.3      sessioninfo_1.2.3  
    #> [46] classInt_0.4-11     tools_4.4.3         units_0.8-7         httpuv_1.6.15       glue_1.8.0          promises_1.3.2      grid_4.4.3          generics_0.1.3      gtable_0.3.6        tzdb_0.5.0          class_7.3-23        hms_1.1.3           xml2_1.3.8          pillar_1.10.2       yulab.utils_0.2.0  
    #> [61] later_1.4.1         lattice_0.22-6      FNN_1.1.4.1         deldir_2.0-4        tidyselect_1.2.1    rvcheck_0.2.1       miniUI_0.1.1.1      knitr_1.50          gitcreds_0.1.2      gridExtra_2.3       xfun_0.52           credentials_2.0.2   stringi_1.8.7       lazyeval_0.2.2      yaml_2.3.10        
    #> [76] evaluate_1.0.3      codetools_0.2-20    BiocManager_1.30.25 cli_3.6.3           xtable_1.8-4        munsell_0.5.1       spacetime_1.3-3     gert_2.1.5          Rcpp_1.0.14         readtext_0.91       bdsmatrix_1.3-7     parallel_4.4.3      ellipsis_0.3.2      dotCall64_1.2       profvis_0.4.0      
    #> [91] urlchecker_1.0.1    mvtnorm_1.3-3       scales_1.3.0        xts_0.14.1          e1071_1.7-16        crayon_1.5.3        purrr_1.0.4         rlang_1.1.5

## NOAA README

This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.

## NOAA License

Software code created by U.S. Government employees is not subject to
copyright in the United States (17 U.S.C. §105). The United
States/Department of Commerce reserve all rights to seek and obtain
copyright protection in countries other than the United States for
Software authored in its entirety by the Department of Commerce. To this
end, the Department of Commerce hereby grants to Recipient a
royalty-free, nonexclusive license to use, copy, and create derivative
works of the Software outside of the United States.

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" alt="NOAA Fisheries" height="75"/>

[U.S. Department of Commerce](https://www.commerce.gov/) | [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) |
[NOAA Fisheries](https://www.fisheries.noaa.gov/)
