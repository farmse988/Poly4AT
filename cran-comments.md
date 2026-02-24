

## Resubmission as Version 1.0.2
This is a resubmission of Poly4AT as version 1.0.2. 
The previous version was removed from CRAN because the package (dependency) 'leaflet.extras' is no longer available on CRAN. 
In this release, 'leaflet.extras' has been removed, and any features relying on it (e.g., fullscreen map control) have been removed from the package. 
All remaining functionality has been tested, and R CMD check passes with 0 errors and 0 warnings.
Changes are documented in NEWS.md under version 1.0.2.


## Resubmission
This is a resubmission. In this version I have:
 
* Updated the DESCRIPTION title to use single quotes for 'INVEKOS'.
* Updated the DESCRIPTION description to use single quotes for 'shiny' and 'INVEKOS'.
* Added a web reference for the INVEKOS API in the DESCRIPTION.

I have tested the package on multiple platforms and verified that all checks pass with `devtools::check()`.





## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
