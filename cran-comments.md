## Submission
* New version 0.9.5.0 (nickname: "95% confident")

## Test environments
* local WIN-64x install, R 4.0.3
* win-builder devel 
* win-builder release
* Ubuntu 16.04.6 LTS R release (travis-ci.org)

## R CMD check results

* There were no ERRORs and no WARNINGs.

* There was 1 NOTE from Travis-CI: Docs folder exceeds 1 Mb: as this package
is about creating plots, it is difficult to generate documentations
with less than 1 Mb (that would represents only 4 to 5 examples). 

* There was 1 NOTE from win-builder about the maintainer.

## Downstream dependencies

* All packages that I could install passed.

