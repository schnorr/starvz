## Resubmission

This is a resubmission of a package that was previously archived on CRAN.

The package was archived because the SystemRequirements field in the DESCRIPTION
file contained an invalid C++ specification ("C++"), which resulted in
installation warnings on multiple CRAN platforms.

This issue has been fixed by correcting the SystemRequirements field to use a
valid C++ standard specification. The package now installs cleanly and passes
R CMD check --as-cran.

## R CMD check results

0 errors | 0 warnings | 2 notes

Notes:
- This is a resubmission of an archived package.
