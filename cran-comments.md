##  Submission RuHere 1.0.0
This is the first submission of version 1.0.0.

## Test environments
* Windows 11, R 4.5.1 (local)
* MacOS 15.7.3, R release (GitHub Actions)
* Windows 10.0.26100, R release (GitHub Actions)
* Ubuntu 24.04.3 LTS, R release (GitHub Actions)
* Ubuntu 24.04.3 LTS, R devel (GitHub Actions)
* Ubuntu 24.04.3 LTS, R oldrel-1 (GitHub Actions)

## R CMD check results
There were no ERRORs:
There were no WARNINGs:
There were 1 NOTE:
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Weverton Trindade <wevertonf1993@gmail.com>'
A Note that reminds CRAN maintainers to check that the submission comes actually from his maintainer and not anybody else.

* This is a new release.

## Resubmission RuHere 1.0.1
> Please do not start the description with "An R package", package name,
title or similar.

* Removed the redundant package name and the phrase "An R package" from the start of the Description field in the DESCRIPTION file.

> The Description field is intended to be a (one paragraph) description of
what the package does and why it may be useful. Please add more details
about the package functionality and implemented methods in your
Description text.
For more details:
<https://contributor.r-project.org/cran-cookbook/general_issues.html#description-length>

* Expanded the Description field to detail the package's six-step workflow, including metadata flagging and the integration of expert-derived distribution data for identifying spatial errors.

> If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'https:' and angle brackets for
auto-linking. (If you want to add a title as well please put it in
quotes: "Title")
For more details:
<https://contributor.r-project.org/cran-cookbook/description_issues.html#references>

* Added the reference for the methods in the Description field of the DESCRIPTION file.

> \dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user. That's why wrapping examples in \dontrun{} adds the comment ("# Not run:") as a warning for the user. Does not seem necessary. Please replace \dontrun with \donttest. Please unwrap the examples if they are executable in < 5 sec, or replace dontrun{} with \donttest{}.Please put functions which download data in \donttest{}. For more details: <https://contributor.r-project.org/cran-cookbook/general_issues.html#structuring-of-examples>

* Updated the documentation by replacing `\dontrun{}` with `\donttest{}` in all functions involving data downloads (such as `wcvp_here()` and `prepare_gbif_download()`) or interactive visualizations (`map_here()`). The use of `\dontrun{}` was reserved for functions relying in credential-setting, such as `set_gbif_credentials()`, `set_specieslink_credentials()`, `get_specieslink()`, `iucn_here()`, `request_gbif()`, and `import_gbif()`, as these fall under the policy exception for requiring private API keys.

> Please ensure that your functions do not write by default or in your
examples/vignettes/tests in the user's home filespace (including the
package directory and getwd()). This is not allowed by CRAN policies.
Please omit any default path in writing functions. In your
examples/vignettes/tests you can write to tempdir().
For more details:
<https://contributor.r-project.org/cran-cookbook/code_issues.html#writing-files-and-directories-to-the-home-filespace>
-> R/set_gbif_credentials.R; R/set_iucn_credentials.R;
R/set_specieslink_credentials.R

* Refactored `set_gbif_credentials()`, `set_iucn_credentials()`, and `set_specieslink_credentials()` to avoid writing to the user's home filespace by default. By default, credentials are now only set for the current session via `Sys.setenv()`. Permanent storage in .Renviron now requires explicit user consent by setting the new argument `permanently = TRUE`.

> Please make sure that you do not change the user's options, par or
working directory. If you really have to do so within functions, please
ensure with an *immediate* call of on.exit() that the settings are reset
when the function is exited.
-> R/wcvp_here.R
e.g.:
...
old <- options() # code line i
on.exit(options(old)) # code line i+1
...
options(...) # somewhere after
...
e.g.:
If you're not familiar with the function, please check ?on.exit. This
function makes it possible to restore options before exiting a function
even if the function breaks. Therefore it needs to be called immediately
after the option change within a function.
For more details:
<https://contributor.r-project.org/cran-cookbook/code_issues.html#change-of-options-graphical-parameters-and-working-directory>

* Updated `wcvp_here.R` to include an immediate `on.exit(options(timeout = original_timeout))` call.

> Please always make sure to reset to user's options(), working directory
or par() after you changed it in examples and vignettes and demos.
e.g.:
oldpar <- par(mfrow = c(1,2))
...
par(oldpar)
-> inst/doc/flagging_records_species_list.R
For more details:
<https://contributor.r-project.org/cran-cookbook/code_issues.html#change-of-options-graphical-parameters-and-working-directory>

* Updated the package vignette `flagging_records_species_list.Rmd` to ensure graphical parameters are properly restored.

> Please do not set a seed to a specific number within a function.
-> R/flag_env_moran.R; R/flag_geo_moran.R
For more details:
<https://contributor.r-project.org/cran-cookbook/code_issues.html#setting-a-specific-seed>

* Removed `set.seed()` calls from the functions `flag_env_moran.R` and `flag_geo_moran.R`.
