# NHSRpostcodetools development log

## Development version 0.0.0.9001 (8 Feb 2024)

* Fixed issue #20 by removing `batch_it_simple()` and simplifying the code in
    `batch_it()` so it in turn is simpler.
    For the purposes of this package, it only has to do a simple thing.
* Updated `lintr` rules to exclude vignette `.Rmd`s.
* Ran `styler` over the package.
* Updated DESCRIPTION file to add an OrcID and to add minimum R version and
    some minimum package versions.

## Development version 0.0.0.9000 (23 Nov 2023)

* Package created with the functions from package [{myrmidon}][myr_gh] created by Fran Barton.
* Added `postcode_data_join.R` which uses the [postcodes.io][pio_api] API to get additional postcode data and which removes the requirement to save the large file from the [Open Geography portal][ogp] maintained by the ONS (Office for National Statistics).
* Added dependency function `batch_it()`


[myr_gh]: https://github.com/francisbarton/myrmidon
[pio_api]: https://postcodes.io
[ogp]: https://geoportal.statistics.gov.uk/datasets/postcode-to-output-area-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-authority-district-november-2018-lookup-in-the-uk-3/about