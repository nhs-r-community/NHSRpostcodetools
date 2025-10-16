
# NHSRpostcodetools ![R](https://www.r-project.org/favicon-32x32.png) 📦📬🏘️

<!-- badges: start -->

![GitHub License][gh_licence]
[![Project Status: Active – The project has reached a stable, usable state and
is being actively developed][repostatus_svg]][repostatus_info]
![GitHub R package version][gh_ver]

[gh_licence]: https://img.shields.io/github/license/nhs-r-community/NHSRpostcodetools
[gh_ver]: https://img.shields.io/github/r-package/v/nhs-r-community/NHSRpostcodetools
[repostatus_info]: https://www.repostatus.org/#active
[repostatus_svg]: https://www.repostatus.org/badges/latest/active.svg
<!-- badges: end -->

The package helps you send a collection of UK postcodes to the
[postcodes.io][pcio] API, and handles the data returned by the API.

This gives you easy access to the coordinates of the centroids of the postcode
areas, and other information such as the statistical, administrative and health
geographies the postcodes sit within.

For example: its LSOA (or equivalent), local authority, and parliamentary
constituency.

The package also helps you check a list of postcodes for currency/validity,
and suggests replacement current postcodes for any terminated (out of date)
codes.

[pcio]: https://postcodes.io/


## Installation and basic usage

You can install the development version of `{NHSRpostcodetools}` from
[GitHub][repo] with:

```r
# install.packages("pak") # run this if you don't yet have {pak} installed
pak::pak("nhs-r-community/NHSRpostcodetools")

# then load the package with `library()`
library(NHSRpostcodetools)
```

You can get postcode data by submitting a vector of postcodes:

```r
get_postcode_data(c("NP22 3PS", "NP22 4PS", "NP22 5PS"))
```
or if you have a data frame with a column of postcodes, you might do:

```r
postcode_data_join(my_data, .col = "postcodes")
```

setting the `.col` argument to the name of the column containing the codes
(this is "postcode" by default, so in that case you don't need to supply `.col`)

If you have a list of postcodes that may contain errors or out of date codes,
you can check them with `suggest_fixes()`:

```r
suggest_fixes(c("NP22 3PQ", "NP22 3PR", "NP22 3MN"))
```

This will return a summary data frame for any codes that are not found to be
current and valid, and will suggest nearby replacement postcodes where possible.
This allows you to fix invalid postcodes in your data before running
`get_postcode_data()`.

NB `suggest_fixes()` isn't magic! It can try to deal with terminated (out of
date) postcodes and postcodes with an incorrect or missing final letter, but it
can't help with incorrect outcodes (the first half of the postcode).

So if your postcode data has "NR22 3PS" when it should have "NP22 3PS", you are
on your own 😜! Please sense-check your data before using `suggest_fixes()`,
and use care when implementing its suggestions.

[repo]: https://github.com/nhs-r-community/NHSRpostcodetools


## Further guidance on usage

The [package website][intro] contains more guidance on how to use the functions
in this package, and what to expect.

[intro]: https://nhs-r-community.github.io/NHSRpostcodetools/


## Getting help and getting involved

If you experience any problems using this package, find a bug, or have ideas
for how to improve it, you are welcome to [create a GitHub issue][issues].

Before submitting your issue, please read the NHS-R Community's
[guidance on how to contribute to packages][tools].

[issues]: https://github.com/nhs-r-community/NHSRpostcodetools/issues
[tools]: https://tools.nhsrcommunity.com/contribution.html

This project is released with a [Contributor Code of Conduct][coc].
By contributing to this project, you agree to abide by its terms.

[coc]: CODE_OF_CONDUCT.md
