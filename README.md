# ashr.rogue
[![Travis build status](https://travis-ci.org/ashther/ashr.rogue.svg?branch=master)](https://travis-ci.org/ashther/ashr.rogue)
[![R Style Guide: Good Parts](https://img.shields.io/badge/code%20style-goodparts-blue.svg)](http://adv-r.had.co.nz/Style.html)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

The goal of ashr.rogue is to provide simple function which wraps `httr` package, function like `GET` will use random proxy and user agent with multiple try until got the right response.

## Installation

You can install the released version of ashr.rogue with:

``` r
# install.packages('devtools')
devtools::install_github("ashther/ashr.rogue")
```

## Example

use `ROGUE` function just like in `httr`:

``` r
ROGUE('GET', url = 'http://httpbin.org/get', .proxy = proxy, .user_agent = useragent, iter_max = 10)
```

## TODO
- proxyBadConfirm function
- R6 maybe?

