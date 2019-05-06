# ashr.rogue
[![Travis build status](https://travis-ci.org/ashther/ashr.rogue.svg?branch=master)](https://travis-ci.org/ashther/ashr.rogue)
[![Coverage status](https://codecov.io/gh/ashther/ashr.rogue/branch/master/graph/badge.svg)](https://codecov.io/github/ashther/ashr.rogue?branch=master)
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
- fix bug in ROGUE function, add sys sleep time between request
- proxyBadConfirm function
- R6 maybe?

