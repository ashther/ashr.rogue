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
``` r
# create a Rogue instance
rogue <- Rogue$new()
# or with proxy and user-agent, and record the proxy connection history
rogue <- Rogue$new(proxy = proxys, useragent = useragents, is_record = TRUE)
# select proxy not randomly, but consider the connection history, which means select better ones
rogue <- Rogue$new(proxy = proxys, useragent = useragents, is_record = TRUE, is_random = FALSE)

# add new proxy any time, and delete old ones
rogue$proxy_add(proxy_new, delete = TRUE)

# send query
rogue$get('http://httpbin.org/get')

# check the proxy connection history
rogue$proxy_show()
```

