context("test-Rogue")

test_that("can create a rogue instance", {
  expect_silent({rogue <- Rogue$new()})
  expect_equal(rogue$proxy, NULL)
  expect_equal(rogue$useragent, NULL)
  expect_equal(rogue$iter_max, 1)
  expect_equal(rogue$is_quite, FALSE)
  expect_equal(rogue$is_random, FALSE)
  expect_equal(rogue$is_record, FALSE)

  proxys <- list(list(ip = '1.1.1.1', port = 1),
                 list(ip = '2.2.2.2', port = 2),
                 list(ip = '3.3.3.3', port = 3))
  proxys_long <- lapply(letters[1:11], function(x) {
    list(ip = x, port = 1)
  })
  proxys_df <- do.call(
    rbind, lapply(proxys, function(x)
      data.frame(ip = x$ip, port = x$port, stringsAsFactors = FALSE))
  )
  proxys_modify <- lapply(proxys, function(x) {
    x$times <- 1
    x
  })
  useragents <- c('ua1', 'ua2', 'ua3')

  expect_silent({rogue <- Rogue$new(proxys, useragents)})
  expect_equal(rogue$proxy, proxys_modify)
  expect_equal(rogue$useragent, useragents)
  expect_equal(rogue$iter_max, length(proxys))

  expect_silent({rogue <- Rogue$new(proxys_long)})
  expect_equal(length(rogue$proxy), 11)
  expect_equal(rogue$iter_max, 10)

  expect_silent({rogue <- Rogue$new(proxys_df)})
  expect_equal(rogue$proxy, proxys_modify)
  expect_equal(rogue$iter_max, length(proxys))

  expect_error(Rogue$new('test'), 'proxy must be list or data.frame')
  expect_error(Rogue$new(list(list(port = 1))), 'proxy must have ip and port at least')
  expect_error(Rogue$new(list(list(ip = '1'))), 'proxy must have ip and port at least')
  expect_error(Rogue$new(list(list(ip = 1, port = 1))), 'ip must be character, not double')
  expect_error(Rogue$new(list(list(ip = '1', port = '1'))), 'port must be integer or numeric, not character')
  temp <- list(list(ip = '1', port = 1, times = 1))
  expect_silent({rogue <- Rogue$new(temp)})
  expect_equal(rogue$proxy, temp)
  expect_error(Rogue$new(list(list(ip = '1', port = 1, times = '1'))), 'times must be integer or numeric, not character')

  temp <- replicate(2, list(ip = '1', port = 1), FALSE)
  expect_silent({rogue <- Rogue$new(temp)})
  expect_equal(rogue$proxy, list(list(ip = '1', port = 1, times = 1)))
  expect_equal(rogue$iter_max, 1)
})

test_that("can add and show proxy", {

  rogue <- Rogue$new()
  temp <- rogue$.__enclos_env__$private$proxySetdiff(list(), list(1))
  expect_equal(temp, list())
  temp <- rogue$.__enclos_env__$private$proxySetdiff(list(1), list())
  expect_equal(temp, list(1))
  temp <- rogue$.__enclos_env__$private$proxySetdiff(
    list(list(ip = 1, port = 1), list(ip = 2, port = 2)),
    list(list(ip = 2, port = 2))
  )
  expect_equal(temp, list(list(ip = 1, port = 1)))
  temp <- rogue$.__enclos_env__$private$proxySetdiff(
    list(list(ip = 1, port = 1), list(ip = 2, port = 2)),
    list(list(ip = 3, port = 3))
  )
  expect_equal(temp, list(list(ip = 1, port = 1), list(ip = 2, port = 2)))

  proxys <- list(list(ip = '1', port = 1),
                 list(ip = '2', port = 2),
                 list(ip = '3', port = 3))
  proxys_modify <- lapply(proxys, function(x) {
    x$times <- 1
    x
  })
  expect_silent(rogue$proxy_add(proxys))
  expect_equal(rogue$proxy, proxys_modify)
  expect_warning(rogue$proxy_add(list()), 'no new proxy to add')
  expect_warning(rogue$proxy_add(proxys), 'no new proxy to add')
  expect_silent(rogue$proxy_add(list(list(ip = '4', port = 4))))
  expect_equal(length(rogue$proxy), 4)
  expect_silent(rogue$proxy_add(list(list(ip = '4', port = 4), list(ip = '5', port = 5))))
  expect_equal(length(rogue$proxy), 5)
  expect_silent(rogue$proxy_add(list(list(ip = '6', port = 6)), delete = TRUE))
  expect_equal(length(rogue$proxy), 5)
  rogue$proxy <- lapply(rogue$proxy, function(x){
    x$times <- 2
    x
  })
  expect_silent(rogue$proxy_add(list(list(ip = '7', port = 7)), delete = TRUE))
  expect_equal(length(rogue$proxy), 6)
  expect_silent(rogue$proxy_add(list(list(ip = '8', port = 8), list(ip = '9', port = 9)), delete = TRUE))
  expect_equal(length(rogue$proxy), 7)

  expect_is(rogue$proxy_show(), 'data.frame')
  expect_named(rogue$proxy_show(), c('ip', 'port', 'times'))

  rogue <- Rogue$new()
  expect_is(rogue$proxy_show(), 'data.frame')
  expect_named(rogue$proxy_show(), c('ip', 'port', 'times'))
  expect_equal(nrow(rogue$proxy_show()), 0)
})

test_that('can use proxy and useragent', {
  rogue <- Rogue$new()
  expect_silent({temp <- rogue$get('http://httpbin.org/get')})
  expect_equal(status_code(temp), 200)
  expect_silent({temp <- rogue$post('http://httpbin.org/post')})
  expect_equal(status_code(temp), 200)
  expect_silent({temp <- rogue$put('http://httpbin.org/put')})
  expect_equal(status_code(temp), 200)
  expect_silent({temp <- rogue$delete('http://httpbin.org/delete')})
  expect_equal(status_code(temp), 200)
  temp <- rogue$get('http://httpbin.org/get', httr::authenticate('u', 'p'))
  expect_equal(content(temp)$header$Authorization, 'Basic dTpw')

  proxy <- list(list(ip = '121.33.220.158', port = 808))
  rogue <- Rogue$new(proxy)
  temp <- rogue$get('http://httpbin.org/ip')
  expect_true(proxy[[1]]$ip %in% trimws(unlist(strsplit(content(temp)$origin, ','))))

  rogue <- Rogue$new(useragent = 'test user')
  temp <- rogue$get('http://httpbin.org/user-agent')
  expect_equal(content(temp)$`user-agent`, 'test user')
})

test_that('can select proxys', {

  proxys <- unname(mapply(function(i, p) {
    list(ip = i, port = p)
  }, letters[1:6], 1:6, SIMPLIFY = FALSE))

  # iter_max samller, random
  rogue <- Rogue$new(proxys, is_random = TRUE)
  set.seed(1)
  selected <- sapply(rogue$.__enclos_env__$private$proxySelect(), `[[`, 'ip')
  set.seed(1)
  temp <- sapply(sample(proxys, 6), `[[`, 'ip')
  expect_equal(selected, temp)

  proxys_modify <- lapply(proxys, function(x) {
    x$times <- sample.int(100, 1)
    x
  })

  # iter_max samller, not random
  rogue <- Rogue$new(proxys_modify, is_random = FALSE)
  set.seed(1)
  selected <- sapply(rogue$.__enclos_env__$private$proxySelect(), `[[`, 'ip')
  expect_true(any(selected != temp))
  set.seed(1)
  temp <- sapply(
    sample(proxys, 6, prob = sapply(proxys_modify, `[[`, 'times')), `[[`, 'ip'
  )
  expect_equal(selected, temp)

  # iter_max larger, random
  suppressWarnings({
    rogue <- Rogue$new(proxys, is_random = TRUE, iter_max = 10)
  })
  set.seed(1)
  selected <- sapply(rogue$.__enclos_env__$private$proxySelect(), `[[`, 'ip')
  expect_equal(length(selected), 10)
  set.seed(1)
  temp <- sapply(sample(proxys, 10, replace = TRUE), `[[`, 'ip')
  expect_equal(selected, temp)

  # iter_max larger, not random
  suppressWarnings({
    rogue <- Rogue$new(proxys_modify, is_random = FALSE, iter_max = 15)
  })
  set.seed(1)
  selected <- sapply(rogue$.__enclos_env__$private$proxySelect(), `[[`, 'ip')
  expect_equal(length(selected), 15)
  set.seed(1)
  temp <- sapply(unlist(append(
    replicate(
      2, sample(proxys, 6, prob = sapply(proxys_modify, `[[`, 'times')), FALSE
    ),
    list(sample(proxys, 3, prob = sapply(proxys_modify, `[[`, 'times')))
  ), recursive = FALSE), `[[`, 'ip')
  expect_equal(selected, temp)

})

test_that('can confirm good and bad', {

  proxys <- unname(mapply(function(i, p) {
    list(ip = i, port = p)
  }, letters[1:6], 1:6, SIMPLIFY = FALSE))

  proxy_good <- list(ip = 'a', port = 1)
  proxy_not_exist <- list(ip = 'a', port = 2)

  rogue <- Rogue$new(proxys)
  expect_silent(rogue$.__enclos_env__$private$proxyGoodConfirm(proxy_good))
  expect_equal(sapply(rogue$proxy, `[[`, 'times'), c(2, rep(1, 5)))
  rogue$.__enclos_env__$private$proxyGoodConfirm(proxy_not_exist)
  expect_equal(sapply(rogue$proxy, `[[`, 'times'), c(2, rep(1, 5)))

  rogue$.__enclos_env__$private$proxyGoodConfirm(proxy_good)
  proxy_bad <- list(list(ip = 'a', port = 1))
  expect_silent(rogue$.__enclos_env__$private$proxyBadConfirm(proxy_bad))
  expect_equal(sapply(rogue$proxy, `[[`, 'times'), c(2, rep(1, 5)))

  proxy_bad <- list(list(ip = 'a', port = 1), list(ip = 'b', port = 2))
  rogue$.__enclos_env__$private$proxyBadConfirm(proxy_bad)
  expect_equal(sapply(rogue$proxy, `[[`, 'times'), rep(1, 6))
})
