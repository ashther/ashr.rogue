context("test-rogue")

test_that("rogue function", {
  proxy_good <- list(list(ip = "61.128.208.94", port = 3128, times = 1))
  user_agent_good <- "AdsBot-Google ( http://www.google.com/adsbot.html)"

  temp <- ROGUE(
    'GET', url = 'http://httpbin.org/get', .proxy = proxy_good, .user_agent = user_agent_good
  )
  expect_is(temp, 'list')
  expect_equal(status_code(temp$response), 200)
  expect_equal(temp$response$request$options$proxy, temp$proxy_good$ip)
  expect_equal(temp$response$request$options$proxyport, temp$proxy_good$port)
  expect_equal(temp$response$request$options$useragent, user_agent_good)

  temp <- ROGUE(
    'POST', url = 'http://httpbin.org/post', .proxy = proxy_good, .user_agent = user_agent_good
  )
  expect_is(temp, 'list')
  expect_equal(status_code(temp$response), 200)
  expect_equal(temp$response$request$options$proxy, temp$proxy_good$ip)
  expect_equal(temp$response$request$options$proxyport, temp$proxy_good$port)
  expect_equal(temp$response$request$options$useragent, user_agent_good)

  temp <- ROGUE(
    'PUT', url = 'http://httpbin.org/put', .proxy = proxy_good, .user_agent = user_agent_good
  )
  expect_is(temp, 'list')
  expect_equal(status_code(temp$response), 200)
  expect_equal(temp$response$request$options$proxy, temp$proxy_good$ip)
  expect_equal(temp$response$request$options$proxyport, temp$proxy_good$port)
  expect_equal(temp$response$request$options$useragent, user_agent_good)

  temp <- ROGUE(
    'DELETE', url = 'http://httpbin.org/delete', .proxy = proxy_good, .user_agent = user_agent_good
  )
  expect_is(temp, 'list')
  expect_equal(status_code(temp$response), 200)
  expect_equal(temp$response$request$options$proxy, temp$proxy_good$ip)
  expect_equal(temp$response$request$options$proxyport, temp$proxy_good$port)
  expect_equal(temp$response$request$options$useragent, user_agent_good)
})
