context("test-request")

test_that("request function", {
  proxy_good <- list(list(ip = "221.206.100.133", port = 54781, times = 1))
  user_agent_good <- "AdsBot-Google ( http://www.google.com/adsbot.html)"

  temp <- request(
    'GET', url = 'http://httpbin.org/get', .proxy = proxy_good, .user_agent = user_agent_good
  )
  expect_is(temp, 'list')
  expect_equal(status_code(temp$response), 200)
  expect_equal(temp$response$request$options$proxy, temp$proxy_good$ip)
  expect_equal(temp$response$request$options$proxyport, temp$proxy_good$port)
  expect_equal(temp$response$request$options$useragent, user_agent_good)

  temp <- request(
    'POST', url = 'http://httpbin.org/post', .proxy = proxy_good, .user_agent = user_agent_good
  )
  expect_is(temp, 'list')
  expect_equal(status_code(temp$response), 200)
  expect_equal(temp$response$request$options$proxy, temp$proxy_good$ip)
  expect_equal(temp$response$request$options$proxyport, temp$proxy_good$port)
  expect_equal(temp$response$request$options$useragent, user_agent_good)

  temp <- request(
    'PUT', url = 'http://httpbin.org/put', .proxy = proxy_good, .user_agent = user_agent_good
  )
  expect_is(temp, 'list')
  expect_equal(status_code(temp$response), 200)
  expect_equal(temp$response$request$options$proxy, temp$proxy_good$ip)
  expect_equal(temp$response$request$options$proxyport, temp$proxy_good$port)
  expect_equal(temp$response$request$options$useragent, user_agent_good)

  temp <- request(
    'DELETE', url = 'http://httpbin.org/delete', .proxy = proxy_good, .user_agent = user_agent_good
  )
  expect_is(temp, 'list')
  expect_equal(status_code(temp$response), 200)
  expect_equal(temp$response$request$options$proxy, temp$proxy_good$ip)
  expect_equal(temp$response$request$options$proxyport, temp$proxy_good$port)
  expect_equal(temp$response$request$options$useragent, user_agent_good)
})
