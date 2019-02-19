context("test-proxygoodconfirm")

test_that("proxyGoodConfirm works", {
  .proxy <- c()
  proxy_good <- list(ip = '1.1.1.1', port = 111)
  expect_error(proxyGoodConfirm(proxy_good, .proxy), '.proxy must be list class')

  .proxy <- list()
  temp <- proxyGoodConfirm(proxy_good, .proxy)
  expect_is(temp, 'list')
  expect_equal(temp[[1]], modifyList(proxy_good, list(times = 1)))

  .proxy <- list(list(ip = '1.1.1.1', port = 111, times = 1),
                 list(ip = '2.2.2.2', port = 222, times = 2))
  temp <- proxyGoodConfirm(proxy_good, .proxy)
  expect_is(temp, 'list')
  expect_equal(temp[[1]], modifyList(.proxy[[1]], list(times = 2)))
})
