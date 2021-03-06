
proxySelect <- function(.proxy) {
  if ('times' %in% names(.proxy[[1]]))
    result <- sample(
      .proxy, 1, prob = vapply(.proxy, `[[`, 'times', FUN.VALUE = numeric(1))
    )
  else
    result <- sample(.proxy, 1)
  result[[1]]
}

#' request function
#'
#' @param .f single string like `GET`, `POST`, `PUT` and `DELETE`
#' @param ... further named parameters which will be pass to `httr` function
#' @param .proxy proxy list, which has `ip` and `port` at least, if `times` was provided,
#' the random selection of proxy will use `times` as `prob` parameter in `sample`
#' @param .user_agent user agent strings vector, which is already in this package
#' @param iter_max the max try times
#'
#' @return list, httr response and proxy in use
#' @export
#'
#' @import httr
#' @examples
#' \dontrun{
#' proxy <- list(
#'   list(ip = '1.1.1.1', port = 3128, times = 1),
#'   list(ip = '2.2.2.2', port = 3128, times = 2) # use times as prob in sample
#' )
#' request('GET', url = 'http://httpbin.org/get', .proxy = proxy, .user_agent = useragent)
#' }
request <- function(.f = c('GET', 'POST', 'PUT', 'DELETE'), ...,
                  .proxy = NULL, .user_agent = NULL, iter_max = 10) {
  .f <- match.arg(.f)

  res_got <- 0
  iter <- 1
  parameters <- list(...)

  proxy_bad <- list()
  while (iter <= iter_max) {
    if (!is.null(.proxy)) {
      proxy <- proxySelect(.proxy)
      proxy_bad <- append(proxy_bad, list(proxy))
      parameters <- append(parameters, list(use_proxy(proxy$ip, proxy$port)))
    }
    if (!is.null(.user_agent)) {
      userAgent <- sample(.user_agent, 1)
      parameters <- append(parameters, list(user_agent(userAgent)))
    }

    tryCatch({
      response <- do.call(.f, parameters)
      stop_for_status(response)

      proxy_good <- list(
        ip = response$request$options$proxy,
        port = response$request$options$proxyport
      )
      proxy_bad <- Filter(function(x){
        x$ip != proxy_good$ip & x$port != proxy_good$port
      }, proxy_bad)
      res_got <- 1
    }, error = function(e) {
      message(e$message)
    })

    if (res_got == 1)
      break()
    iter <- iter + 1
  }

  if (res_got != 1 && iter > iter_max) {
    message('Try too many times.')
    return(list(response = NULL, proxy_good = NULL, proxy_bad = proxy_bad))
  }

  list(response = response, proxy_good = proxy_good, proxy_bad = proxy_bad)
}
