#' confirm good proxy
#'
#' @param proxy_good list with ip and port properties
#' @param .proxy list of list with ip and port properties, times
#'
#' @return list of list with `ip`, `port` and `times` properties
#' @details .proxy must be list class, if no elments in it, the return times will
#'  be 1, otherwise, the time value will be added 1
#' @export
#'
#' @importFrom utils modifyList
#' @examples
#' \dontrun{
#' .proxy <- list(list(ip = '1.1.1.1', port = 111, times = 1),
#'                list(ip = '2.2.2.2', port = 222, times = 2))
#'  proxy_good <- list(ip = '1.1.1.1', port = 111)
#' .proxy <- proxyGoodConfirm(proxy_good, .proxy)
#' .proxy
#' }
proxyGoodConfirm <- function(proxy_good, .proxy) {
  if (class(.proxy) != 'list')
    stop('.proxy must be list class')

  if (length(.proxy) == 0) {
    .proxy[[1]] <- modifyList(proxy_good, list(times = 1))
    return(.proxy)
  }

  i <- 1
  while (TRUE) {
    temp <- .proxy[[i]]
    if (temp$ip == proxy_good$ip && temp$port == proxy_good$port) {

      times <- ifelse('times' %in% names(temp), temp$times + 1, 1)
      .proxy[[i]] <- modifyList(temp, list(times = times))
      break()

    }

    i <- i + 1
    if (i > length(.proxy))
      break()
  }

  .proxy
}
