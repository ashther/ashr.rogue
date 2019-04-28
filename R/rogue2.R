
Rogue <- R6Class('Rogue', public = list(
  proxy = NULL,
  useragent = NULL,
  iter_max = 10,
  response = NULL,
  is_record = FALSE,

  # initialize the object
  initialize = function(proxy = NULL, useragent = NULL, iter_max = 10, is_record = FALSE) {
    self$proxy <- proxy
    self$useragent <- useragent
    self$iter_max <- iter_max
    self$is_record <- is_record
  },

  # print to console
  print = function(...) {
    cat('Rogue: \n')
    cat(sprintf('  proxy: %s\n', length(self$proxy)))
    cat(sprintf('  user agent: %s\n', length(self$useragent)))
    cat(sprintf('  iter max: %s\n', self$iter_max))
    cat(sprintf('  record proxy history: %s', self$is_record))
    invisible(self)
  },

  # http get
  get = function(...) {
    private$ROGUE('GET', ...)
    invisible(self) # TODO get response directly
  },

  # http post
  post = function(...) {
    private$ROGUE('POST', ...)
    invisible(self)
  },

  # http put
  put = function(...) {
    private$ROGUE('PUT', ...)
    invisible(self)
  },

  # http delete
  delete = function(...) {
    private$ROGUE('DELETE', ...)
    invisible(self)
  }
), private = list(
  proxyGoodConfirm = function(proxy_good) {
    if (class(self$proxy) != 'list')
      stop('.proxy must be list class')

    if (length(self$proxy) == 0) {
      self$proxy[[1]] <- modifyList(proxy_good, list(times = 1))
      return(invisible(TRUE))
    }

    i <- 1
    while (TRUE) {
      temp <- self$proxy[[i]]
      if (temp$ip == proxy_good$ip && temp$port == proxy_good$port) {

        times <- ifelse('times' %in% names(temp), temp$times + 1, 1)
        self$proxy[[i]] <- modifyList(temp, list(times = times))
        break()

      }

      i <- i + 1
      if (i > length(self$proxy))
        break()
    }

    invisible(TRUE)
  },
  proxyBadConfirm = function(proxy_bad) {
    if (class(self$proxy) != 'list')
      stop('.proxy must be list class')

    if (length(self$proxy) == 0) {
      self$proxy <- modifyList(proxy_bad, list(times = 1))
      return(invisible(TRUE))
    }

    for (p in proxy_bad) {
      i <- 1
      while (TRUE) {
        temp <- self$proxy[[i]]
        if (temp$ip == p$ip && temp$port == p$port) {

          times <- ifelse('times' %in% names(temp), max(temp$times - 1, 1), 1)
          self$proxy[[i]] <- modifyList(temp, list(times = times))
          break()

        }

        i <- i + 1
        if (i > length(self$proxy))
          break()
      }
    }
    invisible(TRUE)
  },
  ROGUE = function(.f = c('GET', 'POST', 'PUT', 'DELETE'), ...) {
    .f <- match.arg(.f)

    res_got <- 0
    iter <- 1
    parameters <- list(...)

    proxy_bad <- list()
    while (iter <= self$iter_max) {
      if (!is.null(self$proxy)) {
        proxy <- proxySelect(self$proxy)
        proxy_bad <- append(proxy_bad, list(proxy))
        parameters <- append(parameters, list(use_proxy(proxy$ip, proxy$port)))
      }
      if (!is.null(self$useragent)) {
        userAgent <- sample(self$useragent, 1)
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

    if (res_got != 1 && iter > self$iter_max) {
      message('Try too many times.')
      self$response <- NULL
      if (self$is_record)
        private$proxyBadConfirm(proxy_bad)
    }

    self$response <- response
    if (self$is_record) {
      private$proxyGoodConfirm(proxy_good)
      private$proxyBadConfirm(proxy_bad)
    }

    invisible(self)
  }
))
