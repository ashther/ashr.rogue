
#' @importFrom R6 R6Class
#' @name Rogue
NULL

#' @export
Rogue <- R6Class(
  'Rogue',
  public = list(
    proxy = NULL,
    useragent = NULL,
    iter_max = 10,
    is_record = FALSE,
    is_random = FALSE,
    is_quite = FALSE,

    # initialize the object
    initialize = function(proxy = NULL, useragent = NULL, iter_max = 10,
                          is_record = FALSE, is_random = FALSE, is_quite = FALSE) {
      if (!is.null(proxy)) {
        if (!'list' %in% class(proxy))
          stop('proxy must be list class', call. = FALSE)

        self$proxy <- lapply(proxy, function(x) {
          if (!all(c('ip', 'port') %in% names(x)))
            stop('proxy must have ip and port at least', call. = FALSE)
          if (!is.character(x$ip))
            stop(
              sprintf('ip must be character, not %s', typeof(x$ip)),
              call. = FALSE
            )
          if (!is.numeric(x$port))
            stop(
              sprintf('port must be integer or numeric, not %s', typeof(x$port)),
              call. = FALSE
            )

          if (!'times' %in% names(x))
            x$times <- 1
          if (!is.numeric(x$times))
            stop(
              sprintf('times must be integer or numeric, not %s', typeof(x$times)),
              call. = FALSE
            )

          x
        })
      } else {
        self$proxy <- proxy
      }

      if (!is.numeric(iter_max))
        stop(
          sprintf('iter_max must be integer or numeric, not %s', typeof(iter_max)),
          call. = FALSE
        )
      # TODO add warning when iter_max bigger than proxy length
      if (!is.logical(is_record))
        stop(
          sprintf('is_record must be logical, not %s', typeof(is_record)),
          call. = FALSE
        )

      self$useragent <- useragent
      self$iter_max <- iter_max
      self$is_record <- is_record
      self$is_random <- is_random
    },

    # print to console
    print = function(...) {
      cat('Rogue: \n')
      cat(sprintf(
        '  proxy: %s\n',
        ifelse(is.null(self$proxy), 'NULL', length(self$proxy))
      ))
      cat(sprintf(
        '  user agent: %s\n',
        ifelse(is.null(self$useragent), 'NULL', length(self$useragent))
      ))
      cat(sprintf('  iter max: %s\n', self$iter_max))
      cat(sprintf('  random select: %s\n', self$is_random))
      cat(sprintf('  record history: %s\n', self$is_record))
      cat(sprintf('  quite: %s\n', self$is_quite))
    },

    # add proxy by users anytime
    proxy_add = function(proxy, delete = FALSE) {

    },

    # show proxy with the tibble format
    proxy_show = function(proxy) {

    },

    # http get
    get = function(...) {
      private$ROGUE('GET', ...)
    },

    # http post
    post = function(...) {
      private$ROGUE('POST', ...)
    },

    # http put
    put = function(...) {
      private$ROGUE('PUT', ...)
    },

    # http delete
    delete = function(...) {
      private$ROGUE('DELETE', ...)
    }
  ), private = list(

    # select from self proxys, based on times
    # all proxys including initializing and adding, must has elements with times name
    # if they don't have, must generate for them
    proxySelect = function() {
      prob <- vapply(self$proxy, `[[`, 'times', FUN.VALUE = numeric(1))

      if (self$iter_max <= length(self$proxy)) {
        if (self$is_random) {
          return(sample(self$proxy, self$iter_max))
        } else {
          return(sample(self$proxy, self$iter_max, prob = prob))
        }
      } else {
        if (self$is_random) {
          return(sample(self$proxy, self$iter_max, replace = TRUE))
        } else {
          modulus <- self$iter_max %/% length(self$proxy)
          reminder <- self$iter_max %% length(self$proxy)

          temp <- append(
            replicate(
              modulus, sample(self$proxy, length(self$proxy), prob = prob), FALSE
            ),
            list(sample(self$proxy, reminder, prob = prob))
          )
          return(unlist(temp, recursive = FALSE))
        }
      }
    },

    # confirm good proxy
    proxyGoodConfirm = function(proxy_good) {

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

    # confirm bad proxy
    proxyBadConfirm = function(proxy_bad) {

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

    # core function
    ROGUE = function(.f = c('GET', 'POST', 'PUT', 'DELETE'), ...) {
      .f <- match.arg(.f)

      res_got <- 0
      iter <- 1
      parameters <- list(...)
      if (!is.null(self$proxy))
        proxy_selected <- private$proxySelect()

      proxy_bad <- list()
      while (iter <= self$iter_max) {
        if (!is.null(self$proxy)) {
          proxy <- proxy_selected[[iter]]
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
          if (!self$is_quite)
            message(e$message)
        })

        if (res_got == 1) break()
        iter <- iter + 1
      }

      if (res_got != 1 && iter > self$iter_max) {
        if (!self$is_quite)
          message('Try too many times.')
        if (!is.null(self$proxy) & self$is_record)
          private$proxyBadConfirm(proxy_bad)
        return(NULL)
      }

      if (!is.null(self$proxy) & self$is_record) {
        private$proxyGoodConfirm(proxy_good)
        private$proxyBadConfirm(proxy_bad)
      }

      response
    }
  )
)
