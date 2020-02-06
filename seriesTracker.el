(require 'request)

  ;; POST("https://api.thetvdb.com/login",
  ;;      config = add_headers(ContentType = "application/json",
  ;;                           Accept = "application/json"),
  ;;      body = body,
  ;;      encode = "json") %>%
  ;;   content %>%
  ;;   .$token

(request
  "https://api.thetvdb.com/login"
  :type "POST"
  :data (json-encode
         '(("username" . username)
           ("userkey" . userkey)
           ("apikey" . apikey)))
  ;; :data (concat "username=" username
  ;; "&userkey=" userkey
  ;; "&apikey=" apikey)
  :parser 'json-read
  :headers '(("Content-Type" . "application/json")
             ("Accept" . "application/json"))
  :sync t)

;; search
;; GET(str_c("https://api.thetvdb.com/search/series?name=", search),
;;     config = add_headers(Accept = "application/json",
;;                          Authorization = str_c("Bearer ", db$token))) %>%
;;   content %>%
;;   .$data -> res

;; getSeries <- function(series, token)
;; {
;;   GET(str_c("https://api.thetvdb.com/series/", series),
;;       config = add_headers(Accept = "application/json",
;;                            Authorization = str_c("Bearer ", token))) %>%
;;     content -> content
;;   content$data$seriesName
;; }
