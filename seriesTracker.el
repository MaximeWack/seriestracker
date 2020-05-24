;;; package --- Summary
;;; Commentary:
;;; Code:

;;;; Requires

(require 'url)
(require 'json)

;;;; Functions
;;;;; getJSON

(defun getJSON (url-buffer)
  "Parse the JSON in the URL-BUFFER returned by url."

  (with-current-buffer url-buffer
    (goto-char (point-max))
    (move-beginning-of-line 1)
    (json-read-object)))

;;;;; alist-select

(defun alist-select (alist fields)
  "Keep only FIELDS in ALIST."

  (reduce (lambda (list item)
            (acons item (alist-get item alist) list)
            )
          fields
          :initial-value nil))

;;;;; names

(defun names (alist)
  "Return the names of the ALIST."

  (mapcar 'car alist))

;;;;; epoch-to-datestring

(defun epoch-to-datestring (epoch)
  "Convert EPOCH to a datestring."

  (format-time-string "%Y-%m-%d %H:%M:%S" (time-add 0 epoch)))

;;;;; datestring-to-epoch

(defun datestring-to-epoch (datestring)
  "Convert DATESTRING to an epoch."

  (format-time-string "%s" (date-to-time datestring)))

;;;;; login

(defun login (username apikey userkey)
  "Login using USERNAME, APIKEY and USERKEY.
Return the token"

  (alist-get 'token
             (getJSON
              (let ((url-request-method "POST")
                    (url-request-extra-headers '(("Content-Type" . "application/json")
                                                 ("Accept" . "application/json")))
                    (url-request-data (concat "{\"apikey\": \"" apikey "\", \"userkey\": \"" userkey "\", \"username\": \"" username "\" }"))
                    (url-show-status nil))
                (url-retrieve-synchronously "https://api.thetvdb.com/login" t)))))

;;;;; refresh_token

(defun refresh_token (token)
  "Refresh the TOKEN."

  (alist-get 'token
              (let ((params "refresh_token"))
                (tvdb token params))))

;;;;; tvdb

(defun tvdb (token params)
  "Generic function to query tvdbapi with PARAMS.
Needs a TOKEN to work.  The TOKEN can be obtained using the LOGIN function.
Returns the result as a parsed JSON object"

  (getJSON
   (let* ((url-request-method "GET")
          (bearer (concat "Bearer " token))
          (url-request-extra-headers `(("Accept" . "application/json")
                                       ("Authorization" . ,bearer)))
          (url-show-status nil))
     (url-retrieve-synchronously (concat "https://api.thetvdb.com/" params)))))


;;;;; search

(defun search (token seriesName)
  "Search for SERIESNAME.
Needs a TOKEN to work.  The TOKEN can be obtained using the LOGIN function."

  (mapcar (lambda (serie)
            (alist-select serie (reverse '(id
                                      seriesName
                                      firstAired
                                      status
                                      season
                                      network
                                      overview
                                      genre
                                      siteRating
                                      siteRatingCount))))
          (alist-get 'data
                     (let ((params (concat "search/series?name=" seriesName)))
                       (tvdb token params)))))

;;;;; episodes

(defun episodes (token id)
  "Get informations about a specific episode ID.
Needs a TOKEN to work.  The TOKEN can be obtained using the LOGIN function."

  (alist-select
   (alist-get 'data
              (let ((params (concat "episodes/" id)))
                (tvdb token params)))
   (reverse '(id
              episodeName
              airedSeason
              airedEpisode))))

;;;;; series

(defun series (token id)
  "Get informations about a specific series ID.
Needs a TOKEN to work.  The TOKEN can be obtained using the LOGIN function."

  (alist-select
   (alist-get 'data
              (let ((params (concat "series/" id)))
                (tvdb token params)))
   (reverse '(id
              seriesName
              firstAired
              status
              season
              network
              overview
              genre
              siteRating
              siteRatingCount))))

;;;;; seriesEpisodes

(defun series/episodes (token id)
  "Get all episodes for a specific series ID.
Needs a TOKEN to work.  The TOKEN can be obtained using the LOGIN function."

  (mapcar (lambda (episode)
            (alist-select episode
                     (reverse '(id
                                airedSeason
                                airedEpisodeNumber
                                episodeName
                                firstAired
                                seriesId
                                siteRating
                                siteRatingCount))))
          (alist-get 'data
                     (let ((params (concat "series/" id "/episodes")))
                       (tvdb token params)))))

;;;;; update

(defun update (token fromTime)
  "Return an array of series that have changed since FROMTIME.
Needs a TOKEN to work.  The TOKEN can be obtained using the LOGIN function."

  (mapcar (lambda (update)
            (setf (alist-get 'lastUpdated update) (epoch-to-datestring (alist-get 'lastUpdated update)))
            update)
          (alist-get 'data
                     (let ((params (concat "updated/query?fromTime=" (datestring-to-epoch fromTime))))
                       (tvdb token params)))))
;;;;  Tests

(setq token (login ""
                   ""
                   ""))

(setq token (refresh_token token))

(setq series-list
      (search token "Game of Thrones"))

series-list

(setq serie
      (series token "121361"))

(setq episodes
      (series/episodes token "121361"))

(update token "2020-03-30 00:00:00")

(provide 'seriesTracker)
;;; seriesTracker.el ends here
