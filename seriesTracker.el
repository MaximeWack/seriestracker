;;; seriesTracker.el --- Series tracker -*- lexical-binding: t; -*-
;; Package-Requires: ((dash "2.12.1"))
;;; Commentary:
;;; Code:

;;; Requirements

(require 'url)                                                                  ; used to fetch api data
(require 'json)                                                                 ; used to parse api response
(require 'dash)                                                                 ; threading etc.

;;; Helpers

;;;; alist-select

(defun tvdb--utils-alist-select (fields alist)
  "Keep only FIELDS in ALIST by constructing a new alist containing only these elements.

alist-select '(a c) '((a .1) (b , \"b\") (c . c)
returns '((a . 1) (c . c))"

  (->> fields
       reverse
       (--reduce-from (acons it (alist-get it alist) acc)
                     nil)))

;;;; array-select

(defun tvdb--utils-array-select (fields array)
  "Keep only FIELDS in every alist in the ARRAY.

array-select '(a c) '(((a . 1) (b . 2) (c . c)) ((a . 3) (b . 5) (c . d)))
returns '(((a . 1) (c . c)) ((a . 3) (c . d)))"

  (--map (tvdb--utils-alist-select fields it) array))

;;;; array-pull

(defun tvdb--utils-array-pull (field array)
  "Keep only FIELD in every alist in the ARRAY and flatten.

array-pull 'a '(((a . 1) (b . 2)) ((a . 3) (b . 4)))
returns '(1 3)"

  (--map (alist-get field it) array))

;;;; epoch/datestring

(defun tvdb--utils-current-epoch ()
    "Get (current-time) as an epoch."

    (string-to-number (format-time-string "%s" (current-time))))

(defun tvdb--utils-date-to-epoch (date)
  "Convert DATE to an epoch."

  (->> (concat date " 00:00:00")
       date-to-time
       (format-time-string "%s")
       (string-to-number)))

;;;; getJSON

(defun tvdb--getJSON (url-buffer)
  "Parse the JSON in the URL-BUFFER returned by url."

  (with-current-buffer url-buffer
    (goto-char (point-max))
    (move-beginning-of-line 1)
    (json-read-object)))

;;; tvdb API

;;;; Login

(defvar tvdb-user
  nil
  "TheTVdbAPI Username")

(defvar tvdb-apikey
  nil
  "TheTVdbAPI API key")

(defvar tvdb-userkey
  nil
  "TheTVdbAPI user key")

(defvar tvdb--token
  nil
  "Auth token")

(defun tvdb--login ()
  "Login using TVDB-USER, TVDB-APIKEY and TVDB-USERKEY.
Sets tvdb--token."

  (->> (let ((url-request-method "POST")
            (url-request-extra-headers '(("Content-Type" . "application/json")
                                         ("Accept" . "application/json")))
            (url-request-data (concat "{\"apikey\": \"" tvdb-apikey "\", \"userkey\": \"" tvdb-userkey "\", \"username\": \"" tvdb-user "\" }"))
            (url-show-status nil))
        (url-retrieve-synchronously "https://api.thetvdb.com/login" t nil 2))
      tvdb--getJSON
      (alist-get 'token)
      (setq tvdb--token)))

;;;; Queries

;;;;; tvdb

(defun tvdb--tvdb-raw (params)
  "Raw API query.
Uses TVDB-TOKEN.
If not present or invalid token, try refreshing the token or re-logging."

  (tvdb--getJSON
   (let* ((url-request-method "GET")
          (bearer (concat "Bearer " tvdb--token))
          (url-request-extra-headers `(("Accept" . "application/json")
                                       ("Authorization" . ,bearer)))
          (url-show-status nil))
     (url-retrieve-synchronously (concat "https://api.thetvdb.com" params) nil nil 2))))

(defun tvdb--tvdb (&rest params)
  "Generic function to query tvdbapi."

  (if params
      (alist-get 'data
                 (tvdb--tvdb-raw (apply 'concat params)))
    (alist-get 'token
               (tvdb--tvdb-raw "/refresh_token"))))

;;;;; search

(defun tvdb--search (seriesName)
  "Search for SERIESNAME."

  (->> seriesName
      (tvdb--tvdb "/search/series?name=")
      (tvdb--utils-array-select '(id
                      seriesName
                      firstAired
                      status
                      network
                      overview))))

;;;;; series

(defun tvdb--series (id)
  "Get informations about a specific series ID."

  (->> id
      int-to-string
      (tvdb--tvdb "/series/")
      (tvdb--utils-alist-select '(id
                      seriesName
                      status
                      lastUpdated))))

;;;;; series/episodes

;;;;;; one page results

(defun tvdb--series/episodes1Page (id page)
  "Get one page of results for episodes in a series."

  (tvdb--tvdb-raw (concat "/series/" (int-to-string id) "/episodes?page=" (int-to-string page))))

(defun tvdb--series/episodesLastPage (id)
  "Get the number of pages of results for the episodes of series ID."

  (->> (tvdb--series/episodes1Page id 1)
       (alist-get 'links)
       (alist-get 'last)))

;;;;;; all pages

(defun tvdb--series/episodesPage (id page acc)
  "Get the whole episode list of show ID recursively."

  (let* ((query (tvdb--series/episodes1Page id page))
         (next (->> query
                    (alist-get 'links)
                    (alist-get 'next)))
         (data (->> query
                    (alist-get 'data)
                    (tvdb--utils-array-select '(id
                                    absoluteNumber
                                    airedSeason
                                    airedEpisodeNumber
                                    episodeName
                                    firstAired
                                    siteRating
                                    siteRatingCount)))))
    (if next
        (tvdb--series/episodesPage id next (append acc data))
      (append acc data))))

(defun tvdb--series/episodes (id &optional startPage)
  "Get all episodes for a specific series ID, starting from STARTPAGE (default = 1)."

  (->> (tvdb--series/episodesPage id (or startPage 1) nil)
       (--filter (> (alist-get 'airedSeason it) 0))
       (--sort (< (alist-get 'absoluteNumber it)
                  (alist-get 'absoluteNumber other)))))

;;;;; update

(defun tvdb--update-one-week (fromTime)
  "Return an array of series that have changed in the week after FROMTIME (datetime)."

  (tvdb--tvdb "/updated/query?fromTime=" fromTime))

(defun tvdb--update (fromTime)
  "Return an array of series that have changed since FROMTIME (epoch)."

  (->> (number-sequence
        fromTime
        (tvdb--utils-current-epoch)
        (* 3600 24 7))
       (-map 'int-to-string)
       (--map (tvdb--update-one-week it))
       (car)))

;;; Internal API

;;;; Data model

(defvar tvdb--data
  nil
  "Internal data containing followed series and episode.

Of the form :

'(((id . seriesId) (props . value) (…) (episodes ((id . episodeId) (watched . t) (props.value) (…))
                                                 ((id . episodeId) (watched . nil) (props.value) (…))))
  ((id . seriesId) (…) (episodes ((id . episodeId) (…))
                                 ((id . episodeId) (…)))))")

;;;; Methods

;;;;; Renew token

(defun tvdb-renew-token ()
  "Renew the token in tvdb--token.
Check that the token is renewed, else try to login using the credentials in TVDB-USER, TVDB-APIKEY, and TVDB-USERKEY.
If all fails, give an error message."

  (setq tvdb--token (tvdb--tvdb))
  (when (not tvdb--token)
    (when (not (tvdb--login))
      (message "Couldn't login to TheTVdbAPI. Please check tvdb-user, tvdb-apikey, and tvdb-userkey."))))

;;;;; Search series

(defun tvdb-search (seriesName)
  "Search SERIESNAME."

  (tvdb--search seriesName))

;;;;; Add series

(defun tvdb-add (id)
  "Add series with ID to tvdb--data.
Adding an already existing series resets it."

  (setq tvdb--data
        (--> tvdb--data
            (--remove (= id (alist-get 'id it)) it)
            (-snoc it (--> (tvdb--series id)
                          (-snoc it `(lastPage . ,(tvdb--series/episodesLastPage id)))
                          (-snoc it `(episodes ,@(tvdb--series/episodes id))))))))

;;;;; Remove series

(defun tvdb-remove (id)
  "Remove series with ID from tvdb--data."

  (setq tvdb--data
        (--remove (= id (alist-get 'id it)) tvdb--data)))

;;;;; List series

(defun tvdb-list-series ()
  "List followed series."

  (tvdb--utils-array-select '(id seriesName) tvdb--data))

;;;;; Get series episodes

(defun tvdb-get-episodes (id)
  "Get episodes of series with ID."

  (alist-get 'episodes
             (--find (= id (alist-get 'id it)) tvdb--data)))

;;;;; Watch episode

(defun tvdb-watch (seriesId episodeId)
  "Watch episode EPISODEID in series SERIESID."

  (->> tvdb--data
       (-map-when (lambda (series) (= seriesId (alist-get 'id series)))
                  (lambda (series)
                    (setf (alist-get 'episodes series)
                          (-map-when (lambda (episode) (= episodeId (alist-get 'id episode)))
                                     (lambda (episode)
                                       (setf (alist-get 'watched episode) t)
                                       episode)
                                     (alist-get 'episodes series)))))))

;;;;; Watch all episodes

(defun tvdb-watch-all (seriesId)
"Watch all episodes in SERIESID."

(->> tvdb--data
     (-map-when (lambda (series) (= seriesId (alist-get 'id series)))
                (lambda (series)
                  (setf (alist-get 'episodes series)
                        (-map (lambda (episode)
                                (setf (alist-get 'watched episode) t)
                                episode)
                              (alist-get 'episodes series)))))))

;;;;; Watch all episodes up to episode

(defun tvdb-watch-up (seriesId episodeId)
  "Watch all episodes up to EPISODEID in SERIESID."

  (let ((upto (->> tvdb--data
                   (--filter (= seriesId (alist-get 'id it)))
                   (-flatten-n 1)
                   (alist-get 'episodes)
                   (--filter (= episodeId (alist-get 'id it)))
                   (-flatten-n 1)
                   (alist-get 'absoluteNumber))))
    (->> tvdb--data
         (-map-when (lambda (series) (= seriesId (alist-get 'id series)))
                    (lambda (series)
                      (setf (alist-get 'episodes series)
                            (-map-when (lambda (episode)
                                         (<= (alist-get 'absoluteNumber episode)
                                             upto))
                                       (lambda (episode)
                                         (setf (alist-get 'watched episode) t)
                                         episode)
                                       (alist-get 'episodes series))))))))

;;;;; Upcoming episodes

(defun tvdb-upcoming ()
  "List upcoming episodes."

  (->> tvdb--data
      (-map (lambda (series)
              (let ((outseries (-clone series)))
                (setf (alist-get 'episodes outseries)
                      (--filter (equal nil (alist-get 'watched it))
                               (alist-get 'episodes outseries)))
                outseries)))
      (--remove (equal nil (alist-get 'episodes it)))))

;;;;; Episodes to watch

(defun tvdb-to-watch ()
  "List of episodes to watch."

  (->> (tvdb-upcoming)
      (-map (lambda (series)
              (let ((outseries (-clone series)))
                (setf (alist-get 'episodes outseries)
                      (--remove (> (tvdb--utils-date-to-epoch (alist-get 'firstAired it)) (tvdb--utils-current-epoch))
                               (alist-get 'episodes outseries)))
                outseries)))))

;;;;; Query updates

(defun tvdb--update-series (id)
  "Query new episodes for series ID, and add them to the list.
Update the other series properties."

  (let* ((series (--find (= id (alist-get 'id it)) tvdb--data))
         (lastPage (alist-get 'lastPage series))
         (oldEpisodes (alist-get 'episodes series))
         (allEpisodes (tvdb--series/episodes id lastPage))
         (newEpisodesId (-difference (tvdb--utils-array-pull 'id allEpisodes) (tvdb--utils-array-pull 'id oldEpisodes)))
         (newEpisodes (--filter (-contains? newEpisodesId (alist-get 'id it)) allEpisodes))
         (newSeries (--> (tvdb--series id)
                         (-snoc it `(lastPage . ,(tvdb--series/episodesLastPage id)))
                         (-snoc it `(episodes ,@(append oldEpisodes newEpisodes))))))
    (setq tvdb--data
          (--> tvdb--data
               (--remove (= id (alist-get 'id it)) it)
               (-snoc it newSeries)))))

(defun tvdb-update ()
  "Query all updated series since the last known update.
Keep only series that are followed.
Update new episodes."

  (-some->> tvdb--data
    (tvdb--utils-array-pull 'lastUpdated)
    -max
    (tvdb--update)
    (tvdb--utils-array-pull 'id)
    (-intersection (tvdb--utils-array-pull 'id (tvdb-list-series)))
    (-map 'tvdb--update-series)))

;;;;; Load/save data

(defvar tvdb--file
  "~/.emacs.d/tvdb.el"
  "Location of the save file")

(defun tvdb--save ()
  (with-temp-file tvdb--file
    (prin1 tvdb--data (current-buffer))))

(defun tvdb--load ()
  (with-temp-buffer
    (insert-file-contents tvdb--file)
    (cl-assert (eq (point) (point-min)))
    (setq tvdb--data (read (current-buffer)))))

;;; Interface

;;;; Faces

(defface tvdb-series
  '((t (:height 1.9 :weight bold :foreground "DeepSkyBlue")))
  "Face for series names")

(defface tvdb-season
  '((t (:height 1.7 :weight bold :foreground "MediumPurple")))
  "Face for seasons")

(defface tvdb-watched
  '((t (:foreground "DimGrey" :strike-through t)))
  "Face for watched episodes")

;;;; Draw buffer

(defun tvdb-refresh ()
  "Refresh the tvdb buffer.
Updates the database and redraws the buffer."
  (interactive)

  (if (and (string-equal (buffer-name) "tvdb") (string-equal mode-name "tvdb"))
      (let ((line (line-number-at-pos)))
        (tvdb-renew-token)
        (tvdb-update)
        (tvdb--draw-buffer)
        (goto-line line))
    (message "Not in tvdb buffer!")))

(defun tvdb--draw-buffer ()
  "Draw the buffer.
Erase first then redraw the whole buffer."

  (let ((inhibit-read-only t))
    (erase-buffer)
    (-each tvdb--data 'tvdb--draw-series)))

(defun tvdb--draw-series (series)
  "Print the series id and name."

  (let ((id (alist-get 'id series))
        (name (alist-get 'seriesName series))
        (episodes (alist-get 'episodes series)))
    (let ((start (point)))
      (insert (concat name "\n"))
      (set-text-properties start (point)
                           `(face tvdb-series
                                  tvdb-series ,id
                                  tvdb-season nil
                                  tvdb-episode nil)))
    (--each episodes (tvdb--draw-episode id it))))

(defun tvdb--draw-episode (series episode)
  "Print the episode id, S**E**, and name."

  (let ((id (alist-get 'id episode))
        (season (alist-get 'airedSeason episode))
        (episode (alist-get 'airedEpisodeNumber episode))
        (name (alist-get 'episodeName episode))
        (firstAired (alist-get 'firstAired episode))
        (watched (alist-get 'watched episode)))
    (when (= episode 1)
      (let ((start (point)))
        (insert (concat "Season " (int-to-string season) "\n"))
        (set-text-properties start (point)
                             `(face tvdb-season
                                    tvdb-series ,series
                                    tvdb-season ,season
                                    tvdb-episode nil))))
    (let ((start (point)))
      (let ((start (point)))
        (insert firstAired)
        (let ((end-date (point)))
          (insert (concat " " (format "%02d" episode) " - " name "\n"))
          (set-text-properties start (point)
                               `(face default
                                      tvdb-series ,series
                                      tvdb-season ,season
                                      tvdb-episode ,id))
          (put-text-property start end-date 'face '(t ((:foreground "MediumSpringGreen")))))
          (when watched
            (set-text-properties start (point)
                                 `(face tvdb-watched
                                        tvdb-series ,series
                                        tvdb-season ,season
                                        tvdb-episode ,id
                                        invisible tvdb-watched))))))))

;;;; Folding

(defun tvdb-fold-at-point ()
  "Fold the section at point."

  (interactive)

  (if (and (string-equal (buffer-name) "tvdb") (string-equal mode-name "tvdb"))
      (let ((inhibit-read-only t)
            (series (get-text-property (point) 'tvdb-series))
            (season (get-text-property (point) 'tvdb-season))
            (episode (get-text-property (point) 'tvdb-episode)))
        (cond (episode (tvdb-fold-episodes))
              (season (tvdb-fold-season))
              (t (tvdb-fold-series))))
    (message "Not in tvdb buffer!")))

(defun tvdb-fold-episodes ()
  "Fold the episodes at point."

  (let* ((season-start (previous-single-property-change (point) 'tvdb-season))
         (fold-start (next-single-property-change season-start 'tvdb-episode))
         (fold-end (next-single-property-change (point) 'tvdb-season nil (point-max))))
    (put-text-property fold-start fold-end 'invisible 'tvdb-folded nil)))

(defun tvdb-fold-season ()
  "Fold the season at point."

  (let* ((fold-start (next-single-property-change (point) 'tvdb-episode))
         (fold-end (next-single-property-change (point) 'tvdb-season nil (point-max))))
    (put-text-property fold-start fold-end 'invisible 'tvdb-folded nil)))

(defun tvdb-fold-series ()
  "Fold the series at point."

  (let* ((fold-start (next-single-property-change (point) 'tvdb-season))
         (fold-end (next-single-property-change (point) 'tvdb-series nil (point-max))))
    (put-text-property fold-start fold-end 'invisible 'tvdb-folded nil)))

(defun tvdb-unfold-at-point ()
  "Unfold the section at point."

  (interactive)

  (if (and (string-equal (buffer-name) "tvdb") (string-equal mode-name "tvdb"))
      (let ((inhibit-read-only t)
            (series (get-text-property (point) 'tvdb-series))
            (season (get-text-property (point) 'tvdb-season))
            (episode (get-text-property (point) 'tvdb-episode)))
        (cond (season (tvdb-unfold-season))
              (t (tvdb-unfold-series))))
    (message "Not in tvdb buffer!")))

(defun tvdb-unfold-season ()
  "Fold the season at point."

  (let* ((fold-start (next-single-property-change (point) 'tvdb-episode))
         (fold-end (next-single-property-change (point) 'tvdb-season nil (point-max))))
    (put-text-property fold-start fold-end 'invisible nil nil)))

(defun tvdb-unfold-series ()
  "Fold the series at point."

  (let* ((fold-start (next-single-property-change (point) 'tvdb-season))
         (fold-end (next-single-property-change (point) 'tvdb-series nil (point-max))))
    (put-text-property fold-start fold-end 'invisible nil nil)))

(defun tvdb-switch-watched ()
  "Switch visibility for watched episodes."

  (interactive)

  (if (-contains? buffer-invisibility-spec 'tvdb-watched)
      (setq-local buffer-invisibility-spec '(t tvdb-folded))
    (setq-local buffer-invisibility-spec '(t tvdb-folded tvdb-watched))))

;;;; Create mode

(defun tvdb ()
  "Run TVDB"

  (interactive)
  (switch-to-buffer "tvdb")
  (tvdb-mode)
  (tvdb-refresh))

(define-derived-mode tvdb-mode special-mode "tvdb"
  "Series tracking with TheTVdbAPI."

  (setq-local buffer-invisibility-spec '(t tvdb-folded)))

;;; Postamble

(provide 'seriesTracker)

;;; Example

(setq tvdb-user "maximewack")
(setq tvdb-apikey "0c737339fe858fadb896104543d0845b")
(setq tvdb-userkey "5DDF090101B740.38233217")

(tvdb-renew-token)

(tvdb-search "utopia")
(tvdb-search "game of thrones")
(tvdb-search "rick and morty")

(tvdb-add 121361)
(tvdb-add 264991)
(tvdb-add 275274)

(tvdb-remove 121361)

(tvdb-list-series)

(tvdb-get-episodes 264991)

(tvdb-watch 275274 7687399)

(tvdb-watch-all 264991)

(tvdb-watch-up 275274 6231155)

(tvdb-update)

(tvdb-upcoming)

(tvdb-to-watch)

(tvdb--save)

(tvdb--load)

;;; seriesTracker.el ends here
