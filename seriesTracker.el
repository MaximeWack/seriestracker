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

(defun st--utils-alist-select (fields alist)
  "Keep only FIELDS in ALIST by constructing a new alist containing only these elements.

alist-select '(a c) '((a .1) (b , \"b\") (c . c)
returns '((a . 1) (c . c))"

  (->> fields
       reverse
       (--reduce-from (acons it (alist-get it alist) acc)
                     nil)))

;;;; array-select

(defun st--utils-array-select (fields array)
  "Keep only FIELDS in every alist in the ARRAY.

array-select '(a c) '(((a . 1) (b . 2) (c . c)) ((a . 3) (b . 5) (c . d)))
returns '(((a . 1) (c . c)) ((a . 3) (c . d)))"

  (--map (st--utils-alist-select fields it) array))

;;;; array-pull

(defun st--utils-array-pull (field array)
  "Keep only FIELD in every alist in the ARRAY and flatten.

array-pull 'a '(((a . 1) (b . 2)) ((a . 3) (b . 4)))
returns '(1 3)"

  (--map (alist-get field it) array))

;;;; getJSON

(defun st--getJSON (url-buffer)
  "Parse the JSON in the URL-BUFFER returned by url."

  (with-current-buffer url-buffer
    (goto-char (point-max))
    (move-beginning-of-line 1)
    (json-read-object)))

;;; episodate.com API

;;;; search

(defun st--search (name)
  "Search episodate.com db for NAME."

  (->> (let ((url-request-method "GET"))
         (url-retrieve-synchronously (concat "https://www.episodate.com/api/search?q=" name)))
       st--getJSON
       (st--utils-alist-select '(tv_shows))
       car
       cdr
       (st--utils-array-select '(id name start_date status network))))

;;;; series

(defun st--episodes (series)
         (setf (alist-get 'episodes series)
               (mapcar (lambda (x) x) (alist-get 'episodes series)))
         series)

(defun st--series (id)
  "Get series ID info."

  (->> (let ((url-request-method "GET"))
         (url-retrieve-synchronously (concat "https://www.episodate.com/api/show-details?q=" (int-to-string id))))
       st--getJSON
       car
       (st--utils-alist-select '(id name start_date status episodes))
       st--episodes))

;;; Internal API

;;;; Data model

(defvar st--data
  nil
  "Internal data containing followed series and episode.

Of the form :

'(((id . seriesId) (props . value) (…) (episodes ((id . episodeId) (watched . t) (props.value) (…))
                                                 ((id . episodeId) (watched . nil) (props.value) (…)))))
  ((id . seriesId) (…) (episodes ((id . episodeId) (…))
                                 ((id . episodeId) (…)))))

series props are name and start_date.
episodes props are season, episode, name, and air_date.")

;;;; Methods

;;;;; Search series

(defun st-search (seriesName)
  "Search SERIESNAME."

  (st--search seriesName))

;;;;; Add series

(defun st-add (id)
  "Add series with ID to st--data.
Adding an already existing series resets it."

  (setq st--data
        (--> st--data
            (--remove (= id (alist-get 'id it)) it)
            (-snoc it (--> (st--series id))))))

;;;;; Remove series

(defun st-remove (id)
  "Remove series with ID from st--data."

  (setq st--data
        (--remove (= id (alist-get 'id it)) st--data)))

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

(defvar st--file
  "~/.emacs.d/st.el"
  "Location of the save file")

(defun st--save ()
  (with-temp-file st--file
    (prin1 st--data (current-buffer))))

(defun st--load ()
  (with-temp-buffer
    (insert-file-contents st--file)
    (cl-assert (eq (point) (point-min)))
    (setq st--data (read (current-buffer)))))

;;; Interface

;;;; Faces

(defface st-series
  '((t (:height 1.9 :weight bold :foreground "DeepSkyBlue")))
  "Face for series names")

(defface st-season
  '((t (:height 1.7 :weight bold :foreground "MediumPurple")))
  "Face for seasons")

(defface st-watched
  '((t (:foreground "DimGrey" :strike-through t)))
  "Face for watched episodes")

;;;; Draw buffer

(defun st-refresh ()
  "Refresh the st buffer.
Updates the database and redraws the buffer."
  (interactive)

  (if (and (string-equal (buffer-name) "st") (string-equal mode-name "st"))
      (let ((line (line-number-at-pos)))
        (st-update)
        (st--draw-buffer)
        (goto-line line))
    (message "Not in st buffer!")))

(defun st--draw-buffer ()
  "Draw the buffer.
Erase first then redraw the whole buffer."

  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "0")
    (put-text-property (point-min) (point) 'invisible t)
    (put-text-property (point-min) (point) 'st-series 0)
    (-each st--data 'st--draw-series)
    (delete-char (- 1))))

(defun st--draw-series (series)
  "Print the series id and name."

  (let ((id (alist-get 'id series))
        (name (alist-get 'name series))
        (episodes (alist-get 'episodes series)))
    (let ((start (point)))
      (insert (concat name "\n"))
      (set-text-properties start (point)
                           `(face st-series
                                  st-series ,id
                                  st-season nil
                                  st-episode nil)))
    (--each episodes (st--draw-episode id it))))

(defun st--draw-episode (series episode)
  "Print the episode id, S**E**, and name."

  (let ((season (alist-get 'season episode))
        (episode (alist-get 'episode episode))
        (name (alist-get 'name episode))
        (air_date (alist-get 'air_date episode))
        (watched (alist-get 'watched episode)))
    (when (= episode 1)
      (let ((start (point)))
        (insert (concat "Season " (int-to-string season) "\n"))
        (set-text-properties start (point)
                             `(face st-season
                                    st-series ,series
                                    st-season ,season
                                    st-episode nil))))
    (let ((start (point)))
      (insert air_date)
      (let ((end-date (point)))
        (insert (concat " " (format "%02d" episode) " - " name "\n"))
        (set-text-properties start (point)
                             `(face default
                                    st-series ,series
                                    st-season ,season
                                    st-episode ,episode))
        (put-text-property start end-date 'face '(t ((:foreground "MediumSpringGreen")))))
      (when watched
        (set-text-properties start (point)
                             `(face st-watched
                                    st-series ,series
                                    st-season ,season
                                    st-episode ,episode
                                    invisible st-watched))))))

;;;; Movements

(defun tvdb-up ()
  "Move up in the hierarchy."

  (interactive)

  (if (and (string-equal (buffer-name) "tvdb") (string-equal mode-name "tvdb"))
      (let ((inhibit-read-only t)
            (series (get-text-property (point) 'tvdb-series))
            (season (get-text-property (point) 'tvdb-season))
            (episode (get-text-property (point) 'tvdb-episode)))
        (cond (episode (goto-char (previous-single-property-change (point) 'tvdb-season)))
              (season (goto-char (previous-single-property-change (point) 'tvdb-series))))
    (message "Not in tvdb buffer!"))))

(defun tvdb-prev ()
  "Move up in the hierarchy."

  (interactive)

  (if (and (string-equal (buffer-name) "tvdb") (string-equal mode-name "tvdb"))
      (let ((inhibit-read-only t)
            (series (get-text-property (point) 'tvdb-series))
            (season (get-text-property (point) 'tvdb-season))
            (episode (get-text-property (point) 'tvdb-episode)))
        (cond (episode (goto-char (previous-single-property-change (point) 'tvdb-season)))
              (season (goto-char (previous-single-property-change (point) 'tvdb-season)))
              (series (goto-char (previous-single-property-change (point) 'tvdb-season)))))
    (message "Not in tvdb buffer!")))

(defun tvdb-next ()
  "Move up in the hierarchy."

  (interactive)

  (if (and (string-equal (buffer-name) "tvdb") (string-equal mode-name "tvdb"))
      (let ((inhibit-read-only t)
            (series (get-text-property (point) 'tvdb-series))
            (season (get-text-property (point) 'tvdb-season))
            (episode (get-text-property (point) 'tvdb-episode)))
        (cond (episode (goto-char (next-single-property-change (point) 'tvdb-season)))
              (season (goto-char (next-single-property-change (point) 'tvdb-season)))
              (series (goto-char (next-single-property-change (point) 'tvdb-season)))))
    (message "Not in tvdb buffer!")))


(defun tvdb-prev-same ()
  "Move up in the hierarchy."

  (interactive)

  (if (and (string-equal (buffer-name) "tvdb") (string-equal mode-name "tvdb"))
      (let ((inhibit-read-only t)
            (series (get-text-property (point) 'tvdb-series))
            (season (get-text-property (point) 'tvdb-season))
            (episode (get-text-property (point) 'tvdb-episode)))
        (cond (episode (goto-char (previous-single-property-change (point) 'tvdb-season)))
              (season (goto-char (previous-single-property-change (point) 'tvdb-season)))
              (series (goto-char (previous-single-property-change (point) 'tvdb-series)))))
    (message "Not in tvdb buffer!")))

(defun tvdb-next-same ()
  "Move up in the hierarchy."

  (interactive)

  (if (and (string-equal (buffer-name) "tvdb") (string-equal mode-name "tvdb"))
      (let ((inhibit-read-only t)
            (series (get-text-property (point) 'tvdb-series))
            (season (get-text-property (point) 'tvdb-season))
            (episode (get-text-property (point) 'tvdb-episode)))
        (cond (episode (goto-char (next-single-property-change (point) 'tvdb-season)))
              (season (goto-char (next-single-property-change (point) 'tvdb-season)))
              (series (goto-char (next-single-property-change (point) 'tvdb-series)))))
    (message "Not in tvdb buffer!")))

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

(defun st-fold-episodes ()
  "Fold the episodes at point."

  (let* ((season-start (previous-single-property-change (point) 'st-season))
         (fold-start (next-single-property-change season-start 'st-episode))
         (fold-end (next-single-property-change (point) 'st-season nil (point-max))))
    (put-text-property fold-start fold-end 'invisible 'st-folded nil)))

(defun st-fold-season ()
  "Fold the season at point."

  (let* ((fold-start (next-single-property-change (point) 'st-episode))
         (fold-end (next-single-property-change (point) 'st-season nil (point-max))))
    (put-text-property fold-start fold-end 'invisible 'st-folded nil)))

(defun st-fold-series ()
  "Fold the series at point."

  (let* ((fold-start (next-single-property-change (point) 'st-season))
         (fold-end (next-single-property-change (point) 'st-series nil (point-max))))
    (put-text-property fold-start fold-end 'invisible 'st-folded nil)))

(defun st-unfold-at-point ()
  "Unfold the section at point."

  (interactive)

  (if (and (string-equal (buffer-name) "st") (string-equal mode-name "st"))
      (let ((inhibit-read-only t)
            (series (get-text-property (point) 'st-series))
            (season (get-text-property (point) 'st-season))
            (episode (get-text-property (point) 'st-episode)))
        (cond (season (st-unfold-season))
              (t (st-unfold-series))))
    (message "Not in st buffer!")))

(defun st-unfold-season ()
  "Fold the season at point."

  (let* ((fold-start (next-single-property-change (point) 'st-episode))
         (fold-end (next-single-property-change (point) 'st-season nil (point-max))))
    (put-text-property fold-start fold-end 'invisible nil nil)))

(defun st-unfold-series ()
  "Fold the series at point."

  (let* ((fold-start (next-single-property-change (point) 'st-season))
         (fold-end (next-single-property-change (point) 'st-series nil (point-max))))
    (put-text-property fold-start fold-end 'invisible nil nil)))

(defun st-switch-watched ()
  "Switch visibility for watched episodes."

  (interactive)

  (if (-contains? buffer-invisibility-spec 'st-watched)
      (setq-local buffer-invisibility-spec '(t st-folded))
    (setq-local buffer-invisibility-spec '(t st-folded st-watched))))

;;;; Create mode

(defun st ()
  "Run ST"

  (interactive)
  (switch-to-buffer "st")
  (st-mode)
  (st-refresh))

(define-derived-mode st-mode special-mode "st"
  "Series tracking with episodate.com."

  (defun imenu-prev ()
    "imenu-prev-index-position-function for st."

    (let ((newpos (previous-single-property-change (point) 'st-series)))
      (if newpos
          (goto-char newpos)
        nil)))

  (defun imenu-name ()
    "imenu-extract-index-name-function for st."

    (let ((start (point)))
      (end-of-line)
      (buffer-substring start (point))))

  (setq-local buffer-invisibility-spec '(t st-folded))
  (setq-local imenu-prev-index-position-function 'imenu-prev)
  (setq-local imenu-extract-index-name-function 'imenu-name))

;;; Postamble

(provide 'seriesTracker)

;;; Example

(st-search "utopia")
(st-search "game of thrones")
(st-search "rick and morty")

(st--series 31085)
(st--series 23455)
(st--series 32157)

(st-add 31085)
(st-add 23455)
(st-add 32157)

(st-remove 23455)

(tvdb-watch 275274 7687399)

(tvdb-watch-all 264991)

(tvdb-watch-up 275274 6231155)

(tvdb-update)

(st--save)

(st--load)

;;; seriesTracker.el ends here