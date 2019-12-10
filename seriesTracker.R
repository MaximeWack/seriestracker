library(tidyverse)
library(httr)
library(rvest)
library(DBI)
library(RSQLite)

options(scipen = 999)

#' Authenticate to theTVdbAPI
#'
#' @param username User name
#' @param userkey User key
#' @param apikey API key
#' @return A token for theTVdbAPI
authenticate <- function(username, userkey, apikey)
{
  list(apikey = apikey,
       userkey = userkey,
       username = username) -> body

  POST("https://api.thetvdb.com/login",
       config = add_headers(ContentType = "application/json",
                            Accept = "application/json"),
       body = body,
       encode = "json") %>%
    content %>%
    .$token
}

#' Search for a show
#'
#' @param db A db object created with `openDb`
#' @param search A search query
#' @return The search matches
search <- function(db, search)
{
  GET(str_c("https://api.thetvdb.com/search/series?name=", search),
      config = add_headers(Accept = "application/json",
                           Authorization = str_c("Bearer ", db$token))) %>%
    content %>%
    .$data -> res

  tibble(id = res %>% map_int("id"),
         name = res %>% map_chr("seriesName"))
}

help <- function(db)
{
  print("Pipe db to the following verbs\n
- list for a list of shows\n
- search to search a show\n
- addSeries to add a show\n
- removeSeries to remove a show\n
- watch to mark an episode as watched\n
- watchTill to mark all previous episodes as watched\n
- nextUp to see all available and upcoming episodes\n
- toWatch to see all available episodes\n
\n
Then finally closeDb to save the changes to the database"
}

#' Get the name of a series from its id
#'
#' @param series Series id
#' @param token The API token
#' @return The series name
getSeries <- function(series, token)
{
  GET(str_c("https://api.thetvdb.com/series/", series),
      config = add_headers(Accept = "application/json",
                           Authorization = str_c("Bearer ", token))) %>%
    content -> content

  content$data$seriesName
}

#' Get the details for an episode from its id
#'
#' @param episode Episode id
#' @param token The API token
#' @return The airing date and series id for the episode
getEpisode <- function(episode, token)
{
  GET(str_c("https://api.thetvdb.com/episodes/", episode),
      config = add_headers(Accept = "application/json",
                           Authorization = str_c("Bearer ", token))) %>%
    content -> content

    content$data -> data

  tibble(id = data$id,
         date = data$firstAired,
         series = data$seriesId)
}

#' Get the list of episodes for a series
#'
#' @param series The series id
#' @param token The API token
#' @param page The page of the results to get
#' @return The list of episodes with season and episode numbers, the name, and the airing date
getEpisodes <- function(series, token, page = 1)
{
  GET(str_c("https://api.thetvdb.com/series/", series, "/episodes?page=", page),
      config = add_headers(Accept = "application/json",
                           Authorization = str_c("Bearer ", token))) %>%
    content -> content

  content$data -> data

  tibble(id = data %>% map_int("id"),
         season = data %>% map_int("airedSeason"),
         episode = data %>% map_int("airedEpisodeNumber"),
         name = data %>% map_chr("episodeName"),
         date = data %>% map_chr("firstAired")) %>%
    filter(season > 0) %>%
    filter(date != "") %>%
    mutate(date = date %>% as.POSIXct) %>%
    arrange(season, episode) -> res

  if (!content$links$`next` %>% is.null)
    res %>%
      bind_rows(getEpisodes(series, token, content$links$`next`))
  else
    res
}

#' Open or create the database
#'
#' @param username User name to use to create the db
#' @param userkey User key to use to create the db
#' @param apikey Api key to use to create the db
#' @return An initialized db objet
openDb <- function(username = NULL, userkey = NULL, apikey = NULL)
{
  dbConnect(RSQLite::SQLite(), "~/tv.db") -> db

  if (dbListTables(db) %>% length == 0)
  {
    if (any(list(username, userkey, apikey) %>% map_lgl(is.null)))
      stop("Please provide username, userkey, and apikey")

    list(db = db,
         credentials = data.frame(username = username,
                                  userkey = userkey,
                                  apikey = apikey),
         token = authenticate(username, userkey, apikey),
         shows = data.frame(id = integer(0), name = character(0)),
         watched = data.frame(id = integer(0)))
  } else {
    credentials = dbGetQuery(db, "SELECT * from credentials")
    list(db = db,
         credentials = credentials,
         token = authenticate(credentials$username, credentials$userkey, credentials$apikey),
         shows = dbGetQuery(db, "SELECT * FROM shows"),
         watched = dbGetQuery(db, "SELECT * FROM watched"))
  }
}

#' Save and close the database
#'
#' Writes all information and disconnects the database
#'
#' @param db The db object to use
closeDb <- function(db)
{
  dbWriteTable(db$db, "credentials", db$credentials, overwrite = T)
  dbWriteTable(db$db, "shows", db$shows, overwrite = T)
  dbWriteTable(db$db, "watched", db$watched, overwrite = T)
  dbDisconnect(db$db)
}

#' Add a series
#'
#' @param db The db object to use
#' @param id The series id to add
#' @return The db object with the added series
addSeries <- function(db, id)
{
  db$shows <- db$shows %>%
    add_row(id = id, name = getSeries(id, db$token)) %>%
    distinct

  db
}

#' Add a series
#'
#' @param db The db object to use
#' @param showId The series id to remove
#' @return The db object with the removed series
removeSeries <- function(db, showId)
{
  db$shows <- db$shows %>%
    filter(id != showId)

  db$watched <- db$watched %>%
    anti_join(getEpisodes(showId, db$token))

  db
}

#' Watch an episode
#'
#' @param db The db object to use
#' @param episode The episode to watch
#' @return The db object with the watched episode
watchEp <- function(db, episode)
{
  db$watched <- db$watched %>%
    add_row(id = episode) %>%
    distinct

  db
}

#' Watch multiple episodes
#'
#' @param db The db object to use
#' @param episodes A vector of episodes
#' @return The db object with the watch episodes
watch <- function(db, episodes) reduce(episodes, watchEp, .init = db)

#' Watch all episodes from a series up to the given one
#'
#' @param db The db object to use
#' @param episode The episode to watch up to
#' @return The db object with the removed episodes
watchTill <- function(db, episode)
{
  ep <- getEpisode(episode, db$token)

  db$watched <- db$watched %>%
    bind_rows(getEpisodes(ep$series, db$token) %>%
                anti_join(db$watched) %>%
                filter(date <= ep$date) %>%
                select(id)) %>%
    distinct

  db
}

#' Show the upcoming episodes for the series in the db
#'
#' @param db The db object to use
#' @return The next episodes to watch, sorted by airing date
upNext <- function(db)
{
  tibble(showId = db$shows$id) %>%
    mutate(data = showId %>% map(getEpisodes, token = db$token)) %>%
    unnest(data) %>%
    bind_rows %>%
    anti_join(db$watched) %>%
    inner_join(db$shows, by = c("showId" = "id")) %>%
    select(show = name.y,
           id,
           season,
           episode,
           name = name.x,
           date) %>%
  arrange(date)
}

#' Show only the available episodes not yet watched
#'
#' @param db The db object to use
#' @return The available episodes to watch
toWatch <- function(db)
{
  db %>%
    upNext %>%
    filter(date < lubridate::today())
}

################################################################################

  openDb() -> db

  db %>% toWatch
  db %>% upNext

  db %>%
    watch(db %>% toWatch %>% pull(id)) %>%
    closeDb
