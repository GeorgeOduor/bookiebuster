rm(list = ls())
library(glue);library(httr);library(dplyr);library(jsonlite);library(purrr);library(tidyr);library(tibble);library(rvest)
library(stringr);library(R6);library(futile.logger);library(logging)

classOddsScrapper <- R6Class(public = list(
  headers = c(
    `authority` = "www.oddsportal.com",
    `accept` = "application/json, text/plain, */*",
    `accept-language` = "en-US,en;q=0.9",
    `dnt` = "1",
    `referer` = "https://www.oddsportal.com/football/",
    `sec-ch-ua` = '"Not_A Brand";v="8", "Chromium";v="120", "Google Chrome";v="120"',
    `sec-ch-ua-mobile` = "?0",
    `sec-ch-ua-platform` = '"Windows"',
    `sec-fetch-dest` = "empty",
    `sec-fetch-mode` = "cors",
    `sec-fetch-site` = "same-origin",
    `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
    `x-requested-with` = "XMLHttpRequest",
    `x-xsrf-token` = "eyJpdiI6ImQ3Uml2Z3dqb3VicEVuSk1IS3B2RkE9PSIsInZhbHVlIjoiMUYzRmc3MFdRZVhaWTA2ck01bkcrYUo2d3FTenpxNi8zRW8rRFpHQlRxWUJwc2luRkZUc3VUQ2JmWGxiQWhkeGZkbVBJZU5oNFNvMTZ2QUVidmg3REFLSlJqaFIxaUhkTWppa3cyWlB3UlhOWjlnNUJsNFJreU9kZVg2MnJ0bXYiLCJtYWMiOiI1ODMxNDQ1NDdjZWQ3YzVlZTUzMTlhODE4ZGYwNmE5M2JhMGJjMzE5YjUwOGYxZTkxNThiN2Y0NWMzMjYzMjFlIiwidGFnIjoiIn0="
  ),
  res = "https://www.oddsportal.com/ajax-getSportsMenuDataBySports/1/3/",
  db = NA,
  initialize = function(db){
    self$res = httr::GET(self$res, httr::add_headers(.headers=self$headers))
    self$db = db
  },
  primaryLinks = function(res) {
    # Configure the logger

    tryCatch({
      primary_links <- (res$content %>% rawToChar() %>% fromJSON())$`1` %>%
        map(
          .f = function(x) {
            x %>%
              pluck("inner_sub") %>%
              map(
                .f = function(x) {
                  pluck(x, 'tournament_url')
                }
              ) %>% unlist()
          }
        ) %>% unlist() %>%
        as_tibble() %>%
        mutate(value = paste0("https://www.oddsportal.com", value, "results")) %>%
        pluck("value")

      flog.info(glue("Found {length(unique(primary_links))} unique links"))
      return(unique(primary_links))
    }, error = function(e) {
      flog.error(glue("Error in primaryLinks function: {e$message}"))
      return(NULL)
    })
  },
  availableSeasons = function(primary_links) {
    # Configure the logger
    flog.threshold(INFO)
    tryCatch({
      availableSeasons <- read_html(primary_links) %>%
        html_elements("select") %>%
        html_elements("option") %>%
        html_attr("value")

      flog.info(glue("Found {length(availableSeasons)} available seasons in {primary_links}"))
      return(availableSeasons)
    }, error = function(e) {
      flog.error(glue("Error in availableSeasons function: {e$message}"))
      return(NULL)
    })
  },
  season = function(url) {
    # Configure the logger
    flog.threshold(INFO)
    tryCatch({
      odds_url <- read_html(url) %>%
        html_element("tournament-component") %>%
        html_attr(":sport-data") %>%
        fromJSON() %>% pluck("oddsRequest") %>% pluck("url")
      match_id = unlist(strsplit(odds_url,"/"))
      match_id <- unlist(match_id)[length(match_id)]
      out <- list(
        odds_url = paste0("https://www.oddsportal.com", odds_url, "X0/1/0/?_=1703600980101"),
        match_id = match_id
      )
      flog.info(glue("Processed season for tournament ID: {match_id}"))
      return(out)
    }, error = function(e) {
      flog.error(glue("Error in season function: {e$message}"))
      return(NULL)
    })
  },
  seasonPages = function(season) {
    # Configure the logger
    flog.threshold(INFO)

    tryCatch({
      res <- httr::GET(url = season$odds_url, httr::add_headers(.headers = self$headers))

      matches_page <- res$content %>% rawToChar() %>% fromJSON()

      available_pages <- matches_page$d$pagination$pageCount

      page_links <- map(seq(1, available_pages), .f = function(x) {
        paste0("https://www.oddsportal.com/ajax-sport-country-tournament-archive_/1/", season$match_id, "/X0/1/0/page/", x, "/?_=1703603025281")
      }) %>% unlist()

      flog.info(glue("Found {length(page_links)} pages for season {season$match_id}"))

      return(page_links)
    }, error = function(e) {
      flog.error(glue("Error in seasonPages function: {e$message}"))
      return(NULL)
    })
  },
  pageOdds = function(page) {
    # Configure the logger
    flog.threshold(INFO)

    tryCatch({
      res <- httr::GET(url = page, httr::add_headers(.headers = init$headers))

      match_odds <- fromJSON(rawToChar(res$content))

      odds_summary <- match_odds$d$rows$odds %>%
        map_df(.f = function(x) {
          select(x,
                 id = eventId, avgOdds, maxOdds) %>%
            mutate(Outcome_categories = c('home', 'draw', 'away')) %>%
            reshape2::melt(id.vars = c("id", "Outcome_categories")) %>%
            reshape2::dcast(
              id ~ Outcome_categories + variable,
              value.var = "value",
              fun.aggregate = sum,
              na.rm = TRUE
            ) %>%
            select(
              id, contains(c("home", "draw", "away"))
            )
        })

      all_odds <- match_odds$d$rows %>%
        select(id, matchid = encodeEventId, name, `country-name`, `tournament-name`, `home-name`, `away-name`, result, `home-winner`, `away-winner`, url, `away-participant-images`, `home-participant-images`,`date-start-base`) %>%
        mutate_at(vars(url, `home-participant-images`, `away-participant-images`), .funs = function(x) {
          paste0("https://www.oddsportal.com", x)
        }) %>%
        inner_join(odds_summary, by = 'id')

      flog.info(glue("Processed odds for page: {page}"))

      return(all_odds)
    }, error = function(e) {
      flog.error(glue("Error in pageOdds function: {e}"))
      return(NULL)
    })
  },
  oddsCompare = function(matchurl) {
    # Configure the logger
    flog.threshold(INFO)

    tryCatch({
      res <- httr::GET(url = matchurl, httr::add_headers(.headers = self$headers))

      pattern <- "/(\\d+-\\d+-\\w+-\\d+-\\d+-\\w+)\\.dat"

      match <- str_extract(str_replace_all(rawToChar(res$content), "\n| ", ""), pattern)

      if (!is.na(match)) {
        odds_link <- paste0("https://www.oddsportal.com/feed/match-event", match)
        res <- httr::GET(url = odds_link, httr::add_headers(.headers = self$headers))

        odds_compare <- res$content %>%
          rawToChar() %>%
          fromJSON()

        match_id = gsub("/", "", unlist(strsplit(matchurl, "-")))

        odds_ <- odds_compare$d$oddsdata$back$`E-1-2-0-0-0`$odds %>%
          map2_df(., names(.), .f = function(x, y) {
            as_tibble(x) %>%
              mutate(
                bookieid = y
              ) %>%
              select(
                bookieid, home_odds = `0`, draw_odds = `1`, away_odds = `2`
              ) %>%
              mutate(
                bookieid = as.numeric(bookieid),
                matchid = match_id[length(match_id)]
              )
          }) %>%
          arrange(bookieid) %>%
          select(matchid, everything())

        flog.info(glue("Processed odds for matchurl: {matchurl}"))

        return(odds_)
      }
    }, error = function(e) {
      flog.error(glue("Error in oddsCompare function: {e$message}"))
      return(NULL)
    })
  },
  main = function(number = NULL,seasons_count = NULL,primary_links) {
    con <- self$db
    # get available league links
    #primary_links <- self$primaryLinks(self$res)
    # if (!is.null(number)) {
    #   primary_links <- primary_links[seq(number)]
    # }

    odds <- map(primary_links,.f = function(x){
      #available seasons
      fetched_seasons <- self$availableSeasons(x)
      if (!is.null(seasons_count)) {
        fetched_seasons <- fetched_seasons[seq(seasons_count)]
      }
      return(fetched_seasons)
      })

    odds %>%
      unlist() %>%
      map(# available pages per season
        self$season) %>%
      map(self$seasonPages) %>%
      unlist() %>%
      map(.f = function(x){
        tryCatch({
          odds <- self$pageOdds(x)
          if (!is.null(odds)) {

            odds$url %>%
              map(.f = function(x) {
                tryCatch({
                  if (!is.null(x)) {
                    # DBI::dbWriteTable(con,"odds_comparison",self$oddsCompare(x),append = T)
                  }
                },error = function(e){
                  flog.error(glue("Error in main function: {e$message}"))
                })
              }
              )
            if (!is.null(odds)) {
              # chek if the table exist
              if (nrow(gs4_find("match_avgodds")) == 0) {
                ss <- gs4_create("match_avgodds", sheets = odds)
                }else {
                  ss = gs4_find("match_avgodds")
                  sheet_append(ss,data = odds)
              }
              # DBI::dbWriteTable(con, "match_avgodds", odds, append = T)
            }

            }
        },error=function(e){
          flog.error(glue("Error in main function: {e$message}"))
        })
      })

    return(odds)
  }
))
init <- classOddsScrapper$new(db = NULL)
primaryLinks <- init$primaryLinks(res = init$res) %>% unique()

# con <- DBI::dbConnect(RSQLite::SQLite(),glue("inst/app/bookiebuster_{group}.db"))
init <- classOddsScrapper$new(db = con)
init$main(primary_links = variable$links)

# files <- tibble(links=primaryLinks) %>%
#   mutate(group = rep(1:20,length.out = nrow(.))) %>%
#   split(.$group)

# for (variable in files) {
#   group <- pull(variable,group) %>% unique()
#   job::job(title = paste0("Group_",group) ,{
#     library(glue);library(httr);library(dplyr);library(jsonlite);library(purrr);library(tidyr);library(tibble);library(rvest);library(stringr);library(R6);library(futile.logger);library(logging)
#     con <- DBI::dbConnect(RSQLite::SQLite(),glue("inst/app/bookiebuster_{group}.db"))
#     init <- classOddsScrapper$new(db = con)
#     # primaryLinks <- init$primaryLinks(res = init$res)
#     seasons_ <- init$main(primary_links = variable$links)
#   }, import = "all")

# }
# available_data <- function(con) {
#   match_avgodds <- dir("inst/app/",pattern = ".db") %>%
#     map_df(.f = function(x){
#       con <- DBI::dbConnect(RSQLite::SQLite(),glue("inst/app/{x}"))
#       # dbListTables(con)
#       tbl(con,"match_avgodds") %>% collect()
#     })
#   return(match_avgodds)
# }




