compile_benchmarks <- function(highlight, vertical_input = FALSE, date_begin = FALSE, date_end = FALSE){
  library(dplyr)
  library(ggplot2)
  library(svglite)
  library(knitr)
  #library(purrr)
  
  setwd('/home/rstudio/scripts/benchmarks/')
  source('./functions/benchmark_vertical.R')
  source('./functions/benchmark_channel.R')
  source('./functions/benchmark_retention.R')
  source('./functions/benchmark_optin.R')
  source('./functions/function_is_error.R')
  #' borrowed functions
  source('/home/rstudio/scripts/daily_feed/functions/function_last_file.R')
  #' load connections
  source('/home/rstudio/scripts/db_connection.R')
  
  file_path <- '/home/rstudio/gdrive/Reports/Benchmarks/'
  
  
  if(date_begin == FALSE){
    date_begin <- lubridate::floor_date(seq(from = Sys.Date(), by = '-1 year', length.out = 2)[2], unit = 'year')
  } else {
    date_begin <- as.Date(date_begin)
  }
  if(date_end == FALSE){
    date_end <- lubridate::ceiling_date(seq(from = Sys.Date(), by = '-1 month', length.out = 2)[1], unit = 'month') - 1
  } else {
    date_end <- as.Date(date_end)
  }
  
  
  #' gather namespace data from database connection
  flush.console()
  print('gathering namespace data from database')
  print('counters')
  daily_counters <- dbGetQuery(db_connection, paste0("select table_id, date, name, label, channel, campaign_type, total from bq.counters 
                                                     where date >='", date_begin, "' and date <='", date_end, "' and
                                                     name in ('delivered', 'ia_delivered', 'delivered_ghost', 'engaged', 'goal', 'opt_outs', 
                                                     'clicks', 'opens', 'email_unique_opens', 'unsubs', 'uninstalled_ghost', 'email_unique_clicks')"))
  assign('daily_counters', value = daily_counters, pos = 1)
  gc()
  
  flush.console()
  print('retention')
  retention <- list.files(path = '/home/rstudio/scripts/daily_feed/output/retention/',
                          pattern = 'weekly_retention*',
                          full.names = TRUE) %>%
               purrr::map_df(~read_csv(.))
  assign('retention', value = retention, pos = 1)
  
  flush.console()
  print('opt_in')
  opt_in <- last_file(location = '/home/rstudio/scripts/daily_feed/output/opt_in/', pattern_find = 'monthly') %>%
            purrr::map_df(~read_csv(.))
  assign('opt_in', value = opt_in, pos = 1)
  
  flush.console()
  print('customers')
  customers <- read_csv(file = '/home/rstudio/scripts/daily_feed/output/customers/salesforce_data.csv')
  customers <- customers[!(is.na(customers$Namespace)),]
  assign('customers', value = customers, pos = 1)
  
  
  flush.console()
  print('vertical_engagement')
  vertical_engagement_graph <- benchmark_vertical(highlight = highlight,
                                            vertical_input = vertical_input,
                                            metric = 'engagement',
                                            date_begin = date_begin,
                                            date_end = date_end)

  flush.console()
  print('vertical_opens')
  vertical_opens_graph <- benchmark_vertical(highlight = highlight,
                                            vertical_input = vertical_input,
                                            metric = 'opens',
                                            date_begin = date_begin,
                                            date_end = date_end)

  flush.console()
  print('vertical_clicks')
  vertical_clicks_graph <- benchmark_vertical(highlight = highlight,
                                            vertical_input = vertical_input,
                                            metric = 'clicks',
                                            date_begin = date_begin,
                                            date_end = date_end)

  flush.console()
  print('vertical_goal')
  vertical_goal_graph <- benchmark_vertical(highlight = highlight,
                                            vertical_input = vertical_input,
                                            metric = 'goal',
                                            date_begin = date_begin,
                                            date_end = date_end)

  flush.console()
  print('vertical_optout')
  vertical_optout_graph <- benchmark_vertical(highlight = highlight,
                                            vertical_input = vertical_input,
                                            metric = 'optout',
                                            date_begin = date_begin,
                                            date_end = date_end)

  flush.console()
  print('vertical_uninstall')
  vertical_uninstall_graph <- benchmark_vertical(highlight = highlight,
                                            vertical_input = vertical_input,
                                            metric = 'uninstall',
                                            date_begin = date_begin,
                                            date_end = date_end)

  flush.console()
  print('channel')
  channel_graph <- benchmark_channel(highlight = highlight,
                               vertical_input = vertical_input,
                               date_begin = date_begin,
                               date_end = date_end)

  flush.console()
  print('channel_facet')
  channel_facet_graph <- benchmark_channel(highlight = highlight,
                                     vertical_input = vertical_input,
                                     facet_metric = TRUE,
                                     date_begin = date_begin,
                                     date_end = date_end)
  
  flush.console()
  print('retention')
  retention_graph <- benchmark_retention(highlight = highlight, 
                                   vertical_input = vertical_input, 
                                   chart = 'line', 
                                   date_begin = date_begin,
                                   date_end = date_end)
  flush.console()
  print('opt_in')
  opt_in_graph <- benchmark_optin(highlight = highlight,
                            vertical_input = vertical_input)
  


  rm(raw, daily_counters, customers, db_connection, retention, opt_in)
  gc()
  gc()
  
  #' check if directory exists for namespace
  if(file.exists(paste0(file_path, highlight)) == FALSE){
    dir.create(paste0(file_path, highlight))
  }

  

  #knit benchmark report together
  rmarkdown::render('/home/rstudio/scripts/benchmarks/scripts/render_html.R',
                    output_dir = paste0(file_path, highlight, '/'),
                    output_file = paste0('benchmarks_', 
                                         format(Sys.Date(), format = '%b%d%Y'),
                                         '_',
                                         toupper(vertical_input_value),
                                         '_',
                                         date_begin, ' to ',
                                         date_end, 
                                         '.html'))
  
  
  #push file to googledrive
  system(paste0("/home/rstudio/go/bin/drive push -ignore-name-clashes -exclude-ops \"delete\" -force ", 
                '"/home/rstudio/gdrive/Reports/Benchmarks/', highlight, "/", '"'))

#' delete benchmark file from server, otherwise, each time bench runs
#' any old files sitting on server are pushed to google drive
# system(paste0("rm /home/rstudio/gdrive/Reports/Benchmarks/", highlight, "/bench*"))

}
