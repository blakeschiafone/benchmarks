#' description
#' graph a called metric by vertical
#'
#'
#' params
#' highlight = REQUIRED.  The namespace that will be used in benchmark
#' facet_metric = OPTIONAL.  If False, then chart facets by channel.  If True, then chart facets by metric. 
#' vertical_input = OPTIONAL.  If False, then the vertical in Salesforce is used.  Otherwise, you can override the vertical to benchmark against
#' date_begin = OPTIONAL.  If False, then begins on Jan 1 of previous year
#' date_end = OPTIONAL.  If False, then ends on Dec 31 of previous year
#' 
#' 
#' example
#' benchmark(metric = 'uninstall', 'vertical_input' = 'Social', highlight = 'tumblr', date_begin = '2016-06-01')
#' benchmark(metric = 'engagement', highlight = 'hooq')

benchmark_channel <- function(highlight, facet_metric = FALSE, vertical_input = FALSE, date_begin = FALSE, date_end = FALSE){
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(ggplot2)) 
  source('./functions/find_percentile.R', local = TRUE)
  source('./functions/find_metric.R', local = TRUE)
  source('./functions/find_plot_color.R', local = TRUE)
  options(error = stop)
  
  #@PARAMETERS
  metric <- 'all'
  metric_list <- c('all', 'uninstall', 'engagement', 'goal', 'optout', 'clicks', 'opens', 'unsubs')
  scales_flag <- ifelse(facet_metric, 'free_y', 'free_x')
  if(date_begin == FALSE){
    date_begin <- lubridate::floor_date(seq(from = Sys.Date(), by = '-1 year', length.out = 2)[2], unit = 'year')
  } else {
    date_begin <- as.Date(date_begin)
  }
  if(date_end == FALSE){
    date_end <- lubridate::ceiling_date(seq(from = Sys.Date(), by = '-1 year', length.out = 2)[2], unit = 'year') - 1
  } else {
    date_end <- as.Date(date_end)
  }
  
  #check if namespace used in benchmark has data
  #if it does not, then error out
  if(!(highlight %in% unique(daily_counters$table_id))){
    warning(paste0(toupper(highlight), ' does not exist is data'))
    warning_flag <- 1
  } else if (sum(daily_counters$total[daily_counters$table_id == highlight & daily_counters$name %in% c('delivered', 'ia_delivered')]) < 1000){
    warning(paste0(toupper(highlight), ' does not have enough data to benchmark'))
    warning_flag <- 1
  }
  
  #check if metric exists in available list
  # tryCatch(metric %in% metric_list)
  if (!(metric %in% metric_list)){
    warning(c(paste0(toupper(metric), ' does not exist in available list of '), paste0(metric_list, sep = ', ')))
    warning_flag <- 1
  }
  
  #check if vertical_input is FALSE
  #if it is, we need to look-up what namespace was inputted
  #and find the matching vertical
  if(vertical_input == FALSE){
    vertical_input <- customers$Industry[customers$Namespace == highlight]
  }
  
  #create vertical list filter.  the list is every namespace
  #that falls into the vertical.  also, need to test length of
  #vector to make sure it's >= 3 namespaces in the industry
  #otherwise, it should produce an error saying the vertical
  #does not have enough namespaces to compile benchmark
  vertical_list <- as.list(na.omit(customers$Namespace[customers$Industry == vertical_input]))
  vertical_list <- unique(daily_counters$table_id[daily_counters$table_id %in% vertical_list])
  vertical_list <- c(vertical_list, highlight)
  if(length(vertical_list) <= 2){
    stop(c(paste0(toupper(vertical_input), ' does not have enough namespaces to complete benchmark.  There are ', length(vertical_list), '.  Expect 3+')))
    warning_flag <- 1
  }
  
  #define metrics to pass in functions below
  #metric_list currently holds supported graphs
    metric_args <- c('delivered', 'ia_delivered', 'delivered_ghost', 'engaged', 'goal', 'opt_outs', 'clicks', 'email_unique_opens', 'unsubs', 'uninstalled_ghost', 'email_unique_clicks')
    
  
  if(!(exists('warning_flag'))){
    daily_counters %>%
      filter(table_id %in% vertical_list) %>%
      filter(date >= date_begin,
             date <= date_end) %>%
      filter(name %in% metric_args) %>%
      mutate(name = ifelse(name == 'ia_delivered', 'delivered', name),
             channel = ifelse(channel == 'email', 'Email', channel),
             channel = ifelse(channel == 'push', 'Push', channel),
             channel = ifelse(channel == 'in_app', 'In App', channel),
             channel = factor(channel, levels = c('Push', 'Email', 'In App'))) %>% 
      select(table_id, channel, name, total) %>%
      group_by(table_id, channel, name) %>% 
      summarize_each(., funs(sum)) %>%
      tidyr::spread(name, total) %>%
      assign('names', ., pos = -1) %>%
      mutate_(.dots = find_metric()) %>%
      #mutate(result = ifelse(is.nan(result), NA, result)) %>% 
      #filter(!is.na(result)) %>%
      mutate(industry = customers$Industry[match(table_id, customers$Namespace)],
             type = ifelse(table_id == highlight, 'colored', 'not colored')) -> highlight_vertical
      metric_args <- metric_args[metric_args != 'ia_delivered']
      highlight_vertical <- highlight_vertical %>% select(-one_of(metric_args))
    
    
    
    #determine channels that exist for benchmarked customer
    #create a list of channels, and then use this list for graph output
    #this is to prevent showing a channel benchmark that a customer does
    #not participate in.  for example, showing email benchmarks but customer
    #does not send emails
    highlight_channels <- highlight_vertical$channel[highlight_vertical$table_id == highlight]
    highlight_vertical <- highlight_vertical[highlight_vertical$channel %in% highlight_channels,]
  
    #get lowerbound and upperbound of tibble.  need to know column index
    #to correctly reshape dataframe from wide to long for plots
    lowerbound <- which(names(highlight_vertical) == 'channel') + 1
    upperbound <- which(names(highlight_vertical) == 'industry') - 1
  
    highlight_vertical <- tidyr::gather(highlight_vertical, name, value, lowerbound:upperbound)
    highlight_vertical <- highlight_vertical %>% filter(value <= 100)
    
    
  
    #plot data
    #red plot point
    highlight_geom_point <- highlight_vertical %>% ungroup() %>% filter(type == 'colored') %>% select(channel, name, value) 
    highlight_geom_point$key <- stringr::str_c(highlight_geom_point$channel, highlight_geom_point$name)
    #median value
    highlight_median_values <- highlight_vertical %>% ungroup() %>% select(-c(type, industry, table_id)) %>% group_by(channel, name) %>% summarize(mean = median(value, na.rm = TRUE)) 
    #match median value with highlight_geom_point
    #then determine plot point color
    #if below median, red.  else green
    highlight_median_values$key <- stringr::str_c(highlight_median_values$channel, highlight_median_values$name)
    highlight_geom_point$median <- highlight_median_values$mean[match(highlight_geom_point$key, highlight_median_values$key)]
    highlight_geom_point <- mutate(highlight_geom_point, color_fill = find_plot_color(name, value, median))
    chart <- ggplot(highlight_vertical, aes_string(x = ifelse(facet_metric, 'channel', 'name'), y = 'value')) + 
      theme_bw() + 
      geom_boxplot() +
      theme(axis.text.x = element_text(hjust = 1, angle = 45),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = 'none') +
      scale_y_continuous(labels = scales::percent) +
      facet_wrap(as.formula(paste('~', ifelse(facet_metric, 'name', 'channel'))), scales= scales_flag) +
      geom_point(data = highlight_geom_point, aes(color = color_fill), size = 3) + 
      scale_color_manual(values = c('green' = '#55a860', 'red' = 'red')) +
      labs(title = stringr::str_c('\n', stringr::str_to_title(highlight), ifelse(facet_metric, ': Benchmarks Across Metrics', ': Benchmarks Across Channels')),
           subtitle = stringr::str_c('Comparing against vertical: ', vertical_input),
           caption = paste0('\nBased on data between ', format(date_begin, '%B %d, %Y'),' - ', format(date_end, '%B %d, %Y')))
  } else {
    chart <- NA
  }
    
return(chart)
    
gc()

}
