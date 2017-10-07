#' description
#' graph a called metric by vertical
#'
#'
#' params
#' highlight = REQUIRED.  The namespace that will be used in benchmark
#' metric = REQUIRED.  The metric that will be benchmarked
#' chart = OPTIONAL.  If False, then chart is boxplot.  Other parameter is 'line'
#' vertical_input = OPTIONAL.  If False, then the vertical in Salesforce is used.  Otherwise, you can override the vertical to benchmark against
#' date_begin = OPTIONAL.  If False, then begins on Jan 1 of previous year
#' date_end = OPTIONAL.  If False, then ends on Dec 31 of previous year
#'
#'
#' example
#' benchmark(metric = 'uninstall', 'vertical_input' = 'Social', highlight = 'tumblr', date_begin = '2016-06-01')
#' benchmark(metric = 'engagement', highlight = 'hooq')

benchmark_vertical <- function(highlight, metric, chart = FALSE, vertical_input = FALSE, date_begin = FALSE, date_end = FALSE){
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(ggplot2)) 
  source('./functions/find_percentile.R', local = TRUE)
  source('./functions/find_plot_color.R', local = TRUE)
  options(error = stop)
  
  #@PARAMETERS
  metric_list <- c('uninstall', 'engagement', 'goal', 'optout', 'clicks', 'opens')
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
  assign('vertical_input_value', vertical_input, pos = 1)
  
  #create vertical list filter.  the list is every namespace
  #that falls into the vertical.  also, need to test length of
  #vector to make sure it's >= 3 namespaces in the industry
  #otherwise, it should produce an error saying the vertical
  #does not have enough namespaces to compile benchmark
  vertical_list <- as.list(na.omit(customers$Namespace[customers$Industry == vertical_input]))
  vertical_list <- unique(daily_counters$table_id[daily_counters$table_id %in% vertical_list])
  vertical_list <- c(vertical_list, highlight)
  if(length(vertical_list) <= 2){
    warning(c(paste0(toupper(vertical_input), ' does not have enough namespaces to complete benchmark.  There are ', length(vertical_list), '.  Expect 3+')))
    warning_flag <- 1
  }
  
  #define metrics to pass in functions below
  #metric_list currently holds supported graphs
  if (metric == 'uninstall'){
    metric_args <- c('delivered_ghost', 'uninstalled_ghost')
  } else if (metric == 'engagement'){
    metric_args <- c('delivered', 'ia_delivered', 'engaged')
  } else if (metric == 'goal'){
    metric_args <- c('delivered', 'ia_delivered', 'goal')
  } else if (metric == 'optout'){
    metric_args <- c('delivered_ghost', 'opt_outs')
  } else if (metric == 'clicks'){
    metric_args <- c('delivered', 'clicks')
  } else if (metric == 'opens'){
    metric_args <- c('delivered', 'opens')
  }
  
  
  daily_counters %>%
    filter(date >= date_begin,
           date <= date_end) %>%
    filter(name %in% metric_args) %>%
    mutate(name = ifelse(name == 'ia_delivered', 'delivered', name),
           category = ifelse(table_id %in% vertical_list, 'client_vertical', 'outside_vertical')) %>% 
    select(category, table_id, name, total) %>%
    group_by(category, table_id, name) %>% 
    summarize_each(., funs(sum)) %>%
    tidyr::spread(name, total) %>%
    mutate_(.dots = list(result = 
                           substitute(round(a/b, digits = 5) * 100, list(
                             a = ifelse(length(metric_args) == 2, as.name(metric_args[2]), as.name(metric_args[3])),
                             b = as.name(metric_args[1])))
                        )
            ) %>%
    mutate(result = ifelse(is.nan(result), NA, result)) %>% 
    filter(!is.na(result),
           result <= 100) %>%
    mutate(industry = customers$Industry[match(table_id, customers$Namespace)],
           industry = ifelse(table_id == highlight, vertical_input, industry),
           type = ifelse(table_id %in% vertical_list, 'colored', 'not colored')) -> highlight_vertical
  highlight_vertical <- highlight_vertical[highlight_vertical[[3]] >= 1000,]
  
  
  
  #check if namespace exists in highlight_vertical
  #if not, error out
  # stopifnot(highlight %in% highlight_vertical$table_id)
  
  #assign variables for later ggplot assignment
  customer_count <- length(unique(highlight_vertical$table_id))

  
  
  #group by industry, to be used in reorder boxplot 
  #used only for boxplot and not linechart
  grouped_vertical <- highlight_vertical %>% 
    ungroup() %>% 
    select(-c(category, table_id)) %>% 
    group_by(industry) %>% 
    summarize_each(funs(mean))
  #match grouped_vertical$result with highlight_vertical
  highlight_vertical$grouped_result <- grouped_vertical$result[match(highlight_vertical$industry, grouped_vertical$industry)]

  #sort dataframe, needed to get correct index number for x_namespace
  highlight_vertical <- highlight_vertical[order(highlight_vertical$result, decreasing = TRUE),]
  
  
  
  if(chart == FALSE & highlight %in% highlight_vertical$table_id & !(exists('warning_flag'))){
    interval <- find_percentile2(x = highlight_vertical)
    x_namespace <- which(highlight_vertical$table_id == highlight)
    y_namespace <- highlight_vertical$result[highlight_vertical$table_id == highlight]
    x_vertical <- which(levels(forcats::fct_reorder(highlight_vertical$industry, highlight_vertical$grouped_result, .desc = TRUE)) == vertical_input)
    y_max <- round(max(highlight_vertical$result)) + 1
    y_min <- round(min(highlight_vertical$result))
    x_min <- findInterval(quantile(highlight_vertical$result)[[2]][1], highlight_vertical$result[order(highlight_vertical$result)])
    x_max <- findInterval(quantile(highlight_vertical$result)[[4]][1], highlight_vertical$result[order(highlight_vertical$result)])
    #boxplot
    chart <- ggplot(highlight_vertical, aes(x = forcats::fct_reorder(industry, grouped_result, .desc = TRUE), y = result)) + 
      theme_bw() + 
      geom_boxplot(aes(fill = type, alpha = 0.5)) +
      scale_fill_manual(values = c('colored' = '#613889', 'not colored' = 'grey')) +
      theme(axis.text.x = element_text(hjust = 1, angle = 45),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = 'none') +
      geom_point(aes(x = x_vertical , y = y_namespace), size = 3, color = find_plot_color(metric, as.numeric(gsub('%', '', interval)) / 100, .5)) + 
      scale_color_manual(c('red' = 'red', 'green' = '#55a860')) +
      labs(title = paste0('\n', paste0(toupper(substr(metric, 1, 1)), substr(metric, 2, nchar(metric))) ,' Rate: ', toupper(highlight)),
           subtitle = paste0(paste0(toupper(substr(highlight, 1, 1)), substr(highlight, 2, nchar(highlight))), ' ranks in the ', interval, ' percentile of ', metric),
           caption = paste0('\nBased on data between ', format(date_begin, '%B %d, %Y'),' - ', format(date_end, '%B %d, %Y'))) 
  } else if(chart == TRUE & highlight %in% highlight_vertical$table_id & !(exists('warning_flag'))) {
    highlight_vertical_tmp <- highlight_vertical %>% filter(industry == vertical_input) %>% arrange(desc(result))
    interval <- find_percentile2(x = highlight_vertical_tmp)
    x_namespace <- which(highlight_vertical_tmp$table_id == highlight)
    y_namespace <- highlight_vertical_tmp$result[highlight_vertical_tmp$table_id == highlight]
    #y_namespace <- highlight_vertical_tmp$result[order(highlight_vertical_tmp$result, decreasing = TRUE)][highlight_vertical_tmp$table_id == highlight]
    y_max <- round(max(highlight_vertical_tmp$result)) + 1
    y_min <- round(min(highlight_vertical_tmp$result))
    x_min <- findInterval(quantile(highlight_vertical_tmp$result)[[2]][1], highlight_vertical_tmp$result[order(highlight_vertical_tmp$result)])
    x_max <- findInterval(quantile(highlight_vertical_tmp$result)[[4]][1], highlight_vertical_tmp$result[order(highlight_vertical_tmp$result)])
    
    #line
    chart <- ggplot(highlight_vertical_tmp, aes(x = forcats::fct_reorder(table_id, result, .desc = TRUE), y = result)) + 
      geom_line(aes(group = industry)) +
      scale_y_continuous(limits = c(0, y_max), expand = c(0,0)) +
      annotate("rect", xmin = 0, xmax = x_min, ymin = 0, ymax = y_max, alpha = .2, fill = 'red') + #bottom 25%
      annotate("rect", xmin = x_max, xmax = length(highlight_vertical_tmp$result) + 1, ymin = 0, ymax = y_max, alpha = .2, fill = 'green') +
      geom_point(aes(x = x_namespace, y = y_namespace), size = 3, color = 'red') +
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)) +
      labs(title = paste0('\n', paste0(toupper(substr(metric, 1, 1)), substr(metric, 2, nchar(metric))) ,' Rate: ', toupper(highlight)),
           subtitle = paste0(paste0(toupper(substr(highlight, 1, 1)), substr(highlight, 2, nchar(highlight))), ' ranks in the ', interval, ' percentile of ', metric),
           caption = paste0('Based on data between January 1, 2016 - December 12, 2016')) 
  } else {
    chart <- NA
  }
return(chart)    
gc()  
}
