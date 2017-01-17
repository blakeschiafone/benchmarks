#graph a called metric by vertical
#@params
#metric = REQUIRED.  The metric that will be benchmarked
#vertical_input = OPTIONAL.  If False, then the vertical in Salesforce is used.  Otherwise, you can override the vertical to benchmark against
#highlight = REQUIRED.  The namespace that will be used in benchmark


benchmark <- function(metric, vertical_input = FALSE, highlight = FALSE){
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(ggplot2))  
  options(error = stop)
  metric_list <- c('uninstall', 'engagement', 'goal', 'optout', 'clicks', 'opens')
  
  #check if metric exists in available list
  # tryCatch(metric %in% metric_list)
  if (!(metric %in% metric_list)){
    stop(c(paste0(toupper(metric), ' does not exist in available list of '), paste0(metric_list, sep = ', ')))
  }
  
  #check if vertical_input is FALSE
  #if it is, we need to look-up what namespace was inputted
  #and find the matching vertical
  if(vertical_input == FALSE){
    vertical_input <- customers$Industry[customers$Namespace == highlight][1]
  }
  
  #create vertical list filter.  the list is every namespace
  #that falls into the vertical.  also, need to test length of
  #vector to make sure it's >= 3 namespaces in the industry
  #otherwise, it should produce an error saying the vertical
  #does not have enough namespaces to compile benchmark
  vertical_list <- as.list(na.omit(customers$Namespace[customers$Industry == vertical_input]))
  vertical_list <- unique(daily_counters$table_id[daily_counters$table_id %in% vertical_list])
  if(length(vertical_list) <= 2){
    stop(c(paste0(toupper(vertical_input), ' does not have enough namespaces to complete benchmark.  There are ', length(vertical_list), '.  Expect 3+')))
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
  
  
  results %>%
    select_('namespace', 'Industry', metric_args[1], metric_args[2]) %>%
    mutate(namespace = as.character(namespace)) %>%
    group_by(namespace, region) %>% 
    summarize_each(., funs(sum)) %>%
    mutate(region = toupper(region)) %>%
    mutate_(.dots = list(result = 
                           substitute(round(a/b, digits = 5) * 100, list(
                             a=as.name(metric_args[2]),
                             b=as.name(metric_args[1])))
    )
    ) %>%
    mutate(result = ifelse(is.nan(result), NA, result)) %>% 
    select(namespace, region, result) %>%
    filter(region == toupper(vertical_input),
           !is.na(result),
           result < 100) -> graph_tmp
  
  if (highlight != FALSE) {
    #check if namespace to be highlighted exists in region
    #if not error out
    if (!(highlight %in% graph_tmp$namespace)) {
      message(paste0(tolower(highlight), ' does not exist in the region ', vertical_input, '.'))
      stop()
    }
    
    
    
    #create empty vector of namespaces except for highlighted namespace
    #this will be used for customer facing showings
    #to create anononimity of performance
    graph_tmp <- graph_tmp[order(graph_tmp$result),]
    high_tmp_rank <- which(graph_tmp$namespace == highlight)
    high_tmp <- graph_tmp[(graph_tmp$namespace == highlight),]
    
    #check if region being subsetted is >1 
    #if not, then no need to create an anonymized vector
    if (nrow(graph_tmp) > 1 & high_tmp_rank != nrow(graph_tmp) & high_tmp_rank != 1){
      #highlight does not equal last rank of region
      graph_tmp <- na.omit(graph_tmp[!(graph_tmp$namespace == highlight),][order(graph_tmp$result), ])
      graph_tmp <- graph_tmp[order(graph_tmp$result),] 
      graph_tmp$namespace <- as.character(c(1:(high_tmp_rank-1), high_tmp_rank: nrow(graph_tmp) + 1))
      graph_tmp <- rbind(high_tmp, graph_tmp)
    } else if (nrow(graph_tmp) > 1 & high_tmp_rank == nrow(graph_tmp)) {
      #highlight equals last rank of region
      graph_tmp <- na.omit(graph_tmp[!(graph_tmp$namespace == highlight),][order(graph_tmp$result), ])
      graph_tmp <- graph_tmp[order(graph_tmp$result),] 
      graph_tmp$namespace <- as.character(c(1:(high_tmp_rank-1)))
      graph_tmp <- rbind(high_tmp, graph_tmp)
    } else if (nrow(graph_tmp) > 1 & high_tmp_rank == 1){
      #highlight equals first rank of region
      graph_tmp <- na.omit(graph_tmp[!(graph_tmp$namespace == highlight),][order(graph_tmp$result), ])
      graph_tmp <- graph_tmp[order(graph_tmp$result),] 
      graph_tmp$namespace <- as.character(c(1:nrow(graph_tmp) + 1))
      graph_tmp <- rbind(high_tmp, graph_tmp)
    }
  }
  
  #calculate percentiles and report number for graph
  graph_label <- as.character(graph_tmp$result[which(graph_tmp$namespace == highlight)])
  interval_max <- round(max(graph_tmp$result), 1)
  interval_min <- round(min(graph_tmp$result), 1)
  interval <- findInterval(graph_tmp$result[which(graph_tmp$namespace == highlight)], 
                           quantile(graph_tmp$result), 
                           rightmost.closed = TRUE)
  if (interval == 1){
    interval <- ifelse(metric == 'uninstall', 'top 25', 'bottom 25')
  } else if (interval == 2){
    interval <- ifelse(metric == 'uninstall', 'top 50', 'bottom 50')
  } else if (interval == 3){
    interval <- ifelse(metric == 'uninstall', 'bottom 50', 'top 50')
  } else {
    interval <- ifelse(metric == 'uninstall', 'bottom 25', 'top 25')
  }
  
  
  
  ggplot(graph_tmp) +
    geom_point(aes(x = forcats::fct_reorder(namespace, result), 
                   y = result,
                   color = ifelse(namespace == highlight, TRUE, FALSE)),
               stat = 'identity') +
    scale_color_manual(values = c('black', 'red')) +
    #scale_x_discrete(labels = c('', '', '', '', 'mudah', '', '', '', '')) + 
    labs(title = paste0('Campaign ', paste0(toupper(substr(metric, 1, 1)), substr(metric, 2, nchar(metric))) ,' Rate by Customer (', toupper(vertical_input), ')'),
         subtitle = paste0(paste0(toupper(substr(highlight, 1, 1)), substr(highlight, 2, nchar(highlight))), ' ranks in the ', interval, ' percentile of ', metric),
         caption = paste0('Based on data between January 1, 2016 - December 12, 2016\n', 'Min ', metric, ': ', interval_min, ' ; ', 'Max ', metric, ': ', interval_max),
         y = paste0('Campaign ', paste0(toupper(substr(metric, 1, 1)), substr(metric, 2, nchar(metric))) , ' %'),
         x = '') +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = 'none') +
    geom_text(aes(x = forcats::fct_reorder(namespace, result),
                  y = result,
                  label = ifelse(result == graph_label, round(as.numeric(graph_label), 1), '')),
              color = 'red',
              size = 3,
              vjust = -1)
}