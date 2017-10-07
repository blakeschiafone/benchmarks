find_plot_color <- function(metric_name, plot_value, comparison_value){
  
  if(exists('plot_value') && is.character(plot_value)) plot_value <- readr::parse_number(plot_value)
  if(exists('comparison_value') && is.character(comparison_value)) comparison_value <- readr::parse_number(comparison_value)

  ifelse(metric_name %in% metric_list,
      metric_name <- switch(metric_name,
             'engagement' = 'Engagement %',
             'uninstall' = 'Uninstall %',
             'clicks' = 'CTR',
             'optout' = 'Opt-Out %',
             'opens' = 'Email Unique Open %',
             'goal' = 'Goal %'),
      metric_name)

  
   ifelse(metric_name %in% c('Opt-Out %', 'Unsub %', 'Uninstall %'),
          ifelse(plot_value >= comparison_value, 'red', 'green'),
            ifelse(metric_name %in% c('CTR', 'Engagement %', 'Goal %', 'Email Unique CTR %', 'Email Unique Open %'), 
                   ifelse(plot_value < comparison_value, 'red', 'green'), 'NULL'))

}

