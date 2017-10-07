function_email_open_proportion <- function(highlight, date_begin = FALSE, date_end = FALSE){
  
  
  query <- "select
(_TABLE_SUFFIX) as table_id,
client_info.client_name,
sum(if(event = 'opened', 1, 0)) as opened
from `tap-nexus.kahuna_email_events.*`
where _TABLE_SUFFIX not like '%demo%' and
_TABLE_SUFFIX not like '%temp%'
group by table_id, client_info.client_name"
  
  results <- query_exec(query = query, project = 'kahuna-bq-access', useLegacySql = FALSE)
  
  #' table lookup to combine some client_name's
  client_name_lookup <- c('Outlook 2010' = 'Outlook', 'Outlook 2007' = 'Outlook')
  results$client_name <- lapply(results$client_name, FUN = function(x){
    ifelse(any(x == names(client_name_lookup)), 
           unname(client_name_lookup[match(x, names(client_name_lookup))]), x)
  })
  
  
  #' remove NA's / unknown's
  results <- na.omit(results)
  results <- results[results$client_name != 'unknown',]
  
  #' add in highlighted column
  results$highlight <- ifelse(results$table_id == highlight, 'yes', 'no')
  
  #' aggregate data to get opened proportion for each table_id
  #' only keep top 10 clients
  results <- results %>%
    group_by(table_id) %>%
    mutate(prop = opened / sum(opened),
           client_name = unlist(client_name)) %>%
    filter(client_name %in% c('Mobile Safari', 'Firefox', 'Android Webkit', 'Apple Mail', 'Chrome', 
                              'IE', 'Safari', 'Outlook', 'Mozilla', 'Thunder'))
  
  #' create table of median proportions, used later for order graphs
  client_name_prop <- aggregate(prop ~ client_name, data = results, FUN = median)
  results$client_name_prop <- client_name_prop$prop[match(results$client_name, client_name_prop$client_name)]
  
  #' graph by client_name
  results %>%
    ggplot(., aes(x = forcats::fct_reorder(client_name, client_name_prop, .desc = TRUE), y = prop)) + 
    theme_bw() +
    theme(axis.text.x = element_text(hjust = 1, angle = 45),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = 'none') +
    labs(title = 'Close5 Email Open Benchmark', 
         subtitle = 'Red dot = Close5 Performance\n',
         y = 'Proportion Emails Open\n',
         caption = 'Data thru June 1, 2017') + 
    scale_y_continuous(labels = scales::percent) +
    geom_boxplot() +
    geom_point(data = results[results$highlight == 'yes',], aes(x = client_name, y = prop), color = 'red')
}
  