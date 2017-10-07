#find quantiles.  this will be used to shade the top 25% 
#and bottom 25% for the graph.
#also, matching the table_id (namespace) with the quantile it
#falls into: 1 - 4
find_percentile <- function(x){
  interval <- findInterval(x$result[which(x$table_id == highlight)], 
                         quantile(x$result), 
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
  return(interval)
}



find_percentile2 <- function(x){
  if(any(names(x) == 'Industry')) {
    names(x)[which(names(x) == 'Industry')] <- 'industry'
  }
  
  tmp <- x %>% filter(industry == vertical_input) %>% arrange(result)
  tmp_quantile <- quantile(sort(tmp$result), seq(0, 1, by = .01))
  interval <- findInterval(tmp$result[which(tmp$table_id == highlight)], 
                           sort(tmp_quantile), rightmost.closed = TRUE)
  #interval <- interval / nrow(tmp)
  interval <- scales::percent(interval/100)
  return(interval)
}
