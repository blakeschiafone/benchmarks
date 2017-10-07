benchmark_retention <- function(highlight, vertical_input = FALSE, chart = 'boxplot', retention_day = FALSE, date_begin = FALSE, date_end = FALSE){
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(ggplot2)) 
  suppressPackageStartupMessages(library(directlabels)) 
  
  #@PARAMETERS
  if(retention_day == FALSE){
    retention_day <- 30
  }
  
  #check if namespace used in benchmark has data
  #if it does not, then error out
  if(!(highlight %in% unique(retention$table_id))){
    warning(paste0(toupper(highlight), ' does not exist is data'))
    warning_flag <- 1
  }
  
  
  if(chart == 'boxplot'){
    
    if(date_begin == FALSE){
      date_begin <- min(retention$start_date)
    } else {
      date_begin <- as.Date(date_begin)
    }
    if(date_end == FALSE){
      date_end <- max(retention$start_date)
    } else {
      date_end <- as.Date(date_end)
    }
    
    
    
    if(!(exists('warning_flag'))){
      retention %>%
      ungroup() %>%
      left_join(customers, by = c('table_id' = 'Namespace')) %>%
      select(is_control, count, first_visit, start_date, Industry) %>%
      group_by(Industry, is_control, first_visit, start_date) %>%
      summarize(count = sum(count)) %>%
      filter(start_date >= first_visit) %>%
      mutate(first_count = first(count),
             return_rate = count / first_count,
             date = row_number(),
             type = ifelse(Industry %in% vertical_input, 'colored', 'not colored')) %>%
      select(Industry, is_control, first_visit, return_rate, date, type) %>%
      arrange(first_visit) %>%
      filter(date %in% c(retention_day)) -> tmp
    
      #aggregate data at Industry level to get an average return rate
      #it's not a weighted return weight do to complexity
      #data is used to order factors
      retention %>%
        ungroup() %>%
        left_join(customers, by = c('table_id' = 'Namespace')) %>%
        mutate(Industry = ifelse(table_id %in% highlight, highlight, Industry)) %>%
        select(is_control, count, first_visit, start_date, Industry) %>%
        group_by(Industry, first_visit, start_date) %>%
        summarize(count = sum(count)) %>%
        filter(start_date >= first_visit) %>%
        mutate(first_count = first(count),
               return_rate = count / first_count,
               date = row_number()) %>%
        filter(date %in% c(retention_day)) %>%
        select(Industry, date, return_rate) %>%
        #arrange(first_visit) %>%
        ungroup() %>%
        group_by(Industry, date) %>%
        summarize(return_rate = mean(return_rate)) -> highlight_geom_point    
      
      tmp$grouped_mean <- highlight_geom_point$return_rate[match(tmp$Industry, highlight_geom_point$Industry)]
      
      #filter highlight_geom_point to only the highlight variable
      #this is the red dot that shows up in boxplot
      highlight_geom_point <- highlight_geom_point[highlight_geom_point$Industry == highlight,]
      highlight_geom_point[which(highlight_geom_point$Industry == highlight),][1] <- vertical_input
      highlight_geom_point <- na.omit(highlight_geom_point)
      
      #recode factors and sort descending by grouped_mean
      tmp %>% ungroup() %>% mutate(Industry = as.factor(Industry)) -> tmp
      tmp %>% ungroup() %>% mutate(Industry = forcats::fct_reorder(tmp$Industry, tmp$grouped_mean, .desc = TRUE)) -> tmp
      
    chart <- ggplot(tmp, aes(x = Industry, y = return_rate)) +
      geom_boxplot(aes(fill = type, alpha = 0.5)) +
      scale_fill_manual(values = c('colored' = '#613889', 'not colored' = 'grey')) +
      theme_bw() +
      theme(axis.text.x = element_text(hjust = 1, angle = 45),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = 'none') +
      scale_y_continuous(labels = scales::percent) +
      geom_point(data = highlight_geom_point, color = 'red', size = 3) + 
      labs(title = 'Retention Rates across Verticals',
           subtitle = stringr::str_c('Users Returning on Day: ', retention_day),
           caption = paste0('\nBased on data between ', format(date_begin, '%B %d, %Y'),' - ', format(date_end, '%B %d, %Y')),
           y = 'Return Rate (%)')
    
    } else {
      chart <- NA
    }
    
    return(chart)
    
  } else {
    
     if(vertical_input == FALSE){
        vertical_input <- customers$Industry[customers$Namespace == highlight]
      } else {
        vertical_input <- vertical_input
      }
    
    if(date_begin == FALSE){
      date_begin <- min(retention$start_date)
    } else {
      date_begin <- as.Date(date_begin)
    }
    if(date_end == FALSE){
      date_end <- max(retention$start_date)
    } else {
      date_end <- as.Date(date_end)
    }
    
    if(!exists('warning_flag')){
      retention %>%
        ungroup() %>%
        left_join(customers, by = c('table_id' = 'Namespace')) %>%
        filter(start_date >= date_begin,
               start_date <= date_end) %>%
        filter(if(vertical_input != 'all'){Industry == vertical_input | table_id == highlight} else {Industry != 'all'}) %>%
        mutate(Industry = ifelse(table_id %in% highlight, highlight, Industry)) %>%
        select(is_control, count, first_visit, start_date, Industry) %>%
        group_by(Industry, first_visit, start_date) %>% 
        summarize(count = sum(count)) %>%
        filter(start_date >= first_visit) %>% 
        mutate(first_count = first(count), 
               date = row_number()) %>%
        group_by(Industry, date) %>%
        summarize(count = sum(count)) %>%
        mutate(first = first(count),
               return_rate = count/first,
               type = ifelse(Industry == highlight, 'colored', 'not colored')) %>% 
        filter(date > 1) %>%
        ggplot(., aes(x = date, y = return_rate)) +
        theme_bw() +
        geom_line(aes(color = type, group = Industry)) + 
        scale_color_manual(values = c('red', 'grey', 'grey')) +
        geom_dl(aes(label = stringr::str_to_title(Industry), 
                    color = type,
                    size = 1), 
                method = list(dl.combine("first.qp"), 
                              cex = .7,
                              rot = 30,
                              hjust = -.1)) +
        scale_y_continuous(labels = scales::percent) +
        theme(axis.ticks = element_blank(),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5),
              legend.position = 'none') +
        labs(title = paste0('\n', stringr::str_to_title(highlight), ': Retention Rates'),
             caption = paste0('\nBased on data between ', format(date_begin, '%B %d, %Y'),' - ', format(date_end, '%B %d, %Y')),
             x = 'Day of Return',
             y = 'Return Rate (%)') -> chart
      
    } else {
      chart <- NA
    }
    
    return(chart)
  }

}
