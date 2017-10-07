benchmark_optin <- function(highlight, vertical_input = FALSE, os_flag = FALSE, date_begin = FALSE, date_end = FALSE){
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(ggplot2)) 
  source('./functions/find_percentile.R', local = TRUE)
  source('./functions/find_plot_color.R', local = TRUE)
  options(error = stop)
  
  
  if(date_begin == FALSE){
    date_begin <- lubridate::floor_date(Sys.Date(), unit = 'month') - 90
  } else {
    date_begin <- as.Date(date_begin)
  }
  if(date_end == FALSE){
    date_end <- lubridate::floor_date(Sys.Date(), unit = 'month')
    #date_end <- lubridate::ceiling_date(seq(from = Sys.Date(), by = '-1 month', length.out = 2)[1], unit = 'month') - 1
  } else {
    date_end <- as.Date(date_end)
  }
  
  #check if namespace used in benchmark has data
  #if it does not, then error out
  if(!(highlight %in% unique(opt_in$table_id))){
    warning(paste0(toupper(highlight), ' does not exist is data'))
    warning_flag <- 1
  } else if (sum(opt_in$total_devices[opt_in$table_id == highlight]) < 1000){
    warning(paste0(toupper(highlight), ' does not have enough data to benchmark'))
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
  vertical_list <- unique(opt_in$table_id[opt_in$table_id %in% vertical_list])
  if(!(highlight %in% vertical_list)) {vertical_list <- c(vertical_list, highlight)}
  if(length(vertical_list) <= 2){
    warning(c(paste0(toupper(vertical_input), ' does not have enough namespaces to complete benchmark.  There are ', length(vertical_list), '.  Expect 3+')))
    warning_flag <- 1
  }
  
  
  if(!exists('warning_flag')){
    opt_in %>%
      left_join(customers, by = c("table_id" = "Namespace")) %>%
      mutate(color = ifelse(table_id %in% vertical_list, 'colored', 'not colored'),
             Industry = ifelse(table_id == highlight, vertical_input, Industry)) %>%
      filter(!(is.na(Industry)),
             total_devices > 0) %>%
      assign('raw', value = ., pos = 1) %>%
      mutate(total_opt_in = (opt_in_count_ios + opt_in_count_android) / total_devices,
             total_ios_opt_in = ifelse(is.nan(opt_in_count_ios / total_ios_devices), NA, opt_in_count_ios / total_ios_devices),
             total_android_opt_in = ifelse(is.nan(opt_in_count_android / total_android_devices), NA, opt_in_count_android / total_android_devices)) %>%
      select(table_id, Industry, color, total_opt_in, total_ios_opt_in, total_android_opt_in) %>% 
      tidyr::gather(type, result, total_opt_in:total_android_opt_in) %>%
      filter(type == 'total_opt_in',
             result <1,
             result > 0) -> highlight_vertical
    
    #assign variables for later ggplot assignment
    total_device_count <- sum(raw$total_devices, na.rm = TRUE)
    customer_count <- length(unique(raw$table_id))
    vertical_customer_count <- length(unique(raw$table_id[raw$color == 'colored']))
    customer_device_count <- sum(raw$total_devices[raw$table_id == highlight])
    vertical_device_count <- sum(raw$total_android_devices[raw$color == 'colored'])
    
      #group by industry, to be used in reorder boxplot 
      #used only for boxplot and not linechart
      grouped_vertical <- highlight_vertical %>% 
        ungroup() %>% 
        filter(type == 'total_opt_in') %>%
        select(Industry, result) %>%
        group_by(Industry) %>%
        summarize(result = mean(result),
                  result_median = median(result))
      #match grouped_vertical$result with highlight_vertical
      highlight_vertical$grouped_result <- grouped_vertical$result[match(highlight_vertical$Industry, grouped_vertical$Industry)]
      
      #sort dataframe, needed to get correct index number for x_namespace
      grouped_vertical <- grouped_vertical[order(grouped_vertical$result, decreasing = TRUE),]
    
      
      
      interval <- find_percentile2(x = highlight_vertical)
      vertical_median <- grouped_vertical$result_median[grouped_vertical$Industry == vertical_input]
      y_namespace <- highlight_vertical$result[highlight_vertical$table_id == highlight]
      x_vertical <- which(grouped_vertical$Industry == vertical_input)
      chart <- ggplot(highlight_vertical, aes(x = forcats::fct_reorder(Industry, grouped_result, .desc = TRUE), y = result)) +
      theme_bw() +
      geom_boxplot(aes(fill = color, alpha = 0.5)) +
      scale_fill_manual(values = c('colored' = '#613889', 'not colored' = 'grey')) +
      scale_y_continuous(labels = scales::percent) +
      theme(axis.text.x = element_text(hjust = 1, angle = 45),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = 'none') +
      geom_point(aes(x = x_vertical , y = y_namespace), size = 3, color = ifelse(y_namespace >= vertical_median, 'green', 'red')) +
      scale_color_manual(c('red' = 'red', 'green' = '#55a860')) +
      labs(title = paste0('\n', 'Opt-In Rate: ', toupper(highlight)),
           subtitle = paste0(paste0(toupper(substr(highlight, 1, 1)), substr(highlight, 2, nchar(highlight))), ' ranks in the ', interval, ' percentile of opt-in\'s'),
           caption = paste0('\nBased on data between ', format(date_begin, '%B %d, %Y'),' - ', format(date_end, '%B %d, %Y')))

    attr(chart, 'customer_count') <- customer_count
    attr(chart, 'customer_device_count') <- customer_device_count
    attr(chart, 'total_device_count') <- total_device_count
    attr(chart, 'vertical_device_count') <- vertical_device_count
    attr(chart, 'vertical_customer_count') <- vertical_customer_count
    attr(chart, 'vertical_customer_list') <- vertical_list
    
  } else if(exists('warning')) {
    chart <- NA
  }
  
  chart
  
}
