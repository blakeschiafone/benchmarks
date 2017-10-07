find_metric <- function(){
    if(metric == 'all'){
    p1 <- setNames(list('Engagement %' = 
                    substitute(round(a/b, digits = 5), list(
                      a = as.name('engaged'),
                      b = as.name('delivered')))), 'Engagement %')
    p2 <- setNames(list(goal = 
                          substitute(round(a/b, digits = 5), list(
                            a = as.name('goal'),
                            b = as.name('delivered')))), 'Goal %')
    p3 <- setNames(list(opt_outs = 
                          substitute(round(a/b, digits = 5), list(
                            a = as.name('opt_outs'),
                            b = as.name('delivered_ghost')))), 'Opt-Out %')
    p4 <- setNames(list(clicks = 
                          substitute(round(a/b, digits = 5), list(
                            a = as.name('clicks'),
                            b = as.name('delivered')))), 'CTR')
    if('email_unique_opens' %in% names(names)){
    p5 <- setNames(list(email_unique_opens = 
                          substitute(round(a/b, digits = 5), list(
                            a = as.name('email_unique_opens'),
                            b = as.name('delivered')))), 'Email Unique Open %')
    }
    if('unsubs' %in% names(names)){p6 <- setNames(list(unsubs = 
                          substitute(round(a/b, digits = 5), list(
                            a = as.name('unsubs'),
                            b = as.name('delivered')))), 'Unsub %')
    }
    p7 <- setNames(list(uninstall = 
                          substitute(round(a/b, digits = 5), list(
                            a = as.name('uninstalled_ghost'),
                            b = as.name('delivered_ghost')))), 'Uninstall %')
    if('email_unique_clicks' %in% names(names)){p8 <- setNames(list(email_unique_clicks = 
                          substitute(round(a/b, digits = 5), list(
                            a = as.name('email_unique_clicks'),
                            b = as.name('delivered')))), 'Email Unique CTR %')
    }
  } else {
    p1 <- setNames(list(result = 
                    substitute(round(a/b, digits = 5), list(
                     a = ifelse(length(metric_args) == 2, as.name(metric_args[2]), as.name(metric_args[3])),
                     b = as.name(metric_args[1])))), metric)
  }
  
  if(metric == 'all'){
    returned_list <- c(p1, p2, p3, p4, p7, if(exists('p5')){p5}, if(exists('p6')){p6}, if(exists('p8')){p8})
    } else {
      returned_list <- p1
    }
  
  return(returned_list)
}