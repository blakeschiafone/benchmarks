#' ---
#' title: 'Benchmark Report'
#' author: '`r NULL`'
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output:
#'    html_document:
#'      toc: true
#'      highlight: zenburn
#' ---

```{r global_options, echo = FALSE, results = "hide", message = FALSE}
knitr::opts_chunk$set(fig.width=14, fig.height=12, echo=FALSE, warning=FALSE, message=FALSE)
```

<br>
<br>
<br>
  
```{r}
#' ## How to read boxplots
#' A box plot shows the distribution of values.  The middle line is the median.  50% of all values fall below/above this value.
#' The "box" itself represents 50% of customers.  That means if the red dot is below the median line and close to the bottom
#' of the "box", the customer only performed better than ~25% of all other customers.  If the red dot is above the median and
#' close to the top of the "box", the customer performed better than ~75% of all customers. 
#' If a customer performs better/worse than the median line, then they will be called out with a green/red circle.
```
<br>
<br>
<br>


```{r}
#' ### Engagement Rates by Vertical
#' This graph shows engagement rates for all verticals.  The customer used in this benchmark will have its
#' vertical highlighted purple.  The customer will have their engagement rate called out with a red/green circle.
if(!is.error(ggplot_build(vertical_engagement_graph))){plot(vertical_engagement_graph)}
```

<br>
<br>
<br>
  

```{r}
#' ### Open Rates by Vertical
#' This graph shows open rates for all verticals.  Open rate is defined as email opens (not unique).  So, this graph will only
#' show data points for customers who sent emails.  The customer in this benchmark will have its vertical highlighted purple.
#' The customer will have their open rate called out with a red/green circle.
if(!is.error(ggplot_build(vertical_opens_graph))){plot(vertical_opens_graph)}
```

<br>
<br>
<br>
   

```{r}
#' ### Click Rates by Vertical
#' This graph shows click rates for all verticals.  Click rate is defined as push clicked (not unique).  The customer in this
#' benchmark will have its vertical highlighted purple.  The customer will have their click rate called out with a red/green circle.
if(!is.error(ggplot_build(vertical_clicks_graph))){plot(vertical_clicks_graph)}
```

<br>
<br>
<br>
   

```{r}
#' ### Goal Rates by Vertical
#' This graph shows goal rates for all verticals.  Goal rate is defined as goal completion.  The customer in this benchmark
#' will have its vertical highlighted purple.  The customer will have their goal completion rate called out with a red/green circle.
if(!is.error(ggplot_build(vertical_goal_graph))){plot(vertical_goal_graph)}
```

<br>
<br>
<br>
   

```{r}
#' ### Opt-Out Rates by Vertical
#' This graph shows opt-out rates for all verticals.  Opt-out rate is defined as a user opt-ing out of push after receiving a
#' push message (within the attribution window).  The customer will have its vertical highlighted purple.  The customer will
#' have their opt-out rate called out with a red/green circle.
if(!is.error(ggplot_build(vertical_optout_graph))){plot(vertical_optout_graph)}
```

<br>
<br>
<br>
   

```{r}
#' ### Campaign Uninstall Rates by Vertical
#' This graph shows campaign uninstall rates for all verticals.  Uninstall rate is defined as a user uninstalling the app after
#' receiving a message (within the attribution window).  The customer will have its vertical highlighted purple.  The customer
#' will have their uninstall rate called out with a red/green circle.
if(!is.error(ggplot_build(vertical_uninstall_graph))){plot(vertical_uninstall_graph)}
```

<br>
<br>
<br>
   

```{r}
#' ### Benchmarks by Channel
#' This graph provides all metrics across channels the customers vertical is using.  For example, if the vertical 'Media' has
#' 40 customers and all are only using push, then the only channel shown will be push.  Channels that can be returned in
#' this graph are Push, Email, In App.  Metrics that can  be returned (depending if data points exist with the vertical) are: CTR, Engagement,
#' Goal Completion, Opt-Out, Uninstall, Email Unique Open, Email Unsub.  The customer will have their call-out for each metric with 
#' a red/green circle.
if(!is.error(ggplot_build(channel_graph))){plot(channel_graph)}
```

<br>
<br>
<br>
   

```{r}
#' ### Benchmarks by Metric
#' This graph shows the same data as graph above (Benchmarks by Channel).  However, instead of showing data split by channel, it shows
#' data split by metric.  The customer will have their call-out for each metric with a red/green circle.
if(!is.error(ggplot_build(channel_facet_graph))){plot(channel_facet_graph)}
```

<br>
<br>
<br>


```{r}
#' ### Retention Rates
#' This graph shows the retention rate for the customer vs. its vertical.  The retention rate shows how many customers returned on each day (x-axis)
#' over the date range provided in the sub-caption (below graph).  The customer will be called out with a red line (in this case, red does not mean bad).
#' The vertical the customer exists in will be a grey line.  This data comes from the Kahuna UI Retention Chart.  
if(!is.error(ggplot_build(retention_graph))){plot(retention_graph)}
```

<br>
<br>
<br>
   

```{r}
#' ### Opt-In Rates
#' This graph shows the opt-in rate for all verticals.  Opt-in rate is defined as percentage of devices that have a push token, are push enabled, 
#' and were seen in the past 90 days.  The customer will have its vertical highlighted purple.  The customer will have their opt-in rate called out
#' with a red/green circle.
if(!is.error(ggplot_build(opt_in_graph))){plot(opt_in_graph)}
```