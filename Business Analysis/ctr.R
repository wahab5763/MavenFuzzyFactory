df_web_data <- left_join(df_website_sessions, df_website_pageviews, by ='website_session_id')

# CTR
#Formula CTR = total clicks/page views *100

df_web_data |> 
  group_by(pageview_url) |> 
  summarise(
    total_page_views = n_distinct(website_pageview_id),
    total_click_throughs = length(unique(pageview_url)),
    ctr = (total_click_throughs / total_page_views) * 100
  ) |> view()

