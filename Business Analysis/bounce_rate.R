#Bounce Rate
df_web_data <- left_join(df_website_sessions, df_website_pageviews, by ='website_session_id')


# Count the number of sessions with only one page view
single_page_sessions <- df_web_data %>%
  group_by(website_session_id) %>%
  summarise(page_view_count = n_distinct(pageview_url)) %>%
  filter(page_view_count == 1) %>%
  nrow()

df_web_data %>%
  mutate(
    single_page_sessions = unique(single_page_sessions),
    total_sessions = unique(n_distinct(website_session_id)),
    bounce_rate = unique((single_page_sessions / total_sessions) * 100)
  ) |> select(single_page_sessions, total_sessions, bounce_rate) -> df_bounce_rate

df_bounce_rate <- df_bounce_rate |> 
  summarise(
    single_page_sessions = unique(single_page_sessions),
    total_sessions = unique(total_sessions),
    bounce_rate = unique((single_page_sessions / total_sessions) * 100)
  )
write_rds(df_bounce_rate, "../data output/df_bounce_rate")

#Based on Sources
# Count the number of unique sessions by utm_source
total_sessions_sources <- df_web_data %>%
  distinct(website_session_id, utm_source) %>%
  group_by(utm_source) %>%
  summarise(total_sessions = n()) %>%
  ungroup()

# Count the number of sessions with only one page view by utm_source
single_page_sessions_sources <- df_web_data %>%
  group_by(website_session_id, utm_source) %>%
  summarise(page_view_count = n_distinct(pageview_url)) %>%
  filter(page_view_count == 1) %>%
  group_by(utm_source) %>%
  summarise(single_page_sessions = n()) %>%
  ungroup()

# Calculate the bounce rate by utm_source
bounce_rate_sources <- single_page_sessions_sources %>%
  left_join(total_sessions_sources, by = "utm_source") %>%
  mutate(bounce_rate = (single_page_sessions / total_sessions) * 100)

write_rds(bounce_rate_sources, "../data output/bounce_rate_sources")
