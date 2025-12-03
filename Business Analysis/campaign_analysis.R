
#Campaign Analysis 
df_website_sessions |> 
  group_by(utm_campaign, utm_source) |> 
  summarise(
    start_date = min(created_at),  # start campaign date
    end_date = max(created_at),  # end campaign date
    camp_duration_in_months =  interval(date(start_date), date(end_date)) %/% months(1)) -> camp_data

df_merged_camp_data <- left_join(df_website_sessions, df_merged_orders)



df_merged_camp_data |> 
  group_by(utm_source, utm_campaign) |> 
  summarise(
    orders = n_distinct(order_id),
    sessions = n_distinct(website_session_id),
    conversion_rate = (orders/sessions)*100,
    revenue_generated = sum(price_usd, na.rm = TRUE)) -> df_campaign_cvr

df_campaign_analysis <- left_join(camp_data, df_campaign_cvr)

#Desktop_targeted Conversion rate
campaign_data <- df_website_sessions |> 
  filter(utm_campaign == 'desktop_targeted') |> 
  group_by(utm_campaign) |> 
  summarise(
    month_before_launch= min(created_at) - ddays(30),  # 30 days before the launch date
    pre_launch_end_date = min(created_at) - ddays(1),  # 1 day before the launch date
    month_post_launch = max(created_at) + ddays(1),  # 1 day after the launch date
    post_launch_end_date = max(created_at) + ddays(30) # 30 days after the launch date
  )
pre_launch_data_desktop_targeted <- subset(df_website_sessions, created_at >= campaign_data$month_before_launch & created_at <= campaign_data$pre_launch_end_date)
post_launch_data_desktop_targeted <- subset(df_website_sessions, created_at >= campaign_data$month_post_launch & created_at <= campaign_data$post_launch_end_date)

#pre_launch Campaign conversion rate (desktop_targeted)
left_join(pre_launch_data_desktop_targeted,df_orders, by = "website_session_id") -> merged_df_campaign_pre_desktop
merged_df_campaign_pre_desktop |> 
  summarise(
    orders = n_distinct(order_id),
    sessions = n_distinct(website_session_id),
    conversion_rate = (orders/sessions)*100,
    reve_generated_pre = sum(price_usd, na.rm = TRUE)
  ) -> cvr_desktop_targeted_campaign_pre_launch

#post_launch Campaign conversion rate (desktop_targeted)
left_join(post_launch_data_desktop_targeted,df_orders, by = "website_session_id") -> merged_df_campaign_post_desktop
merged_df_campaign_post_desktop |> 
  summarise(
    orders = n_distinct(order_id),
    sessions = n_distinct(website_session_id),
    conversion_rate = (orders/sessions)*100,
    reve_generated_post = sum(price_usd, na.rm = TRUE)
  ) -> cvr_desktop_targeted_campaign_post_launch


#Pilot campaign Conversion rate
campaign_data_pilot <- df_website_sessions |> 
  filter(utm_campaign == 'pilot') |> 
  group_by(utm_campaign) |> 
  summarise(
    month_before_launch= min(created_at) - ddays(30),  # 30 days before the launch date
    pre_launch_end_date = min(created_at) - ddays(1),  # 1 day before the launch date
    month_post_launch = max(created_at) + ddays(1),  # 1 day after the launch date
    post_launch_end_date = max(created_at) + ddays(30) # 30 days after the launch date
  )
pre_launch_data_pilot <- subset(df_website_sessions, created_at >= campaign_data_pilot$month_before_launch & created_at <= campaign_data_pilot$pre_launch_end_date)
post_launch_data_pilot <- subset(df_website_sessions, created_at >= campaign_data_pilot$month_post_launch & created_at <= campaign_data_pilot$post_launch_end_date)

#pre_launch Campaign conversion rate (Pilot)
left_join(pre_launch_data_pilot,df_orders, by = "website_session_id") -> merged_df_campaign_pre_pilot
merged_df_campaign_pre_pilot |> 
  summarise(
    orders = n_distinct(order_id),
    sessions = n_distinct(website_session_id),
    conversion_rate = (orders/sessions)*100,
    reve_generated_pre = sum(price_usd, na.rm = TRUE)
  ) -> cvr_pilot_campaign_pre_launch

#post_launch Campaign conversion rate (Pilot)
left_join(post_launch_data_pilot,df_orders, by = "website_session_id") -> merged_df_campaign_post_pilot
merged_df_campaign_post_pilot |> 
  summarise(
    orders = n_distinct(order_id),
    sessions = n_distinct(website_session_id),
    conversion_rate = (orders/sessions)*100,
    reve_generated_post = sum(price_usd, na.rm = TRUE)
  ) -> cvr_pilot_post_launch

write_rds(df_campaign_analysis, "../data output/df_campaign_analysis")
write_rds(cvr_desktop_targeted_campaign_pre_launch, "../data output/cvr_desktop_targeted_campaign_pre_launch")
write_rds(cvr_desktop_targeted_campaign_post_launch, "../data output/cvr_desktop_targeted_campaign_post_launch")
write_rds(cvr_pilot_campaign_pre_launch, "../data output/cvr_pilot_campaign_pre_launch")
write_rds(cvr_pilot_post_launch, "../data output/cvr_pilot_post_launch")


#Other campaign average conversion rate 
merged_df |> 
  mutate(
    Year = format(df_website_sessions$created_at, format = "%Y")
  ) |> 
  filter(utm_campaign != 'desktop_targeted' & utm_campaign != 'pilot') |> 
  group_by(utm_campaign, Year) |> 
  summarise(
    orders = n_distinct(order_id),
    sessions = n_distinct(website_session_id),
    conversion_rate = (orders/sessions)*100,
    rev_generated = sum(price_usd, na.rm = TRUE)
  ) |> 
  summarise(
    avg_orders = median(orders),
    avg_sessions = median(sessions)
  ) |>
  arrange(-avg_conversion_rate) -> campaign_conversion_rate
