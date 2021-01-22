library(tidyverse)

df = read_csv("final_project_data.csv")

survey_hist = function(results) {
  results %>%
    select_at(vars(starts_with("sat"))) %>%
    gather %>%
    ggplot(aes(value)) +
    geom_histogram(stat="count") +
    facet_wrap(~key)
}

survey_hist(df)

survey_cor = survey %>%
  drop_na %>%
  cor %>%
  corrplot


top_purposes = df %>%
  group_by(visitPurpose) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

purpose_splits = df %>%
  group_by(visitPurpose) %>%
  summarise(
    sat = mean(satOverall),
    night_avg = mean(nightsStayed),
    room_avg = mean(nightsStayed * avgRoomSpendPerNight),
    food_avg = mean(nightsStayed * avgFoodSpendPerNight),
    wifi_avg = mean(nightsStayed * avgWifiSpendPerNight),
    spend_avg = mean(nightsStayed * (avgRoomSpendPerNight + avgFoodSpendPerNight + avgWifiSpendPerNight)),
    spend_total = sum(nightsStayed * (avgRoomSpendPerNight + avgFoodSpendPerNight + avgWifiSpendPerNight))
  ) %>%
  arrange(desc(spend_total))



