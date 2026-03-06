library(dashboardr)
library(dplyr)
library(lubridate)

# Load data
atmosfair <- read.csv("/Users/anpafford/PlantVillage/atmosfair_2025_final.csv", sep = ";")

# Clean and add time variables
atmosfair <- atmosfair %>%
  mutate(
    Start.date = ymd_hms(Start.date),
    month = month(Start.date, label = TRUE, abbr = FALSE) %>% as.character(),
    week = paste("Week", isoweek(Start.date))
  )

# aggregate # bags
bags_over_time <- atmosfair %>%
  mutate(bags = as.numeric(Number.of.bags..Associated...Total.)) %>%
  group_by(month) %>%
  summarise(total_bags = sum(bags, na.rm = TRUE))

atmosfair$bags <- atmosfair$Number.of.bags..Associated...Total.

atmosfair <- atmosfair %>%
  select(-Number.of.bags..Associated...Total.)

atmosfair <- atmosfair %>%
  mutate(bags = as.numeric(bags))

# PLOTS

# Survey status treemap
status_counts <- atmosfair %>%
  count(Survey.status, month, week, name = "n")

survey_status <- create_content(data = status_counts, type = "treemap") %>%
  add_viz(value_var = "n", group_var = "Survey.status", title = "Total breakdown by survey status")

# Reject reason treemap - now includes month and week
status_reason_count <- atmosfair %>%
  filter(!is.na(Survey.status.reason), Survey.status.reason != "") %>%
  count(Survey.status.reason, month, week, name = "n")

status_reason <- create_content(data = status_reason_count, type = "treemap") %>%
  add_viz(value_var = "n", group_var = "Survey.status.reason",
          title = "Total breakdown by survey status reason")

month_order <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")

atmosfair <- atmosfair %>%
  mutate(month = factor(month, levels = month_order))

bags_timeline <- create_content(data = atmosfair, type = "timeline") %>%
  add_viz(time_var = "month", y_var = "bags", title = "Total bags over time", agg = "sum",
          group_var = "Observer")

# PAGES

# Landing page
home <- create_page("Home", is_landing_page = TRUE) %>%
  add_text("# Carboneers Operations Dashboard",
           "",
           "This dashboard displays information on farm, observer, number of bags produced, and timeframes for the updated Atmosfair data set.",
           "",
           "Since all of these surveys are linked, the number of bags correspond to the production and application for each entry.",
           "",
           "Start and submit date correspond to the production survey.",
           "",
           "If there's any other information that you need, ask Alex.")

# Overview page
overview <- create_page("Overview", data = atmosfair, type = "bar") %>%
  add_input_row() %>%
  add_input(
    input_id = "month",
    label = "Filter by month",
    type = "select_single",
    filter_var = "month",
    options = sort(unique(atmosfair$month)),
    add_all = TRUE
  ) %>%
  add_input(
    input_id = "week",
    label = "Filter by week",
    type = "select_single",
    filter_var = "week",
    options = unique(atmosfair$week),
    add_all = TRUE
  ) %>%
  end_input_row() %>%
  add_viz(x_var = "Observer", title = "Survey count per observer", tabgroup = "How many surveys?") %>%
  add_viz(x_var = "Farm.name", title = "Survey count per farm", tabgroup = "How many surveys?")

# Survey status page
status <- create_page("Survey status", data = status_counts) %>%
  add_content(survey_status) %>%
  add_text("", "", "We can also break down the survey status by farm and observer.") %>%
  add_input_row() %>%
  add_input(
    input_id = "month",
    label = "Filter by month",
    type = "select_single",
    filter_var = "month",
    options = sort(unique(atmosfair$month)),
    add_all = TRUE
  ) %>%
  add_input(
    input_id = "week",
    label = "Filter by week",
    type = "select_single",
    filter_var = "week",
    options = unique(atmosfair$week),
    add_all = TRUE
  ) %>%
  end_input_row() %>%
  add_viz(data = atmosfair, type = "stackedbar", x_var = "Observer", stack_var = "Survey.status",
          title = "Survey status by observer", tabgroup = "Survey status") %>%
  add_viz(data = atmosfair,type="stackedbar", x_var = "Farm.name", stack_var = "Survey.status",
          title = "Survey status by farm", tabgroup = "Survey status")

# Reject reason page - no filter
survey_status_reason <- create_page("Reject reason", data = status_reason_count) %>%
  add_content(status_reason) %>%
  add_text("Notice the inconsistent formatting/encoding of the same categorical value.",
           "This is something we need to emphasize with the interns.")

# Bags over time 
bags_timeline <- create_page("Bags over time", data = bags_over_time) %>%
  add_content(bags_timeline)

# Generate dashboard
create_dashboard(
  title = "Carboneers x Atmosfair Dashboard",
  output_dir = "carboneers_atmosfair",
  theme = "flatly"
) %>%
  add_pages(home, overview, status, survey_status_reason, bags_timeline) %>%
  generate_dashboard(render = TRUE, open = "browser")
