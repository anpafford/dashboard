library(dashboardr)
library(dplyr)
library(lubridate)
library(readxl)

# What do we want to see?
# Plot 1: Production by kiln
# - broken down by date
# - toggle between burn count and weight
# - filter out rejected surveys
# Plot 2: Production by coordinator
# - broken down by date: last week, last month, all time 
# - last week and last month should be bar charts
# - all time: timeline 
# - toggle between burn count and weight 
# - filter out rejected surveys

# Load data

production_by_kiln <- readRDS("production_by_kiln.rds") %>%
  mutate(period_trunc = as_datetime(period_trunc),
         survey_count = as.integer(survey_count),
         biochar_kg = as.numeric(biochar_kg)        
  )


production_by_coordinator <- readRDS("production_by_coordinator.rds") %>%
  mutate(period_trunc = as_datetime(period_trunc),
         survey_count = as.integer(survey_count),
         biochar_kg = as.numeric(biochar_kg)        
         )

production_by_coordinator %>%
  group_by(entity_name) %>%
  arrange(period_trunc) %>%
  mutate(gap = as.numeric(difftime(period_trunc, lag(period_trunc), units = "weeks"))) %>%
  filter(gap > 1) %>%
  select(entity_name, period_trunc, gap) %>%
  print(n = 50)

glimpse(production_by_kiln)
glimpse(production_by_coordinator)


# Vizzes

# Viz 1: Production by kiln, last week


home <- create_page("Home", is_landing_page = TRUE) %>%
  add_text(
    "# Carboneers Operations Dashboard",
    "",
    "This dashboard displays production statistics per kiln and coordinator.",
    "",
    "Let me know if there is any other information that would be helpful."
  )

surveys_by_kiln <- create_page("Burns per kiln", data = production_by_kiln, type = "bar") %>%
  add_input_row() %>%
  add_input(
    input_id = "project_name",
    label = "Filter by project",
    type = "select_single",
    filter_var = "project_name",
    options = unique(production_by_kiln$project_name),
    add_all = TRUE
  ) %>%
  add_input(
    input_id = "approval_status",
    label = "Filter by status",
    type = "select_single",
    filter_var = "approval_status",
    options = unique(production_by_kiln$approval_status),
    add_all = TRUE
  ) %>%
  end_input_row() %>%
  add_text("Remember that Atmosfair kilns don't have kiln IDs yet.") %>%
  add_viz(x_var = "entity_name", y_var = "survey_count", horizontal = TRUE,
          title = "Last week", tabgroup = "Timeframe",
          x_label = "Kiln", y_label = "Number of burns",
          filter = ~ period_trunc >= Sys.Date() - 7) %>%
  add_viz(x_var = "entity_name", y_var = "survey_count", horizontal = TRUE,
          title = "Last month", tabgroup = "Timeframe",
          x_label = "Kiln", y_label = "Number of burns",
          filter = ~ period_trunc >= Sys.Date() - 30) %>%
  add_viz(type = "timeline", group_var = "entity_name", y_var = "survey_count",
          time_var = "period_trunc", title = "All time", tabgroup = "Timeframe",
          agg = "sum", x_label = "Week", y_label = "Number of burns")

biochar_by_kiln <- create_page("Biochar (kg) per kiln", data = production_by_kiln, type = "bar") %>%
  add_input_row() %>%
  add_input(
    input_id = "project_name",
    label = "Filter by project",
    type = "select_single",
    filter_var = "project_name",
    options = unique(production_by_kiln$project_name),
    add_all = TRUE
  ) %>%
  add_input(
    input_id = "approval_status",
    label = "Filter by status",
    type = "select_single",
    filter_var = "approval_status",
    options = unique(production_by_kiln$approval_status),
    add_all = TRUE
  ) %>%
  end_input_row() %>%
  add_text("Remember that Atmosfair kilns don't have kiln IDs yet.") %>%
  add_viz(x_var = "entity_name", y_var = "biochar_kg", horizontal = TRUE,
          x_label = "Kiln", y_label = "Number of burns",
          title = "Last week", tabgroup = "Timeframe",
          filter = ~ period_trunc >= Sys.Date() - 7) %>%
  add_viz(x_var = "entity_name", y_var = "biochar_kg", horizontal = TRUE,
          title = "Last month", tabgroup = "Timeframe",
          x_label = "Kiln", y_label = "Number of burns",
          filter = ~ period_trunc >= Sys.Date() - 30) %>%
  add_viz(type = "timeline", group_var = "entity_name", y_var = "biochar_kg",
          time_var = "period_trunc", title = "All time", tabgroup = "Timeframe",
          x_label = "Week", y_label = "Biochar (kg)",
          agg = "sum")

surveys_by_coordinator <- create_page("Burns per coordinator", data = production_by_coordinator, type = "bar") %>%
  add_input_row() %>%
  add_input(
    input_id = "project_name",
    label = "Filter by project",
    type = "select_single",
    filter_var = "project_name",
    options = unique(production_by_coordinator$project_name),
    add_all = TRUE
  ) %>%
  add_input(
    input_id = "approval_status",
    label = "Filter by status",
    type = "select_single",
    filter_var = "approval_status",
    options = unique(production_by_coordinator$approval_status),
    add_all = TRUE
  ) %>%
  end_input_row() %>%
  add_viz(x_var = "entity_name", y_var = "survey_count", horizontal = TRUE,
          title = "Last week", tabgroup = "Timeframe", y_label = "Number of burns",
          x_label = "Coordinator",
          filter = ~ period_trunc >= Sys.Date() - 7) %>%
  add_viz(x_var = "entity_name", y_var = "survey_count", horizontal = TRUE,
          title = "Last month", tabgroup = "Timeframe", y_label = "Number of burns",
          x_label = "Coordinator",
          filter = ~ period_trunc >= Sys.Date() - 30) %>%
  add_viz(type = "timeline", group_var = "entity_name", y_var = "survey_count",
          time_var = "period_trunc", title = "All time", tabgroup = "Timeframe",
          y_label = "Number of burns", x_label = "Week", agg = "sum")

biochar_by_coordinator <- create_page("Biochar (kg) per coordinator", data = production_by_coordinator, type = "bar") %>%
  add_input_row() %>%
  add_input(
    input_id = "project_name",
    label = "Filter by project",
    type = "select_single",
    filter_var = "project_name",
    options = unique(production_by_coordinator$project_name),
    add_all = TRUE
  ) %>%
  add_input(
    input_id = "approval_status",
    label = "Filter by status",
    type = "select_single",
    filter_var = "approval_status",
    options = unique(production_by_coordinator$approval_status),
    add_all = TRUE
  ) %>%
  end_input_row() %>%
  add_viz(x_var = "entity_name", x_label = "Coordinator", y_var = "biochar_kg", horizontal = TRUE,
          title = "Last week", tabgroup = "Timeframe", y_label = "Biochar (kg)",
          filter = ~ period_trunc >= Sys.Date() - 7) %>%
  add_viz(x_var = "entity_name", x_label = "Coordinator", y_var = "biochar_kg", horizontal = TRUE,
          title = "Last month", tabgroup = "Timeframe", y_label = "Biochar (kg)",
          filter = ~ period_trunc >= Sys.Date() - 30) %>%
  add_viz(type = "timeline", x_label = "Week", group_var = "entity_name", y_var = "biochar_kg",
          time_var = "period_trunc", title = "All time", tabgroup = "Timeframe",
          agg = "sum")

create_dashboard(
  title = "Carboneers Operations Dashboard",
  output_dir = "docs/operations",
  theme = "flatly"
) %>%
  add_pages(home, surveys_by_kiln, biochar_by_kiln, surveys_by_coordinator, biochar_by_coordinator) %>%
  generate_dashboard(render = TRUE, open = "browser")


# Publish dashboard
update_dashboard()
