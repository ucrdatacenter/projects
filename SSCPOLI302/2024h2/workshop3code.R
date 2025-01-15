library(tidyverse)
library(rio)

meetings_raw <- import("https://github.com/ucrdatacenter/projects/raw/main/SSCPOLI302/2024h2/meetings_per_attendee.xlsx", setclass = "tbl_df") |> 
  mutate(meeting_date = as.Date(meeting_date))

# this command is to import my file from project folder; replace file name with my file name
attendees <- import("https://github.com/ucrdatacenter/projects/raw/main/SSCPOLI302/2024h2/attendees/2022_0396_COD_clean.xlsx", setclass = "tbl_df")

meetings <- meetings_raw |> 
  filter(procedure_reference == "2022/0396(COD)") |> 
  full_join(attendees)

meetings |> 
  count(country, member_capacity) |> 
  ggplot(
    aes(y = country, x = n, fill = member_capacity)
  ) + 
  geom_col()

meetings |> 
  count(class, structure) |> 
  ggplot(
    aes(y = class, x = n, fill = structure)
  ) + 
  geom_col()

meetings |> 
  filter(class == "business") |> 
  distinct(fixed_names)

meetings |> 
  filter(class == "NGO") |> 
  distinct(fixed_names)

meetings |> 
  count(meeting_date) |> 
  ggplot(
    aes(x = meeting_date, y = n)
  ) +
  geom_col()

dates <- meetings |>
  count(meeting_date)


