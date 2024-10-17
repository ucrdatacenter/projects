library(tidyverse)
library(rio)

# import meeting data
meetings_raw <- import("https://github.com/ucrdatacenter/projects/raw/main/SSCPOLI302/2024h2/meetings_per_attendee.xlsx", setclass = "tbl_df") |>
  mutate(meeting_date = as.Date(meeting_date)) |>
  drop_na(procedure_reference)

# import attendee data from packaging waste file
attendees <- import("https://github.com/ucrdatacenter/projects/raw/main/SSCPOLI302/2024h2/attendees/2022_0396_COD_clean.xlsx", setclass = "tbl_df")
# import your own data from the file you copied into your Project Folder
# attendees <- import("YourFileName.xlsx", setclass = "tbl_df")

# retain data for your dossier, then merge with attendee data
meetings <- meetings_raw |>
  filter(procedure_reference == "2022/0396(COD)") |>
  left_join(attendees, by = "attendees")

# assigning colors and names
seats <- tribble(
  ~political_group, ~seats, ~leftright, ~color, ~longname,
  "the Left", 37, 1, "firebrick", "The Left in the European Parliament (GUE/NGL)",
  "Greens", 70, 2, "forestgreen", "Greens/European Free Alliance",
  "S&D", 138, 3, "red", "Progressive Alliance of Socialists and Democrats",
  "Renew", 98, 4, "deepskyblue", "Renew Europe",
  "EPP", 179, 5, "royalblue", "European People's Party",
  "ECR", 69, 6, "darkorange", "European Conservatives and Reformists",
  "ID", 49, 7, "navy", "Identity and Democracy",
  "Non-attached", 63, 8, "gray", "Non-attached Members"
)

# Mapping the number of contacts by political group

# number of meetings per political group
meetings |>
  count(political_group) |>
  left_join(seats) |>
  ggplot(
    aes(x = reorder(political_group, leftright), y = n, fill = color)
  ) +
  geom_col() +
  scale_fill_identity()


#	number of meetings per seat per political group
meetings |>
  count(political_group) |>
  left_join(seats) |>
  mutate(meetings_per_seat = n / seats) |>
  ggplot(
    aes(x = reorder(political_group, leftright), y = meetings_per_seat, fill = color)
  ) +
  geom_col() + scale_fill_identity()

#	number of meetings by MEP-role
meetings |>
  count(member_capacity) |>
  ggplot(
    aes(x = member_capacity, y = n)
  ) +
  geom_col()

# number of meetings by MEP-role and by political group
meetings |>
  count(member_capacity, political_group) |>
  left_join(seats) |>
  ggplot(
    aes(x = reorder(political_group,leftright), y = n, fill = member_capacity)
  ) +
  geom_col()

# number of meetings by organization type
meetings |>
  count(class) |>
  ggplot(
    aes(x = class, y = n)
  ) +
  geom_col()

# number of organizations by political group
meetings |>
  count(political_group, fixed_names) |>
  count(political_group) |>
  left_join(seats) |>
  ggplot(
    aes(x = reorder(political_group, leftright), y = n)
  ) +
  geom_col()

# number of meetings by political group and by class
meetings |>
  count(class, political_group, sort = T) |>
  left_join(seats) |>
  ggplot(
    aes(x = reorder(political_group,leftright), y = n, fill = class)
  ) +
  geom_col()

# number of organizations by political group and by class
meetings |>
  count(class, political_group, fixed_names, sort = T) |>
  count(class, political_group) |>
  left_join(seats) |>
  ggplot(
    aes(x = reorder(political_group,leftright), y = n, fill = class)
  ) +
  geom_col()

# Names of the interest groups with most contacts by political group

# number of contacts per fixed_name by political group
orgxgroups <- meetings |>
  count(political_group, fixed_names) |>
  group_by(fixed_names) |>
  mutate(total_meetings = sum(n)) |>
  pivot_wider(names_from = political_group, values_from = n) |>
  arrange(desc(total_meetings))

# list individual MEPs with most meetings and their political group and roles and distribution over type
ranked_meps <- meetings |>
  count(member_name, member_capacity, political_group, name = "number_of_meetings") |>
  left_join(
    meetings |>
      filter(class == "NGO") |>
      count(member_name, member_capacity, name = "ngo_meetings"),
    by = c("member_name", "member_capacity")
  ) |>
  left_join(
    meetings |>
      filter(class == "business") |>
      count(member_name, member_capacity, name = "business_meetings"),
    by = c("member_name", "member_capacity")
  ) |>
  replace_na(list(ngo_meetings = 0, business_meetings = 0)) |>
  arrange(desc(number_of_meetings))


ranked_meps <- meetings |>
  drop_na(class) |>
  group_by(country, member_capacity, political_group, member_id, member_name) |>
  summarize(
    business = sum(class == "business"),
    ngo = sum(class == "NGO"),
    total = n()
  ) |>
  arrange(desc(total))

# showing meetings for a specific interest group (McDonalds)
meetings |>
  filter(str_detect(fixed_names, "Donal")) |>
  count(political_group, member_capacity) |>
  left_join(seats) |>
  ggplot(
    aes(x = reorder(political_group,leftright), y = n, fill = member_capacity)
  ) +
  geom_col()

# timing of meetings by MEP role
meetings |>
  mutate(meeting_date = floor_date(meeting_date, unit = "months")) |>
  count(member_capacity, meeting_date) |>
  ggplot(
    aes(x = meeting_date, y = n, fill = member_capacity)
  ) +
  geom_col()
