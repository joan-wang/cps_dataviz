library(tidyverse)
library(reshape2)

mytheme <- theme(panel.background = element_rect(fill = "grey80"), panel.grid.major.y = element_blank(),
                 panel.grid.minor.y = element_blank())

mobility <- read_csv('../cps_data/Mobility 01-16.csv')
profile <- read_csv('../cps_data/Profile_1617.csv')
combined_1617 <- read_csv('../cps_data/Combined_1617.csv')
elem_1617 <- filter(combined_1617, Primary_Category == 'ES')
attendance <- read_csv('../cps_data/Attendance_03-17.csv')
attendance <-select(filter(attendance, Group == "All (Excludes Pre-K)"), -Grade)

# Scatter: second year attendance vs reading attainment score
attainment <- melt(select(elem_1617, Student_Attendance_Year_2_Pct, School_ID, Attainment_Reading_Pct_ES, Attainment_Math_Pct_ES), id.vars = c("School_ID", "Student_Attendance_Year_2_Pct"),
            variable.name = "attainment_variable", 
            value.name = "attainment_score")

ggplot(data = attainment, aes(x = Student_Attendance_Year_2_Pct, y = attainment_score)) +
  geom_point(aes(color = attainment_variable)) +
  geom_smooth(model = lm) + 
  xlim(85, 100) + 
  scale_color_discrete(name="Subject",
                        labels=c("Reading", "Math")) + 
  labs(title = "Elementary schools with higher attendance rates see higher attainment scores",
     subtitle = "CPS attendance rates and attainment scores in 2016",
     x = "Attendance Rate",
     y = "Attainment Score (Percentile of National Average)",
     caption = "Source: Chicago Public Schools - School Progress Reports SY1617") + 
  mytheme

# Line: attendance over time

avg_attendance <- melt(attendance, id.vars = c("School ID", "School Name",	"Network", "Group"),
                    variable.name = "year", 
                    value.name = "attendance_rate") %>%
  rename("School_ID" = "School ID") %>%
  inner_join(profile, by="School_ID") %>%
  group_by(year, Zip) %>%
  summarise(attendance_rate = mean(attendance_rate, na.rm = TRUE))

avg_attendance$Zip <- factor(avg_attendance$Zip)

ggplot(data = avg_attendance, aes(x = year, y = attendance_rate, color = factor(Zip))) + 
  geom_line(aes(group = Zip))


# Line: mobility over time
profile$School_ID <- as.character(profile$School_ID)
avg_mobility <- melt(mobility, id.vars = c("School ID", "School Name"),
                       variable.name = "year", 
                       value.name = "mobility_rate") %>%
  rename("School_ID" = "School ID") %>%
  inner_join(profile, by="School_ID") %>%
  group_by(year, Zip) %>%
  summarise(mobility_rate = mean(mobility_rate, na.rm = TRUE))

ggplot(data = avg_mobility, aes(x = year, y = mobility_rate, color = factor(Zip))) + 
  geom_line(aes(group = Zip))
