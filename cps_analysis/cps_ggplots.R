library(tidyverse)
library(reshape2)

mytheme <- theme(panel.background = element_rect(fill = "grey80"), panel.grid.major.y = element_blank(),
                 panel.grid.minor.y = element_blank())

combined_1617 <- read_csv('../cps_data/Combined_1617.csv')
elem_1617 <- filter(combined_1617, Primary_Category == 'ES')

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

# change in attendance between the two years
