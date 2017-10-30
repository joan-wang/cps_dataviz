library(tidyverse)
library(reshape2)
library(RColorBrewer)
library(ggplot2)
library(sp)
library(broom)
library(dplyr)
library(rgeos)
library(rgdal)
library(maptools)
library(RColorBrewer)
library(viridis)
library(ggmap)

# define theme
windowsFonts(YuGothic=windowsFont("Yu Gothic"))
windowsFonts(YuGothicSemibold=windowsFont("Yu Gothic UI Semibold"))
mytheme <- theme(panel.background = element_rect(fill = 'gray95'), 
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(), 
                 panel.border = element_blank(),
                 text = element_text(size=12, family="YuGothicSemibold", color='#34495E'),
                 strip.text = element_text(size=8, family="YuGothic"),
                 plot.title = element_text(size = 14, face = "bold"),
                 plot.subtitle = element_text(size = 12, face = "bold"),
                 plot.caption = element_text(size=8, color='grey60')
                 )
my_palette <- c('#FECEA8', '#A1BBD0', '#D1A827', '#687672', '#3F4234')

# read in data
profile <- read_csv('../cps_data/Profile_1617.csv')

elem_1213 <- read_csv('../cps_data/Elementary_1213.csv')
elem_1314 <- read_csv('../cps_data/Elementary_1314.csv')

combined_1516 <- read_csv('../cps_data/Combined_1516.csv')
elem_1516 <- filter(combined_1617, Primary_Category == 'ES')
combined_1617 <- read_csv('../cps_data/Combined_1617.csv')
elem_1617 <- filter(combined_1617, Primary_Category == 'ES')
combined_1617[combined_1617 == 'NO DATA AVAILBLE'] <- NA
combined_1617[combined_1617 == 'NOT ENOUGH DATA'] <- NA
combined_1617$School_Survey_Safety <- factor(combined_1617$School_Survey_Safety, levels = c('VERY WEAK', 'WEAK', 'NEUTRAL', 'STRONG', 'VERY STRONG'))

neighborhoods <- read_csv('../cps_data/zip_to_ca.csv')
mobility <- read_csv('../cps_data/Mobility 01-16.csv')
attendance <- read_csv('../cps_data/Attendance_03-17.csv') %>%
  filter(Group == "All (Excludes Pre-K)") %>%
  select(-Grade)

crimes <- read_csv('../cps_data/Crimes_2016.csv') # details of crimes in 2016
schools_crimes <- read_csv('schools_crimes.csv') # number of crimes within 1 mile of each school in 2016

# join school data with neighborhoods
combined_1617 <- left_join(combined_1617, neighborhoods, by='Zip')
combined_1617$"Neighborhood" <- factor(combined_1617$"Neighborhood")

# join school data with crime data
school_locations <- readOGR(dsn=path.expand("../cps_data/Chicago Public Schools - School Locations SY1617"), layer="Chicago Public Schools - School Locations SY1617")
combined_1617_crimes <- inner_join(combined_1617, schools_crimes, by="School_ID")
combined_1617_crimes <- merge(combined_1617_crimes, as.data.frame(school_locations), by.x="School_ID", by.y="school_id")

combined_1617_crimes <- within(combined_1617_crimes, crime_quartile <- as.integer(cut(num_crimes_16, quantile(num_crimes_16, probs=0:4/4), include.lowest=TRUE)))
combined_1617_crimes <- within(combined_1617_crimes, crime_median <- as.integer(cut(num_crimes_16, quantile(num_crimes_16, probs=0:2/2), include.lowest=TRUE)))
combined_1617_crimes$crime_quartile <-factor(combined_1617_crimes$crime_quartile, labels = c('Q1', 'Q2', 'Q3', 'Q4'))
combined_1617_crimes$crime_median <- factor(combined_1617_crimes$crime_median, labels = c('Below Median', 'Above Median'))

"""
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
"""

# Density plots: attendance rate by safety rating
ggplot(subset(filter(combined_1617, Primary_Category == "ES"), !is.na(School_Survey_Safety)), aes(x=Student_Attendance_Year_2_Pct, color = School_Survey_Safety, fill=School_Survey_Safety)) +
  geom_density() + 
  scale_fill_viridis(guide = FALSE, discrete=TRUE, option='magma', begin=0.2, end=0.85) + 
  scale_color_viridis(guide = FALSE, discrete=TRUE, option='magma', begin=0.2, end=0.85) + 
  facet_wrap(~ School_Survey_Safety, ncol=1) + 
  labs(title = "Attendance tends to be higher at schools where students feel safe",
       subtitle = "Distribution of elementary school attendance rates by safety ratings*",
       x = "Student Attendance Rate (Percents)",
       y = "Density",
       caption = "Source: Chicago Public Schools - School Progress Reports SY1617\n *Safety ratings based on student survey") +
  mytheme

# Density plots: attainment by safety rating
ggplot(subset(filter(combined_1617, Primary_Category == "ES"), !is.na(School_Survey_Safety)), aes(x=Attainment_Reading_Pct_ES, color = School_Survey_Safety, fill=School_Survey_Safety)) +
  geom_density() + 
  scale_fill_viridis(guide = FALSE, discrete=TRUE, option='magma', begin=0.2, end=0.85) + 
  scale_color_viridis(guide = FALSE, discrete=TRUE, option='magma', begin=0.2, end=0.85) + 
  facet_wrap(~ School_Survey_Safety, ncol=1) + 
  labs(title = "Attainment tends to be higher at schools where students feel safe",
       subtitle = "Distribution of elementary school reading attainment percentiles by safety ratings*",
       x = "Student Reading Attainment (Percentile of National Average)",
       y = "Density",
       caption = "Source: Chicago Public Schools - School Progress Reports SY1617\n *Safety ratings based on student survey") +
  mytheme


# Box & whisker: number of crimes by safety rating
ggplot(subset(combined_1617_crimes, !is.na(School_Survey_Safety)), aes(x=School_Survey_Safety, y=num_crimes_16, fill=School_Survey_Safety)) +
  scale_fill_viridis(option='magma', discrete=TRUE, begin=0.2, end=0.85) + 
  geom_boxplot() + 
  guides(fill=FALSE) + 
  labs(title = "Students feel more unsafe at school in high-crime neighborhoods",
       subtitle = "Schools with weaker safety ratings face higher number of crimes \nwithin 1 mile radius of campus",
       x = "Safety Rating*",
       y = "Number of Crimes in School Vicinity",
       caption = "Source: Chicago Public Schools - School Progress Reports SY1617\n Chicago Police Department - Crimes 2016
        \n *Safety ratings based on student survey") + 
  mytheme

# Scatter: Percent Low Income Students vs. Crimes in Vicinity
profile <- mutate(profile, perc_lowincome = Student_Count_Low_Income / Student_Count_Total)
subset(combined_1617_crimes, !is.na(School_Survey_Safety)) %>%
  merge(profile, by="School_ID") %>%
  ggplot(aes(x = perc_lowincome, y = num_crimes_16)) +
    geom_point(aes(color = School_Survey_Safety), alpha=0.5) +
    geom_smooth(model = lm) + 
    scale_x_continuous(labels = scales::percent) + 
    scale_color_viridis(name="Safety Rating*", option='magma', begin=0.2, end=0.85, discrete=TRUE) + 
    labs(title = "Low income students attend schools in highest-crime neighborhoods ",
         subtitle = "Schools with higher percent of low income students see higher number of \ncrimes wtihin 1 mile radius",
         x = "Percent Low Income Students",
         y = "Number of Crimes in School Vicinity",
         caption = "Source: Chicago Public Schools - School Progress Reports SY1617\n Chicago Police Department - Crimes 2016
          \n *Safety ratings based on student survey") + 
    mytheme  + theme(legend.position="bottom")

# Heat map: safety rating by neighborhood
combined_1617 %>%
  subset(!is.na(School_Survey_Safety) & !is.na(Neighborhood)) %>%
  group_by(Neighborhood, School_Survey_Safety) %>%
  summarize(num_schools=n(), na.rm = TRUE) %>% 
  mutate(prop_schools = num_schools/sum(num_schools)) %>%
  ggplot(aes(x=School_Survey_Safety, y=Neighborhood, fill=prop_schools)) + 
  geom_tile() + 
  scale_fill_distiller(name = 'Proportion of Schools\n in Neighborhood\n', palette='Greys', direction=1) + 
  labs(title = "Students' perception of school safety varies between neighborhoods",
       x = "Safety Rating*",
       y = "Neighborhood",
       caption = "Source: Chicago Public Schools - School Progress Reports SY1617\n *Safety ratings based on student survey") + 
  mytheme + theme(legend.position="bottom")

# Map: School locations, colored by safety ratings and sized by num_crimes
chicago <- ggmap(get_map(location = bbox(school_locations), source='stamen', maptype='toner-lite', color='bw'))

chicago + 
  geom_point(data=combined_1617_crimes %>% subset(!is.na(School_Survey_Safety)), aes(long, lat,  color=School_Survey_Safety, size=num_crimes_16, alpha=0.3)) + 
  scale_alpha(guide=FALSE) + 
  scale_color_viridis(name = "Safety Rating*", option="magma", discrete=TRUE, begin=0.2, end=0.85)+
  scale_size(name='Number of Crimes within \n1 Mile in 2016')
  coord_equal() + 
  labs(title = "Students' perception of safety varies across the city",
       caption = "Source: Chicago Public Schools - School Progress Reports SY1617\n Chicago Police Department - Crimes 2016
       \n *Safety ratings based on student survey") + 
  mytheme + 
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank())


# Map: Safe Passage Routes
safe_passage <- readOGR(dsn=path.expand("../cps_data/Chicago Public Schools - Safe Passage Routes SY1617"), layer="geo_export_05e1123e-61d4-4f97-b8c6-1a8223234e8c")
safe_passage_df <- tidy(safe_passage, region="rt_num")

chicago + 
  geom_line(data = safe_passage_df, aes(long, lat, group=group), size=1.2) + 
  geom_point(data=combined_1617_crimes, aes(long, lat, color=crime_quartile), alpha=0.4, size=3) + 
  coord_equal() + 
  scale_alpha(guide=FALSE) + 
  scale_color_viridis(option='plasma', direction=-1, discrete=TRUE, name = "Number of Crimes within \n1 Mile in 2016 (Quartile)") + 
  labs(title = "Safe passage routes cover some schools in high-crime\nneighborhoods, but not all ",
       caption = "Source: Chicago Public Schools - School Progress Reports SY1617\n Chicago Police Department - Crimes 2016\nChicago Public Schools - Safe Passage Routes SY1617
       \n *Safety ratings based on student survey") + 
  mytheme + 
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank())



