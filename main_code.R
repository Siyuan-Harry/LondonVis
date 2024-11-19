library(sf)
library(tidyverse)
library(patchwork)
library(stargazer)

###### Set Up: $#################################################################
# please switch working directory to the folder where this R file lies ##########

getwd()
setwd("desktop/dspp/econ0128/midterm") 
# Please change this PATH according to your computer

#################################################################################

# Target 1: Visualize London Education Level Distribution (in Post District Level)

london_post_sectors <- st_read("london_geolytix_postal_district_shp4326/london_geolytix_postal_district.shp")
head(london_post_sectors$post_dist) # [1] "EC1M" "UB11" "IG9"  "TW18" "W8"   "E20" 
head(london_post_sectors$post_area) # [1] "EC" "UB" "IG" "TW" "W"  "E" 

# a set of london's central districts - must be filtered before making map
# otherwise the central area gets all jumbled up, because the zip codes in which are so dense
central_districts <- c(
  "W1H", "W1U", "W1G", "W1B", "W1C", "W1W", "W1A", "W1T", "W1K", "W1S", "W1F", "W1D", "W1J",
  "WC1E", "WC1H", "WC1B", "WC1N", "WC1H", "WC1X", "WC2H", "WC1A", "WC1V", "WC1R", "WC2H", "WC2B", 
  "WC2E", "WC2N", "WC2A", "WC2R",
  "EC1R", "EC1V", "EC1N", "EC1M", "EC1A", "EC1Y", "EC2Y", "EC2A", "EC2M", "EC2V", "EC4A", "EC4Y",
  "EC4M", "EC4V", "EC2R", "EC4N", "EC2N", "EC3V", "EC3A", "EC3M", "EC3N", "EC3R", "EC4R",
  "SW1Y", "SW1A", "SW1H", "SW1E", "SW1P"
)
# filter to exclude central postcodes (this action is only for visualisation)
london_post_sectors_onlyWider <- london_post_sectors |>
  filter(!post_dist %in% central_districts)

# merge london post district map and education data (in lsoa scale)
match_oa_lsoa <- read.csv("match_oa_lsoa.csv")
# add one more column which stands for "Post District" - for merging with educational data (lsoa level)
match_pcd_oa_lsoa <- match_oa_lsoa |> 
  mutate(PostDist = str_extract(pcds, "^[A-Za-z0-9]+")) 
str(match_pcd_oa_lsoa) # 5m+ rows

London_PostDist_LSOA <- london_post_sectors_onlyWider |>
  left_join(match_pcd_oa_lsoa, by = c("post_dist" = "PostDist"))
# deduplicate to only keep distinct LSOA level data (ignoring OA level)
London_PostDist_LSOA <- London_PostDist_LSOA |> 
  group_by(lsoa21cd) |>
  slice(1) |>
  ungroup()

str(London_PostDist_LSOA)
#seems first row means nothing.
London_PostDist_LSOA <- London_PostDist_LSOA[-1, ]

# inspect the merged data "London_PostDist_LSOA"
London_PostDist_LSOA |>
  filter(lsoa21cd == "E01034386")
London_PostDist_LSOA |>
  filter(pcds == "N7 0EG") # this returns nothing - expected because lots of data that is finer than the lsoa area are already filtered out
London_PostDist_LSOA |>
  filter(post_dist == "N7")
# good, all printed results are expected.

# now we import education data in each LSOA area
uk_edu_lsoa <- read.csv("census2021-ts067/census2021-ts067-lsoa.csv")
str(uk_edu_lsoa)

# Last steps before visualization: 
# 1. merge datasets based on london lsoa
# 2. group_by post_dist (e.g. N7)
# 3. sum the number of people at each level of education
# 4. de-duplicate

London_Edu_LSOA <- London_PostDist_LSOA |>
  left_join(uk_edu_lsoa, by = c("lsoa21cd" = "geography.code")) |>
  rename(
    total_edu = Highest.level.of.qualification..Total..All.usual.residents.aged.16.years.and.over,
    no_edu = Highest.level.of.qualification..No.qualifications,
    edu_lv1 = Highest.level.of.qualification..Level.1.and.entry.level.qualifications,
    edu_lv2 = Highest.level.of.qualification..Level.2.qualifications,
    edu_lv3 = Highest.level.of.qualification..Level.3.qualifications,
    edu_lv4 = Highest.level.of.qualification..Level.4.qualifications.and.above
  )

str(London_Edu_LSOA) # 5854 * 31 - nice shape

# Then, group_by post_dist (e.g. N7) and sum the education data in district
# update the London_Edu_LSOA

London_Edu_PostDist <- London_Edu_LSOA |> 
  group_by(post_dist) |>  # 按 PostDist 分组
  summarize(
    total_edu = sum(total_edu, na.rm = TRUE), 
    total_no_edu = sum(no_edu, na.rm = TRUE), 
    total_edu_lv1 = sum(edu_lv1, na.rm = TRUE),
    total_edu_lv2 = sum(edu_lv2, na.rm = TRUE),
    total_edu_lv3 = sum(edu_lv3, na.rm = TRUE),
    total_edu_lv4 = sum(edu_lv4, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  mutate( #
    frac_no_edu = total_no_edu / total_edu,
    frac_edu_lv4 = total_edu_lv4 / total_edu
  )

# visualize distribution of high qualification people
p1_edu <- ggplot(data = London_Edu_PostDist) +
  geom_sf(aes(fill = frac_edu_lv4), color = "black", size = 0.2) +  
  scale_fill_gradient(low = "lightblue", high = "darkblue", guide = "colorbar") +  # Dark colors represent high proportions
  theme_minimal() +
  labs(title = "London Post Districts Education Map",
       subtitle = "Education level in the central area is higher",
       fill = "Fraction Education \nLevel 4 or Above",
       caption = "Post District Data from geolytix.co.uk") +  
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),  
        panel.grid = element_blank(),
        legend.text = element_text(size = 8), 
        legend.position = "right",
        legend.margin = margin(0, 0, 0, 0)) +  
  geom_sf_text(aes(label = post_dist), size = 1, color = "white")
# visualize distribution of no education qualification people
p2_edu <- ggplot(data = London_Edu_PostDist) +
  geom_sf(aes(fill = frac_no_edu), color = "black", size = 0.2) +  
  scale_fill_gradient(low = "lightblue", high = "darkblue", guide = "colorbar") +  # Dark colors represent high proportions
  theme_minimal() +
  labs(title = "London Post Districts Education Map",
       subtitle = "Post districts EN3, UB2 and N18 has larger proportion of uneducated popuation.",
       fill = "Fraction \nNo Education",
       caption = "Post District Data from geolytix.co.uk") +  
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),  
        panel.grid = element_blank()) +
  geom_sf_text(aes(label = post_dist), size = 1, color = "white")

p1_edu
p2_edu
p1_edu / p2_edu # not good, remake

p1_edu_for_combination <- ggplot(data = London_Edu_PostDist) +
  geom_sf(aes(fill = frac_edu_lv4), color = "black", size = 0.2) +  
  scale_fill_gradient(low = "lightblue", high = "darkblue", guide = "colorbar") + # Dark colors represent high proportions
  theme_minimal() +
  labs(title = "London Post Districts Education Map",
       fill = "Fraction Education \nLevel 4 or Above") + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),  
        panel.grid = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size = 5))
p2_edu_for_combination <- ggplot(data = London_Edu_PostDist) +
  geom_sf(aes(fill = frac_no_edu), color = "black", size = 0.2) +  
  scale_fill_gradient(low = "lightblue", high = "darkblue", guide = "colorbar") + # Dark colors represent high proportions
  theme_minimal() +
  labs(fill = "Fraction of Population \nNo Education Qualification",
       caption = "Post District Data from geolytix.co.uk") +  
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),  
        panel.grid = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size = 5))

p1_edu_for_combination / p2_edu_for_combination








# Target 2: Construct a dataset which contains London's education, crime rate, employment status data,
# and test my hypothesis with modelling based on this dataset

# Step 2.1: construct education lsoa dataset for all london (including central area)
london_postDist <- london_post_sectors |>
  as.data.frame() |> 
  select(post_dist)

# match with london post district data to constrain it into only London (Not whole UK)
London_PostDist_LSOA_all <- london_postDist |>
  left_join(mach_pcd_oa_lsoa, by=c("post_dist"="PostDist"))
# clear all sf attributes to make it a plain dataframe 
attr(London_PostDist_LSOA_all, "sf_column") <- NULL
attr(London_PostDist_LSOA_all, "agr") <- NULL
str(London_PostDist_LSOA_all)

# Again, deduplicate to only keep distinct LSOA level data (ignoring OA level)
London_LSOA_all <- London_PostDist_LSOA_all |> 
  group_by(lsoa21cd) |>
  slice(1) |>
  ungroup()

str(London_LSOA_all) # london's all LSOA code
# again, remove first row
London_LSOA_all <- London_LSOA_all[-1, ]

# again, joining uk education data to get a dataset that contains all london's LSOA level educational data
London_Edu_LSOA_all <- London_LSOA_all |> 
  left_join(uk_edu_lsoa, by = c("lsoa21cd" = "geography.code")) |>
  rename(
    total_edu = Highest.level.of.qualification..Total..All.usual.residents.aged.16.years.and.over,
    no_edu = Highest.level.of.qualification..No.qualifications,
    edu_lv1 = Highest.level.of.qualification..Level.1.and.entry.level.qualifications,
    edu_lv2 = Highest.level.of.qualification..Level.2.qualifications,
    edu_lv3 = Highest.level.of.qualification..Level.3.qualifications,
    edu_lv4 = Highest.level.of.qualification..Level.4.qualifications.and.above
  )

str(London_Edu_LSOA_all) # 5,897 × 25 good

London_Edu_LSOA_all <- London_Edu_LSOA_all |> # select useful columns
  select(
    total_edu, no_edu, edu_lv1, edu_lv2,
    edu_lv3, edu_lv4, lsoa21cd
  ) |>
  mutate( # add two
    frac_no_edu = no_edu / total_edu,
    frac_edu_lv4 = edu_lv4 / total_edu
  )

str(London_Edu_LSOA_all) # this is the expected "education lsoa dataset for all london (including central area)"

# Step 2.2: construct crime rate dataset which contains all london lsoa area crime rate data

crime_london_202309 <- read.csv("london_crime/2023-09-city-of-london-street.csv")
crime_london_202310 <- read.csv("london_crime/2023-10-city-of-london-street.csv")
crime_london_202311 <- read.csv("london_crime/2023-11-city-of-london-street.csv")
crime_london_202312 <- read.csv("london_crime/2023-12-city-of-london-street.csv")
crime_london_202401 <- read.csv("london_crime/2024-01-city-of-london-street.csv")
crime_london_202402 <- read.csv("london_crime/2024-02-city-of-london-street.csv")
crime_london_202403 <- read.csv("london_crime/2024-03-city-of-london-street.csv")
crime_london_202404 <- read.csv("london_crime/2024-04-city-of-london-street.csv")
crime_london_202405 <- read.csv("london_crime/2024-05-city-of-london-street.csv")
crime_london_202406 <- read.csv("london_crime/2024-06-city-of-london-street.csv")
crime_london_202407 <- read.csv("london_crime/2024-07-city-of-london-street.csv")
crime_london_202408 <- read.csv("london_crime/2024-08-city-of-london-street.csv")

yearly_crime_london_by_lsoa <- data.frame(
  lsoa_code = London_Edu_LSOA_all$lsoa21cd, # unique value of each lsoa district in London
  crime_total_202309 = 0,
  crime_total_202310 = 0,
  crime_total_202311 = 0,
  crime_total_202312 = 0,
  crime_total_202401 = 0,
  crime_total_202402 = 0,
  crime_total_202403 = 0,
  crime_total_202404 = 0,
  crime_total_202405 = 0,
  crime_total_202406 = 0,
  crime_total_202407 = 0,
  crime_total_202408 = 0 # no extra comma here
)
str(yearly_crime_london_by_lsoa)

crime_data_list <- list(
  crime_london_202309, crime_london_202310, crime_london_202311, crime_london_202312,
  crime_london_202401, crime_london_202402, crime_london_202403, crime_london_202404,
  crime_london_202405, crime_london_202406, crime_london_202407, crime_london_202408
) # list all raw crime data for each month - make it convenient for looping over

str(crime_london_202309)

crime_quant_months <- colnames(yearly_crime_london_by_lsoa)[-1]  # except for column1

# calculate bu accumulating data from each crime record csv
for (i in seq_along(crime_data_list)) {
  crime_data <- crime_data_list[[i]]
  month_col <- crime_quant_months[i]
  
  for (j in 1:nrow(crime_data)) {
    crime_loc_code <- crime_data[j, "LSOA.code"]
    loc_index <- which(yearly_crime_london_by_lsoa$lsoa_code == crime_loc_code)
    yearly_crime_london_by_lsoa[[month_col]][loc_index] <- 
      yearly_crime_london_by_lsoa[[month_col]][loc_index] + 1
  }
}

sum(yearly_crime_london_by_lsoa$crime_total_202309 == 0)
length(yearly_crime_london_by_lsoa$crime_total_202309)
# Now we have only calculated data for "City of London" Area - this is not the whole London
# We should add Metropolitan area to include Greater London's crime data

crime_metro_202309 <- read.csv("metropolitan_crime/2023-09-metropolitan-street.csv")
crime_metro_202310 <- read.csv("metropolitan_crime/2023-10-metropolitan-street.csv")
crime_metro_202311 <- read.csv("metropolitan_crime/2023-11-metropolitan-street.csv")
crime_metro_202312 <- read.csv("metropolitan_crime/2023-12-metropolitan-street.csv")
crime_metro_202401 <- read.csv("metropolitan_crime/2024-01-metropolitan-street.csv")
crime_metro_202402 <- read.csv("metropolitan_crime/2024-02-metropolitan-street.csv")
crime_metro_202403 <- read.csv("metropolitan_crime/2024-03-metropolitan-street.csv")
crime_metro_202404 <- read.csv("metropolitan_crime/2024-04-metropolitan-street.csv")
crime_metro_202405 <- read.csv("metropolitan_crime/2024-05-metropolitan-street.csv")
crime_metro_202406 <- read.csv("metropolitan_crime/2024-06-metropolitan-street.csv")
crime_metro_202407 <- read.csv("metropolitan_crime/2024-07-metropolitan-street.csv")
crime_metro_202408 <- read.csv("metropolitan_crime/2024-08-metropolitan-street.csv")

crime_data_list_metro <- list(
  crime_metro_202309, crime_metro_202310, crime_metro_202311, crime_metro_202312,
  crime_metro_202401, crime_metro_202402, crime_metro_202403, crime_metro_202404,
  crime_metro_202405, crime_metro_202406, crime_metro_202407, crime_metro_202408
)

# again, accumulating each crime record csv
# NOTICE: this loop could take a while to run
for (i in seq_along(crime_data_list_metro)) {
  crime_data <- crime_data_list_metro[[i]]
  month_col <- crime_quant_months[i]
  
  for (j in 1:nrow(crime_data)) {
    crime_loc_code <- crime_data[j, "LSOA.code"]
    loc_index <- which(yearly_crime_london_by_lsoa$lsoa_code == crime_loc_code)
    yearly_crime_london_by_lsoa[[month_col]][loc_index] <- 
      yearly_crime_london_by_lsoa[[month_col]][loc_index] + 1
  }
}
# check results
sum(yearly_crime_london_by_lsoa$crime_total_202309 == 0) # 837
length(yearly_crime_london_by_lsoa$crime_total_202309) # 5397

# add one column to summarize crime for the year:
yearly_crime_london_by_lsoa <- yearly_crime_london_by_lsoa |>
  mutate(crime_year_total = rowSums(across(starts_with("crime_total_"))))

str(yearly_crime_london_by_lsoa)
# That's it! the yearly crime data for each of the london LSOA district

# Step 2.3: merge crime & education dataset

Education_Crime_LSOA <- London_Edu_LSOA_all |>
  left_join(yearly_crime_london_by_lsoa, by=c("lsoa21cd"="lsoa_code"))
str(Education_Crime_LSOA)

# Step 2.4: add unemployment status to my dataset - unemploy over 12 months ratio

unemploy_lsoa <- read.csv("census2021-ts065/census2021-ts065-lsoa.csv")
str(unemploy_lsoa)

unemploy_lsoa <- unemploy_lsoa |>
  rename(
    lsoa21_code = geography.code,
    ne_current = Unemployment.history..Total..All.usual.residents.aged.16.years.and.over.not.in.employment.the.week.before.the.census,
    ne_w_12m = Unemployment.history..Not.in.employment..Worked.in.the.last.12.months,
    ne_nw_12m = Unemployment.history..Not.in.employment..Not.worked.in.the.last.12.months,
    ne_nw = Unemployment.history..Not.in.employment..Never.worked,
  )

Unemploy_LSOA_London <- London_LSOA_all |> 
  left_join(unemploy_lsoa, by=c("lsoa21cd"="lsoa21_code")) |>
  select(
    lsoa21cd,
    ne_current,
    ne_w_12m,
    ne_nw_12m,
    ne_nw
  )
str(Unemploy_LSOA_London)

# Step 2.5: merge Unemploy_LSOA_London to previous two dataset - creating more complete data

Education_Crime_Employment_LSOA <- Education_Crime_LSOA |>
  left_join(Unemploy_LSOA_London, by="lsoa21cd") |>
  rename(total_resident = total_edu) |> # this field in edu dataset denotes exactly the total number of resident in corresponding district
  mutate ( # add fraction data for employment - make convenience for analysis
    frac_unemploy_now = ne_current / total_resident,
    frac_ne_nw_12m = ne_nw_12m / total_resident,
    frac_ne_nw = ne_nw / total_resident
  )

# add one more column to calculate crime rate
Education_Crime_Employment_LSOA <- Education_Crime_Employment_LSOA |>
  mutate(crime_rate = crime_year_total / (total_resident/1000)) 
# the method of calculating crime rate follows UK crime stats official method.
# https://www.ukcrimestats.com/blog/faqs/what-exactly-does-crime-rate-mean-and-how-do-you-calculate-it/

# Step 2.6: add another feature to dataset - Economically active (excluding full-time students): Unemployed

econActive_lsoa <- read.csv("census2021-ts066/census2021-ts066-lsoa.csv")
str(econActive_lsoa)

econActive_lsoa <- econActive_lsoa |>
  rename(
    lsoa21_code = geography.code,
    all_residents = Economic.activity.status..Total..All.usual.residents.aged.16.years.and.over,
    une_ex_student = Economic.activity.status..Economically.active..excluding.full.time.students...Unemployed
  ) |>
  mutate(frac_une_ex_student = une_ex_student/all_residents)

econActive_LSOA_London <- London_LSOA_all |> 
  left_join(econActive_lsoa, by=c("lsoa21cd"="lsoa21_code")) |>
  select(lsoa21cd, frac_une_ex_student)
str(econActive_LSOA_London)

# merge into our dataset
Education_Crime_Employment_LSOA <- Education_Crime_Employment_LSOA |>
  left_join(econActive_LSOA_London, by="lsoa21cd")

str(Education_Crime_Employment_LSOA)

# This is the ultimate dataset we will use to analyse










# Target3: Analyse the data and try to construct sensible model to explain the correlations

str(Education_Crime_Employment_LSOA)

lm1 <- lm(crime_rate ~ frac_no_edu + frac_edu_lv4, data = Education_Crime_Employment_LSOA)
summary(lm1)

lm2 <- lm(crime_rate ~ frac_une_ex_student * frac_ne_nw_12m + frac_no_edu * frac_edu_lv4, data=Education_Crime_Employment_LSOA)
summary(lm2)
# lm1 and lm2 are not good. Let's see what is going on.

# Visulize the distribution of crime rate
ggplot(data = Education_Crime_Employment_LSOA, aes(x = crime_rate)) +
  geom_density(fill = "blue", alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Density Plot of Crime Rate",
    x = "Crime Rate",
    y = "Density"
  )
# Crime rate is seriously skewed. 
# Make a log transformation to crime rate
Education_Crime_Employment_LSOA <- Education_Crime_Employment_LSOA |>
  mutate(crime_rate_log = log1p(crime_rate))  # log(1 + x), because crime rate could possibly be 0

# Visulize the distribution of transformed crime rate 
ggplot(data = Education_Crime_Employment_LSOA, aes(x = crime_rate_log)) +
  geom_density(fill = "blue", alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Density Plot of Crime Rate (log Transformed)",
    x = "Crime Rate (Log Transformed)",
    y = "Density"
  )

ggplot(data = Education_Crime_Employment_LSOA, aes(x = frac_no_edu)) +
  geom_density(fill = "grey", alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Density Plot of Population without Educational Qualifications",
    x = "Fraction of populaion without Educational Qualifications",
    y = "Density"
  )

ggplot(data = Education_Crime_Employment_LSOA, aes(x = frac_ne_nw_12m)) +
  geom_density(fill = "orange", alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Density Plot of Unemployment Rate",
    x = "Fraction not worked and employed in last 12 months",
    y = "Density"
  )

ggplot(data = Education_Crime_Employment_LSOA, aes(x = frac_edu_lv4)) +
  geom_density(fill = "green", alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Density Plot of Advanced Educational Status",
    x = "Fraction edcation qualification at least Level 4",
    y = "Density"
  )

## try box plot
ggplot(data = Education_Crime_Employment_LSOA, aes(x = "", y = crime_rate_log)) +
  geom_boxplot(fill = "blue", color = "black") +
  theme_minimal() +
  labs(
    title = "Boxplot of Crime Rate (Log Transformed)",
    x = "",
    y = "Crime Rate (Log Transformed)"
  )
ggplot(data = Education_Crime_Employment_LSOA, aes(x = "", y = frac_no_edu)) +
  geom_boxplot(fill = "grey", color = "black") +
  theme_minimal() +
  labs(
    title = "Boxplot of Population without Educational Qualifications",
    x = "",
    y = "Fraction of Population without Educational Qualifications"
  )
ggplot(data = Education_Crime_Employment_LSOA, aes(x = "", y = frac_ne_nw_12m)) +
  geom_boxplot(fill = "orange", color = "black") +
  theme_minimal() +
  labs(
    title = "Boxplot of Unemployment Rate (Not Worked in Last 12 Months)",
    x = "",
    y = "Fraction Not Worked or Employed in Last 12 Months"
  )



# try some scatter plot

ggplot(data = Education_Crime_Employment_LSOA, aes(x = crime_rate_log, y = frac_une_ex_student)) +
  geom_point(alpha = 0.6, color = "grey") +  
  geom_smooth(method = "lm", color = "black", se = TRUE) +  
  theme_minimal()

ggplot(data = Education_Crime_Employment_LSOA, aes(x = crime_rate_log, y = frac_edu_lv4)) +
  geom_point(alpha = 0.6, color = "grey") +  
  geom_smooth(method = "lm", color = "black", se = TRUE) +  
  theme_minimal()


ggplot(data = Education_Crime_Employment_LSOA, aes(x = crime_rate_log, y = frac_no_edu)) +
  geom_point(alpha = 0.6, color = "grey") +  
  geom_smooth(method = "lm", color = "black", se = TRUE) +  
  theme_minimal()

ggplot(data = Education_Crime_Employment_LSOA, aes(x = crime_rate_log, y = frac_ne_nw)) +
  geom_point(alpha = 0.6, color = "grey") +  
  geom_smooth(method = "lm", color = "black", se = TRUE) +  
  theme_minimal()

ggplot(data = Education_Crime_Employment_LSOA, aes(x = crime_rate_log, y = frac_ne_nw_12m)) +
  geom_point(alpha = 0.6, color = "grey") +  
  geom_smooth(method = "lm", color = "black", se = TRUE) +  
  theme_minimal() # this is interesting..

important_vars <- Education_Crime_Employment_LSOA |>
  select(crime_rate, frac_no_edu, frac_edu_lv4, frac_ne_nw_12m, frac_une_ex_student)

# try to get a table
stargazer(important_vars, type = "html", title = "Summary Statistics of Important Variables", out = "vars.html")

# try to fit some linear models (OLS method)
lm_edu_1 <- lm(crime_rate_log ~ frac_no_edu + frac_edu_lv4, data = Education_Crime_Employment_LSOA)
summary(lm_edu_1)

lm_edu_2 <- lm(crime_rate_log ~ frac_no_edu * frac_edu_lv4, data = Education_Crime_Employment_LSOA)
summary(lm_edu_2)

lm_employ_1 <- lm(crime_rate_log ~ frac_une_ex_student + frac_ne_nw_12m, data = Education_Crime_Employment_LSOA)
summary(lm_employ_1)

lm_employ_2 <- lm(crime_rate_log ~ frac_ne_nw * frac_ne_nw_12m, data = Education_Crime_Employment_LSOA)
summary(lm_employ_2)

lm_mix_1 <- lm(crime_rate_log ~ frac_une_ex_student * frac_ne_nw_12m + frac_no_edu * frac_edu_lv4, data=Education_Crime_Employment_LSOA)
summary(lm_mix_1)

stargazer(lm_edu_1, type = "html", 
          title = "Regression Results: lm_edu_1 model",
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "regression1.html")

stargazer(lm_edu_1, lm_mix_1, type = "html", 
          title = "Regression results: comparison between two models",
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "regression2.html")













# Target 4: construct model only for outer london area (see if it gets better)

str(london_post_sectors_onlyWider)

London_Outer_LSOA_PostDist <- london_post_sectors_onlyWider |> 
  left_join(London_LSOA_all, by="post_dist") |>
  select(lsoa21cd, post_dist)

str(London_Outer_LSOA_PostDist) # 5837 obs. of  3 variables

Outer_Education_Crime_Employment_LSOA <- London_Outer_LSOA_PostDist |> 
  left_join(Education_Crime_Employment_LSOA, by="lsoa21cd")

# check visualize to see wether prediction is precise enough
London_CrimeRate_PostDist <- Outer_Education_Crime_Employment_LSOA |>
  group_by(post_dist) |>
  summarize(
    total_resident = sum(total_resident, na.rm = TRUE),
    crime_year_total = sum(crime_year_total, na.rm=TRUE),
  ) |>
  ungroup() |>
  mutate(
    crime_rate = crime_year_total / (total_resident/1000)
  )

ggplot(data = London_CrimeRate_PostDist) +
  geom_sf(aes(fill = crime_rate), color = "black", size = 0.1) +  
  scale_fill_viridis_c(option = "plasma", direction = -1) +  # Dark colors represent high proportions
  theme_minimal() +
  labs(title = "London Post Districts Crime Map",
       #subtitle = "Post districts EN3, UB2 and N18 has larger proportion of uneducated popuation.",
       fill = "Crime rate per 1000 capita",
       caption = "Post District Data from geolytix.co.uk") +  
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),  
        panel.grid = element_blank()) +
  geom_sf_text(aes(label = post_dist), size = 1, color = "white")

lm_mix_1_outer <- lm(crime_rate_log ~ frac_une_ex_student * frac_ne_nw_12m + frac_no_edu * frac_edu_lv4, data=Outer_Education_Crime_Employment_LSOA)
summary(lm_mix_1_outer)
# it works slightly better.















# Target 5: predict crime rate for each LSOA district and visualize them

# Add predicted crime rate (in logarithmic form)
Outer_Education_Crime_Employment_LSOA <- Outer_Education_Crime_Employment_LSOA |>
  mutate(
    predicted_crime_rate_log = predict(lm_mix_1_outer, newdata = Outer_Education_Crime_Employment_LSOA)
  )

# Convert the predicted value back to the original scale
Outer_Education_Crime_Employment_LSOA <- Outer_Education_Crime_Employment_LSOA |>
  mutate(
    predicted_crime_rate = exp(predicted_crime_rate_log)
  )

# Aggregate predicted crime rates to zip level
Predicted_CrimeRate_PostDist <- Outer_Education_Crime_Employment_LSOA |>
  group_by(post_dist) |>
  summarize(
    total_predicted_crime = sum(predicted_crime_rate * total_resident / 1000, na.rm = TRUE), 
    total_resident = sum(total_resident, na.rm = TRUE)
  ) |>
  mutate(
    predicted_crime_rate = total_predicted_crime / (total_resident / 1000) 
  ) 

str(Predicted_CrimeRate_PostDist) # sf [276 × 5]

ggplot(data = Predicted_CrimeRate_PostDist) +
  geom_sf(aes(fill = predicted_crime_rate), color = "black", size = 0.1) +  
  scale_fill_viridis_c(option = "plasma", direction = -1) +  # Dark colors represent high proportions
  theme_minimal() +
  labs(title = "London Post Districts Crime Map (By prediction)",
       fill = "Predicted Crime Rate \nper 1000 capita",
       caption = "Post District Data from geolytix.co.uk") +  
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),  
        panel.grid = element_blank()) +
  geom_sf_text(aes(label = post_dist), size = 1, color = "white")
# predictions are precise, but the problem is the scale of legends are different.
# thus make it hard to intuitively compare the predictions and real values

# Next, I will use a shared legend to ensure the 
combined_range <- range(
  London_CrimeRate_PostDist$crime_rate,
  Predicted_CrimeRate_PostDist$predicted_crime_rate,
  na.rm = TRUE
)

# Predicted Crime rate map (Legend removed)
predicted_map <- ggplot(data = Predicted_CrimeRate_PostDist) +
  geom_sf(aes(fill = predicted_crime_rate), color = "black", size = 0.1) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    limits = combined_range,  # unify the color range
    name = "Crime Rate \n(per 1000 capita)"
  ) +
  theme_minimal() +
  labs(subtitle = "Predicted Crime Rate Map",
       caption = "Post District Data from geolytix.co.uk") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )

# Actual crime rate map (legend removed)
actual_map <- ggplot(data = London_CrimeRate_PostDist) +
  geom_sf(aes(fill = crime_rate), color = "black", size = 0.1) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    limits = combined_range,
    name = "Crime Rate \n(per 1000 capita)"
  ) +
  theme_minimal() +
  labs(subtitle = "Actual Crime Rate Map") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none" 
  )

legend <- ggplot(data = Predicted_CrimeRate_PostDist) +
  geom_sf(aes(fill = predicted_crime_rate), color = "black", size = 0.1) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    limits = combined_range,
    name = "Crime Rate \n(per 1000 capita)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

final_plot <- (actual_map / predicted_map) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right") 

print(final_plot)
