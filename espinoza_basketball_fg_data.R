# Load relevant libraries----
## Uncomment in case the pacman package is not installed
## install.packages("pacman")

pacman::p_load(tidyverse, grid, jpeg, RCurl, janitor)

# Read in the dataset----
shots_raw <- read_csv("./shots_data.csv")


# Explore the data----
glimpse(shots_raw)

## note that the `fgmade` column is being treated as numeric
shots_raw %>% 
  ggplot()+
  geom_point(aes(x = x, y = y, color = team))

# Clean the dataset----
## Not much cleaning needed...made the fgmade a factor----
shots_clean <- shots_raw %>% 
  mutate(fgmade = factor(fgmade, levels = c(0,1), 
                         labels = c("Missed", "Made")))

## summarize total shot counts by team----
shots_count <- shots_clean %>% 
  group_by(team) %>% 
  summarize(count = n())


# Create the image for the ggplot----
court_img <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"

court <- grid::rasterGrob(readJPEG(getURLContent(court_img)),
                    width = unit(1, "npc"), height = unit(1, "npc"))

## This ended up not being as clean as I'd hoped. I may be off with my coordinates

# Calculate distance from hoop to each x,y coordinate----
## Also classify each shot based on PDF criteria
shots_stats <- shots_clean %>% 
  mutate(distance_from_hoop = sqrt(((x - 0)^2) + ((y - 0)^2)),
         fg_zone = case_when(x > 22 & y <= 7.8 | x < -22 & y <= 7.8 ~ "C3", 
                             distance_from_hoop > 23.75 & y > 7.8 ~ "NC3", 
                             TRUE ~ "2PT"))

# Plot to estimate if classification method is accurate----
## The filters for each shot category seem to be accurate
shots_stats %>% 
  filter(fg_zone == "C3") %>% 
  arrange(desc(distance_from_hoop)) %>% 
  ggplot()+
  theme_minimal()+
  annotation_custom(court, xmin = -25, xmax = 25, 
                    ymin = -3, ymax = 40)+
  scale_y_continuous(limits = c(-3, 30))+
  scale_x_continuous(limits = c(-25, 25))+
  geom_point(aes(x = x, y = y, color = team), alpha = .6)


# Calculate FG  stats ----
# Summarize the data per shot classification----
shots_summarized <- shots_stats %>% 
  group_by(team, fgmade, fg_zone) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = fgmade, values_from = n) %>% 
  mutate(fg_att = rowSums(across(where(is.numeric))), 
         fg_perc = round(Made /fg_att, digits = 2))

## Calculate total 3 pt shots----
made_threes <- shots_summarized %>% 
  filter(fg_zone != "2PT") %>% 
  group_by(team) %>% 
  summarize(total_3pt = sum(Made)) %>% 
  ungroup()

# Shot distribution per zone----
shots_distribution <- shots_summarized %>% 
  left_join(shots_count, by = "team") %>% 
  group_by(team, fg_zone) %>% 
  summarize(perc_shot_dist = round(fg_att / count, digits = 3))


## Calculate effective FGs----
effective_fgs_total <- shots_stats %>% 
  group_by(team, fgmade) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = fgmade, values_from = n) %>% 
  left_join(made_threes, by = "team") %>% 
  mutate(fg_att = rowSums(across(where(is.numeric))), 
         eff_fg_perc = (Made + (.5 * total_3pt)) / fg_att)

# Effective FG percentage per zone----
effective_fgs_zone <- shots_stats %>% 
  group_by(team, fgmade, fg_zone) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = fgmade, values_from = n) %>% 
  mutate(fg_att = rowSums(across(where(is.numeric))), 
         fg_perc = round(Made /fg_att, digits = 2),
         three_in_zone = ifelse(fg_zone == "2PT", 0, Made), 
         eff_fg_pct = (Made + (.5 * three_in_zone)) / fg_att) %>% 
  select(-three_in_zone)


# Relevant deliverables----
shots_distribution

effective_fgs_zone
