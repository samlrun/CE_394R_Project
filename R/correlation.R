library(tidyverse)
library(pacman)

# First Attempt - ignore #######
df <- read_csv("R/Crash_Data_14-20.csv") %>%
  as_tibble() %>%
  select(c(crash_severity_id,manner_collision_id)) %>%
  group_by(crash_severity_id) %>%
  summarize(
    Angle = sum(manner_collision_id == 1),
    Front_to_Rear = sum(manner_collision_id == 2),
    Head_On = sum(manner_collision_id == 3),
    Sideswipe_Same_Dir = sum(manner_collision_id == 4),
    Sideswipe_Opp_Dir = sum(manner_collision_id == 5),
    Parked_Veh = sum(manner_collision_id == 6),
    Rear_to_Side = sum(manner_collision_id == 7),
    Rear_to_Rear = sum(manner_collision_id == 8),
    Single_Veh = sum(manner_collision_id == 96)
  ) %>%
  print()

df <- read_csv("R/Crash_Data_14-20.csv") %>%
  as_tibble() %>%
  select(c(manner_collision_id,crash_severity_id)) %>%
  filter(manner_collision_id != 97 & manner_collision_id != 99 & manner_collision_id != 89) %>%
  table() %>%
  print()

write.csv(df,"R/sev_correlation.csv",row.names=FALSE)

# Plot the table ########

# Side-by-side bar
df %>%
  barplot(
    legend = rownames(.),  # Dot is placeholder for pipe
    beside = T  # Put bars next to each other
  )



# Improved Code
# CORRELATION MATRIX #######################################

df <- read_csv("R/CAMS_Crashes_14-18.csv") %>%
  as_tibble() %>%
  select(c(SEG_ID,MANNER_COLLISION_ID,CRASH_SEVERITY_ID)) %>%
  filter(MANNER_COLLISION_ID != 97 & MANNER_COLLISION_ID != 99 & MANNER_COLLISION_ID != 89) %>%
  group_by(SEG_ID) %>%
  summarize(
    Angle = sum(MANNER_COLLISION_ID == 1),
    Front_to_Rear = sum(MANNER_COLLISION_ID == 2),
    Head_On = sum(MANNER_COLLISION_ID == 3),
    Sideswipe_Same_Dir = sum(MANNER_COLLISION_ID == 4),
    Sideswipe_Opp_Dir = sum(MANNER_COLLISION_ID == 5),
    Parked_Veh = sum(MANNER_COLLISION_ID == 6),
    Rear_to_Side = sum(MANNER_COLLISION_ID == 7),
    Rear_to_Rear = sum(MANNER_COLLISION_ID == 8),
    Single_Veh = sum(MANNER_COLLISION_ID == 96),
    Sev_1 = sum(CRASH_SEVERITY_ID == 1),
    Sev_2 = sum(CRASH_SEVERITY_ID == 2),
    Sev_3 = sum(CRASH_SEVERITY_ID == 3),
    Sev_4 = sum(CRASH_SEVERITY_ID == 4),
    Sev_5 = sum(CRASH_SEVERITY_ID == 5),
  ) %>%
  print()

cbind(rowSums(df[]))
df %>%
  cbind()

# Correlation matrix for data frame
df %>%
  cor() %>%
  round(2)

# SINGLE CORRELATION #######################################

# Can test one pair of variables at a time
# Gives r, hypothesis test, and confidence interval
cor.test(df$Sev_5+df$Sev_4+df$Sev_3, df$Angle)

# P-VALUES FOR MATRIX ######################################

# Install "Hmisc" package to get p-values for matrix
p_load(Hmisc)

# Need to coerce from dataframe to matrix to get both a
# correlation matrix and p-values (in separate tables)
df %>%
  as.matrix() %>%
  rcorr()

# Visualize ########################################

# create scatter plots
df %>%
  plot()  # Plot all associations

df %>%
  select(c(Head_On, Sev_3)) %>% 
  plot()
