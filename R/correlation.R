# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(corrplot, magrittr, pacman, rio, tidyverse)
# corrplot: for visualizing correlation matrices
# magrittr: for pipes
# pacman: for loading/unloading packages
# rio: for importing data
# tidyverse: for so many reasons

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

# Plot the table ########

# Side-by-side bar
df %>%
  barplot(
    legend = rownames(.),  # Dot is placeholder for pipe
    beside = T  # Put bars next to each other
  )



# Improved Code
# CORRELATION MATRIX #######################################

df <- read_csv("data/CAMS_Crashes_14-21.csv") %>%
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
    'Sev_3-5' = sum(CRASH_SEVERITY_ID == 3, CRASH_SEVERITY_ID == 4, CRASH_SEVERITY_ID == 5),
    'Sev_4-5' = sum(CRASH_SEVERITY_ID == 4, CRASH_SEVERITY_ID == 5),
  ) %>%
  print()

# Correlation matrix for data frame
cortable <- df %>%
  cor() %>%
  round(2) %>%
  print()

cortable <- cortable[11:17,2:10] %>%
  as.data.frame()

# Visualize correlation matrix with corrplot() from
# corrplot package
df %>%
  cor() %>%
  corrplot(
    type   = "upper",     # Matrix: full, upper, or lower
    diag   = F,           # Remove diagonal
    order  = "original",  # Order for labels
    tl.col = "black",     # Font color
    tl.srt = 45           # Label angle
  )

# I just wrote to csv and edited in excel
write.csv(df,"data/sev_correlation.csv",row.names=FALSE)

# SINGLE CORRELATION #######################################

# Can test one pair of variables at a time
# Gives r, hypothesis test, and confidence interval
cor.test(df$Severe, df$Angle)

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
  select(c(Angle, `Sev_3-5`)) %>% 
  plot(
    main = "Correlation Between Angle Crashes and Severe Crashes at Intersections",
    xlab = "Angle Crashes",
    ylab = "Severity 3-5 Crashes",
    col = "gray",
    col.axis = "black",
    pch = 20
  )

df %>%
  lm(`Sev_3-5` ~ Angle, data = .) %>%
  abline()



df %>%
  select(c(Front_to_Rear, `Sev_3-5`)) %>%
  plot(
    main = "Correlation Between Front to Rear Crashes and Severe Crashes at Intersections",
    xlab = "Front to Rear Crashes",
    ylab = "Severity 3-5 Crashes",
    col = "gray",
    col.axis = "black",
    pch = 20
  )

df %>%
  lm(`Sev_3-5` ~ Front_to_Rear, data = .) %>%
  abline()



df %>%
  select(c(Head_On, `Sev_3-5`)) %>% 
  plot(
    main = "Correlation Between Head On Crashes and Severe Crashes at Intersections",
    xlab = "Head On Crashes",
    ylab = "Severity 3-5 Crashes",
    col = "gray",
    col.axis = "black",
    pch = 20
  )

df %>%
  lm(`Sev_3-5` ~ Head_On, data = .) %>%
  abline()



df %>%
  select(c(Sideswipe_Same_Dir, `Sev_3-5`)) %>% 
  plot(
    main = "Correlation Between Sideswipe (Same Direction) Crashes and Severe Crashes at Intersections",
    xlab = "Sideswipe (Same Direction) Crashes",
    ylab = "Severity 3-5 Crashes",
    col = "gray",
    col.axis = "black",
    pch = 20
  )

df %>%
  lm(`Sev_3-5` ~ Sideswipe_Same_Dir, data = .) %>%
  abline()



df %>%
  select(c(Sideswipe_Opp_Dir, `Sev_3-5`)) %>% 
  plot(
    main = "Correlation Between Sideswipe (Opposite Direction) Crashes and Severe Crashes at Intersections",
    xlab = "Sideswipe (Opposite Direction) Crashes",
    ylab = "Severity 3-5 Crashes",
    col = "gray",
    col.axis = "black",
    pch = 20
  )

df %>%
  lm(`Sev_3-5` ~ Sideswipe_Opp_Dir, data = .) %>%
  abline()



df %>%
  select(c(Parked_Veh, `Sev_3-5`)) %>% 
  plot(
    main = "Correlation Between Parked Vehicle Crashes and Severe Crashes at Intersections",
    xlab = "Parked Vehicle Crashes Crashes",
    ylab = "Severity 3-5 Crashes",
    col = "gray",
    col.axis = "black",
    pch = 20
  )

df %>%
  lm(`Sev_3-5` ~ Parked_Veh, data = .) %>%
  abline()



df %>%
  select(c(Rear_to_Side, `Sev_3-5`)) %>% 
  plot(
    main = "Correlation Between Rear to Side Crashes and Severe Crashes at Intersections",
    xlab = "Rear to Side Crashes",
    ylab = "Severity 3-5 Crashes",
    col = "gray",
    col.axis = "black",
    pch = 20
  )

df %>%
  lm(`Sev_3-5` ~ Rear_to_Side, data = .) %>%
  abline()



df %>%
  select(c(Rear_to_Rear, `Sev_3-5`)) %>% 
  plot(
    main = "Correlation Between Rear to Rear Crashes and Severe Crashes at Intersections",
    xlab = "Rear to Rear Crashes",
    ylab = "Severity 3-5 Crashes",
    col = "gray",
    col.axis = "black",
    pch = 20
  )

df %>%
  lm(`Sev_3-5` ~ Rear_to_Rear, data = .) %>%
  abline()



df %>%
  select(c(Single_Veh, `Sev_3-5`)) %>% 
  plot(
    main = "Correlation Between Single Vehicle Crashes and Severe Crashes at Intersections",
    xlab = "Single Vehicle Crashes",
    ylab = "Severity 3-5 Crashes",
    col = "gray",
    col.axis = "black",
    pch = 20
  )

df %>%
  lm(`Sev_3-5` ~ Single_Veh, data = .) %>%
  abline()



# CLEAN UP #################################################

# Clear data
rm(list = ls())  # Removes all objects from the environment

# Clear packages
detach("package:datasets", unload = T)  # For base packages
p_unload(all)    # Remove all contributed packages

# Clear plots
graphics.off()   # Clears plots, closes all graphics devices

# Clear console
cat("\014")      # Mimics ctrl+L

# Clear R
#   You may want to use Session > Restart R, as well, which 
#   resets changed options, relative paths, dependencies, 
#   and so on to let you start with a clean slate

# Clear mind :)