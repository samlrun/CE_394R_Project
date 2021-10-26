# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(corrplot, magrittr, pacman, rio, tidyverse, kableExtra)
# corrplot: for visualizing correlation matrices
# magrittr: for pipes
# pacman: for loading/unloading packages
# rio: for importing data
# tidyverse: for so many reasons
# kableExtra: build nice tables

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

colnames(cortable) <- c(
  "Angle",
  "Front to Rear",
  "Head on",
  "Sideswipe in the Same Direction",
  "Sideswipe in the Opposite Direction",
  "Collision with Parked Vehicle",
  "Rear to Side",
  "Rear to Rear",
  "Single Vehicle Crash"
)

rownames(cortable) <- c(
  "Severity 1 (Property Damage Only)",
  "Severity 2 (Possible Injury)",
  "Severity 3 (Suspected Minor Injury)",
  "Severity 4 (Suspected Major Injury)",
  "Severity 5 (Fatal Injury)",
  "Severities 3-5 (Injury Crashes)",
  "Severities 4-5 (Severe Injury Crashes)"
)

cortable %>%
  kbl(booktabs = T, caption = "Correlation Matrix") %>%
  column_spec(1, bold = T)
  