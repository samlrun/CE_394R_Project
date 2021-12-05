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


# DESCRIPTIVE SUMMARY ######################################

#df <- read_csv("data/CAMS_Crashes_14-21.csv") %>%
#  as_tibble() %>%
#  select(c(MANNER_COLLISION_ID,CRASH_SEVERITY_ID)) %>%
#  filter(MANNER_COLLISION_ID != 97 & MANNER_COLLISION_ID != 99 & MANNER_COLLISION_ID != 89)

#datasummary_balance(
#  ~MANNER_COLLISION_ID, 
#  data = df, 
#  dinm = FALSE,
#  title = "Descriptive Statistics of Dataset"
#)

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
    Sev_5 = sum(CRASH_SEVERITY_ID == 5)
  ) %>%
  select(-SEG_ID)

colnames(df) <- c(
  "Angle",
  "Front to Rear",
  "Head on",
  "Sideswipe in the Same Direction",
  "Sideswipe in the Opposite Direction",
  "Collision with Parked Vehicle",
  "Rear to Side",
  "Rear to Rear",
  "Single Vehicle Crash",
  "Severity 1 (Property Damage Only)",
  "Severity 2 (Possible Injury)",
  "Severity 3 (Suspected Minor Injury)",
  "Severity 4 (Suspected Major Injury)",
  "Severity 5 (Fatal Injury)"
)

# Datasummary Correlation Table
#datasummary_correlation(
#  data = df,
#  title = "Correlation Matrix",
#  linesep = "\\addlinespace"
#  ) %>%
#  column_spec(1, bold = T, width = "8em") %>%
#  column_spec(2, width = "10em") %>%
#  column_spec(3, width = "10em") %>%
#  column_spec(4, width = "10em") %>%
#  column_spec(5, width = "10em") %>%
#  column_spec(6, width = "10em") %>%
#  column_spec(7, width = "10em") %>%
#  column_spec(8, width = "10em") %>%
#  column_spec(9, width = "10em") %>%
#  column_spec(10, width = "10em") %>%
#  column_spec(11, width = "10em") %>%
#  column_spec(12, width = "10em") %>%
#  column_spec(13, width = "10em") %>%
#  column_spec(14, width = "10em") %>%
#  kable_styling(
#    latex_options = "scale_down",
#    bootstrap_options = "condensed",
#    full_width = F
#    )

# Correlation matrix for data frame
cortable <- df %>%
  cor() %>%
  round(2)

cortable <- cortable[1:9,10:14] %>%
  as.data.frame()

cortable %>%
  kbl(booktabs = T, caption = "Correlation Matrix", linesep = "\\addlinespace", align = "c") %>%
  column_spec(1, bold = T, width = "8em") %>%
  column_spec(2, width = "8em") %>%
  column_spec(3, width = "8em") %>%
  column_spec(4, width = "8em") %>%
  column_spec(5, width = "8em") %>%
  column_spec(6, width = "8em") %>%
  kable_styling(
    latex_options = c("scale_down","striped"),
    bootstrap_options = "condensed",
    full_width = F
  )

# Correlation matrix for pedestrian variables

df <- read_csv("data/ISAM_Crashes_14-21_extra.csv")

df <- df %>%
  as_tibble() %>%
  group_by(INT_ID) %>%
  summarize(
    PEDESTRIAN_INVOLVED = sum(PEDESTRIAN_INVOLVED),
    BICYCLIST_INVOLVED = sum(BICYCLIST_INVOLVED),
    MOTORCYCLE_INVOLVED = sum(MOTORCYCLE_INVOLVED),
    NUM_SCHOOLS = max(NUM_SCHOOLS),
    SCHOOL_Present = as.logical(max(NUM_SCHOOLS)),
    NUM_UTA = max(NUM_UTA),
    UTA_Present = as.logical(max(NUM_UTA)),
    Sev_1 = sum(Sev_1_Crashes),
    Sev_2 = sum(Sev_2_Crashes),
    Sev_3 = sum(Sev_3_Crashes),
    Sev_4 = sum(Sev_4_Crashes),
    Sev_5 = sum(Sev_5_Crashes),
    'Sev_3-5' = sum(Severe_Crashes),
    'Sev_4-5' = sum(Sev_4_Crashes, Sev_5_Crashes),
    Total = sum(Total_Crashes)
  ) %>%
  select(-INT_ID)

# Correlation matrix for data frame
cortable <- df %>%
  cor(use = "complete.obs") %>%
  round(2)

cortable <- cortable[1:7,8:15] %>%
  as.data.frame()

cortable


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
  