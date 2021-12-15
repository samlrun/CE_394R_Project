# This is where you write functions that can be called from 
# _targets
# 
# 

#' Summarize a mean value
#' 
#' @param dataset A data frame or tibble with an `x` variable.
summ <- function(dataset) {
  summarize(dataset, mean_x = mean(x))
}


# create correlation table for Manner of Collision
corrManner <- function(df, ID_Name) {
  
  ID_Name <- enquo(ID_Name)
  
  df <- df %>%
    as_tibble() %>%
    select(c(!!ID_Name,MANNER_COLLISION_ID,CRASH_SEVERITY_ID)) %>%
    filter(MANNER_COLLISION_ID != 97 & MANNER_COLLISION_ID != 99 & MANNER_COLLISION_ID != 89) %>%
    group_by(!!ID_Name) %>%
    summarize(
      Angle = sum(MANNER_COLLISION_ID == 1),
      Front_to_Rear = sum(MANNER_COLLISION_ID == 2),
      Head_On = sum(MANNER_COLLISION_ID == 3),
      Sideswipe_Same_Dir = sum(MANNER_COLLISION_ID == 4),
      Sideswipe_Opp_Dir = sum(MANNER_COLLISION_ID == 5),
      Parked_Veh = sum(MANNER_COLLISION_ID == 6),
      Rear_to_Side = sum(MANNER_COLLISION_ID == 7),
      Rear_to_Rear = sum(MANNER_COLLISION_ID == 8),
      #Single_Veh = sum(MANNER_COLLISION_ID == 96),
      Sev_5 = sum(CRASH_SEVERITY_ID == 5),
      'Sev_3-5' = sum(CRASH_SEVERITY_ID == 3, CRASH_SEVERITY_ID == 4, CRASH_SEVERITY_ID == 5),
      Total = sum(CRASH_SEVERITY_ID == 1, CRASH_SEVERITY_ID == 2, CRASH_SEVERITY_ID == 3, CRASH_SEVERITY_ID == 4, CRASH_SEVERITY_ID == 5)
    ) %>%
    select(-!!ID_Name)
  
  colnames(df) <- c(
    "Angle",
    "Front to Rear",
    "Head on",
    "Sideswipe in the Same Direction",
    "Sideswipe in the Opposite Direction",
    "Collision with Parked Vehicle",
    "Rear to Side",
    "Rear to Rear",
    #"Single Vehicle Crash",
    "Severity 5 (Fatal)",
    "Severity 3-5 (Injury)",
    "Total Crashes"
  )
  
  # Correlation matrix for data frame
  cortable <- df %>%
    cor() %>%
    round(2)
  
  cortable <- cortable[1:8,9:11] %>%
    as.data.frame()
  
  cortable
}

# create correlation table for pedestrian factors
corrPed <- function(df, ID_Name) {
  
  ID_Name <- enquo(ID_Name)
  
  df <- df %>%
    as_tibble() %>%
    group_by(!!ID_Name) %>%
    summarize(
      PEDESTRIAN_INVOLVED = sum(PEDESTRIAN_INVOLVED),
      BICYCLIST_INVOLVED = sum(BICYCLIST_INVOLVED),
      MOTORCYCLE_INVOLVED = sum(MOTORCYCLE_INVOLVED),
      NUM_SCHOOLS = max(NUM_SCHOOLS),
      SCHOOL_Present = as.logical(max(NUM_SCHOOLS)),
      NUM_UTA = max(NUM_UTA),
      UTA_Present = as.logical(max(NUM_UTA)),
      Sev_5 = sum(Sev_5_Crashes),
      'Sev_3-5' = sum(Severe_Crashes),
      'Sev_4-5' = sum(Sev_4_Crashes, Sev_5_Crashes),
      Total = sum(Total_Crashes)
    ) %>%
    select(-!!ID_Name)
  
  colnames(df) <- c(
    "Pedestrian Involved",
    "Pedacycle Involved",
    "Motorcycle Involved",
    "Number of Schools Within 1000 Feet",
    "Presence of Schools Within 1000 Feet",
    "Number of Transit Stops Within 1000 Feet",
    "Presence of Transit Stops Within 1000 Feet",
    "Severity 5 (Fatal)",
    "Severity 3-5 (Injury)",
    "Total Crashes"
  )
  
  # Correlation matrix for data frame
  cortable <- df %>%
    cor(use = "complete.obs") %>%
    round(2)
  
  cortable <- cortable[1:7,8:10] %>%
    as.data.frame()
  
  cortable
}

# create correlation table for pedestrian factors for CAMS
corrPedCAMS <- function(df, ID_Name) {
  
  ID_Name <- enquo(ID_Name)
  
  df <- df %>%
    as_tibble() %>%
    group_by(!!ID_Name) %>%
    summarize(
      PEDESTRIAN_INVOLVED = sum(as.logical(PEDESTRIAN_INVOLVED)),
      BICYCLIST_INVOLVED = sum(as.logical(BICYCLIST_INVOLVED)),
      MOTORCYCLE_INVOLVED = sum(as.logical(MOTORCYCLE_INVOLVED)),
      NUM_SCHOOLS = mean(NUM_SCHOOLS),
      SCHOOL_Present = as.logical(max(NUM_SCHOOLS)),
      NUM_UTA = mean(NUM_UTA),
      UTA_Present = as.logical(max(NUM_UTA)),
      Sev_5 = sum(CRASH_SEVERITY_ID == 5),
      'Sev_3-5' = sum(CRASH_SEVERITY_ID == 3, CRASH_SEVERITY_ID == 4, CRASH_SEVERITY_ID == 5),
      Total = sum(CRASH_SEVERITY_ID == 1, CRASH_SEVERITY_ID == 2, CRASH_SEVERITY_ID == 3, CRASH_SEVERITY_ID == 4, CRASH_SEVERITY_ID == 5)
    ) %>%
    select(-!!ID_Name)
  
  colnames(df) <- c(
    "Pedestrian Involved",
    "Pedacycle Involved",
    "Motorcycle Involved",
    "Number of Schools Within 1000 Feet",
    "Presence of Schools Within 1000 Feet",
    "Number of Transit Stops Within 1000 Feet",
    "Presence of Transit Stops Within 1000 Feet",
    "Severity 5 (Fatal)",
    "Severity 3-5 (Injury)",
    "Total Crashes"
  )
  
  # Correlation matrix for data frame
  cortable <- df %>%
    cor() %>%
    round(2)
  
  cortable <- cortable[1:7,8:10] %>%
    as.data.frame()
  
  cortable
}

# Generate Kable table from correlation table
makeTable <- function(cortable, cortable2, title) {
  cortable <- cbind(cortable, cortable2)
  
  cortable %>%
    kbl(booktabs = T, caption = title, linesep = "\\addlinespace", align = "c") %>%
    column_spec(1, bold = T) %>%
    add_header_above(c(" " = 1, "Segments" = 3, "Intersections" = 3)) %>%
    kable_styling(
      latex_options = c("scale_down","striped"),
      bootstrap_options = "condensed",
      full_width = FALSE,
      font_size = 10
    )
}
