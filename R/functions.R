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


# create correlation table
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
      Single_Veh = sum(MANNER_COLLISION_ID == 96),
      Sev_1 = sum(CRASH_SEVERITY_ID == 1),
      Sev_2 = sum(CRASH_SEVERITY_ID == 2),
      Sev_3 = sum(CRASH_SEVERITY_ID == 3),
      Sev_4 = sum(CRASH_SEVERITY_ID == 4),
      Sev_5 = sum(CRASH_SEVERITY_ID == 5),
      'Sev_3-5' = sum(CRASH_SEVERITY_ID == 3, CRASH_SEVERITY_ID == 4, CRASH_SEVERITY_ID == 5),
      'Sev_4-5' = sum(CRASH_SEVERITY_ID == 4, CRASH_SEVERITY_ID == 5)
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
    "Single Vehicle Crash",
    "Severity 1 (Property Damage Only)",
    "Severity 2 (Possible Injury)",
    "Severity 3 (Suspected Minor Injury)",
    "Severity 4 (Suspected Major Injury)",
    "Severity 5 (Fatal Injury)",
    "Severities 3-5 (Injury)",
    "Severities 4-5 (Severe Injury)"
  )
  
  # Correlation matrix for data frame
  cortable <- df %>%
    cor() %>%
    round(2)
  
  cortable <- cortable[1:9,10:16] %>%
    as.data.frame()
  
  cortable
}

# Generate Kable table from correlation table
makeCorrTable <- function(cortable, title) {
  cortable %>%
    kbl(booktabs = T, caption = title, linesep = "\\addlinespace", align = "c") %>%
    column_spec(1, bold = T, width = "8em") %>%
    column_spec(2, width = "8em") %>%
    column_spec(3, width = "8em") %>%
    column_spec(4, width = "8em") %>%
    column_spec(5, width = "8em") %>%
    column_spec(6, width = "8em") %>%
    column_spec(7, width = "8em") %>%
    column_spec(8, width = "8em") %>%
    kable_styling(
      latex_options = c("scale_down","striped"),
      bootstrap_options = "condensed",
      full_width = F
    )
}