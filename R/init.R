#' Initialize Dataframe for given tier
#' 
#' This function initializes the dataframe for a given tier. It pre-processes the data into a standardized form.
#' 
#' @param tier A string of desired tier name. Possible tiers: body_position, eye_gaze, eye_shape (closed), eyebrows, head_move, head_x, head_y, head_z, lip_corners (mouth.action), lips (mouth.action), nose, shoulders
#' @param replace_value A boolean, whether values need to be replaced
#' @param Annotator.a Annotator label A - name of annotator
#' @param Annotator.b Annotator label B - name of annotator
#' @return df: A pre-processed dataframe for the given tier
#' @export
init <- function(tier, replace_value = FALSE, Annotator.a = "C1", Annotator.b = "C2") {
  file <- paste("annotations/", tier, ".csv",sep="")
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  df <- df %>%
    # Set up basic structure
    rename_all(~ gsub("\\.\\.\\.ss.msec", "", .)) %>%
    rename_all(~ gsub("NMM\\..*", "NMM", .)) %>%
    mutate(Annotator = case_when(grepl("MO", File) ~ Annotator.a, # Adjust to own annotator label
                                 grepl("TDR", File) ~ Annotator.b,  # Adjust to own annotator label
                                 TRUE ~ NA_character_)) %>%
    mutate_at('NMM', ~ str_replace(., "-", ".")) %>%
    
    # Can be adjusted according to own needs, or removed
    mutate(NMM = case_when(
      replace_value & tier == "eye_shape" & NMM == "closed" ~ "neutral",
      replace_value & tier %in% c("lips", "lip_corners") & NMM == "mouth.action" ~ "neutral",
      TRUE ~ NMM)) %>%
    mutate_if(is.character, as.factor) %>%
    
    # Round times to 0.05 for whole number of frames (necessary for time-based annotations)
    mutate(Begin.Time = round(Begin.Time / 0.05) * 0.05,
           End.Time = round(End.Time / 0.05) * 0.05) %>% 
    
    # Fill time gaps between annotations, find first and last annotation
    group_by(Question, Annotator) %>%
    arrange(Begin.Time) %>%
    mutate(time_diff = lead(Begin.Time) - End.Time) %>%
    replace(is.na(.), 0) %>%
    mutate(Begin.Time = ifelse(time_diff < 0, Begin.Time + time_diff, Begin.Time),
           End.Time = ifelse(time_diff > 0, End.Time + time_diff, End.Time),
           Duration = End.Time - Begin.Time,
           First.Annotator = ifelse(row_number() == 1, 1, 0),
           Last.Annotator = ifelse(row_number() == n(), 1, 0)) %>%
    ungroup() %>%
    
    # Ensure that the annotations for each question match in begin and end times
    group_by(Question) %>%
    mutate(Begin.Time.Min = min(Begin.Time, na.rm = TRUE),
           End.Time.Max = max(End.Time, na.rm = TRUE),
           Begin.Time = ifelse(((Begin.Time > Begin.Time.Min) & First.Annotator), Begin.Time.Min, Begin.Time),
           End.Time = ifelse(((End.Time < End.Time.Max) & Last.Annotator), End.Time.Max, End.Time),
           Duration = round(End.Time - Begin.Time, 2)) %>%
    ungroup() %>%
    
    # Calculate the frames (using 1-indexing)
    mutate(Begin.Frame = ((Begin.Time - Begin.Time.Min) * 60), 
           End.Frame = Begin.Frame + (Duration * 60), 
           Frames = Duration * 60) %>%
    
    arrange(Annotator, Question) %>%
    mutate(ID = 1:nrow(df)) %>%
    select(Question, ID, Annotator, NMM, Begin.Time, End.Time, Duration, Begin.Frame, End.Frame, Frames)
  
  # In the event-based approach, we remove the neutral annotations, as these are not events
  df <- filter.drop.levels(df, df$NMM != "neutral")
  df$ID <- 1:nrow(df)
  
  return(df)
}