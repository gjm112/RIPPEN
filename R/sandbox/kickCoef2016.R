kickCoef <- function(nfl){
	# Collect league kicker data
  kicker <- subset(nfl, 
                   play_type=="field_goal", 
                   select= c("field_goal_distance", "field_goal_result"))

  kicker$Good <- (kicker$field_goal_result=="made") + 0 # Converts T/F to numbers

  kicker <- kicker[!is.na(kicker$Good),]

  boot <- glm(Good ~ field_goal_distance, data = kicker, family = "binomial")

  # Coefficients for Field Goals
  kickCoef <- boot$coefficients

  return(kickCoef)
}