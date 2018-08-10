


data <- data.frame(
  last_name = members$last_name[1:10],
  first_name = members$first_name[1:10], 
  common_name = members$common_name[1:10],
  chamber = members$chamber[1:10],
  congress = 115
)

data$first_name[1:8] <- members$common_name[1:8]
data$chamber[4:6] <- NA

data2 <- data 
