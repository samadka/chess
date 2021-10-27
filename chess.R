

library(foreign)

chess <- read.csv(file.choose())

summary(chess)

attach(chess)

chess$victory_status <- as.factor(chess$victory_status)

chess$winner <- as.factor(chess$winner)

plot(victory_status, turns, ylab= "Number of moves", xlab="Victory Status")

plot(winner, ylab= "Number of victories", xlab="Winner")

table(winner)

chess$ratingdiff <- abs(chess$white_rating - chess$black_rating) # variable of rating difference created

library(car)

chess$victory_status2 <- recode(chess$victory_status,'"draw"=1; "resign"=2; "outoftime"=3; "mate"=4' )

chess$victory_status2 <- as.numeric(chess$victory_status2)

reg <- lm(victory_status2 ~ ratingdiff, data = chess)
summary(reg)

classification <- glm(victory_status~ratingdiff, data = chess, family = ordered() )
summary(classification)

## getting the time variable 

chess$time <- substr(chess$increment_code, 1, 2)
chess$time <- sub('[+]', "", chess$time)
chess$time <- as.numeric(chess$time)

attach(chess)

plot(victory_status, time, ylab= "Game Time", xlab="Victory Status")


reg2 <- lm(time ~ ratingdiff*rated)
summary(reg2)

plot(victory_status, ratingdiff, ylab="Rating Difference", xlab="Victory Status")





