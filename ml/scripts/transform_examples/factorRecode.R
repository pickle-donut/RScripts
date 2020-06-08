# Convert the wealth rating to a factor
donors$wealth_rating <- factor(donors$wealth_rating, levels = c(0,1,2,3), labels = c('Unknown','Low','Medium','High'))

# Use relevel() to change reference category (i.e.)
donors$wealth_rating <- relevel(donors$wealth_rating, ref = 'Medium')

# See how our factor coding impacts the model
summary(glm(donated ~ wealth_rating, data = donors, family = 'binomial'))