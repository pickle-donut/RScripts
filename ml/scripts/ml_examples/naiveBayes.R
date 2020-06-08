# The 'naivebayes' package is loaded into the workspace already
# The Naive Bayes location model (locmodel) has already been built

# Observe the predicted probabilities for a weekend afternoon
predict(locmodel, weekend_afternoon, type = 'prob')

# Build a new model using the Laplace correction
# Allows for the samall chance that something not accounted for in the data might happen in the future (i.e. joint probabilities are equal to zero)
locmodel2 <- naive_bayes(location ~ daytype + hourtype, data = locations, laplace = 1)

# Observe the new predicted probabilities for a weekend afternoon
predict(locmodel2, weekend_afternoon, type = 'prob')