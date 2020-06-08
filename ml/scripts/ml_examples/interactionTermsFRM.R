# Build a recency, frequency, and money (RFM) model
# recency * frequency is an interaction term
rfm_model <- glm(donated ~ money + recency * frequency, data = donors, family = 'binomial')

# Summarize the RFM model to see how the parameters were coded
summary(rfm_model)

# Compute predicted probabilities for the RFM model
rfm_prob <- predict(rfm_model, type = 'response')

# Plot the ROC curve and find AUC for the new model
library(pROC)
ROC <- roc(donors$donated, rfm_prob)
plot(ROC, col = 'red')
auc(ROC)