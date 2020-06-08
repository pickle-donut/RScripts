# Create hierarchical clustering model: hclust.out
hclust.out <- hclust(dist(x))

# Inspect the result
summary(hclust.out)

# Cut by height
cutree(hclust.out, h=7)

# Cut by number of clusters
cutree(hclust.out, k=3)