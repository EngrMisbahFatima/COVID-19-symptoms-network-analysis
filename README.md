# COVID-19-symptoms-analysis
Graph Network analysis on COVID-19 symptoms analysis - Social media analysis project
Kaggle Dataset link: https://www.kaggle.com/iamhungundji/covid19-symptoms-checker

How to run:
Run Code1.R file to check relationship between certain number of symptoms common among two patients
Run Code2.R file to check relationship between total number of symptoms common among two patients
Run Code3.R file to check relationship between two patients who have exactly same symptoms
To view network degree distribution plot and Histogram plot, please run last two commands of code one by one.
The last two commands of code are following:

# Degree distribution
degree(
  net,
  v = V(net),
  mode = c("all"),
  loops = TRUE,
  normalized = FALSE
)
plot(degree_distribution(net, cumulative = TRUE,), pch=19, cex=1.2, 
     col="orange", xlab="Degree", ylab="Cumulative Frequency")

# Histogram
hist(d, breaks=1:vcount(net)-1, main="Histogram of node degree")
