# code for defining edges relationship : 
# if total number of symptoms are common between two patients

library(highcharter)
library(igraph)
library(dplyr)

data = read.csv('Cleaned-Data.csv')
data=sample_n(data,50)
data
# getting required data 
df =  data[,c(1:27)]
df

#  -----------  same number of symptoms ---------------
check_number_of_symptoms <- function(left, right){
  # matching_symptoms_count <- length((which(left == right) == TRUE))
  left_symptoms_count <- sum(left)
  right_symptoms_count <- sum(right)
  if (left_symptoms_count == right_symptoms_count){
    print('matched')
    return(1)
  }
  else{
    print('not matched')
    return(0)
  }
}



#  -----------  Data Wrangling ---------------

symptoms_data = df[,c(1:10)]
bio_data = df[,c(12:26)]

bio_data$age_range = NA

bio_data$age_range[which(bio_data$Age_0.9 == 1)] = 'Age_0-9'
bio_data$Age_0.9 = NULL
bio_data$age_range[which(bio_data$Age_10.19 == 1)] = 'Age_10-19'
bio_data$Age_10.19 = NULL
bio_data$age_range[which(bio_data$Age_20.24 == 1)] = 'Age_20-24'
bio_data$Age_20.24 = NULL
bio_data$age_range[which(bio_data$Age_25.59 == 1)] = 'Age_25-59'
bio_data$Age_25.59 = NULL
bio_data$age_range[which(bio_data$Age_60. == 1)] = 'Age_60'
bio_data$Age_60. = NULL

bio_data$gender = NA

bio_data$gender[which(bio_data$Gender_Female == 1)] = 'female'
bio_data$Gender_Female = NULL
bio_data$gender[which(bio_data$Gender_Male == 1)] = 'male'
bio_data$Gender_Male = NULL
bio_data$gender[which(bio_data$Gender_Transgender == 1)] = 'transgender'
bio_data$Gender_Transgender = NULL

bio_data$severity = NA

bio_data$severity[which(bio_data$Severity_Mild==1)] = 'Mild'
bio_data$Severity_Mild = NULL
bio_data$severity[which(bio_data$Severity_Moderate==1)] = 'Moderate'
bio_data$Severity_Moderate = NULL
bio_data$severity[which(bio_data$Severity_None==1)] = 'None'
bio_data$Severity_Mild = NULL
bio_data$severity[which(bio_data$Severity_Severe==1)] = 'Severe'
bio_data$Severity_Mild = NULL

bio_data$contact = NA

bio_data$contact[which(bio_data$Contact_Dont.Know==1)] = 'Dont Know'
bio_data$Contact_Dont.Know = NULL
bio_data$contact[which(bio_data$Contact_No==1)] = 'No'
bio_data$Contact_No = NULL
bio_data$contact[which(bio_data$Contact_Yes==1)] = 'Yes'
bio_data$Contact_Yes = NULL

#  ------------------------

#  -----------  creating matrix for graph ---------------

#  -------  creating graph for same no of symptoms --------
matrix_ = apply(symptoms_data,1, function(x){
  # seq=seq+1
  # browser()
  apply(symptoms_data,1, function(y){
    # browser()
    check_number_of_symptoms(x,y)
  })
})


net = graph_from_adjacency_matrix(adjmatrix = matrix_,  mode='undirected', diag=F )

wc <- cluster_walktrap(net)
wc
N = dim(matrix_)[1]
V(net)$label <- seq(N)
V(net)$name <- paste("Patient #", seq(N))
V(net)$gender <- bio_data$gender
V(net)$age_group <- bio_data$age_range
V(net)$severity <- bio_data$severity
V(net)$contact <- bio_data$contact
V(net)$color <- colorize(membership(wc))
V(net)$size <- rep(c(7),each=N)


hchart(net, layout = layout_with_fr)
edge_density(net, loops = FALSE)
# calculating degree
d = degree(net)
d
# find nodes having maximum & minimum degree

V(net)$name[degree(net)==max(degree(net))]
V(net)$name[degree(net)==min(degree(net))]

# Calculate graph density i.e. the ratio of the number of edges and the number of possible edges.
edge_density(net, loops = FALSE)

# average path length
average.path.length(net)

# extracting ego-network
net.ego.networks = make_ego_graph(graph = net)
ego = net.ego.networks[[1]]


# Nodes forming adjacent triangles
adjacent.triangles(net)

# Average graph transitivity/ clustering coefficient i.e. the probability that the adjacent vertices of a vertex are connected
transitivity(net, type = "local")
transitivity(net, type = "average")

# Eccentricity of each node i.e. the shortest path distance of one node from the farthest other node
eccentricity(net)

# centrality
degree(net, mode="all")
centr_degree(net, mode="all")

# Closeness
closeness(net, mode="all", weights=NA) 

# betweeness
betweenness(net, directed=T, weights=NA)

# Graph diameter
diameter(net, directed = F)

#Edge Density
edge_density(net, loops = FALSE)

#Degree distribution
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