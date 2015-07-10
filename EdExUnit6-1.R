# Load functions
source('functions.R')

# Load the data into R
movies = read.table("D:/Data/u.txt", header=FALSE, sep="|",quote="\"")
str(movies)

# Add column names
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

# Remove unnecessary variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

# Remove duplicates
movies = unique(movies)

# Take a look at our data again:
str(movies)

# Compute distances
distances = dist(movies[2:20], method = "euclidean")

# Hierarchical clustering
clusterMovies = hclust(distances, method = "ward") 

# Plot the dendrogram
plot(clusterMovies)

# Assign points to clusters
clusterGroups = cutree(clusterMovies, k = 10)

# Use the tapply function to compute the percentage of movies in each genre and cluster
tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)
tapply(movies$Adventure, clusterGroups, mean)
tapply(movies$Animation, clusterGroups, mean)
tapply(movies$Childrens, clusterGroups, mean)
tapply(movies$Comedy, clusterGroups, mean)
tapply(movies$Crime, clusterGroups, mean)
tapply(movies$Documentary, clusterGroups, mean)
tapply(movies$Drama, clusterGroups, mean)
tapply(movies$Fantasy, clusterGroups, mean)
tapply(movies$FilmNoir, clusterGroups, mean)
tapply(movies$Horror, clusterGroups, mean)
tapply(movies$Musical, clusterGroups, mean)
tapply(movies$Mystery, clusterGroups, mean)
tapply(movies$SciFi, clusterGroups, mean)
tapply(movies$Thriller, clusterGroups, mean)
tapply(movies$War, clusterGroups, mean)
tapply(movies$Western, clusterGroups, mean)

# Find which cluster Men in Black is in.
subset(movies, Title=="Men in Black (1997)")
clusterGroups[257]

# Create a new data set with just the movies from cluster 2
cluster2 = subset(movies, clusterGroups==2)

# Look at the first 10 titles in this cluster:
cluster2$Title[1:10]

# Assign points to clusters
twoGroups = cutree(clusterMovies, k = 2)

# Use the tapply function to compute the percentage of movies in each genre and cluster
tapply(movies$Action, twoGroups, mean)
tapply(movies$Romance, twoGroups, mean)
tapply(movies$Adventure, twoGroups, mean)
tapply(movies$Animation, twoGroups, mean)
tapply(movies$Childrens, twoGroups, mean)
tapply(movies$Comedy, twoGroups, mean)
tapply(movies$Crime, twoGroups, mean)
tapply(movies$Documentary, twoGroups, mean)
tapply(movies$Drama, twoGroups, mean)
tapply(movies$Fantasy, twoGroups, mean)
tapply(movies$FilmNoir, twoGroups, mean)
tapply(movies$Horror, twoGroups, mean)
tapply(movies$Musical, twoGroups, mean)
tapply(movies$Mystery, twoGroups, mean)
tapply(movies$SciFi, twoGroups, mean)
tapply(movies$Thriller, twoGroups, mean)
tapply(movies$War, twoGroups, mean)
tapply(movies$Western, twoGroups, mean)

