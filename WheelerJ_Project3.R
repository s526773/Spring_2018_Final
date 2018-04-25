# Name: Jacob Wheeler
# Course: 44-149 Scientific Computing
# Assignment # Project 3
# Due Date: 4/20/18
# Brief: Algorithms and Clustering
# By submitting this, I pledge that the code in this file was written by the author indicated above,
#  and that all assistance from outside sources was correctly attributed in comments.  Additionally, I 
#  agree to abide by the rules expressed in the CSIS Academic Honesty Policy

#With updated optimization to speed it up as done in class

N <- 12
ITERS <- 5

census <- read.csv('us_census.csv')
for (i in 1:ITERS){
#The next line of code will exclude Alaska, Hawaii, and Puerto Rico
contiguous <- census[!census$state %in% c('AK', 'HI', 'PR'),]
#Optimaization to Speed It up as did in class
latitudes <- contiguous$latitude
longitudes <- contiguous$longitude
#plot(contiguous$longitude, contiguous$latitude, type = 'p', col=contiguous$state)

chosen_counties <- sample(1:nrow(contiguous), N)

#print(chosen_counties)

centers <- matrix(0, nrow = N, ncol = 2)
# for (i in 1:N){
#   centers[i][1] = contiguous[chosen_counties[i], 'latitude']
#   centers[i][2] = contiguous[chosen_counties[i], 'longitude']
# }

centers[,1] = contiguous$latitude[chosen_counties]
centers[,2] = contiguous$longitude[chosen_counties]

#print(centers)

#Data Frame
centers_df=contiguous[chosen_counties, 3:4]
#print(contiguous[1,])
#print(centers[1, ])

dist_sq <- function(county, center){
  deltax <- latitudes[county] - center[1]
  deltay <- longitudes[county] - center[2]
  deltax ^ 2 + deltay ^ 2
}

#deltax <- contiguous[1, 'latitude'] - centers[1,1]
#deltay <- contiguous[1, 'longitude'] - centers[1,2]
#Distance - Don't Need to sqrt
#print(dist_sq(contiguous[1, ], centers[1, ]))

#Belongs_to[i] means the ith county belongs_to the cluster at belons_to[i]
belongs_to <- rep(0, nrow(contiguous))


#Figure out closest cluster
for (county in 1:nrow(contiguous)){
  closest_center <- 1
  closest_distance <- dist_sq(county, centers[1, ])

  for (cluster in 2:N){
    d <- dist_sq(county, centers[cluster,])
    if (d < closest_distance){
      closest_distance <- d
      closest_center <- cluster
    }
  }
  belongs_to[county] <- closest_center
}
#print(belongs_to)
plot(contiguous$longitude, contiguous$latitude, type = 'p', col = belongs_to)
#belongs_to == 1
for (i in 1:ITERS){
  cluster_of_interest <- contiguous[belongs_to == 1, ]
  total_population <- sum(cluster_of_interest$population)
  print(sum(cluster_of_interest$population))
  new_latitude <- sum(cluster_of_interest$latitude * cluster_of_interest$population)/total_population
  new_longitude <- sum(cluster_of_interest$longitude * cluster_of_interest$population)/total_population
  centers[1,1] <- new_latitude
  centers[1,2] <- new_longitude
#print(nrow(cluster_of_interest))
#print(sum(belongs_to == 1))
}
}


