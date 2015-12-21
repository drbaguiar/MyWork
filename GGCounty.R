library(devtools)
library(ggcounty)

maine <- ggcounty("Maine")
maine$gg


# built-in US population by FIPS code data set
data(population)

# define appropriate (& nicely labeled) population breaks
population$brk <- cut(population$count, 
                      breaks=c(0, 100, 1000, 10000, 100000, 1000000, 10000000), 
                      labels=c("0-99", "100-1K", "1K-10K", "10K-100K", 
                               "100K-1M", "1M-10M"),
                      include.lowest=TRUE)

# get the US counties map (lower 48)
us <- ggcounty.us()

# start the plot with our base map
gg <- us$g

# add a new geom with our population (choropleth)
gg <- gg + geom_map(data=population, map=us$map,aes(map_id=FIPS, fill=brk),color="white", size=0.125)

# define nice colors
gg <- gg + scale_fill_manual(values=c("#ffffcc", "#c7e9b4", "#7fcdbb", 
                                      "#41b6c4", "#2c7fb8", "#253494"), 
                             name="Population")

# plot the map
gg

ri <- ggcounty("Rhode Island", fill="#253494", color="white")
ny <- ggcounty("New York", fill="#c7e9b4", color="white")
nj <- ggcounty("New Jersey", fill="#41b6c4", color="white")
pa <- ggcounty("Pennsylvania", fill="#253494", color="white")
ny$gg + nj$geom + pa$geom + ri$geom

ri$gg
