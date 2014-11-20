new.data = with(mtcars, expand.grid(am = unique(am),mpg = seq(min(mpg), max(mpg))))
new.data$vs <- predict.glm(logodds, newdata = new.data, type = "response")
ggplot(new.data, aes(mpg, vs, colour = am)) + geom_line(aes(group = am))

