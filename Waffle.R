library(waffle)

parts <- c(80, 30, 20, 10)
waffle(parts, rows=8)

# slightly more complex example
parts <- c(`Un-breached\nUS Population`=(318-11-79), `Premera`=11, `Anthem`=79)

# One square = 1 million people
waffle(parts, rows=8, size=1, colors=c("#969696", "#1879bf", "#009bda"))

# one square = 10 million people
waffle(parts/10, rows=3, colors=c("#969696", "#1879bf", "#009bda")) 


# Iron example (left-align & padding for multiple plots)
pain.adult.1997 <- c( `YOY (406)`=406, `Adult (24)`=24)

A <- waffle(pain.adult.1997/2, rows=7, size=0.5, 
            colors=c("#c7d4b6", "#a3aabd"), 
            title="Paine Run Brook Trout Abundance (1997)", 
            xlab="1 square = 2 fish", pad=3)

pine.adult.1997 <- c( `YOY (221)`=221, `Adult (143)`=143)
B <- waffle(pine.adult.1997/2, rows=7, size=0.5, 
            colors=c("#c7d4b6", "#a3aabd"), 
            title="Piney River Brook Trout Abundance (1997)", 
            xlab="1 square = 2 fish", pad=8)

stan.adult.1997 <- c( `YOY (270)`=270, `Adult (197)`=197)
C <- waffle(stan.adult.1997/2, rows=7, size=0.5, 
            colors=c("#c7d4b6", "#a3aabd"), 
            title="Staunton River Trout Abundance (1997)", 
            xlab="1 square = 2 fish")

iron(A, B, C)
