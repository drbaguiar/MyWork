if (!require("epicalc")){
        install.packages("epicalc")
}
## Load in reproducible data
data(DHF99)

## Create model
model.poisson <- glm(containers ~ education + viltype, 
                     family=poisson, data=DHF99)

## Standard summary
summary(model.poisson)

## IRRs and IRR confidence levels by simple exponentiation
exp(coef(model.poisson))
exp(confint(model.poisson))

## IRRs by epicalc's summary function
idr.display(model.poisson)
