my_function <- function(x){
  sum <- 0
  for(i in 1:x)
    sum <- x*5+2-10
  return(sum)
}
my_function(10)


boston <- read.csv( "https://people.bu.edu/kalathur/datasets/bostonCityEarnings.csv", colClasses = c("character", "character", "character", "integer", "character"))

head(boston)

(boston_dep <- as.data.frame(boston$Department))

srswor <- srswor(50, nrow(boston_dep))

rows <- (1:nrow(boston_dep))[srswor!=0]
rows <- rep(rows, srswor[srswor != 0])

sample.1 <- boston_dep[rows, ]

(df <- table(sample.1))
(prop.table(df)*100)

### Systematic Sample ###
(pik <- inclusionprobabilities(boston$Earnings, 50))

s <- UPsystematic(pik)

sample <- boston[s != 0, ]

table(sample$Earnings)
prop.table(boston$Earnings)





