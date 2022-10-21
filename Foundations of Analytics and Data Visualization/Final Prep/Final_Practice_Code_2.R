my_function <- function(x){
  sum <- 0
  for(i in 1:x)
    sum <- x*5+2-10
  return(sum)
}
my_function(10)


boston <- read.csv( "https://people.bu.edu/kalathur/datasets/bostonCityEarnings.csv", colClasses = c("character", "character", "character", "integer", "character"))

head(boston)

### SRSWOR Sample ###
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

## STRATIFIED Sample ###

set.seed(123)
# Stratified, equal sized strata


head(data)

freq <- table(boston$ZipCode)

section.ids <- rep(LETTERS[1:4], each = 25)

section.scores <- round(runif(100, 60, 80))

strata_data <- data.frame(
  Section = section.ids, 
  Score = section.scores)

st.1 <- strata(strata_data, stratanames = c("Section"),
               size = rep(2,10), method = "srswor",
               description = TRUE)

st.1

st.sample1 <- getdata(data, st.1)

st.sample1


?strata()

my_function <- function(x) {
  x <- c(20, 30, 40, 50)
  sum <- 0
  for(i in x)
    sum <- 10*x+1
  return(sum)
}

my_function(10)

my_function <- function(x) {
  return (x * 10)
}

my_function(5)



n = 100

p = 0.5

qnbinom(seq(0, 1, by = 0.01), size = n, prob = p)









