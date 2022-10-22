# Final Exam
# David Lotito

# Question #1

library(prob)
(odd <- c(49, 51, 63, 77, 83))

(x <- seq(from = 20, to = 95, by = 2))

x[x == odd]

x <- as.vector(x)

if(odd == x)
  print("ODD")
else
  print("EVEN")

# Question #2

data <- c(240, 690, 205, 770, 530, 470, 750, 830, 880, 720, 475, 855, 730, 700, 685, 195, 790, 235, 600, 440, 420, 675, 795, 535, 110, 710, 820, 355, 465, 225, 660, 305, 320, 895, 575, 580, 285, 645, 585, 150, 630, 325)

(sales <- matrix(data, ncol = 7, byrow = TRUE))

max(sales[1,])
max(sales[2,])
max(sales[3,])
max(sales[4,])
max(sales[5,])
max(sales[6,])
max(sales[7,])
sales[6,3] - 875


# Question #3

fibo <- function(n){
  fib <- seq(from = 0, to = n, by = 1)
  for(i in fib)
    return(sum(i+fib))
}

fibo(10)

library(prob)

?prob()

p = 0.5
n = 5


dbinom(0:15, size = n, prob = p)

qbinom(seq(0, 1, by = 0.05), size = n, prob = p)

set.seed(675)

rbinom(100, size = n, prob = p)


# Question #5
set.seed(839)
cardata <- mtcars
cardata$Make <- sapply( strsplit(rownames(mtcars), " "), "[[", 1)
library(sampling)
#A
(df <- as.data.frame(cardata$Make))

srswor <- srswor(12, nrow(df))
srswor

rows <- (1:nrow(df))[srswor != 0]

rows <- rep(rows, srswor[srswor != 0])

sample <- df[rows, ]

#B
(df_table <- table(sample))
set.seed(839)
pik <- inclusionprobabilities(cardata$mpg, 12)

s <- UPsystematic(pik)

sample_2 <- cardata[s != 0, ]
sample_2

table(sample_2$Make)

table(sample_2$mpg)

#C

df <- as.data.frame(cardata, Num_Cyl = cardata$cyl)

freq <- table(cardata$cyl)

st.sizes = 12 * freq/sum(freq)

sample_3 <- strata(df, stratanames = c("cyl"), size = st.sizes, method = "srswor", description = T)

st.sample_3 <- getdata(df, sample_3)
st.sample_3

table(st.sample_3$Make)

mean(df$mpg)
# Question #6

s <- "glst5#"

library(stringr)

st_1 <- str_sub(s, 1,3)
st_2 <- str_sub(s, 4,7)
(combine <- c(st_1, st_2))

?str_sub()

s <- "fhilorv3589#"

st1 <- str_sub(s, 0, c(3))
st2 <- str_sub(s, 4, 6)
st3 <- str_sub(s, 7, 9)
st4 <- str_sub(s, 10, 12)

(combine_all <- c(st1, st2, st3, st4))

str_detect(combine_all, "[r]")

stringr::words

words[str_detect(words, "^[f]")]

words[str_detect(words, "^[f]")]

max(str_length(words[str_detect(words, "^[f]")]))

f_words <- words[str_detect(words, "^[f]")]

f_words[str_length(f_words) == 11]

words[str_detect(words, "^[f]{11}")]

words[str_detect(words, "^[f]+[a-z]{11}")]

str

?regex()


for (i in rows)
  paste0("Store", seq(1, 6), "maximum sales of ", rows, "in Week", ncol), "and trails target 875 by", i)








