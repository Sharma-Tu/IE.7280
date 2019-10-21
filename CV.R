setwd("C:/NORTHEASTERN/FALL2019/IE7280.Stat.Methods/LABS/LAB2")

Lab2 <- read.csv("Lab2.csv", header = TRUE)

#Set K number of cross validations sets
CV_K = 10

# Split the data into 10 equal parts (9 of size 8 and 1 of size 7)
set.seed(2)

# RANDOM SELECTION OF DATA INTO 10 SAMPLES
nr = nrow(Lab2)
div = round(nrow(Lab2)/CV_K)
cv_sets <- lapply(split(sample(nrow(Lab2)), ceiling((1:nrow(Lab2))/div)), 
                  function(i) Lab2[i, ])

# SAVING LAPPLY LISTS AS DF
#for (i in 1:CV_K) {assign(paste0("cv_set_", i), as.data.frame(cv_sets[[i]]))}

#Free memory
remove(nr,div)

#1 Build test and train here
#2 Apply 2 models and validate
#r=1
split_train_validate<- function(r) {
  datalist = list()
  #print(r)
  for (i in 1:CV_K) {
    if (i != r)
    {datalist[[i]] <- cv_sets[[i]]}
  }
  train <- do.call(rbind, datalist)
  test <- cv_sets[[r]]
  rm(i,datalist)
  lm_8var <- lm(y ~ ., data = train)
  lm_2var <- lm(y ~ x1 + x2, data = train)
  SSE_8var <- sum((test[1]-predict(lm_8var,test))^2)
  SSE_2var <- sum((test[1]-predict(lm_2var,test))^2)
  SSE <- as.data.frame(list("SSE_8var" = SSE_8var, "SSE_2var" = SSE_2var))
  return(SSE)
}

SSECV <- data.frame(SSE_8var = double(), SSE_2var = double())
for (i in 1:10)  SSECV[i,] <- split_train_validate(i)
rm(i)

colSums(SSECV)
