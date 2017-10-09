set.seed(111)

# No infinite values
kl_Gaussian <- read.csv('klGaussian.dat', header = FALSE)

# No infinite values
kl_Goldberger <- read.csv('kl_goldberger.dat', header = FALSE)

# No infinite values
kl_Min <-  read.csv('kl_min.dat', header = FALSE)

# Has infinite values
kl_Unscented <- read.csv('kl_unscented.dat', header = FALSE)

# Has infinite values
kl_Variational <- read.csv('kl_variational.dat', header = FALSE)

# Has infinite values
kl_Product <- read.csv('kl_product.dat',header = FALSE)

# Substituting the infinity values with finite values
kl_Unscented_infIndex <- numeric()
kl_Product_infIndex <- numeric()
kl_Variational_infIndex <- numeric()

for (i in 1:nrow(kl_Unscented)){
  if(is.infinite(kl_Unscented[i,])){
    kl_Unscented[i,] <- 100000000
    kl_Unscented_infIndex <- c(kl_Unscented_infIndex,i)
  }
  if(is.infinite(kl_Product[i,])){
    kl_Product[i,] <- 100000000
    kl_Product_infIndex <- c(kl_Product_infIndex,i)
  }
  if(is.infinite(kl_Variational[i,])){
    kl_Variational[i,] <- 100000000
    kl_Variational_infIndex <- c(kl_Variational_infIndex,i)
  }
}


kl_Gaussian_SSE <- numeric()
kl_Goldberger_SSE <- numeric()
kl_Min_SSE <- numeric()
kl_Unscented_SSE <- numeric()
kl_Product_SSE <- numeric()
kl_Variational_SSE <- numeric()

for (i in 2:30) {
  print(paste0("Iteration ",i))
  
  l <- kmeans(kl_Gaussian,i)
  kl_Gaussian_SSE <- c(kl_Gaussian_SSE,l$tot.withinss)

  l <- kmeans(kl_Goldberger,i)
  kl_Goldberger_SSE <- c(kl_Goldberger_SSE,l$tot.withinss)

  l <- kmeans(kl_Min,i)
  kl_Min_SSE <- c(kl_Min_SSE,l$tot.withinss)
  
  l <- kmeans(kl_Unscented,i)
  kl_Unscented_SSE <- c(kl_Unscented_SSE,l$tot.withinss)
  
  l <- kmeans(kl_Product,i)
  kl_Product_SSE <- c(kl_Product_SSE,l$tot.withinss)
  
  l <- kmeans(kl_Variational,i)
  kl_Variational_SSE <- c(kl_Variational_SSE,l$tot.withinss)
  
}

print(paste0("best k value for Gaussian: ",which.min(kl_Gaussian_SSE) + 1)) 
# "best k value for Gaussian: 18"

print(paste0("best k value for Goldberger: ",which.min(kl_Goldberger_SSE) + 1))
# "best k value for Goldberger: 13"

print(paste0("best k value for Min: ",which.min(kl_Min_SSE) + 1))
# "best k value for Min: 19"

print(paste0("best k value for Product: ",which.min(kl_Product_SSE) + 1))
# "best k value for Product: 22"

print(paste0("best k value for Variational: ",which.min(kl_Variational_SSE) + 1))
# "best k value for Variational: 19"

print(paste0("best k value for Unscented: ",which.min(kl_Unscented_SSE) + 1))
# "best k value for Unscented: 29"

# # list of infinite indexes for Unscented
# kl_Unscented_infIndex
#   
# # list of infinite indexes for Product
# kl_Product_infIndex
# 
# # list of infinite indexes for Variational
# kl_Variational_infIndex


