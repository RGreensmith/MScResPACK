########################################
# Parallel processing #
########################################

# http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/

# install.packages("parallel")
library(parallel)


# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

parLapply(cl, 2:4,
          function(exponent)
            2^exponent)