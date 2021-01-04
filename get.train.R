#============================================
# Set the indices for the training/test sets
#============================================
get.train <- function(data.sz, train.sz) {
      # Take subsets of data for training/test samples
      # Return the indices
   train.ind <- sample(data.sz, train.sz)
   test.ind <- (1:data.sz) %w/o% train.ind
   list(train=train.ind, test=test.ind)
}