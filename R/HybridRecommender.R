## create a hybrid recommender

# .REAL_HYBRID_params <- list()

# HybridRecommender <- function(..., weights = NULL) {
HybridRecommender <- function(data, parameter= NULL) {
  
  # ******************************************************************************
  
  print("Modified HybridRecommender")
  
  weights <- parameter$weights
  parameter$weights <- NULL
  
  recommender <- parameter
  # recommender <- list(...)
  
  # ******************************************************************************
  
  if(is.null(weights)) weights <- rep(1, length(recommender))
  else if(length(recommender) != length(weights)) stop("Number of recommender and length of weights does not agree!")
  weights <- weights/sum(weights)
  
  if(!all(sapply(recommender, is, "Recommender"))) stop("Not all supplied models are of class 'Recommender'.")
  
  model <- list(recommender = recommender, weights = weights)
  
  predict <- function(model=NULL, newdata, n=10,
                      data= NULL, type=c("topNList", "ratings", "ratingMatrix"), ...) {
    
    type <- match.arg(type)
    
    ## newdata are userid
    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata, , drop = FALSE]
    }
    
    #if(ncol(newdata) != length(model$labels)) stop("number of items in newdata does not match model.")
    
    pred <- lapply(model$recommender, FUN = function(object)
      object@predict(object@model, newdata, data=data, type="ratings", ...))
    
    ratings <- matrix(NA, nrow=nrow(newdata), ncol = ncol(newdata))
    for(i in 1:nrow(pred[[1]])) {
      ratings[i,] <- colSums(t(sapply(pred, FUN = function(p)
        as(p[i,], "matrix"))) * model$weights, na.rm = TRUE)
    }
    dimnames(ratings) <- dimnames(newdata)
    
    
    ratings <- as(ratings, "realRatingMatrix")
    colnames(ratings) <- colnames(newdata)
    
    
    if(type == "ratingMatrix")
      stop("Hybrid cannot predict a complete ratingMatrix!")
    
    returnRatings(ratings, newdata, type, n)
  }
  
  ## this recommender has no model
  new("Recommender", method = "HYBRID",
      dataType = "ratingMatrix",
      ntrain = NA_integer_,
      model = model,
      predict = predict)
}

# ******************************************************************************
# set HybridRecommender entry into recommenderRegistry

# ## register recommender
recommenderRegistry$set_entry(
  method="HYBRID", dataType = "realRatingMatrix", fun= HybridRecommender,
  description="Hybrid Recommender")

## register recommender
# recommenderRegistry$modify_entry(
#   method="HYBRID", dataType = "realRatingMatrix", fun= HybridRecommender,
#   description="Hybrid Recommender")

# ******************************************************************************

# Working exmaple -----------------------------------------------------------------------------

# Please uncomment it for usage                                  
# You need to load the required library(s) and prepare the data (train) before the below usage
                                      
# Approach 1: POPULAR --------------------
#model.popular <- Recommender(data= train, method= "POPULAR", param= NULL)

# Approach 2: RANDOM --------------------
#model.random <- Recommender(data= train, method= "RANDOM", param= list(range= c(1, 5)))

# Approach 3: IBCF --------------------
#model.IBCF <- Recommender(data= train, method= "IBCF", param= list(
#  k= 20, method= "Cosine", normalize= "center"))

# evaluate on  method HYBRID, with custom recommenders as input
#evaluate(scheme, method="HYBRID", type= "topNList",
#              n= c(1, 3, 5, 10, 15, 20), param=list("popular" = model.popular, 
#                                                    "random" = model.random, 
#                                                    "IBCF" = model.IBCF, 
#                                                    weights= c(.3, .1, .6)))
                                      
# End -----------------------------------------------------------------------------
