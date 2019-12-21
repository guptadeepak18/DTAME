#' Build & Evaluate Classificaion & Regression Trees (CART)
#'
#' This function takes a dataframe as an input along with the name of the response variable.
#' It then sets the seed as per user or default value and split the data into training and testing sets.
#' After the split it builds a CART model on training data and evaluates the performance of the model on different Evaluation methods.
#' It can also optimize the CART tree by pruning it as per the least Cross Validation error, user has to define if they want the model to be pruned or not.
#'
#' @param x Data Frame that you want to work with
#' @param response Dependent(Target) Variable
#' @param seed Seed Value, default = 42
#' @param splitRatio Ratio to split the dataframe into Train and Test
#' @param minbucket Minimum number of observations that should be present in each leaf/terminal node
#' @param cp Cost Complexity Paramter Value
#' @param prune Pruning Tree on the basis of minimum Cross Validation Error (xerror), default = FALSE
#' @param plot Plot the model Tree and CP Table
#' @author Gupta, Deepak <deepak@analyticsacedemy.ml>
#' @return A List with Model Attributes & Evaluation Results
#' @export
#' @examples
#' create.eval.cart(sleep, group, plot = FALSE)




create.eval.cart <- function(x, response, seed = 42, splitRatio = 0.7, minbucket = 10, cp = 0, prune = FALSE, plot = TRUE) {

  # attach(x)
  # return(response)

  # utils::data(hacide)

  # Getting Seed from User
  #  if(is.null(seed))
  #  se <- 42
  #else
  # se <- seed

  #set.seed(seed.f)

  # Getting Response Variable
  # response <- x[, response]

  # Splitting into Train & Test
  #  split <- caTools::sample.split(x, SplitRatio = 0.7)
  # train <- subset(x, split == T)
  #  test <- subset(x, split == F)

  # if(is.null(splitRatio))
  #  ratio <- 0.7
  #else
  y <- seed
  ratio <- splitRatio

  set.seed(y)

  INDEX <- sample(1:nrow(x), ratio*nrow(x))
  train <- x[INDEX, ]
  test <- x[-INDEX, ]

  # return(list(train, test))

  ## match.call return a call containing the specified arguments
  ## and the function name also
  ## I convert it to a list , from which I remove the first element(-1)
  ## which is the function name

  # pars <- as.list(match.call()[-1])
  #aa <- train[,as.character(pars$response)]

  #aa <- eval(substitute(response), train)

  #return(aa)
  # attach(train)
  #return(response)
  response <-  deparse(substitute(response))
  # return(train[[response]])


  # arg <- match.call()
  # max(x[[arg$column]])

  frm<-paste(response, ".", sep="~")
  #anv1 <- lm(formula(frm), dataframe)
  # return(frm)

  min_bucket <- minbucket
  c_p <- cp


  rcontrol <- rpart::rpart.control(minbucket = min_bucket, cp = c_p)

  ##Build first CART model
  tree = rpart::rpart(stats::formula(frm), data = train, method = "class", control = rcontrol)

  cptab <- tree$cptable
  #rpart.rules
  #rpart.rules(tree, cover = TRUE)

  if(plot == TRUE) {
  ##Plot tree
  rpart.plot::rpart.plot(tree, main = "CART Tree")
  rpart.plot::prp(tree, main = "CART DT")
  rpart::plotcp(tree, main = "CP Table Plot for CART Tree")
  }

  ##Print cp value
  #printcp(tree)

  #return(cptab)




  # plot <- rpart.plot::rpart.plot(ptree)
  #  cptable <- ptree$cptable

  # rpart.plot::rpart.plot(ptree, main = "Pruned Tree")
  #  rattle::fancyRpartPlot(ptree, main = "Pruned Tree")
  #  return(list(tree = ptree, cptable = ptree$cptable))


  #
  #=======================================================================
  # Predicting Values
  #=======================================================================
  #

  #response_trn <- train[, response]
  #response_tst <- test[, response]

  # return(table(response_trn, response_tst))

  # baseline_main <- sum(data == "1") / nrow(data)

  # PREDICT (TRAIN) - Let's predict using train data first

  train.pred <- stats::predict(tree, data = train, type ="class")
  #table(train.pred)
  train.prob <- stats::predict(tree, data = train, type ="prob")[,2]
  #train_withPred <- cbind(train.pred, train)
  #train_withProb <- cbind(train.prob, train_withPred)
  #head(train_withProb)

  #return(list(class(response_trn), class(train$pred)))

  # Model Performance Evaluation Parameters (Class values 0 and 1) - MCE, Recall,Sphericity,Accuracy,

  #baseline_train <- sum(response_trn == 1) / nrow(train)

  tab.train <- table(train[[response]], train.pred)
  #tab_DT.train
  #baseline_main
  #baseline_train

  #   return(tab.train)

  # MCE

  Classification.Error.train <- sum(tab.train[1,2], tab.train[2,1]) / sum(tab.train[1,], tab.train[2,])

  #return(Classification.Error.train)

  # 4. Measure  Recall in Train

  rc.train <- tab.train[2,2] / sum(tab.train[2,])
  #rc.dt.train
  #return(rc.train)

  # Sphecificty
  spec.train <- tab.train[1,1] / sum(tab.train[1,])
  #spec.dt.train
  #return(spec.train)

  # Accuracy
  acc.train = sum(diag(tab.train)) / sum(tab.train[1,], tab.train[2,])
  #return(acc.train)
  # Model Performance Evaluation Parameters ( Prob. Scores) - ROC/AUC,Gini, KS,ConDisCo

  train_predobject <- ROCR::prediction(train.prob, train[[response]])
  train_perfobject <- ROCR::performance(train_predobject, "tpr", "fpr")
  #train_perfobject@y.values
  #train_perfobject@x.values
  #train_perfobject@alpha.values

  # ROC ( library(ROCR))
  roc.train <- ROCR::performance(train_predobject, "tpr", "fpr")
  # plot(roc.train, main = "ROC Curve for Training Data")

  # return(acc.train)
  # AUC

  auc.train <- ROCR::performance(train_predobject, "auc")
  auc.train <- as.numeric(auc.train@y.values)
  #tweak.dt.train <- performance(dt.train_predobject,"rec")
  #tweak.dt.train_phi <- performance(dt.train_predobject,"phi")
  #tweak.dt.train_lift <- performance(dt.train_predobject,"lift")
  #return(auc.train)

  # Gini
  gini.train <- ineq::ineq(train.prob, "gini")

  # KS
  #train_predobject@cutoffs
  #train_predobject@fp
  #train_predobject@tp
  #train_predobject@n.pos.pred
  yval <- train_perfobject@y.values
  xval <- train_perfobject@x.values
  #train_perfobject@alpha.values

  ks.train <- max(train_perfobject@y.values[[1]]- train_perfobject@x.values[[1]])
  ks.train.value <- round(ks.train, digits = 2)

  #return(ks.train.value)

  #max(attr(dt.train_perfobject,'y.values')[[1]]-attr(dt.train_perfobject,'x.values')[[1]])

  # CorDisCor (Library(InformationValue))

  #res <-  deparse(substitute(response))
  lev <- levels(train[[response]])
  resp <- ifelse(train[[response]] == lev[1], 0, 1)

  #resp <- as.numeric(train[[res]])
  #resp <- as.factor(train[[res]])
  #resp[resp == "Yes"] <- 1
  #resp <- plyr::revalue(resp, c("No",0))
  #resp <- plyr::revalue(resp, c(2,1))

  cordiscor.train <- InformationValue::Concordance(actuals = resp, predictedScores = train.prob)

  # return(cordiscor.train)

  model.performance_train <- c(Classification.Error.train, rc.train, spec.train, acc.train,
                               auc.train, gini.train, ks.train.value , cordiscor.train)



  names(model.performance_train) <- c("Classification Error Train", "Recall Train",
                                      "Specificity Train", "Accuracy Train", "AUC Train", "Gini Train",
                                      "KS Train", "Concordance", "Discordance", "Ties", "Pairs")

  #model.performance_results


  # return(model.performance_train)


  # Let's predict using test data now and see if the model is stable

  # baseline_test <- sum(response_tst == 1) / nrow(test)
  test.pred <- stats::predict(tree, newdata = test, type ="class")
  #table(test.pred)
  test.prob <- stats::predict(tree, newdata = test, type ="prob")[,2]
  test_withPred <- cbind(test.pred, test)
  test_withProb <- cbind(test.prob, test_withPred)
  #head(test_withProb)
  #baseline_main
  #baseline_test




  # Model Performance Evaluation Parameters (Class values 0 and 1) - MCE, Recall,Sphericity,Accuracy,

  tab.test <- table(test[[response]], test.pred)
  #tab.test
  #baseline_main
  #baseline_test


  # MCE

  Classification.Error.test <- sum(tab.test[1,2], tab.test[2,1]) / sum(tab.test[1,], tab.test[2,])

  # 4. Measure  Recall in test

  rc.test <- tab.test[2,2] / sum(tab.test[2,])
  #rc.test

  # Sphericity
  spec.test <- tab.test[1,1] / sum(tab.test[1,])
  #spec.test


  # Accuracy
  acc.test <- sum(diag(tab.test)) / sum(tab.test[1,], tab.test[2,])


  # Model Performance Evaluation Parameters ( Prob. Scores) - ROC/AUC,Gini, KS,ConDisCo

  test_predobject <- ROCR::prediction(test.prob, test[[response]])
  test_perfobject <- ROCR::performance(test_predobject, "tpr", "fpr")
  #test_perfobject@y.values
  #test_perfobject@x.values

  # ROC ( library(ROCR))
  roc.test <- ROCR::performance(test_predobject, "tpr", "fpr")
  # roc.plot.test <- plot(roc.test)

  # AUC

  auc.test <- ROCR::performance(test_predobject, "auc");
  auc.test <- as.numeric(auc.test@y.values)
  #auc.test
  #tweak.dt.test <- performance(dt.test_predobject,"rec")
  #tweak.dt.test_phi <- performance(dt.test_predobject,"phi")
  #tweak.dt.test_lift <- performance(dt.test_predobject,"lift")

  # Gini
  gini.test <- ineq::ineq(test.prob, "gini")
  #gini.test

  # KS
  #test_predobject@cutoffs
  #test_predobject@fp
  #test_predobject@tp
  #test_predobject@n.pos.pred

  ks.test <- max(test_perfobject@y.values[[1]] - test_perfobject@x.values[[1]])

  #max(attr(dt.test_perfobject,'y.values')[[1]]-attr(dt.test_perfobject,'x.values')[[1]])

  # CorDisCor (Library(InformationValue))

  resp_test <- ifelse(test[[response]] == lev[1], 0, 1)

  cordiscor.test <- InformationValue::Concordance(actuals = resp_test, predictedScores = test.prob)

  #
  model.performance_test <- c(Classification.Error.test, rc.test, spec.test, acc.test, auc.test,
                              gini.test, ks.test, cordiscor.test)
  table.results <- table(model.performance_test)

  ##
  names(model.performance_test) <- c("Classification Error Test", "Recall Test", "Specificity Test",
                                     "Accuracy Test", "AUC Test", "Gini Test", "KS Test", "Concordance",
                                     "Discordance", "Ties", "Pairs")

  #class(model.performance_test)
  #library(plyr)

  masterlist <- list(a = model.performance_train, b = model.performance_test)
  master.df <- plyr::ldply(masterlist, data.frame)

  #  return(master.df)

  #library(data.table)

  DT <- data.table::rbindlist(masterlist, use.names=FALSE)
  colnames(DT)<- c("Classification Error", "Recall", "Specificity", "Accuracy", "AUC",
                   "Gini", "KS", "Concordance", "Discordance", "Ties", "Pairs")


  Dataset <- c("Train", "Test")

  DT <- cbind(Dataset, DT)



  if(prune == TRUE)
  {

    cp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
    ptree = rpart::prune(tree, cp = cp, "CP")
    #plt <- rpart.plot::rpart.plot(ptree)

    if(plot == TRUE) {

      rpart.plot::rpart.plot(ptree, main = "Pruned Tree")
      rpart.plot::prp(tree, main = "Pruned DT")
      rpart::plotcp(ptree, main = "CP Table Plot for Pruned Tree")

    }
    # plot <- rpart.plot::rpart.plot(ptree)
    #  cptable <- ptree$cptable

    # rpart.plot::rpart.plot(ptree, main = "Pruned Tree")
    #  rattle::fancyRpartPlot(ptree, main = "Pruned Tree")
    #  return(list(tree = ptree, cptable = ptree$cptable))


    #
    #=======================================================================
    # Predicting Values
    #=======================================================================
    #

    #response_trn <- train[, response]
    #response_tst <- test[, response]

    # return(table(response_trn, response_tst))

    # baseline_main <- sum(data == "1") / nrow(data)

    # PREDICT (TRAIN) - Let's predict using train data first

    train.pred <- stats::predict(ptree, data = train, type ="class")
    #table(train.pred)
    train.prob <- stats::predict(ptree, data = train, type ="prob")[,2]
    #train_withPred <- cbind(train.pred, train)
    #train_withProb <- cbind(train.prob, train_withPred)
    #head(train_withProb)

    #return(list(class(response_trn), class(train$pred)))

    # Model Performance Evaluation Parameters (Class values 0 and 1) - MCE, Recall,Sphericity,Accuracy,

    #baseline_train <- sum(response_trn == 1) / nrow(train)

    tab.train <- table(train[[response]], train.pred)
    #tab_DT.train
    #baseline_main
    #baseline_train

    #   return(tab.train)

    # MCE

    Classification.Error.train <- sum(tab.train[1,2], tab.train[2,1]) / sum(tab.train[1,], tab.train[2,])

    #return(Classification.Error.train)

    # 4. Measure  Recall in Train

    rc.train <- tab.train[2,2] / sum(tab.train[2,])
    #rc.dt.train
    #return(rc.train)

    # Sphecificty
    spec.train <- tab.train[1,1] / sum(tab.train[1,])
    #spec.dt.train
    #return(spec.train)

    # Accuracy
    acc.train = sum(diag(tab.train)) / sum(tab.train[1,], tab.train[2,])
    #return(acc.train)
    # Model Performance Evaluation Parameters ( Prob. Scores) - ROC/AUC,Gini, KS,ConDisCo

    train_predobject <- ROCR::prediction(train.prob, train[[response]])
    train_perfobject <- ROCR::performance(train_predobject, "tpr", "fpr")
    #train_perfobject@y.values
    #train_perfobject@x.values
    #train_perfobject@alpha.values

    # ROC ( library(ROCR))
    roc.train <- ROCR::performance(train_predobject, "tpr", "fpr")
    # plot(roc.train, main = "ROC Curve for Training Data")

    # return(acc.train)
    # AUC

    auc.train <- ROCR::performance(train_predobject, "auc")
    auc.train <- as.numeric(auc.train@y.values)
    #tweak.dt.train <- performance(dt.train_predobject,"rec")
    #tweak.dt.train_phi <- performance(dt.train_predobject,"phi")
    #tweak.dt.train_lift <- performance(dt.train_predobject,"lift")
    #return(auc.train)

    # Gini
    gini.train <- ineq::ineq(train.prob, "gini")

    # KS
    #train_predobject@cutoffs
    #train_predobject@fp
    #train_predobject@tp
    #train_predobject@n.pos.pred
    yval <- train_perfobject@y.values
    xval <- train_perfobject@x.values
    #train_perfobject@alpha.values

    ks.train <- max(train_perfobject@y.values[[1]]- train_perfobject@x.values[[1]])
    ks.train.value <- round(ks.train, digits = 2)

    #return(ks.train.value)

    #max(attr(dt.train_perfobject,'y.values')[[1]]-attr(dt.train_perfobject,'x.values')[[1]])

    # CorDisCor (Library(InformationValue))

    cordiscor.train <- InformationValue::Concordance(actuals = resp, predictedScores = train.prob)

    # return(cordiscor.train)

    model.performance_train <- c(Classification.Error.train, rc.train, spec.train, acc.train,
                                 auc.train, gini.train, ks.train.value , cordiscor.train)



    names(model.performance_train) <- c("Classification Error Train", "Recall Train",
                                        "Specificity Train", "Accuracy Train", "AUC Train", "Gini Train",
                                        "KS Train", "Concordance", "Discordance", "Ties", "Pairs")

    #model.performance_results


    # return(model.performance_train)


    # Let's predict using test data now and see if the model is stable

    # baseline_test <- sum(response_tst == 1) / nrow(test)
    test.pred <- stats::predict(ptree, newdata = test, type ="class")
    #table(test.pred)
    test.prob <- stats::predict(ptree, newdata = test, type ="prob")[,2]
    test_withPred <- cbind(test.pred, test)
    test_withProb <- cbind(test.prob, test_withPred)
    #head(test_withProb)
    #baseline_main
    #baseline_test




    # Model Performance Evaluation Parameters (Class values 0 and 1) - MCE, Recall,Sphericity,Accuracy,

    tab.test <- table(test[[response]], test.pred)
    #tab.test
    #baseline_main
    #baseline_test


    # MCE

    Classification.Error.test <- sum(tab.test[1,2], tab.test[2,1]) / sum(tab.test[1,], tab.test[2,])

    # 4. Measure  Recall in test

    rc.test <- tab.test[2,2] / sum(tab.test[2,])
    #rc.test

    # Sphericity
    spec.test <- tab.test[1,1] / sum(tab.test[1,])
    #spec.test


    # Accuracy
    acc.test <- sum(diag(tab.test)) / sum(tab.test[1,], tab.test[2,])


    # Model Performance Evaluation Parameters ( Prob. Scores) - ROC/AUC,Gini, KS,ConDisCo

    test_predobject <- ROCR::prediction(test.prob, test[[response]])
    test_perfobject <- ROCR::performance(test_predobject, "tpr", "fpr")
    #test_perfobject@y.values
    #test_perfobject@x.values

    # ROC ( library(ROCR))
    roc.test <- ROCR::performance(test_predobject, "tpr", "fpr")
    # roc.plot.test <- plot(roc.test)

    # AUC

    auc.test <- ROCR::performance(test_predobject, "auc");
    auc.test <- as.numeric(auc.test@y.values)
    #auc.test
    #tweak.dt.test <- performance(dt.test_predobject,"rec")
    #tweak.dt.test_phi <- performance(dt.test_predobject,"phi")
    #tweak.dt.test_lift <- performance(dt.test_predobject,"lift")

    # Gini
    gini.test <- ineq::ineq(test.prob, "gini")
    #gini.test

    # KS
    #test_predobject@cutoffs
    #test_predobject@fp
    #test_predobject@tp
    #test_predobject@n.pos.pred

    ks.test <- max(test_perfobject@y.values[[1]] - test_perfobject@x.values[[1]])

    #max(attr(dt.test_perfobject,'y.values')[[1]]-attr(dt.test_perfobject,'x.values')[[1]])

    # CorDisCor (Library(InformationValue))

    cordiscor.test <- InformationValue::Concordance(actuals = resp_test, predictedScores = test.prob)

    #
    model.performance_test <- c(Classification.Error.test, rc.test, spec.test, acc.test, auc.test,
                                gini.test, ks.test, cordiscor.test)
    table.results <- table(model.performance_test)

    ##
    names(model.performance_test) <- c("Classification Error Test", "Recall Test", "Specificity Test",
                                       "Accuracy Test", "AUC Test", "Gini Test", "KS Test", "Concordance",
                                       "Discordance", "Ties", "Pairs")

    #class(model.performance_test)
    #library(plyr)

    masterlist <- list(a = model.performance_train, b = model.performance_test)
    master.df <- plyr::ldply(masterlist, data.frame)

    #  return(master.df)

    #library(data.table)

    DTP <- data.table::rbindlist(masterlist, use.names=FALSE)
    colnames(DTP)<- c("Classification Error", "Recall", "Specificity", "Accuracy", "AUC",
                     "Gini", "KS", "Concordance", "Discordance", "Ties", "Pairs")


    Dataset <- c("Train", "Test")

    DTP <- cbind(Dataset, DTP)



  #  return(DT)




  }




if(prune == TRUE)
  return(list(CART_Model = tree, Pruned_Model = ptree, CART_Evals = DT, Pruned_Evals = DTP))
else
  return(list(CART_Model = tree, CART_Evals = DT))



}
