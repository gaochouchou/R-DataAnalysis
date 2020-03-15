#载入数据
library(tidyverse)
library(readxl)
cjb <- read_excel("data/cjb.xlsx")

#对于分类与回归问题，除了认识数据中的其他一些数据探索外
#通常需要观察不同自变量，相对于不同因变量取值时的数据分布
#考察其分类的潜力
#我们可以借助caret::featurePlot()和plotluck::plotluck()来进行观察
cjb %<>%
  mutate(zcj = rowSums(.[, 4:12])) %>%
  mutate_at(vars(xb, bj, wlfk), factor) %>%
  filter(zcj != 0) %>%
  select(xb:wlfk)
library(caret)
featurePlot(
  x = cjb %>%
    select(yw:sw),
  y = cjb[, "wlfk"] %>%
    as_vector(),
  plot = "density",
  scales = list(
    x = list(relation = "free"),
    y = list(relation = "free")
  ),
  adjust = 1.5,
  pch = "|"
)

#k折交叉检验
cv_kfold <- function(data, k = 10, seed = 2012) {
  n_row <- nrow(data)#计算数据的行数
  n_foldmarkers <- rep(1:k, ceiling(n_row / k))[1:n_row]
  set.seed(seed)
  n_foldmarkers <- sample(n_foldmarkers)  #打乱顺序
  kfold <- lapply(1:k, function(i) {
    (1:n_row)[n_foldmarkers == i]
  })
  return(kfold)
}

cv_kfold(cjb)

#k的具体取值，没有统一的标准，视数据量大小，
#可以取5、10等
#对于少量数据，甚至可以将其推到极致，
#取nrow(cjb)折
#也就是我们常说的留一法
#kfolds <- cv_kfold(cjb, nrow(cjb))
#我们这里取k=10
kfolds <- cv_kfold(cjb)


# Global performance ------------------------------------------------------

#对于分类模型的评估，首先需要看是否存在类不平衡问题，
#如果存在类不平衡，评估指标的选取，
#不能单纯用正确率、错误率来评估，
#比如：10000人中，有10个人得SARS。现在不采用任何模型，
#只是用众数进行预测，也就是判定所有人都不得SARS，
#此时模型的正确率为(10000 - 10) / 10000 = 99.90%，
#正确率达到99.9%，然而，这种预测没有任何意义
#故此，还需要引入召回率Recall和Precision，
#以及二者的调和平均数F1值等
#从plotluck()的结果可以看出，我们所拿到的数据，并不存在
#类不平衡问题
#plotluck(cjb, .~1)
#由于类是相对均衡的，本实验仅采用分类正确率和错误率
global_performance <- NULL
imetrics <- function(method, type, predicted, actual) {
  con_table <- table(predicted, actual)
  cur_one <- data.frame(
    method = method,
    #算法模型的名称
    type = type,
    #取值为train或是test
    accuracy = sum(diag(con_table)) / sum(con_table),
    error_rate = 1 - sum(diag(con_table)) / sum(con_table)
  )
  assign("global_performance",
         rbind(get("global_performance", envir = .GlobalEnv) ,
               cur_one),
         envir = .GlobalEnv)
}

# kknn --------------------------------------------------------------------
train_set_idx <- sample(nrow(cjb), 0.7 * nrow(cjb))
test_set_idx <- (1:nrow(cjb))[-train_set_idx]
library(kknn)
set.seed(2012)
#作为惰性学习法，训练和测试同时进行
imodel <- kknn(wlfk ~ .,
               train = cjb[train_set_idx,],
               test = cjb[-train_set_idx,])
predicted_test <- imodel$fit
Metrics::ce(cjb$wlfk[-train_set_idx], predicted_test)
#> [1] 0.2446352
#分开训练集和和测试集合后，错误率提高
#通过参数调优优化结果
#选取最优的k和核
train_kk <- train.kknn(
  wlfk  ~ .,
  data = cjb,
  kmax = 100,
  kernel = c(
    "rectangular",
    "epanechnikov",
    "cos",
    "inv",
    "gaussian",
    "optimal"
  )
)

#提取不同k和核相应的分类错误率
ce_kk <- train_kk$MISCLASS
#View(ce_kk)
#最小错误率
min_ce <- min(train_kk$MISCLASS)
str(ce_kk)
#最佳的k值
best_k <- train_kk$best.parameters$k
best_kernel <- train_kk$best.parameters$kernel
#通过ggplot2进行绘制
ce_kk %>%
  as.data.frame() %>%
  mutate(k = row_number()) %>%
  gather(key = "kernel", value = "ce", -k) %>%
  ggplot(aes(x = k, y = ce, colour = kernel)) +
  geom_vline(aes(xintercept = best_k), linetype = "dashed") +
  geom_hline(aes(yintercept = min_ce), linetype = "dashed") +
  geom_line() +
  geom_point(aes(shape = kernel)) +
  theme(legend.position = c(0.9, 0.8))

#进行k-折交叉检验k-fold cross validation
library(kknn)
sp <- Sys.time() #记录开始时间
cat("\n[Start at:", as.character(sp))
for (i in 1:length(kfolds)) {
  curr_fold <- kfolds[[i]] #当前这一折
  train_set <- cjb[-curr_fold, ] #训练集
  test_set <- cjb[curr_fold, ] #测试集
  predicted_train <- kknn(
    wlfk ~ .,
    train = train_set,
    test = train_set,
    k = best_k,
    kernel = best_kernel
  )$fit
  imetrics("kknn", "Train", predicted_train, train_set$wlfk)
  predicted_test <- kknn(
    wlfk ~ .,
    train = train_set,
    test = test_set,
    k = best_k,
    kernel = best_kernel
  )$fit
  imetrics("kknn", "Test", predicted_test, test_set$wlfk)
}
ep <- Sys.time()
cat("\tFinised at:", as.character(ep), "]\n")
cat("[Time Ellapsed:\t",
    difftime(ep, sp, units = "secs"),
    " seconds]\n")
