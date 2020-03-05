#加载数据
library(tidyverse)
library(reader)
cjb_url <- "https://github.com/byaxb/RDataAnalytics/raw/master/data/cjb.csv"
cjb <- read.csv(cjb_url)
#数据预处理
#数据分箱
as_five_grade_scores <- function(x){
  cut(x,
      breaks = c(0,seq(60,100,by=10)),#60-100步长为10分割
      include.lowest = TRUE,right = FALSE,
      ordered_result = TRUE,
      labels = c("不及格","及格","中","良","优"))
}
cjb %<>%
  mutate_at(vars(xb, wlfk), factor) %>% #类型转换
  mutate_at(vars(yw:sw), as_five_grade_scores) %>%#数据分箱
  select(-c(1:2))#姓名、班级两列不参与规则挖掘
  
library(arules)
#转为事务
cjb_trans <- as(cjb,'transactions')#用单引号！！！！
inspect(head(cjb_trans))#查找
#算法实现
library(arulesViz)
irules_args_default <- apriori(cjb_trans)
inspect(head(irules_args_default))#查找

#设置置信度、支持度、最小长度等参数
irule <- apriori(
  cjb_trans,
  parameter = list(
    minlen = 2,
    support = 50/length(cjb_trans),
    confidence =0.8
  ),
  appearance = list(
    rhs = c("wlfk=文科","wlfk=理科"))
)
inspectDT(irule)
#按照提升度进行排序
irules_sorted <- sort(irule,by = "lift")
inspectDT(irules_sorted)

#删除冗余规则
#新建一个变量，判断一个元素是不是另一个元素的子集，构建矩阵
subset.matrix <-
  is.subset(irules_sorted, irules_sorted,sparse = FALSE)
#将下三角和对角线赋空
subset.matrix[lower.tri(subset.matrix, diag = TRUE)] <- NA
#对每一列的和大于1的进行剔除, na.rm移除数据
redundant <- colSums(subset.matrix, na.rm = TRUE) >= 1
as.integer(which(redundant))
#剔除规则
(irules_pruned <- irules_sorted[!redundant])
inspect(irules_pruned)
inspectDT(irules_pruned)

#规则可视化
library(arulesViz)
plot(irules_pruned[1:10],method = "graph")

#交互的规则可视化
library(tcltk2)
plot(irules_pruned,
     method = "graph",
     interactive = TRUE)

#查看评估规则
quality(irules_pruned)

more_measures <- interestMeasure(
  irules_pruned,
  measure = c("support","confidence","lift","casualConfidence"),
  transactions = cjb_trans
)

save(irules_pruned,
     file="rules.rda")

irules_pruned_in_df <- as(irules_pruned,"data.frame")
write.csv(irules_pruned_in_df,
           file = "Rules.csv",
           quote=TRUE,
          row.names = FALSE)

irules_pruned_in_df %<>%
  separate(
    rules,
    seq = "=>",
    into = c("LHS","RHS")
  ) %>%
  mutate_at(
    vars("LHS","RHS"),
    funs(gsub("[\\{\\}]","", .))
  )

irules_pruned_in_df <- separate(irules_pruned_in_df,rules,c("LHS","RHS"),"=>")
library(tidyverse)
irules_pruned_in_df <- irules_pruned_in_df  %>% mutate_at(
  vars("LHS","RHS"),
  funs(gsub("[\\{\\}]","", .))
)

write.csv(irules_pruned_in_df,
          file = "Rules.csv",
          quote=TRUE,
          row.names = FALSE)


