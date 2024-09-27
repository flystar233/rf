# rf
使用R从头实现随机森林
## 准确性评估
### 分类
```r
data = read.csv("wine.txt",header = T)
forest <- random_forest(data[1:13],data[14],
                        n_trees = 100, max_depth = 5, min_samples_split = 5,
                        min_samples_leaf = 2,replace = T,type = "classification")
print(forest$oob_error)
#0.02247191
acc = calculate_accuracy(forest,data[1:13],data[14])
print(acc)
#1
```
### 回归
```r
data = read.csv("housing.txt",header = T)
forest <- random_forest(data[1:13],data[14],
                        n_trees = 100, max_depth = 5, min_samples_split = 10,
                        min_samples_leaf = 5,replace = T,type = "regression")
print(forest$oob_error)
#MSE
#25.20217
acc = calculate_accuracy(forest,data[1:13],data[14])
print(acc)
#r2
#0.8604324
```
## 决策树可视化
```r
tree <- tree_info(forest,tree_number = 10)
p <- plot_tree(tree)
SetGraphStyle(p, rankdir = "TB")
SetEdgeStyle(p, arrowhead = "vee", color = "grey35", penwidth = 2)
SetNodeStyle(p, style = "filled,rounded", shape = "box", 
            fontname = "helvetica", tooltip = GetDefaultTooltip)
plot(p)
```
![](https://github.com/user-attachments/assets/e09055cf-f896-401f-9d04-1e08b1ff3fe5)


