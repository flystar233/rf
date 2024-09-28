# rf
使用R从头实现随机森林
## 随机森林构建过程
### 读入参数
关键参数为：

1. n_trees：森林中树的数量
2. max_depth：树的最大深度
3. min_samples_split：节点分裂所需的最小样本数
4. min_samples_leaf：叶节点所需的最小样本数
5. replace：是否有放回地抽样
6. type：分类还是回归
### 构建森林
使用future_lapply 并行构建森林中的多棵树，每棵树使用的样本和特征都是随机抽取的。由于默认使用的是有放回抽样，所以每棵树使用的样本和特征都是不同的。且大概会有1/3的样本没有被使用，这些样本被用作袋外误差估计。
### 构建决策树
将样本和特征随机抽取后，使用决策树算法构建决策树。决策树算法中，分类问题使用信息增益作为特征和特征值选择的标准，回归问题使用 MSE（均方误差）作为特征和特征值选择的标准，然后递归地构建决策树。即每一次都循环所有特征（默认设置mtry参数，所以输入每棵决策树的特征数量为数据集中所有特征的平方根个），预设分割点为特征值的100个分位数。将小于分割点数量的样本划入左子树，大于等于分割点数量的样本划入右子树。如果特征类型为离散型，则使用所有可能的取值作为分裂点。等于分裂点的样本划入左子树，不等于分裂点的样本划入右子树。然后计算左右子树的信息增益，选择信息增益最大的特征作为分裂特征。对应的分裂点作为该特征的分裂点（回归问题同理）。

通过最佳分割特征和分裂点，将样本划分为左右子树，然后递归地构建决策树（将左右子树重复以上操作）。直到满足以下条件之一，停止构建决策树：
1. 节点包含的样本数量小于min_samples_split
2. 节点包含的样本数量小于min_samples_leaf
3. 树的深度等于max_depth

并计算该节点的预测值。如果是分类问题，则使用样本中类别最多的类别作为预测值。如果是回归问题，则使用样本中目标值的平均值作为预测值。
### 预测
使用构建好的森林对每一个样本输入进行并行预测。对于分类问题，使用投票法，即每棵树预测一个类别，最后选择投票最多的类别作为预测结果。对于回归问题，使用平均法，即每棵树预测一个目标值，最后选择所有目标值的平均值作为预测结果。



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


