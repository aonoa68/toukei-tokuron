# データの作成
data <- data.frame(
  訴訟請求額 = c(5.5, 4.8, 6.2, 5.9, 5.1, 5.6, 6, 5.2, 6.4, 4.9),
  争点複雑指数 = c(3.3, 3.5, 3, 3.9, 2.8, 3.2, 2.9, 3.8, 3.4, 3.1),
  証拠点数 = c(7.1, 6.5, 7.9, 7.4, 6.8, 7.2, 7.6, 6.6, 8, 6.4),
  審理期間 = c(2.6, 2, 2.8, 1.9, 2.3, 2.1, 2.7, 2.5, 2.2, 2.4),
  弁護士費用 = c(6.8, 6.3, 7.1, 6.9, 6.5, 6.7, 7, 6.4, 7.2, 6.2)
)

# データの標準化
data_scaled <- scale(data)

# PCAの実行
pca_result <- prcomp(data_scaled)

# 主成分得点の表示
print(pca_result$x)

# 主成分負荷量(固有ベクトル）の表示
print(pca_result$rotation)

# 固有値の表示
eigenvalues <- pca_result$sdev^2
print(eigenvalues)

# 累積寄与率の計算
cumulative_proportion <- cumsum(eigenvalues) / sum(eigenvalues)
print(cumulative_proportion)

#日本語化
par(family = "HiraKakuProN-W3")

# バイプロットの生成
biplot(pca_result)

#相関行列から固有値と固有ベクトルを求める
cor_matrix <- cor(data_scaled)

# 相関行列の表示
print(cor_matrix)

# 固有値と固有ベクトルの計算
eigen_result <- eigen(cor_matrix)
print(eigen_result$values)

# 固有ベクトルの表示
print(eigen_result$vectors)
