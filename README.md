# ENQview


## install方法
```r
if (!require(devtools)){
    install.packages('devtools')
    library(devtools)
}
devtools::install_github("419kfj/ENQview", upgrade="never")
```

## 呼び出し手順

- インストールのあと

library(ENQview)

## データを使えるようにする。
```r
data(taste_J)
ENQview(taste_J) # 分析対象データの単純集計
Shiny_speMCA(taste_J) #分析対象データのspeMCA分析
```

## 履歴
```r
2025/06/15　Shiny_GDAとしてレイアウトを変更。eta2など、1−2軸、3−2軸、1−3軸のマップも描画するようにした
2025/06/14　集中楕円の描画機能を組み込み
2025/06/13　GitHub登録のファイルを整理。必要なPackage類をDESCRUPTIONに記述し、各Rファイルでは、#' @importや#' @importFrom で対応した
2025/05/27　２変数のクロスの表示、mosaic plot、カイ２乗検定など、tableのなかで、df内の変数参照を変更。[,vari]から[[vari]]へ
　　　　　　　 v0.8.0.9000　-> 0.8.1.1227 
2024/12/22　一応、うごくようになった。
2024/12/19　ENQviewをpackage化することにした。（ver3.0 -> v0.8.0.9000）

ver3.0 2024/12/19 ENQview としてパッケージ化,
ver2.1.1 2024/11/20 ISSP2016データを追加,
ver2.1 2024/10/14 変数選択を一元化。Grid集計を汎用化,
ver2.0 2024/10/12 「文化と不平等」データを中心に基本集計機能を拡充,
ver1.7 2024/10/07 CYDERデータ分析からiwateデータ分析ように修正,
ver1.6 2024/01/30 gitでversion管理を開始、NLP2024論文をLINK,
ver1.5 2023/10/23 クロス集計にgtsummary::tbl_crossを適用,
ver1.0 2023/06/12 プロトタイプから利用可能なレベルにしてリリース,
```
