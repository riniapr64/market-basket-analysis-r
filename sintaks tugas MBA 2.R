library(arules)
library(arulesViz)
library(Matrix)
grosir <- read.csv("D:\\KULIAH\\sem 6\\Data Mining\\25 maret\\KS01AB.csv", sep=",")
head(grosir)
dim(grosir)
trans <- as(split(grosir[,"NAMA"],grosir[,"NO"]),"transactions")
trans
itemFrequencyPlot(trans, type="absolute", topN=10, main= "TOP 10 ITEM")
tabel <- crossTable(trans, sort=TRUE)
tabel[1:4,1:4]
summary(trans)
rules <- apriori(trans, parameter = list(supp=0.01, conf=0.2))
rules
inspect(sort(rules))
#kalau dibawah 1 berarti hubungan antara barang tidak ada, jika 1 masih ada hubungan antar barang akan kebeli, jika diatas 1 maka hubungan kedua barang sangat kuat atau bisa dibilang jika membeli barang a maka akan beli barang b jga
crossTable(trans, measure='lift', sort=T)[1:5,1:5]
crossTable(trans, measure='chi', sort=T)[1:5,1:5]
plot(sort(rules,by="lift"), method="graph", control=list(type="items"))
rules2 <- apriori(trans, parameter = list(supp=0.001, conf=0.05),
                  appearance = list(default="rhs", lhs="FILMA POUCH 1 LT"))
inspect(sort(rules2))
plot(sort(rules2,by="lift"), method="graph", control=list(type="items"))

