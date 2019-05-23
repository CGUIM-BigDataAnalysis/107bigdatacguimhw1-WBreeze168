install.packages("data.table")
library(readr)
library(dplyr)
library(magrittr)
library(data.table)

f103 = read_csv('103.csv')
f104 = read_csv('104.csv')
f105 = read_csv('105.csv')
f106 = read_csv('106.csv')

f103 %<>% select(class = "大職業別", US103 = "大學-薪資", UG103 = "大學-女/男",
              MS103 = "研究所及以上-薪資", MG103 = "研究所及以上-女/男")
f104 %<>% select(class = "大職業別", US104 = "大學-薪資", UG104 = "大學-女/男",
              MS104 = "研究所及以上-薪資", MG104 = "研究所及以上-女/男")
f105 %<>% select(class = "大職業別", US105 = "大學-薪資", UG105 = "大學-女/男",
              MS105 = "研究所及以上-薪資", MG105 = "研究所及以上-女/男")
f106 %<>% select(class = "大職業別", US106 = "大學-薪資", UG106 = "大學-女/男",
              MS106 = "研究所及以上-薪資", MG106 = "研究所及以上-女/男")

f106$class <- gsub('_','、',f106$class)

data = full_join(f103, f104, by = "class")
data = full_join(data, f105, by = "class")
data = left_join(data, f106, by = "class")
data[,2:17] <- apply(data[,2:17],2,as.numeric)

data1 = data.table(data)
data1 = data1[US106>US103,.(class,raise_rate=(((US106-US103)/US103)*100))]
head(data1$class[order(data1$raise_rate,decreasing = TRUE)],10)

data2 = data1[raise_rate>5,.(class, raise_rate)]
print(data2$class)

main.class = function(x){
  temp = strsplit(x,'-')
  x = temp[[1]][1]
  return(x)
  
}
data2$class = sapply(data2$class,main.class)
table(data2$class)

data3 = data.table(data)
data3 = data3[!is.na(MS106)&!is.na(US106),.(class,raise_rate = ((MS106-US106)/US106)*100)]
data3 = data3[order(raise_rate,decreasing = T)]
head(data3,10)

data4 = data.table(data)
data4 = data4[c(79:81,100:102),.(class,US106,MS106,raise=(MS106-US106),raise_rate=(MS106-US106)/US106*100)]
print(data4)
mean(data4$raise)

data5 = data.table(data)
data5 = data5[,.(class,UG103,UG104,UG105,UG106)]
count(data5[!is.na(UG106)])
data5[UG103>=100|UG104>=100|UG105>=100|UG106>=100]
data5[UG103<=95|UG104<=95|UG105<=95|UG106<=95]
