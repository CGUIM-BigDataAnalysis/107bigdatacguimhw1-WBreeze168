107-2 大數據分析方法 作業一
================
許雲翔 B0444144

搞不清楚各行各業的薪資差異嗎? 念研究所到底對第一份工作的薪資影響有多大? CP值高嗎? 透過分析**初任人員平均經常性薪資**-
[開放資料連結](https://data.gov.tw/dataset/6647)，可初步了解台灣近幾年各行各業、各學歷的起薪。

## 比較103年度和106年度大學畢業者的薪資資料

### 資料匯入與處理

``` r
library(readr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(magrittr)
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

``` r
f103 = read_csv('103.csv')
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_double(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
f104 = read_csv('104.csv')
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
f105 = read_csv('105.csv')
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
f106 = read_csv('106.csv')
```

    ## Parsed with column specification:
    ## cols(
    ##   年度 = col_double(),
    ##   大職業別 = col_character(),
    ##   `經常性薪資-薪資` = col_double(),
    ##   `經常性薪資-女/男` = col_character(),
    ##   `國中及以下-薪資` = col_character(),
    ##   `國中及以下-女/男` = col_character(),
    ##   `高中或高職-薪資` = col_character(),
    ##   `高中或高職-女/男` = col_character(),
    ##   `專科-薪資` = col_character(),
    ##   `專科-女/男` = col_character(),
    ##   `大學-薪資` = col_character(),
    ##   `大學-女/男` = col_character(),
    ##   `研究所及以上-薪資` = col_character(),
    ##   `研究所及以上-女/男` = col_character()
    ## )

``` r
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
Sys.setlocale(locale = "us")
```

    ## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"

``` r
data[,2:17] <- apply(data[,2:17],2,as.numeric)
```

    ## Warning in apply(data[, 2:17], 2, as.numeric): NAs introduced by coercion
    
    ## Warning in apply(data[, 2:17], 2, as.numeric): NAs introduced by coercion
    
    ## Warning in apply(data[, 2:17], 2, as.numeric): NAs introduced by coercion
    
    ## Warning in apply(data[, 2:17], 2, as.numeric): NAs introduced by coercion
    
    ## Warning in apply(data[, 2:17], 2, as.numeric): NAs introduced by coercion
    
    ## Warning in apply(data[, 2:17], 2, as.numeric): NAs introduced by coercion
    
    ## Warning in apply(data[, 2:17], 2, as.numeric): NAs introduced by coercion
    
    ## Warning in apply(data[, 2:17], 2, as.numeric): NAs introduced by coercion
    
    ## Warning in apply(data[, 2:17], 2, as.numeric): NAs introduced by coercion
    
    ## Warning in apply(data[, 2:17], 2, as.numeric): NAs introduced by coercion
    
    ## Warning in apply(data[, 2:17], 2, as.numeric): NAs introduced by coercion
    
    ## Warning in apply(data[, 2:17], 2, as.numeric): NAs introduced by coercion
    
    ## Warning in apply(data[, 2:17], 2, as.numeric): NAs introduced by coercion
    
    ## Warning in apply(data[, 2:17], 2, as.numeric): NAs introduced by coercion
    
    ## Warning in apply(data[, 2:17], 2, as.numeric): NAs introduced by coercion
    
    ## Warning in apply(data[, 2:17], 2, as.numeric): NAs introduced by coercion

``` r
Sys.setlocale(locale = "cht")
```

    ## [1] "LC_COLLATE=Chinese (Traditional)_Taiwan.950;LC_CTYPE=Chinese (Traditional)_Taiwan.950;LC_MONETARY=Chinese (Traditional)_Taiwan.950;LC_NUMERIC=C;LC_TIME=Chinese (Traditional)_Taiwan.950"

在閱讀完題目後，確認需要的資料欄僅有大學及研究所以上和職業別的部分，因此在讀取完資料後，僅留下需要的資料且改名方便後面取用。
然後再合併資料表時發現106年的職業別與之前有幾筆有有"\_“和”、"的差異，因此改成與前幾年相同，方便整合。
然後將資料表合併，最後把資料型態改成數值。

### 106年度薪資較103年度薪資高的職業有哪些?

``` r
data1 = data.table(data)
data1 = data1[US106>US103,.(class,raise_rate=(((US106-US103)/US103)*100))]
head(data1$class[order(data1$raise_rate,decreasing = TRUE)],10)
```

    ##  [1] "其他服務業-技術員及助理專業人員"          
    ##  [2] "住宿及餐飲業-服務及銷售工作人員"          
    ##  [3] "用水供應及污染整治業-技術員及助理專業人員"
    ##  [4] "專業、科學及技術服務業-專業人員"          
    ##  [5] "其他服務業-技藝、機械設備操作及組裝人員"  
    ##  [6] "營造業-服務及銷售工作人員"                
    ##  [7] "其他服務業-專業人員"                      
    ##  [8] "資訊及通訊傳播業-專業人員"                
    ##  [9] "不動產業-專業人員"                        
    ## [10] "教育服務業-事務支援人員"

把資料轉為data.table的形式，然後選出有成長的職業病增加成長率的欄位，且進一步的把多餘的欄位篩除。 傳回答案

### 提高超過5%的的職業有哪些?

``` r
data2 = data1[raise_rate>5,.(class, raise_rate)]
print(data2$class)
```

    ##  [1] "工業及服務業部門-專業人員"                          
    ##  [2] "工業部門-專業人員"                                  
    ##  [3] "礦業及土石採取業-技藝、機械設備操作及組裝人員"      
    ##  [4] "製造業-專業人員"                                    
    ##  [5] "電力及燃氣供應業-服務及銷售工作人員"                
    ##  [6] "電力及燃氣供應業-技藝、機械設備操作及組裝人員"      
    ##  [7] "用水供應及污染整治業"                               
    ##  [8] "用水供應及污染整治業-專業人員"                      
    ##  [9] "用水供應及污染整治業-技術員及助理專業人員"          
    ## [10] "用水供應及污染整治業-事務支援人員"                  
    ## [11] "用水供應及污染整治業-服務及銷售工作人員"            
    ## [12] "用水供應及污染整治業-技藝、機械設備操作及組裝人員"  
    ## [13] "營造業-專業人員"                                    
    ## [14] "營造業-事務支援人員"                                
    ## [15] "營造業-服務及銷售工作人員"                          
    ## [16] "服務業部門"                                         
    ## [17] "服務業部門-專業人員"                                
    ## [18] "服務業部門-技術員及助理專業人員"                    
    ## [19] "服務業部門-事務支援人員"                            
    ## [20] "服務業部門-技藝、機械設備操作及組裝人員"            
    ## [21] "運輸及倉儲業"                                       
    ## [22] "運輸及倉儲業-技術員及助理專業人員"                  
    ## [23] "運輸及倉儲業-事務支援人員"                          
    ## [24] "運輸及倉儲業-技藝、機械設備操作及組裝人員"          
    ## [25] "住宿及餐飲業"                                       
    ## [26] "住宿及餐飲業-技術員及助理專業人員"                  
    ## [27] "住宿及餐飲業-服務及銷售工作人員"                    
    ## [28] "住宿及餐飲業-技藝、機械設備操作及組裝人員"          
    ## [29] "資訊及通訊傳播業"                                   
    ## [30] "資訊及通訊傳播業-專業人員"                          
    ## [31] "資訊及通訊傳播業-技術員及助理專業人員"              
    ## [32] "資訊及通訊傳播業-事務支援人員"                      
    ## [33] "資訊及通訊傳播業-服務及銷售工作人員"                
    ## [34] "金融及保險業-事務支援人員"                          
    ## [35] "不動產業-專業人員"                                  
    ## [36] "專業、科學及技術服務業"                             
    ## [37] "專業、科學及技術服務業-專業人員"                    
    ## [38] "專業、科學及技術服務業-技術員及助理專業人員"        
    ## [39] "專業、科學及技術服務業-服務及銷售工作人員"          
    ## [40] "專業、科學及技術服務業-技藝、機械設備操作及組裝人員"
    ## [41] "支援服務業-技術員及助理專業人員"                    
    ## [42] "支援服務業-服務及銷售工作人員"                      
    ## [43] "支援服務業-技藝、機械設備操作及組裝人員"            
    ## [44] "教育服務業"                                         
    ## [45] "教育服務業-專業人員"                                
    ## [46] "教育服務業-技術員及助理專業人員"                    
    ## [47] "教育服務業-事務支援人員"                            
    ## [48] "教育服務業-服務及銷售工作人員"                      
    ## [49] "醫療保健服務業-技術員及助理專業人員"                
    ## [50] "醫療保健服務業-技藝、機械設備操作及組裝人員"        
    ## [51] "藝術、娛樂及休閒服務業"                             
    ## [52] "藝術、娛樂及休閒服務業-事務支援人員"                
    ## [53] "藝術、娛樂及休閒服務業-技藝、機械設備操作及組裝人員"
    ## [54] "其他服務業"                                         
    ## [55] "其他服務業-專業人員"                                
    ## [56] "其他服務業-技術員及助理專業人員"                    
    ## [57] "其他服務業-事務支援人員"                            
    ## [58] "其他服務業-技藝、機械設備操作及組裝人員"

上一步已經算完成長率了，所以取出來就好了

### 主要的職業種別是哪些種類呢?

``` r
main.class = function(x){
  temp = strsplit(x,'-')
  x = temp[[1]][1]
  return(x)
}
data2$class = sapply(data2$class,main.class)
table(data2$class)
```

    ## 
    ##       工業及服務業部門               工業部門               不動產業 
    ##                      1                      1                      1 
    ##             支援服務業   用水供應及污染整治業           住宿及餐飲業 
    ##                      3                      6                      4 
    ##             其他服務業             服務業部門           金融及保險業 
    ##                      5                      5                      1 
    ## 專業、科學及技術服務業             教育服務業       資訊及通訊傳播業 
    ##                      5                      5                      5 
    ##           運輸及倉儲業       電力及燃氣供應業                 製造業 
    ##                      4                      2                      1 
    ##                 營造業         醫療保健服務業 藝術、娛樂及休閒服務業 
    ##                      3                      2                      3 
    ##       礦業及土石採取業 
    ##                      1

取’-’號前面的字串，然後算數量

## 男女同工不同酬現況分析

男女同工不同酬一直是性別平等中很重要的問題，分析資料來源為103到106年度的大學畢業薪資。

### 103到106年度的大學畢業薪資資料，哪些行業男生薪資比女生薪資多?

``` r
#這是R Code Chunk
```

### 哪些行業女生薪資比男生薪資多?

``` r
#這是R Code Chunk
```

## 研究所薪資差異

以106年度的資料來看，哪個職業別念研究所最划算呢 (研究所學歷薪資與大學學歷薪資增加比例最多)?

``` r
#這是R Code Chunk
```

## 我有興趣的職業別薪資狀況分析

### 有興趣的職業別篩選，呈現薪資

``` r
#這是R Code Chunk
```

### 這些職業別研究所薪資與大學薪資差多少呢？

``` r
#這是R Code Chunk
```
