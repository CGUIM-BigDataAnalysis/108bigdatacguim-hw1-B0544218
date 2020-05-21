library(jsonlite)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(rvest)
data104 <- read_csv("./104/104occupation.csv", locale = locale(encoding = "BIG5"))
data107 <- read_csv("./107/107occupation.csv")
data = inner_join(data104,data107,by="大職業別") 
# 把資料變成數值型態方便後續計算 
# 欄位.x代表104年 欄位.y代表107年
View(data)
data$`大學-薪資.x` = as.numeric(gsub("—|…",NA,data$`大學-薪資.x`))
data$`大學-薪資.y` = as.numeric(gsub("—|…",NA,data$`大學-薪資.y`))
data$`大學-女/男.x` = as.numeric(gsub("—|…",NA,data$`大學-女/男.x`))
data$`大學-女/男.y` = as.numeric(gsub("—|…",NA,data$`大學-女/男.y`))
data$`大學-薪資.y` = as.numeric(gsub("—|…",NA,data$`大學-薪資.y`))
data$`研究所-薪資` = as.numeric(gsub("—|…",NA,data$`研究所-薪資`))

##1
#欄位BS_increase_ratio 是 107/104大學薪資的比例
data$BS_increase_ratio = data$`大學-薪資.y`/data$`大學-薪資.x`
#將 大學薪資比例 進行降冪排序
sort_data = data[order(data$BS_increase_ratio,decreasing = T), ]
#大學薪資比例 前10高的資料
View(sort_data[1:10,c("大職業別","BS_increase_ratio")] )

#印出 大學薪資比例 大於1.05的資料 (並排除掉NA資料)
View(sort_data[
  sort_data$BS_increase_ratio > 1.05& !is.na(sort_data$BS_increase_ratio), 
  c("大職業別","BS_increase_ratio")] )

#找出大職業別中 主要的職業種別
get_string = function(dash_locate,occupation){
  return_value = NA
  if(!is.na(dash_locate)){
    #取出 從第一個字 ~ 符號(-)的前一個字
    return_value = substr(occupation, start = 1,stop=dash_locate - 1)
  }else{
    #如果沒有符號(-) 直接取 職業別
    return_value = occupation
  }
  return_value
}
#找出 欄位:大職業別 中各個row 符號(-)的位置
#str_locate 會回傳 字串中 符號第一次出現的位置和最後一次出現的位置 (這裡用[,1]取第一次出現的位置 )
dash_locate = str_locate(data$`大職業別`, "-")[,1]
occupation = data$`大職業別`
#取得主要的職業種別
main_occupation = unlist(map2(dash_locate,occupation,get_string))
table(main_occupation)

##2
#104大學-女/男薪資比例 進行升冪
as_sort_data104 = data[order(data$`大學-女/男.x`), ] 
#107大學-女/男薪資比例 進行升冪
as_sort_data107 = data[order(data$`大學-女/男.y`), ] 
#104大學-女/男薪資比例 進行降冪
de_sort_data104 = data[order(data$`大學-女/男.x`,decreasing =T ), ] 
#107大學-女/男薪資比例 進行降冪
de_sort_data107 = data[order(data$`大學-女/男.y`,decreasing =T ), ] 
# 大學男女薪資比例 = 女生薪資/男生薪資* 100%

#104男比女多的前10筆資料
filter(as_sort_data104,`大學-女/男.x` < 100)[1:10,c("大職業別","大學-女/男.x")] 
#107男比女多的前10筆資料
filter(as_sort_data107,`大學-女/男.y` < 100)[1:10,c("大職業別","大學-女/男.y")] 
#104女比男多的前10筆資料
filter(de_sort_data104,`大學-女/男.x` > 100)[1:10,c("大職業別","大學-女/男.x")] 
#107女比男多的前10筆資料
filter(de_sort_data107,`大學-女/男.y` > 100)[1:10,c("大職業別","大學-女/男.y")] 


##3
#欄位MS_BS_ratio 是 研究所-薪資/大學-薪資 的比例
data$MS_BS_ratio = data$`研究所-薪資` / data$`大學-薪資.y`
#將 MS_BS_data 進行降冪排序
MS_BS_data = data[order(data$MS_BS_ratio,decreasing = T), ]
#印出MS_BS_ratio 前10高的資料
View(MS_BS_data[1:10,c("大職業別","MS_BS_ratio")])


##4
#1111人力銀行 演算法開發工程師薪水
url = "https://www.jobsalary.com.tw/salarysummary.aspx?codeNo=140207"
salary = read_html(url) %>% html_nodes("div .avgSalaryListPart02") %>% html_text() 
bachelor_s = gsub("\t|\n|\r","",salary[3]) #大學生薪資
master_s = gsub("\t|\n|\r","",salary[4])   #研究所薪資
#做資料的清洗、型態轉換
bachelor_s = as.numeric(unlist(
                          strsplit (bachelor_s,"\\$"))[2:6])
master_s = as.numeric(unlist(
                          strsplit (master_s,"\\$"))[2:6])
# row1 : 1年以下薪資，row2 : 1~3年薪資，row3 : 3~5年薪資
# row4 : 5~7年薪資，row2 : 7年以上薪資
engineer_salary = data.frame("大學" = bachelor_s,"研究所" = master_s)
engineer_salary
engineer_salary$"研究所"
engineer_salary$differ = engineer_salary$"研究所" - engineer_salary$"大學"
View(engineer_salary)
