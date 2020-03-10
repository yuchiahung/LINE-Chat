library(jiebaR)
library(stringr)
library(tidyr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(wordcloud2)

# 讀檔
chat <- readLines("chat.txt", encoding = "UTF-8")
seg <- worker() 
df_chat <- data.frame(chat) 

# 取出日期
df <- separate(df_chat, chat, c("time", "name", "content"), sep = "\t") %>% 
  mutate(date = "")

for (i in seq_len(nrow(df))) {
  if (str_detect(df[i, "time"], pattern = "\\d{2}/\\d{2}/\\d{4}") == TRUE) {
    df$date[i] <- str_extract(df[i, "time"], pattern = "\\d{2}/\\d{2}/\\d{4}")
  } else {
    df$date[i] <- df$date[i-1]
  }
}

df <- df %>% 
  mutate(Time = as.POSIXct(paste(df$date, df$time), format = "%d/%m/%Y %H:%M"))

# 把日期轉換成class 是 Date
df <- df %>% 
  mutate(Date = as.Date(df$date, "%d/%m/%Y"))

# remove NA
df <- df[complete.cases(df), ]
# remove date
df <- df[, -c(1,4)]

# 新增一欄切割的
df <- df %>% mutate(segged = "")

for (i in seq_len(nrow(df))){
  segged <- segment(df$content[i], seg)
  df$segged[i] <- paste0(segged, collapse = " ")
}

# customize stop words
custom_stop_words <- tribble(
  # Column names should match stop_words
  ~word, ~lexicon,
  # Add 阿啊 as custom stop words
  "啊", "CUSTOM",
  "阿", "CUSTOM",
  "的", "CUSTOM",
  "了", "CUSTOM",
  "嗎", "CUSTOM",
  "吧", "CUSTOM",
  "那", "CUSTOM",
  "就", "CUSTOM",
  "是", "CUSTOM",
  "要", "CUSTOM",
  "喔", "CUSTOM",
  "我", "CUSTOM",
  "你", "CUSTOM",
  "妳", "CUSTOM",
  "他", "CUSTOM",
  "但", "CUSTOM",
  "在", "CUSTOM",
  "欸", "CUSTOM",
  "哈", "CUSTOM",
  "沒有", "CUSTOM",
  "有", "CUSTOM",
  "很", "CUSTOM",
  "哦", "CUSTOM"
)

# 變成tidytext format 把詞提出來
tidy_text_format <- df %>%
  unnest_tokens(output = "word", input = "content") %>% 
  anti_join(custom_stop_words)

# 兩人詞頻 (由高到低排序)
tidy_text <- tidy_text_format %>% 
  group_by(name, word) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(ranking = dense_rank(desc(n)))
"""
> head(tidy_text)
# A tibble: 6 x 5
# Groups:   name [2]
name  word         n   prop ranking
<chr> <chr>    <int>  <dbl>   <int>
  1 girl  stickers  3063 0.0362       1
2 girl  哈哈      2580 0.0305       2
3 boy   喔喔      1318 0.0230       1
4 boy   齁        1151 0.0201       2
5 girl  好        1140 0.0135       3
6 boy   stickers  1007 0.0176       3
"""
# 某一期間的詞頻
frequency_period <- function(text, date1, date2){
  arranged <- text %>% 
    filter(Date > as.Date(date1) & Date < as.Date(date2)) %>% 
    group_by(name, word) %>% 
    summarise(n = n()) %>% 
    arrange(desc(n)) %>% 
    mutate(prop = n / sum(n)) %>% 
    mutate(fre = n / as.numeric(as.Date(date2)-as.Date(date1)) *100) # 每一百天說幾次
  
  return(arranged)
}

# 約會前詞頻
before_dating <- frequency_period(tidy_text_format, "2016-12-01", "2017-09-29")

# 約會後詞頻
after_dating <- frequency_period(tidy_text_format, "2017-09-29", "2020-01-01")

### 熱戀前及後各用最多次/最高頻的字詞

# 熱戀期 20170929-20180228 (152 days)
in_love <- frequency_period(tidy_text_format, "2017-09-29", "2018-02-28")

fre_in_love <- in_love[, c(1,2,5)] %>% 
  spread(name, fre) %>% 
  mutate(sum = boy + girl) %>% 
  arrange(desc(sum))
# 熱戀期後 20180228-20191209 (649 days)
after_love <- frequency_period(tidy_text_format, "2018-02-28", "2019-12-09")
fre_after_love <- after_love[, c(1,2,5)] %>% 
  spread(name, fre) %>% 
  mutate(sum = boy + girl) %>% 
  arrange(desc(sum))

# 熱戀期 前15名高頻詞(dodge)
in_love[(in_love$word %in% top_n(fre_in_love, 15, sum)$word),] %>% 
  ggplot() +
  geom_bar(aes(reorder(word, fre), fre, fill = name), stat = "identity", position = "dodge") +
  scale_x_reordered() +
  labs(title = "熱戀期 最高頻的前15個詞", x = "詞彙", y = "次/100天")+
  coord_flip()
# 熱戀期 前15名高頻詞(stack)
in_love[(in_love$word %in% top_n(fre_in_love, 15, sum)$word),] %>% 
  ggplot() +
  geom_bar(aes(reorder(word, fre), fre, fill = name), stat = "identity") +
  scale_x_reordered() +
  labs(title = "熱戀期 最高頻的前15個詞", x = "詞彙", y = "次/100天")+
  coord_flip()
# 熱戀期後 前15名高頻詞(dodge)
after_love[(after_love$word %in% top_n(fre_after_love, 15, sum)$word),] %>% 
  ggplot() +
  geom_bar(aes(reorder(word, fre), fre, fill = name), stat = "identity", position = "dodge") +
  scale_x_reordered() +
  labs(title = "熱戀期後 最高頻的前15個詞", x = "詞彙", y = "次/100天")+
  coord_flip()
# 熱戀期後 前15名高頻詞(stack)
after_love[(after_love$word %in% top_n(fre_after_love, 15, sum)$word),] %>% 
  ggplot() +
  geom_bar(aes(reorder(word, fre), fre, fill = name), stat = "identity") +
  scale_x_reordered() +
  labs(title = "熱戀期後 最高頻的前15個詞", x = "詞彙", y = "次/100天")+
  coord_flip() +
  theme(text=element_text(family = "黑體-繁 細體", size = 9))

# 兩人綜合最高頻20字
tidy_text_format %>%
  count(word) %>%
  mutate(word = reorder(word, n)) %>%   # 依照 n 排序文字
  top_n(20, n) %>%                      # 取 n 排名前 20 者
  ggplot() +
  geom_bar(aes(word, n), stat = "identity", fill = "skyblue") +
  coord_flip() +
  theme(text=element_text(family = "黑體-繁 細體", size = 9))

### boy before & after 選定幾個同樣詞的比較

# 熱戀前後這幾個特別的詞 詞頻差別
special <- function(data){
  r <- data[str_detect(data$word, "(^heart|最愛|^愛$|晚安|早安|^胖$|胖子|love|^陪|^正$)"), c('name', 'word', 'fre')]
  return(r)
}
# name, word, fre.in, fre.after
love_special <- full_join(special(in_love), special(after_love), by = c("name","word"), suffix= c(".in",".after")) %>% 
  replace_na(list(fre.in = 0, fre.after = 0)) %>% 
  mutate(fre_dif = fre.after - fre.in)

# plot
love_special %>% 
  gather(love, fre, fre.in, fre.after) %>% 
  ggplot() +
  geom_bar(aes(reorder(word, fre_dif), fre_dif, fill = name), stat = "identity", position = "dodge") +
  scale_x_reordered() +
  labs(title = "熱戀期前後特定字詞使用頻率比較", x = "字詞", y = "熱戀期後-熱戀期前(次/100天)") +
  coord_flip() +
  theme(text=element_text(family = "黑體-繁 細體", size = 9))+
  theme(aspect.ratio = 0.5)

# plot
love_special %>% 
  gather(love, fre, fre.in, fre.after) %>% 
  ggplot() +
  geom_bar(aes(reorder(word, fre), fre, fill = love), stat = "identity", position = "dodge") +
  scale_x_reordered() +
  labs(title = "熱戀期前後特定字詞使用頻率比較", x = "字詞", y = "次/100天") +
  facet_wrap(~name) +
  coord_flip() +
  theme(text=element_text(family = "黑體-繁 細體", size = 9))

### 熱戀前後頻率差最多的字詞

# 各字詞熱戀前後頻率差/比例差
love_total <- full_join(in_love[, -3], after_love[, -3], by = c("name", "word"), suffix= c(".in",".after")) %>% 
  replace_na(list(fre.in = 0, fre.after = 0)) %>% 
  mutate(fre_dif = fre.after - fre.in) %>% 
  mutate(prop_dif = prop.after - prop.in) 

# plot 比例差最多的20個字詞
love_total %>% 
  arrange(desc(abs(prop_dif))) %>% 
  head(20) %>% 
  ggplot() +
  geom_bar(aes(reorder(word, prop_dif),prop_dif, fill = name), stat = "identity", position = "dodge") +
  scale_x_reordered() +
  labs(title = "熱戀期前後使用比例差最多的字詞", x = "字詞", y = "熱戀期後-熱戀期使用比例") +
  coord_flip() +
  theme(text=element_text(family = "黑體-繁 細體", size = 9))+
  theme(aspect.ratio = 0.5)

### 遠距(spain) v.s. 校內

far <- frequency_period(tidy_text_format, "2018-08-27", "2019-07-11")
near <- frequency_period(tidy_text_format, "2018-02-26", "2018-07-29")

distance_total <- full_join(far[, -3], near[, -3], by = c("name", "word"), suffix= c(".far",".near")) %>% 
  replace_na(list(fre.far = 0, fre.near = 0)) %>% 
  mutate(fre_dif = fre.far - fre.near) %>% 
  mutate(prop_dif = prop.far - prop.near) 

# plot 比例差最多的20個字詞
distance_total %>% 
  arrange(desc(abs(prop_dif))) %>% 
  head(20) %>% 
  ggplot() +
  geom_bar(aes(reorder(word, prop_dif),prop_dif, fill = name), stat = "identity", position = "dodge") +
  scale_x_reordered() +
  labs(title = "遠距與否 使用比例差最多的字詞", x = "字詞", y = "遠距-非遠距使用比例") +
  coord_flip() +
  theme(text=element_text(family = "黑體-繁 細體", size = 9))+
  theme(aspect.ratio = 0.5)

### 過多久之後回

# 兩人回覆時間
time_girl = c()
time_boy = c()

for (i in seq(nrow(df)-1)){
  if (df$name[i] == "boy" & df$name[i+1] == "girl"){
    time_girl <- append(time_girl, as.numeric(df$Time[i+1] - df$Time[i], units = "mins"))
    names(time_girl)[length(time_girl)] <- as.character(df$Date[i])
    next
  } else if ((df$name[i] == "girl") & (df$name[i+1] == "boy")) {
    time_boy <- append(time_boy, as.numeric(df$Time[i+1] - df$Time[i], units = "mins"))
    names(time_boy)[length(time_boy)] <- as.character(df$Date[i])
  }
}

df_time_girl <- data.frame(time_girl, date = as.Date(names(time_girl)))
df_time_boy <- data.frame(time_boy, date = as.Date(names(time_boy)))

# girl 

df_time_girl <- cbind(df_time_girl, str_split_fixed(df_time_girl$date, "-", n = 3))
names(df_time_girl) <- c("time", "date", "year", "month", "day")

y <- 0
m <- 0
average_list <- "" # 每日回復時間
m_list <- "" # 年+月
m_average_list <- "" #所有的月平均list
d_time_list <- "" # 這個月的回覆list

for (i in 1:nrow(df_time_girl)){
  if (m == df_time_girl$month[i] & y == df_time_girl$year[i]){
    d_time_list <- append(d_time_list, df_time_girl$time[i])
  } else {
    y <- df_time_girl$year[i]
    m <- df_time_girl$month[i]
    m_average <- round(mean(d_time_list),1) # 前一個月平均
    m_average_list <- append(m_average_list, m_average) # 放到所有的月平均list
    m_list <- append(m_list, paste(df_time_girl$year[i-1], df_time_girl$month[i-1], sep="-")) # 前一個月的年+月
    d_time_list <- df_time_girl$average[i] # 這日的回復時間放到 這個月的回覆list
  }
}
m_list <- append(m_list, "2019-12")

m_average_girl <- data.frame(date = m_list[-1], m_average = m_average_list[-1])
m_average_girl <- m_average_girl[-1, ]

# boy

# 每月回復時間平均
# 年月日分開各成一欄
df_time_boy <- cbind(df_time_boy, str_split_fixed(df_time_boy$date, "-", n = 3))
names(df_time_boy) <- c("time", "date", "year", "month", "day")

y <- 0
m <- 0
average_list <- "" # 每日回復時間
m_list <- "" # 年+月
m_average_list <- "" #所有的月平均list
d_time_list <- "" # 這個月的回覆list

for (i in 1:nrow(df_time_boy)){
  if (m == df_time_boy$month[i] & y == df_time_boy$year[i]){
    d_time_list <- append(d_time_list, df_time_boy$time[i])
  } else {
    y <- df_time_boy$year[i]
    m <- df_time_boy$month[i]
    m_average <- round(mean(d_time_list),1) # 前一個月平均
    m_average_list <- append(m_average_list, m_average) # 放到所有的月平均list
    m_list <- append(m_list, paste(df_time_boy$year[i-1], df_time_boy$month[i-1], sep="-")) # 前一個月的年+月
    d_time_list <- df_time_boy$average[i] # 這日的回復時間放到 這個月的回覆list
  }
}

m_list <- append(m_list, "2019-12")
m_average_boy <- data.frame(date = m_list[-1], m_average = m_average_list[-1])
m_average_boy <- m_average_boy[-1, ]
#m_average_girl[6,2] <- as.factor(2.36363636)
# 兩人的組合起來
full_m <- full_join(m_average_girl, m_average_boy, by = "date", suffix = c(".girl", ".boy")) %>% 
  gather(name, m_average, m_average.girl, m_average.boy)
# m_average 轉成數字
full_m[, "m_average"] <- as.numeric(full_m[, "m_average"])
# 轉成Class: Date
full_m$date <- as.Date(paste0(full_m$date, "-01"))
full_m$name <- str_replace_all(full_m$name, pattern = "m_average.girl", "girl")
full_m$name <- str_replace_all(full_m$name, pattern = "m_average.boy", "boy")

ggplot(full_m) +
  geom_rect(aes(xmin = as.Date("2018-09-01"), xmax = as.Date("2019-07-11"), ymin = -Inf, ymax = Inf), fill = "wheat", alpha = 0.2) +
  geom_text(aes(x = as.Date("2019-02-01"), y = 10, label = "long distance"), size = 3) +
  geom_line(aes(date, m_average, group = name, color = name)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)) +
  ylim(0, 300) +
  xlim(as.Date("2017-06-01"), as.Date("2019-12-01")) +
  labs(title = "每月平均回覆速度(分鐘)", x = "月份", y = "平均回覆速度(分鐘)") +
  theme(text=element_text(family = "黑體-繁 細體", size = 9))


full <- full_m %>% 
  group_by(name, date) %>% 
  summarise(m = mean(m_average))

before_min <- full %>%  
  filter(date < as.Date("2018-09-01")& date >= as.Date("2017-09-01")) %>% 
  group_by(name) %>% 
  summarise(mean_min = mean(m)) 

after_min <- full %>%  
  filter(date >= as.Date("2019-08-01")) %>% 
  group_by(name) %>% 
  summarise(mean_min = mean(m)) 

ggplot(after_min) +
  geom_bar(aes(x = name, y = mean_min, fill = name), stat = "identity") +
  ylim(0,45) +
  coord_fixed(ratio = 1/8)+
  labs(title = "2019-08後回覆速度平均", y = "平均回覆時間（分鐘）") +
  theme(text=element_text(family = "黑體-繁 細體", size = 9))

ggplot(before_min) +
  geom_bar(aes(x = name, y = mean_min, fill = name), stat = "identity") +
  ylim(0,45) +
  coord_fixed(ratio = 1/8)+
  labs(title = "2018-09前回覆速度平均", y = "平均回覆時間（分鐘）") +
  theme(text=element_text(family = "黑體-繁 細體", size = 9))

### Wordcloud

# 全部文字雲（去掉哈哈和貼圖）
total <- tidy_text %>% 
  group_by(word) %>% 
  summarise(n = sum(n)) %>% 
  arrange(desc(n)) %>% 
  filter(word != "stickers" & word != "哈哈")

wordcloud2(head(total, 150))
# boy's word cloud（去掉哈哈和貼圖）
boy <- tidy_text %>% 
  filter(name == "boy") %>% 
  filter(word != "stickers" & word != "哈哈")
wordcloud2(head(boy[,c("word", "n")], 150))
# girl's word cloud（去掉哈哈和貼圖）
girl <- tidy_text %>% 
  filter(name == "girl") %>% 
  filter(word != "stickers" & word != "哈哈")
wordcloud2(head(girl[,c("word", "n")], 150))

# girl 約會前後 wordcloud

wordcloud2(girl_before_dating,size = 2, shape = 'cardioid')
wordcloud2(girl_after_dating, size = 2,color='random-dark')
# boy 熱戀期 wordcloud
wordcloud2(boy_in_love, size = 2, shape = 'cardioid')