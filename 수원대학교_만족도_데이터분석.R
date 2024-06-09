##### 데이터 불러오기 #####
# install.packages("readxl")
library(readxl)
setwd('~/Survey-Research-Analysis/')
df = read_xlsx('수원대만족도데이터.xlsx') ; str(df) ; View(df) 
##### 0.Data Pre-processing #####
# 1) 컬럼명 Q1~Q46 로 변경 (타임스탬프 제거)
df[,"타임스탬프"] = NULL
cols=c()
for (col in 1:46) {
  cols = c(cols,paste("Q",col,sep=""))
}
colnames(df) = cols ; str(df)
# 2) 컬럼별 NA 값 개수 확인
check_number_of_na = function(df, range=c(1:46)){
  for (col in range) {
    col_name = paste("Q", col, sep = "")
    indicies_of_na = which(is.na(df[[col_name]]))
    num_of_na = length(indicies_of_na)
    if (num_of_na > 0){
      print(paste("Q-",col," NA 개수 : ",num_of_na,"개", sep=''))
      indicies_of_na = which(is.na(df[[col_name]]))
      if (length(indicies_of_na) > 0) print(indicies_of_na)
    }
  }
}
check_number_of_na(df)
# 9~46번까지 문항은 만족도를 설명하는 척도 변수들이므로 결측치가 있을시 해당 데이터를 제외
df = df[-c(106,105,31,394,333,317,491,471,217),]
check_number_of_na(df)
# 2) 만족도 설문 문항 데이터 수치화
answer_text_list = c("전혀 그렇지 않다","대체로 그렇지 않다","보통이다","대체로 그렇다","매우 그렇다")
answer_num_list = 1:length(answer_text_list)
for (col in cols) {
  for (i in answer_num_list)
    df[df[col]==answer_text_list[i] & !is.na(df[col]),col] = as.character(i) 
}
str(df)
# 3) 팩터를 보고 이상치 확인
check_factor = function(df, range=c(1:46)){
  for (col in range) {
    print(paste("######### Q-",col," #########", sep=''))
    col_name = paste("Q", col, sep = "")
    print(levels(as.factor(df[[col_name]])))
  }
}
check_factor(df)
# 3-1) 회화과\r -> 회화과 로 전처리
# 전처리 이유 : 특수문자 제거
df[df$Q2 == "회화과\r","Q2"] = "회화과"
check_factor(df,2)
# 3-2) 지능형sw융합대학 -> 지능형SW융합대학 로 전처리
# 전처리 이유 : 대소문자 통일
df[df$Q1 == "지능형sw융합대학","Q1"] = "지능형SW융합대학"
check_factor(df,1)
# 3-2) 인문사회융합학부 -> 인문사회융합대학 로 전처리
# 전처리 이유 : 단과대학 자리에 학부 입력됨
df[df$Q1 == "인문사회융합학부","Q1"] = "인문사회융합대학"
check_factor(df,1)
# 3-3) 전기공학과 2학년 -> 전기전자공학과 로 전처리
# 전처리 이유 : 데이터 오입력
df[df$Q2 == "전기공학과" & df$Q3 == "2학년","Q2"] = "전기전자공학과"
check_factor(df,2)
# 4) chr 을 numeric 으로 변환
for (col in 7:46) {
  col_name = paste("Q", col, sep = "")
  df[[col_name]] = as.numeric(df[[col_name]])
}
str(df)
# 5) 여성 응답자의 미필의 군필여부는 모두 NA로 통일
df[df$Q4 == "여" & !is.na(df$Q4),"Q5"] = NA
which(is.na(!is.na(df$Q4) & df$Q4 == "여" & df$Q5 != "미필"))
##### 1. 조사 개인 특성 파악 #####
# 응답자가 모집단을 대표할 수 있는지
# Q1, Q2, Q3, Q4, Q5, Q6

# pie 차트 그리는 함수 
library(dplyr)
library(ggplot2)
library(RColorBrewer)
plot_pie_chart <- function(df, column_name, title, title_of_legend) {
  df1 <- df[!is.na(df[[column_name]]),] %>% 
    group_by_at(column_name) %>%
    count() %>% 
    ungroup() %>%
    mutate(perc = `n` / sum(`n`)) %>% 
    arrange(perc) %>%
    mutate(labels = scales::percent(perc))
  
  pal = colorRampPalette( brewer.pal(8,"Dark2") )(nlevels(as.factor(df[[column_name]])))
  # 파이 차트 그리기
  pie_chart = ggplot(df1, aes(x = '', fill = .data[[column_name]], y = perc)) +
    geom_col() +
    theme_void() +
    geom_text(
      aes(label = labels),
      position = position_stack(vjust = 0.5),
      color='white',
      size=8) +
    coord_polar(theta="y") +
    labs(title = paste('<',title,'>',sep=''), x = NULL, y = NULL) +
    scale_fill_manual(values = pal) +  # 범주 이름 및 색상 지정
    guides(fill=guide_legend(title=title_of_legend)) +
    theme(
      legend.position = "right",
      # 한글 글꼴 적용
      text = element_text(family = 'NanumGothic'),  # 모든 텍스트에 적용
      legend.text = element_text(family = 'NanumGothic'),  # 범례 텍스트에 적용
      plot.title = element_text(
        colour = "black",
        size=20,
        hjust = 0.5,
        family = 'NanumGothic',
        margin=margin(30,0,0,0)
      )  # 그래프 제목에 적용
    )
  
  # 그래프 출력
  print(pie_chart)
}

par(family='NanumGothic')
plot_bar_chart <- function (df, column_name, title) {
  data <- df[[column_name]]
  frequency  = table(data)
  pal = colorRampPalette( brewer.pal(8,"Dark2") )(nlevels(as.factor(data)))
  barplot(frequency, 
          col= pal, 
          border="white", 
          font.axis=2, 
          beside=T, 
          legend=rownames(data), 
          xlab="group", 
          font.lab=2,
          main=paste('<',title,'>',sep=''))
}

# 파이차트 : 성별, 군필, 거주지, 단과대학, 학년
chart_titles = c("응답자 소속단과대학 비율","응답자 소속학과/학부 비율","응답자 학년 비율","응답자 성별 비율", "남성 응답자 군필여부 비율", "응답자 거주지 비율")
chart_titles_of_legend = c("단과대학","학과/학부","학년","성별","군필여부","거주지")
for (col in c(1,3,4,5,6)) {
  col_name = paste("Q",col,sep='')
  plot_pie_chart(df, column_name=col_name, title=chart_titles[col],title_of_legend = chart_titles_of_legend[col])
}

# 학년별 군필여부 파이 차트 그리기
df_plot = df %>% 
  filter(Q4=="남") %>% 
  group_by(Q5,Q3) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(Q5.Q3=paste(Q5,Q3,sep=',')) %>% 
  mutate(labels = scales::percent(freq))

pal = colorRampPalette( brewer.pal(8,"Dark2") )(nlevels(as.factor(df_plot$Q5))*nlevels(as.factor(df_plot$Q3)))
pie_chart = ggplot(df_plot, aes(x = '', fill = Q5.Q3, y = n)) +
  geom_col() +
  theme_void() +
  #geom_label_repel(aes(y = text_y, label = labels), size=4, show.legend = F,nudge_x = 0.6, nudge_y = 0.6) +
  coord_polar(theta="y") +
  labs(title = paste('<','학년/군필여부 비율','>',sep=''), x = NULL, y = NULL) +
  scale_fill_manual(values = pal) +  # 범주 이름 및 색상 지정
  guides(fill=guide_legend(title="군필여부,학년")) +
  theme(
    legend.position = "right",
    # 한글 글꼴 적용
    text = element_text(family = 'NanumGothic'),  # 모든 텍스트에 적용
    legend.text = element_text(family = 'NanumGothic'),  # 범례 텍스트에 적용
    plot.title = element_text(
      colour = "black",
      size=20,
      hjust = 0.5,
      family = 'NanumGothic',
      margin=margin(30,0,0,0)
    )  # 그래프 제목에 적용
  )

# 그래프 출력
print(pie_chart)

##### 2. 척도 방향성을 통일시켜야 한다 #####
# 교육과정 만족도에 대해 긍정 / 부정
# Q9 : 긍정
# Q10 : 긍정
# Q11 : 긍정
# Q12 : 긍정
# Q13 : 긍정
# Q14 : 긍정
# Q15 : 긍정
# Q16 : 긍정
# Q17 : 긍정
# Q18 : 긍정
# Q19 : 긍정
# Q20 : 긍정
# Q21 : 긍정
# 대학구성원 만족도에 대해 긍정 / 부정
# Q22 : 긍정
# Q23 : 긍정
# Q24 : 긍정
# Q25 : 긍정
# Q26 : 긍정
# Q27 : 긍정
# Q28 : 긍정
# Q29 : 긍정
# Q30 : 긍정
# Q31 : 긍정
# Q32 : 긍정
# Q33 : 긍정
# Q34 : 긍정
# 교육환경 만족도에 대해 긍정 / 부정
# Q35 : 긍정
# Q36 : 긍정
# Q37 : 긍정
# Q38 : 긍정
# Q39 : 긍정
# Q40 : 긍정
# Q41 : 긍정
# Q42 : 긍정
# Q43 : 긍정
# Q44 : 긍정
# Q45 : 긍정
# Q46 : 긍정
# 모든 문항이 긍정을 물어보는 질문이므로 응답의 방향성이 일치한다
##### 3. 문항별 평균 비교 #####
# 문항별 평균 비교
means_of_data = function (df, column_names) {
  m = c()
  for (col_name in column_names){
    m = c(m,mean(df[[col_name]]))
  }
  return(m)
}
mean_of_data = function (df, column_names) {
  s = 0
  c = 0
  for (col_name in column_names){
    s = s + sum(df[[col_name]])
    c = c + length(df[[col_name]])
  }
  return(s/c)
}
vector_of_data = function (df, column_names) {
  res = c()
  for (col_name in column_names){
    res = c(res,df[[col_name]])
  }
  return(res)
}
cols = paste0("Q",7:46)
means = means_of_data(df[!is.na(df$Q7) & !is.na(df$Q8),], cols) ; means
names(means) = cols ; means
tot_mean = mean_of_data(df[!is.na(df$Q7) & !is.na(df$Q8),],cols)
tot_vector = vector_of_data(df[!is.na(df$Q7) & !is.na(df$Q8),],cols)
pal = colorRampPalette( brewer.pal(8,"Dark2") )(length(means))
b = barplot(sort(means), 
        col= pal, 
        border="white", 
        font.axis=2, 
        beside=T, 
        legend=rownames(data), 
        xlab="문항", 
        main="<문항별 평균>",
        cex.main = 2,
        las=2)
#문항의 전체 평균에 대한 라인
abline(h=tot_mean, col = 'Blue', lty=2)
text(
  x = b,
  y = 0,
  labels = round(sort(means),4),
  pos = 3,
  srt = 90,
  offset = 1.5,
  col = 'white'
)
# Best & Worst
# Best(3.747951) : 대학구성원-교육서비스 만족도  [1. 우리 대학 교수진은 학생들의 실력향상을 위해 최선을 다한다.]
t.test(df$Q25)
# Worst(2.969262) : 교육과정 만족도 [1. 전공교육은 내가 기대한 수준의 전문지식을 제공한다.]
t.test(df$Q22)
##### 4. 크롬파흐 알파로 문항을 선택,재외 (신뢰성) #####
# install.packages("psych")
library(psych)

cols = paste0("Q",9:21)
alpha(df[,cols])

cols = paste0("Q",22:34)
alpha(df[,cols])

cols = paste0("Q",35:46)
alpha(df[,cols])

cols = paste0("Q",9:46)
alpha(df[,cols])

##### 5. 구성체 타당성 검증 #####
# 모든 질문들의 상관계수를 구하고 성역할,성에 대한 개방성끼리 상관계수가 높고 반대의 경우는 낮아야 한다.
cols = paste0("Q",9:46)
df_cor = df[,cols]
cor_df = cor(df_cor)
print(round(cor_df,1))

# 시각화
#install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(cor_df)
# 0.1324150 0.1324150 0.1569122 0.1569122 0.1810762 0.1810762 0.1854706 0.1854706 0.1913617 0.1913617 0.1948022 0.1948022
##### 요인분석 #####
df.factor <- principal(df_cor, rotate="none")
names(df.factor)
df.factor$values
plot(df.factor$values, type="b")
abline(h=1,col='Blue',lty=2)

df.Varimax = principal(df_cor, nfactors = 6, rotate="varimax")
loadings <- df.Varimax$loadings

# 각 요인별로 관련 문항 출력
for (i in 1:ncol(loadings)) {
  cat("\nFactor", i, "관련 문항:\n")
  print(sort(loadings[, i], decreasing = TRUE))
}
# 다시 신뢰성 검증
alpha(df[,paste0('Q',c(9:15))])
alpha(df[,paste0('Q',c(22:23))])
alpha(df[,paste0('Q',c(24:25))])
alpha(df[,paste0('Q',c(26:28))])
alpha(df[,paste0('Q',c(16:19))])
alpha(df[,paste0('Q',c(39:41))])
alpha(df[,paste0('Q',c(35:37))])
alpha(df[,paste0('Q',c(20:21,29:34,38,42:46))])
##### 5. 척도의 구성 및 종합점수 집계 #####
# 척도의 구성
sum_of_columns = function(df,column_names){
  res = df[[column_names[1]]]
  for (col_name in column_names[2:length(column_names)]) {
    res = res + df[[col_name]]
  }
  return(res)
}
cols = paste0('Q',c(9:15))
df$major = (sum_of_columns(df,cols) - length(cols)) / (4 * length(cols)) * 100
cols = paste0('Q',c(22:23))
df$professor = (sum_of_columns(df,cols) - length(cols)) / (4 * length(cols)) * 100
cols = paste0('Q',c(24:25))
df$student_council = (sum_of_columns(df,cols) - length(cols)) / (4 * length(cols)) * 100
cols = paste0('Q',c(26:28))
df$pride = (sum_of_columns(df,cols) - length(cols)) / (4 * length(cols)) * 100
cols = paste0('Q',c(16:19))
df$liberal = (sum_of_columns(df,cols) - length(cols)) / (4 * length(cols)) * 100
cols = paste0('Q',c(40:41))
df$enviroment = (sum_of_columns(df,cols) - length(cols)) / (4 * length(cols)) * 100
cols = paste0('Q',c(35:37))
df$convenience = (sum_of_columns(df,cols) - length(cols)) / (4 * length(cols)) * 100
cols = paste0('Q',c(20:21,29:34,38,42:46))
df$support = (sum_of_columns(df,cols) - length(cols)) / (4 * length(cols)) * 100

# 교육과정 만족도에 대한 척도 점수
cols = c()
for (i in 9:21) {
  cols=c(cols,paste("Q",i,sep=''))
}
df$curriculum = (sum_of_columns(df,cols) - length(cols)) / (4 * length(cols)) * 100
#대학 구성원 만족도에 대한 척도 점수
cols = c()
for (i in 22:34) {
  cols=c(cols,paste("Q",i,sep=''))
}
df$member = (sum_of_columns(df,cols) - length(cols)) / (4 * length(cols)) * 100
# 교육환경 만족도에 대한 척도 점수
cols = c()
for (i in 35:46) {
  cols=c(cols,paste("Q",i,sep=''))
}
df$edu_enviroment = (sum_of_columns(df,cols) - length(cols)) / (4 * length(cols)) * 100
# 만족도에 대한 종합점수
cols = c()
for (i in 9:46) {
  cols=c(cols,paste("Q",i,sep=''))
}
df$tot = (sum_of_columns(df,cols) - length(cols)) / (4 * length(cols)) * 100
##### 6. 척도 및 종합점수 기초통계량 및 시각화 #####
cols = c("major","professor","student_council","pride","liberal","enviroment","convenience","tot")
for (col in cols){
  print(col)
  print(t.test(df[[col]]))
}
hist(df$tot,probability = T,ylim = c(0,0.03)) ; lines(density(df$tot))
for (col in cols){
  print(col)
  print(summary(df[[col]]))
} 
summary(df$curriculum) ; summary(df$member); summary(df$edu_enviroment) ; summary(df$tot)
library(psych)
describe(df$curriculum) ; describe(df$member) ; describe(df$edu_enviroment) ; describe(df$tot)
for (col in cols){
  print(col)
  print(describe(df[[col]]))
} 
library(tidyr)
library(ggplot2)
library(RColorBrewer)
# 주어진 열들에 해당하는 데이터 가져오기
selected_data <- df[, c("curriculum","member","enviroment","tot"), drop = FALSE]
selected_data <- df[, cols, drop = FALSE]

# 데이터프레임 재구성
df_plot <- gather(selected_data, key = "Variable", value = "Value")

# 박스플롯 그리기
ggplot(df_plot, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  labs(title = "여덟 척도와 종합점수에 대한 박스플롯", x = "Columns", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(
    # 한글 글꼴 적용
    text = element_text(family = 'NanumGothic'),  # 모든 텍스트에 적용
    legend.text = element_text(family = 'NanumGothic'),  # 범례 텍스트에 적용
    plot.title = element_text(family = 'NanumGothic')  # 그래프 제목에 적용
  )

cols = c("major","professor","student_council","pride","liberal","enviroment","convenience","support")
means = means_of_data(df, cols) ; means
names(means) = cols
pal = colorRampPalette( brewer.pal(8,"Dark2") )(length(means))
par(family='NanumGothic')
b = barplot(sort(means), 
            col= pal, 
            border="white", 
            font.axis=1, 
            beside=T, 
            legend=rownames(data), 
            xlab="척도", 
            main="<척도별 평균>",
            cex.main = 2,
            las=1)
abline(h=mean(means), col = 'Blue', lty=2)
text(
  x = b,
  y = 0,
  labels = round(sort(means),4),
  pos = 3,
  srt = 0,
  offset = 1.5,
  col = 'White'
)
mean(means)
for (col in cols){
  print(paste(col,'mean of measures'))
  print(t.test(df[[col]],means))
}

t.test(df$curriculum) ; t.test(df$member); t.test(df$enviroment) ; t.test(df$tot)
##### 7. 기타 세부적인 분석 #####
# 성별 종합점수 평균
t.test(tot~Q4,df)
# 군필/미필 종합점수 평균
t.test(tot~Q5,df)
# 소속단과대학별 종합점수 평균 비교
t.test(tot~Q1,df)

p = c()
cols = c("major","professor","student_council","pride","liberal","enviroment","convenience","support", "tot")
for (col in cols) {
  measures = paste0('Q',seq(1,6)[-2])
  for (measure in measures){
    res = aov(df[[col]]~as.factor(df[[measure]]))
    p = c(p,summary(res)[[1]][["Pr(>F)"]][1])
  }
}
p=matrix(p,5,9)
rownames(p) = c("단과대학","학년","성별","군필여부","거주지")
colnames(p) = c("전공수업","교수진","학생회","자부심","교양","환경","편의시설","지윈","종합")
View(p)

# 학교지원에 대한 평균
df %>% 
  group_by(Q1) %>% 
  summarise(mean=mean(support)) %>% 
  arrange(mean)

df %>% 
  group_by(Q3) %>% 
  summarise(mean=mean(support)) %>% 
  arrange(mean)

# 전공수업에 대한 평균
df %>% 
  group_by(Q1) %>% 
  summarise(mean=mean(major)) %>% 
  arrange(mean)

df %>% 
  group_by(Q3) %>% 
  summarise(mean=mean(major)) %>% 
  arrange(mean)

# 교수진에 대한 평균
df %>% 
  group_by(Q1) %>% 
  summarise(mean=mean(professor)) %>% 
  arrange(mean)

df %>% 
  group_by(Q3) %>% 
  summarise(mean=mean(professor)) %>% 
  arrange(mean)

# 학생회에 대한 평균
df %>% 
  group_by(Q1) %>% 
  summarise(mean=mean(student_council)) %>% 
  arrange(mean)

df %>% 
  group_by(Q3) %>% 
  summarise(mean=mean(student_council)) %>% 
  arrange(mean)

# 자부심에 대한 평균
df %>% 
  group_by(Q1) %>% 
  summarise(mean=mean(pride)) %>% 
  arrange(mean)

df %>% 
  group_by(Q3) %>% 
  summarise(mean=mean(pride)) %>% 
  arrange(mean)

# 환경에 대한 평균
df %>% 
  group_by(Q1) %>% 
  summarise(mean=mean(enviroment)) %>% 
  arrange(mean)

df %>% 
  group_by(Q3) %>% 
  summarise(mean=mean(enviroment)) %>% 
  arrange(mean)

# 편의시설에 대한 평균
df %>% 
  group_by(Q1) %>% 
  summarise(mean=mean(convenience)) %>% 
  arrange(mean)

df %>% 
  group_by(Q3) %>% 
  summarise(mean=mean(convenience)) %>% 
  arrange(mean)

# 구쉉원에 대한 평균
df %>% 
  group_by(Q1) %>% 
  summarise(mean=mean(member)) %>% 
  arrange(mean)

df %>% 
  group_by(Q3) %>% 
  summarise(mean=mean(member)) %>% 
  arrange(mean)

# 교육환경에 대한 평균
df %>% 
  group_by(Q1) %>% 
  summarise(mean=mean(edu_enviroment)) %>% 
  arrange(mean)

df %>% 
  group_by(Q3) %>% 
  summarise(mean=mean(edu_enviroment)) %>% 
  arrange(mean)

# 종합점수에 대한 평균
df %>% 
  group_by(Q1) %>% 
  summarise(mean=mean(tot)) %>% 
  arrange(mean)

df %>% 
  group_by(Q3) %>% 
  summarise(mean=mean(tot)) %>% 
  arrange(mean)

df %>% 
  group_by(Q6) %>% 
  summarise(mean=mean(tot)) %>% 
  arrange(mean)

# 교양수업에 대한 평균
df %>% 
  group_by(Q1) %>% 
  summarise(mean=mean(liberal)) %>% 
  arrange(mean)

df %>% 
  group_by(Q3) %>% 
  summarise(mean=mean(liberal)) %>% 
  arrange(mean)
