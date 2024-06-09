##### 데이터 불러오기 #####
install.packages("readxl")
library(readxl)
setwd('~/Survey-Research-Analysis/')
df = read_xlsx('data.xlsx') ; str(df) ; View(df) 
##### 0.결측치 제거 #####
# Data Editing (통계학) == Data Preprocessing
# 제일 먼저 변수별 종류를 봐서 잘못 압력된 값이 있는지 확인 ex) 남녀 => 1,2 군필 => _,1,2
# 응답에 이상이 있으면 Missing 대답 안한 경우로 
# 응답의 이상 : 문항간 상호관계성 이상
# 1) 팩터를 보고 이상치 확인
for (col in c(1:23, '20-1')) {
  col_name = paste("Q", col, sep = "")
  print(levels(as.factor(df[[col_name]])))
}
# Q3, Q6, Q7, Q8, Q10, Q15, Q19 는 팩터의 수준에 "."이 포함되어있음
# . 이라고 입력된 경우는 무응답으로 간주하고 데이터에서 제외
df = subset(df, Q3 != '.' & Q6 != '.' & Q7 != '.' & Q8 != '.' & Q10 != '.' & Q15 != '.' & Q19 != '.')
for (col in c(3,6,7,8,10,15,19)) {
  col_name = paste("Q", col, sep = "")
  print(levels(as.factor(df[[col_name]])))
}
# chr 을 integer 로 변환
for (col in c(3,6,7,8,10,15,19)) {
  col_name = paste("Q", col, sep = "")
  df[[col_name]] = as.numeric(df[[col_name]])
}
str(df)
##### 1. 조사 개인 특성 파악 #####
# 응답자가 모집단을 대표할 수 있는지
# Q20, Q20-1, Q21, Q22, Q23
# Load ggplot2 & 맥에서 한글 깨짐 현상 해결
library(ggplot2)
library(RColorBrewer)
# 맥에서 한글 깨짐 현상 해결해주는 함수
#theme_set(theme_grey(base_family='NanumGothic'))
#par(family='AppleGothic')

# pie 차트 그리는 함수 
# TODO : 그래프 정렬, 차트 영역내 비율 추가 필요 
plot_pie_chart <- function(df, column_name, category_names=NULL, title) {
  # 주어진 열에 해당하는 데이터 가져오기
  column_data <- df[[column_name]]
  
  # 데이터의 빈도수 계산
  frequency <- table(column_data)
  
  pal = brewer.pal(8, "Dark2")
  # 파이 차트 그리기
  pie_chart <- ggplot(data = NULL, aes(x = factor(1), fill = names(frequency), y = as.vector(frequency))) +
    geom_bar(stat = "identity") +
    coord_polar("y", start = 0) +
    labs(title = paste(title,"(", column_name,")"), x = NULL, y = NULL) +
    scale_fill_manual(values = pal, labels = category_names) +  # 범주 이름 및 색상 지정
    theme_void() +
    theme(legend.position = "right") + 
    theme(
      # 한글 글꼴 적용
      text = element_text(family = 'NanumGothic'),  # 모든 텍스트에 적용
      legend.text = element_text(family = 'NanumGothic'),  # 범례 텍스트에 적용
      plot.title = element_text(family = 'NanumGothic')  # 그래프 제목에 적용
    )
  
  # 그래프 출력
  print(pie_chart)
}

plot_bar_chart <- function (df, column_name) {
  data <- df[[column_name]]
  frequency  = table(data)
  pal = brewer.pal(8, "Dark2")
  barplot(frequency, 
          col= pal, 
          border="white", 
          font.axis=2, 
          beside=T, 
          legend=rownames(data), 
          xlab="group", 
          font.lab=2)
}

plot_pie_chart(df, column_name="Q20", category_names=c("남성", "여성"), title="응답자 성별 비율")
plot_pie_chart(df[df$Q20==1,], column_name="Q20-1", category_names=c("군필", "미필"), title="응답자 군필/미필 비율")
plot_pie_chart(df, column_name="Q21", category_names=levels(as.factor(df[["Q21"]])), title="응답자 계열 비율")
plot_pie_chart(df, column_name="Q22", category_names=levels(as.factor(df[["Q22"]])), title="응답자 학번 비율")
plot_bar_chart(df, column_name = "Q22")
plot_pie_chart(df, column_name="Q23", category_names=c("무교","천주교","기독교","불교","기타"), title="응답자 종교 비율")


##### 2. 척도 방향성을 통일시켜야 한다 #####
# 성역할에 대해 긍정 : 성역할 평등 / 부정 : 성역할은 정해짐
# Q6 : 긍정
# Q7 : 긍정
# Q8 : 긍정
# Q9 : 부정
# Q10 : 부정
# Q11 : 긍정
# Q12 : 부정
# 성인지에 대해 긍정 : 개방적 / 부정 : 보수적
# Q13 : 긍정
# Q14 : 긍정
# Q15 : 긍정
# Q16 : 긍정
# Q17 : 긍정
# Q18 : 긍정
# Q19 : 긍정
# Q9, Q10, Q12 는 척도를 구성하는 항목들과 방향성이 다르므로
df$Q9 = 6-df$Q9
df$Q10 = 6-df$Q10
df$Q12 = 6-df$Q12
##### 3. 문항별 평균 비교 #####
# 문항별 평균 비교
means_of_data = function (df, column_names) {
  m = c()
  for (col_name in column_names){
    m = c(m,mean(df[[col_name]]))
  }
  return(m)
}
cols = c()
for (i in 1:19) {
  cols=c(cols,paste("Q",i,sep=''))
}
means = means_of_data(df, cols) ; means
names(means) = cols ; means
pal = brewer.pal(8, "Dark2")
barplot(means, 
        col= pal, 
        border="white", 
        font.axis=2, 
        beside=T, 
        legend=rownames(data), 
        xlab="group", 
        font.lab=2)
##### 4. 척도의 구성 및 종합점수 집계 #####
# 척도의 구성
sum_of_columns = function(df,column_names){
  res = df[[column_names[1]]]
  for (col_name in column_names[2:length(column_names)]) {
    res = res + df[[col_name]]
  }
  return(res)
}
# 성역할에 대한 척도 점수
cols = c()
for (i in 6:12) {
  cols=c(cols,paste("Q",i,sep=''))
}
df$rule = sum_of_columns(df,cols) / (5 * length(cols)) * 100
#성의식에 대한 개방성 척도 점수
cols = c()
for (i in 13:19) {
  cols=c(cols,paste("Q",i,sep=''))
}
df$open = sum_of_columns(df,cols) / (5 * length(cols)) * 100
# 성의식에 대한 종합점수
cols = c()
for (i in 6:19) {
  cols=c(cols,paste("Q",i,sep=''))
}
df$tot = sum_of_columns(df,cols) / (5 * length(cols)) * 100
##### 5. 척도 및 종합점수 기초통계량 및 시각화 #####
summary(df$open) ; summary(df$open) ; summary(df$tot)
library(psych)
describe(df$rule) ; describe(df$open) ; describe(df$tot)
library(tidyr)
# 주어진 열들에 해당하는 데이터 가져오기
selected_data <- df[, c("rule","open","tot"), drop = FALSE]

# 데이터프레임 재구성
df_plot <- gather(selected_data, key = "Variable", value = "Value")

# 박스플롯 그리기
ggplot(df_plot, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  labs(title = "두 척도와 종합점수에 대한 박스플롯", x = "Columns", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(
    # 한글 글꼴 적용
    text = element_text(family = 'NanumGothic'),  # 모든 텍스트에 적용
    legend.text = element_text(family = 'NanumGothic'),  # 범례 텍스트에 적용
    plot.title = element_text(family = 'NanumGothic')  # 그래프 제목에 적용
  )
##### 6. 크롬파흐 알파로 문항을 선택,재외 (신뢰성) #####
install.packages("psych")
library(psych)
# 성역할에 대한 내적일치도 검사 결과 0.56 < 0.6 이므로
# 서로 같은 개념을 나타내는지에 대한 수치인 내적일치도가 비교적 낮다고 볼 수 있고,
# 성역할을 나타내기에 척도의 신뢰성이 부족하다고 볼 수 있다.
# 대신에 Q12 : 남성과 여성의 흡연에 대한 문항을 제외할 때 0.6까지 오르므로
# Q6~11은 성역할에 대한 척도로 활용될 수 있다.
cols = c()
for (col in 6:12) {
  col_name = paste("Q",col,sep='')
  cols = c(cols,col_name)
}
alpha(df[,cols])
# 성인지의 인식에 대한 내적일치도 검사 결과 0.57 < 0.6 이므로
# 성인지 인식에 대한 개념을 측정하기에 신뢰성이 부족하다고 볼 수 있다.
# 그러나 어떤 변수를 제거하더라도 알파 값이 떨어지므로 
# Q13~19는 성인지 인식에 대한 개념 측정을 위해 채택될 수 있다.
cols = c()
for (col in 13:19) {
  col_name = paste("Q",col,sep='')
  cols = c(cols,col_name)
}
alpha(df[,cols])

##### 7. 기타 세부적인 분석 #####
# 성별 종합점수 평균
t.test(tot~Q20,df)
# 남성
male <- df[df$Q20==1, c("rule","open","tot"), drop = FALSE]
df_plot <- gather(male, key = "Variable", value = "Value")
ggplot(df_plot, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  labs(title = "남성의 두 척도와 종합점수에 대한 박스플롯", x = "Columns", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(
    # 한글 글꼴 적용
    text = element_text(family = 'NanumGothic'),  # 모든 텍스트에 적용
    legend.text = element_text(family = 'NanumGothic'),  # 범례 텍스트에 적용
    plot.title = element_text(family = 'NanumGothic')  # 그래프 제목에 적용
  )

# 여성
female <- df[df$Q20==1, c("rule","open","tot"), drop = FALSE]
df_plot <- gather(female, key = "Variable", value = "Value")
ggplot(df_plot, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  labs(title = "여성의 두 척도와 종합점수에 대한 박스플롯", x = "Columns", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(
    # 한글 글꼴 적용
    text = element_text(family = 'NanumGothic'),  # 모든 텍스트에 적용
    legend.text = element_text(family = 'NanumGothic'),  # 범례 텍스트에 적용
    plot.title = element_text(family = 'NanumGothic')  # 그래프 제목에 적용
  )

# 군필/미필 종합점수 평균
t.test(tot~`Q20-1`,df)
# 군필
military_ok <- df[df$`Q20-1`==1, c("rule","open","tot"), drop = FALSE]
df_plot <- gather(military_ok, key = "Variable", value = "Value")
ggplot(df_plot, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  labs(title = "군필의 두 척도와 종합점수에 대한 박스플롯", x = "Columns", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(
    # 한글 글꼴 적용
    text = element_text(family = 'NanumGothic'),  # 모든 텍스트에 적용
    legend.text = element_text(family = 'NanumGothic'),  # 범례 텍스트에 적용
    plot.title = element_text(family = 'NanumGothic')  # 그래프 제목에 적용
  )

# 미필
military_not_yet <- df[df$`Q20-1`==2, c("rule","open","tot"), drop = FALSE]
df_plot <- gather(military_not_yet, key = "Variable", value = "Value")
ggplot(df_plot, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  labs(title = "미필의 두 척도와 종합점수에 대한 박스플롯", x = "Columns", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(
    # 한글 글꼴 적용
    text = element_text(family = 'NanumGothic'),  # 모든 텍스트에 적용
    legend.text = element_text(family = 'NanumGothic'),  # 범례 텍스트에 적용
    plot.title = element_text(family = 'NanumGothic')  # 그래프 제목에 적용
  )

##### . 구성체 타당성 확인 (타당성) #####
# 모든 질문들의 상관계수를 구하고 성역할,성에 대한 개방성끼리 상관계수가 높고 반대의 경우는 낮아야 한다.
# 척도 구성시 신뢰성 검증 결과를 반영하여 뺴고 구성 (신뢰성 검증 결과는 보여줄 필요 x 부록에 넣자)
# 보고서에 결과 화면은 부록에 ex) t.test 결과 
# 집단비교 분석 시 박스플롯으로 비교 하지말고 t.test와 p값을 보고 낮은지 판단
# 근데 왜 4개의 척도가 나옴??
# 1. 서론 : 조사개요, ...
# 2. 기초분석
#   2-1. 응답자 특성 분석
#   2-2. 문항별 만족도
#   2-3. 척도구성
#     2-3-1. 신뢰도 분석
#     2-3-2. (구성체) 타당성 분석
#     2-3-3. ...따라서, 최종 척도 구성
#   2-4. 척도별 기초통계
#     2-4-1. 종합점수 분석 (평균과 신뢰구간(표준편차는 x), 신뢰구간은 낮아야함)
#     2-4-2. 척도별 만족도 (종합만족도와 비교하는 것도 좋음)
#     ...
# 3. 고급분석 (개인 신상 등 토대로 그룹을 지어 특성이 같은지 비교하는 비교분석)
#   3-1. 종합만족도
#     (p-value = 0.001 이면 집단간 차이가 유의미, 근거로 p값 제시, 코드 결과 포함 x, 박스플롯 정도만, p값의 기준)
#     3-1-1. 성별
#     3-1-2. 학번별
#     3-1-3. ....별
#     ...
#   3-2. 전공 만족도
#     ...
#   ...


# 보고서 구성시 
# 양상(분포, 평균이 의미하는 바) 설명 후 그래프를 제시
# 여기까지는 기본 , +++ 추가적인 내용 (barplot 정렬, 색깔로 층 분리)

# PPT 만들때 
# 글은 맥시멈 10줄, accent color로 강조, 내가 말하고자 하는 바가 잘 드러나는 디자인 