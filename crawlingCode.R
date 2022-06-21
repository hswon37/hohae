# tripadvisor crawling code

library(rvest) #웹스크래핑용
library(stringr) #문자열 정제용
library(dplyr) #pipe

setwd('C:\\Users\\HWANG\\Documents\\R특강')

#tripadvisor 리뷰 스크래핑 함수
tripadvisor_scraper <- function(pid, url){ # url주소에서 HTML소스 가져오기
  doc <- read_html(url) 

  # 호텔명
  hotel <- doc %>%
    html_nodes("#HEADING") %>%
    html_text() %>%
    str_trim()
  
  # 작성자
  name <- doc %>%
    html_nodes("div.social-member-event-MemberEventOnObjectBlock__event_type--3njyv > span > a") %>%
    html_text() %>%
    str_trim()
  
  # 리뷰제목
  commenttitle <- doc %>%
    html_nodes("div.location-review-review-list-parts-ReviewTitle__reviewTitle--2GO9Z > a > span > span") %>%
    html_text() %>%
    str_trim()
  
  #리뷰내용
  comment <- doc %>%
    html_nodes("div.location-review-review-list-parts-ExpandableReview__containerStyles--1G0AE > div._2f_ruteS._1bona3Pu._2uD5bLZZ > div > q") %>%
    html_text() %>%
    str_trim()

  #평점
  rating <- doc %>% 
    html_nodes("div.location-review-review-list-parts-SingleReview__mainCol--1hApa > div.location-review-review-list-parts-RatingLine__container--2bjtw > div > span") %>% 
    html_attr("class")
  #평점그림 -> 숫자로 변형
  for (i in 1:5) {
    if (rating[i] == "ui_bubble_rating bubble_50") {
      rating[i] <- gsub("ui_bubble_rating bubble_50",5,rating[i])
    }else if (rating[i] == "ui_bubble_rating bubble_40") {
      rating[i] <- gsub("ui_bubble_rating bubble_40",4,rating[i])
    }else if (rating[i] == "ui_bubble_rating bubble_30") {
      rating[i] <- gsub("ui_bubble_rating bubble_30",3,rating[i])
    }else if (rating[i] == "ui_bubble_rating bubble_20") {
      rating[i] <- gsub("ui_bubble_rating bubble_20",2,rating[i])
    }else {
      rating[i] <- gsub("ui_bubble_rating bubble_10",1,rating[i])
    }
  }
  
  # 수집된 정보를 데이터로 결합
  df <- data.frame(
    pid = pid,
    hotel = hotel,
    name = name,
    commenttitle = commenttitle,
    comment = comment,
    rating=rating,
    stringsAsFactors = F
  )
  return(df)
}  


### 호텔어드바이저 리뷰 스크래핑 START!
tri_reviews <-NULL #빈 문서 파일 생성

#h3 <- 'https://www.tripadvisor.com/Hotel_Review-g294265-d1770798-Reviews-Marina_Bay_Sands-Singapore.html'

pid <-"d1770798"
hotel <- "Marina_Bay_Sands"

ourl1 <- "https://www.tripadvisor.com/Hotel_Review-g294265-"
ourl3 <- "-Reviews-or"
ourl4 <- "-Marina_Bay_Sands-Singapore.html#REVIEWS"

#리뷰 페이지 수(1~94페이지)만큼 반복 스크래핑
for (i in 0:93) { 
  temp <- NULL #임시 파일 생성
  url <- paste(ourl1, pid, ourl3,i*5, ourl4, sep='') #sep : 각 객체 사이 공백 제거 
  print (url) # 수집url check
  Sys.sleep(1) #3초간 쉬었다가 다시 수집해라
  
  temp <- tripadvisor_scraper(pid, url)
  #tripadvisor 리뷰 스크래핑 함수 적용
  tri_reviews <- rbind(tri_reviews, temp) #새 페이지 데이터를 기존 데이터와 행병합
}

write.csv(tri_reviews,paste("rdata","_",hotel,"_",pid,".csv", sep=''),row.names=FALSE)

########################

tri_reviews <-NULL #빈 문서 파일 생성

#h4 <- 'https://www.tripadvisor.com/Hotel_Review-g294265-d301583-Reviews-Raffles_Hotel_Singapore-Singapore.html'

pid <-"d301583"
hotel <- "Raffles_Hotel_Singapore"
trvws <- 4652 #리뷰수
mp <-floor(trvws/10)+1 #리뷰페이지 계산

ourl1 <- "https://www.tripadvisor.com/Hotel_Review-g294265-"
ourl3 <- "-Reviews-or"
ourl4 <- "-Raffles_Hotel_Singapore-Singapore.html#REVIEWS"

#리뷰 페이지 수(1~94페이지)만큼 반복 스크래핑
for (i in 0:93) { 
  temp <- NULL #임시 파일 생성
  url <- paste(ourl1, pid, ourl3,i*5, ourl4, sep='') #sep : 각 객체 사이 공백 제거 
  print (url) # 수집url check
  Sys.sleep(1) #1초간 쉬었다가 다시 수집해라
  
  temp <- tripadvisor_scraper(pid, url)
  #tripadvisor 리뷰 스크래핑 함수 적용
  tri_reviews <- rbind(tri_reviews, temp) #새 페이지 데이터를 기존 데이터와 행병합
  
}

write.csv(tri_reviews,paste("rdata","_",hotel,"_",pid,".csv", sep=''),row.names=FALSE)

########################
tri_reviews <-NULL #빈 문서 파일 생성

#h6 <- 'https://www.tripadvisor.com/Hotel_Review-g294265-d4727829-Reviews-The_Pod_Boutique_Capsule_Hotel-Singapore.html'

pid <-"d4727829"
hotel <- "The_Pod_Boutique_Capsule_Hotel"

ourl1 <- "https://www.tripadvisor.com/Hotel_Review-g294265-"
ourl3 <- "-Reviews-or"
ourl4 <- "-The_Pod_Boutique_Capsule_Hotel-Singapore.html#REVIEWS"

#리뷰 페이지 수(1~94페이지)만큼 반복 스크래핑
for (i in 0:93) { 
  temp <- NULL #임시 파일 생성
  url <- paste(ourl1, pid, ourl3,i*5, ourl4, sep='') #sep : 각 객체 사이 공백 제거 
  print (url) # 수집url check
  Sys.sleep(1) #3초간 쉬었다가 다시 수집해라
  
  temp <- tripadvisor_scraper(pid, url)
  #tripadvisor 리뷰 스크래핑 함수 적용
  tri_reviews <- rbind(tri_reviews, temp) #새 페이지 데이터를 기존 데이터와 행병합
}

write.csv(tri_reviews,paste("rdata","_",hotel,"_",pid,".csv", sep=''),row.names=FALSE)

########################
tri_reviews <-NULL #빈 문서 파일 생성

#h7 <- 'https://www.tripadvisor.com/Hotel_Review-g294265-d310344-Reviews-YMCA_One_Orchard-Singapore.html'

pid <-"d310344"
hotel <- "YMCA_One_Orchard"

ourl1 <- "https://www.tripadvisor.com/Hotel_Review-g294265-"
ourl3 <- "-Reviews-or"
ourl4 <- "-YMCA_One_Orchard-Singapore.html#REVIEWS"

#리뷰 페이지 수(1~94페이지)만큼 반복 스크래핑
for (i in 0:93) { 
  temp <- NULL #임시 파일 생성
  url <- paste(ourl1, pid, ourl3,i*5, ourl4, sep='') #sep : 각 객체 사이 공백 제거 
  print (url) # 수집url check
  Sys.sleep(1) #3초간 쉬었다가 다시 수집해라
  
  temp <- tripadvisor_scraper(pid, url)
  #tripadvisor 리뷰 스크래핑 함수 적용
  tri_reviews <- rbind(tri_reviews, temp) #새 페이지 데이터를 기존 데이터와 행병합
}

write.csv(tri_reviews,paste("rdata","_",hotel,"_",pid,".csv", sep=''),row.names=FALSE)

