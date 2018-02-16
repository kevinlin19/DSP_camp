employee92_95.melt <- melt(employee92_95, id = "目錄")
Warning message:
  attributes are not identical across measure variables; they will be dropped 
> View(employee92_95.melt)
> View(employee92_95)
> year <- c(rep(92:95,each = 64))
> employee92_95.melt <- cbind(employee92_95.melt, year)
> View(employee92_95.melt)


> data_summar <- employee92_95.melt[seq(1, 256, by = 8),]
> View(data_summar)
> data_summary <- data_summar
> View(data_summary)
> data_tmp <- data_summary
> people92_95 <- data_tmp[seq(1, 32, by = 2),]
> View(people92_95)
> people92_95[,1] <- c(rep("受僱人數",16))


t_employee92_95 <- t(employee92_95)
tmp_data <- t_employee92_95[,c(1,9)] 
type <- c(rep("住宿及餐飲業",4), rep("金融及保險業",4), rep("醫療保健服務業",4), rep("藝術、娛樂及休閒服務業",4))
tmp_data <- tmp_data[-1,]
tmp_data <- cbind(tmp_data, type)
rowname <- rownames(tmp_data)
rownames(tmp_data) <- NULL
tmp_data <- as.data.frame(tmp_data)
colnames(tmp_data) <- c("受僱人數", "薪資", "行業別")
year <- c(rep(92:95,4))
tmp_data <- cbind(tmp_data, year)
str(tmp_data)
tmp_data$行業別 <- as.character(tmp_data$行業別)

### 處理有逗點的數值
library(dplyr)
count <- function(x, y) {
  x %>%  
    as.character() %>% 
    strsplit(",") -> x
  for(i in 1:y){
    x[[i]] <- paste0(x[[i]][1], x[[i]][2])
  }
  as.numeric(unlist(x))
}

tmp_data$受僱人數 <- count(tmp_data$受僱人數, 16)
tmp_data$薪資 <- count(tmp_data$薪資, 16)

tmp_data <- bind_cols(tmp_data[,3:4], tmp_data[,1:2])



#刪除行資料並給col名字  
del <- function(x, nam, n) {
  x <- x[-c(1:2,11:14),]
  colnames(x) <- c(nam, rep("住宿及餐飲業",n), rep("金融及保險業",n), rep("醫療保健服務業",n), rep("藝術、娛樂及休閒服務業",n))
  x
}


#刪除行資料並給col名字  
del2 <- function(x, nam, n, f, t) {
  x <- x[-c(1:2,f:t),]
  colnames(x) <- c(nam, rep("住宿及餐飲業",n), rep("金融及保險業",n), rep("醫療保健服務業",n), rep("藝術、娛樂及休閒服務業",n))
  x
}

#轉置
exchange <- function(x, y = type_n, z=rept_year) {
  x <- t(x)
  x <- x[,c(1,9)]
  f_type <- c(rep("住宿及餐飲業",y), rep("金融及保險業",y), rep("醫療保健服務業",y), rep("藝術、娛樂及休閒服務業",y))
  x <- x[-1,]
  x <- cbind(x, f_type)
  rowname <- rownames(x)
  rownames(x) <- NULL
  x <- as.data.frame(x)
  colnames(x) <- c("受僱人數", "薪資", "行業別")
  year <- c(rep(z,4))
  x <- cbind(x, year)
  x$行業別 <- as.character(x$行業別)
  x
}

#employee_97_99
w <- del(employee97_99, "種類", 3)
w <- exchange(w, 3, 97:99)
w$受僱人數 <- count(w$受僱人數, 12)
w$薪資 <- count(w$薪資,12)
w <- bind_cols(w[,3:4], w[,1:2])

#employee100_101
v <- del(employee100_101, "種類", 2)
v <- exchange(v, 2, 100:101)
v$受僱人數 <- count(v$受僱人數, 8)
v$薪資 <- count(v$薪資,8)
v <- bind_cols(v[,3:4], v[,1:2])
tmp_data <- bind_rows(tmp_data,v)

#employee102-104
m <- del(employee102_104, "種類", 3)
m <- exchange(m, 3, 102:104)
m$受僱人數 <- count(m$受僱人數, 12)
m$薪資 <- count(m$薪資,12)
m <- bind_cols(m[,3:4], m[,1:2])
tmp_data <- bind_rows(tmp_data,m)

#first92_95
library(reshape2)
a <- del2(first92_95, "種類", 4, 31, 34)
t <- t(a)
t <- t[,seq(1, 22, by = 7)]
t [-1,]
rowname92_95 <- c(rep(92:95, 4))
type1 <- c("住宿及餐飲業", "金融及保險業", "醫療保健服務業", "藝術、娛樂及休閒服務業")
t <- as.data.frame(t)
colnames(t) <- type1
t$住宿及餐飲業 <- count(t$住宿及餐飲業, 16)
t$金融及保險業 <- count(t$金融及保險業, 16)
t$醫療保健服務業 <- count(t$醫療保健服務業, 16)
t$`藝術、娛樂及休閒服務業` <- count(t$`藝術、娛樂及休閒服務業`, 16)
t$year <- rownames(t)
t.melt <- melt(t, id.var = 'year', variable.name = "行業別", value.name = "初任人員薪資")

#整理first_data
sor <- function(x, a, b, c, d) {
    rowname <- c(a:b)  
    z <- x[,1:(d+1)]
    z <- z[-c(1:2),]
    z <- z[seq(1,22, by = 7),]
    z <- t(z) 
    z <- as.data.frame(z)
    z <- z[-1,]
    colnames(z) <- type1 
    rownames(z) <- rowname
    z$year <- rownames(z)
    z <- melt(z, id.var = 'year', variable.name = "行業別", value.name = "初任人員薪資")
    z$初任人員薪資 <- count(z$初任人員薪資, c)
    z
}
    
    
    z <- z[-c(1:2,31:34),]
    z <- z[,seq(1, ncol(z), by = 7)]  
    z <- z[-1,]  
    z <- as.data.frame(z) 
    z$住宿及餐飲業 <- count(z$住宿及餐飲業, c) 
    z$金融及保險業 <- count(z$金融及保險業, c) 
    z$醫療保健服務業 <- count(z$醫療保健服務業, c) 
    z$`藝術、娛樂及休閒服務業` <- count(z$`藝術、娛樂及休閒服務業`, c) 
    z$year <- rownames(z)  
    z.melt <- melt(z, id.var = 'year', variable.name = "行業別", value.name = "初任人員薪資")

    
#檔案結合做key值
    tmp_data$key <- paste0(tmp_data$行業別, tmp_data$year)
    t.melt$key <- paste0(t.melt$行業別, t.melt$year)
    tmp_data <- left_join(tmp_data, t.melt[,3:4], by = "key")
    
    
# worktime
    h <- worktime92_95[-c(1,2),-1]
    h <- as.data.frame(t(h))
    colnames(h) <- type1
    rownames(h) <- c(92:95)
    
#worktime function
    work <- function(x, a, b) {
      x <- x[-c(1,2), -1]
      x <- as.data.frame(t(x))
      colnames(x) <- type1
      rownames(x) <- c(a:b)
      x$year <- rownames(x)
      x <- melt(x, id.var = 'year', variable.name = "行業別", value.name = "工作時數")
    }
    
    worktime <- bind_rows(work(worktime92_95, 92, 95), work(worktime97_99, 97, 99), work(worktime100_101,100, 101), work(worktime102_104, 102, 104))
    tmp_data <- left_join(tmp_data, worktime[,3:4], by = "key")
a <- exchange(a, 4, 92:95)
w$受僱人數 <- count(w$受僱人數, 12)
w$薪資 <- count(w$薪資,12)
w <- bind_cols(w[,3:4], w[,1:2])