library(curl)
library(rvest)
library(N2H4)
options(stringsAsFactors = F)

query <- function(input, strdate1, enddate1){
    
    success <- function(res){
        cat("Request done! Status:", res$status, "\n")
        #res$content<-iconv(rawToChar(res$content),from="CP949",to="UTF-8")
        res$content<-rawToChar(res$content)
        news_data <<- c(news_data, list(res))
    }
    
    failure <- function(msg){sou
        cat("Oh noes! Request failed!", msg, "\n")
    }
    
    strDate<-as.Date(strdate1)
    endDate<-as.Date(enddate1)
    strTime<-Sys.time()
    midTime<-Sys.time()
    Today<-Sys.Date()
    dir.create("./data",showWarnings=F)
    dir.create(paste0("./data/NewsQuery_",Today),showWarnings=F)
    datD<-data.frame()
    for (i in 1:length(input)){

        for (date in strDate:endDate){
            date<-as.character(as.Date(date,origin = "1970-01-01"))
            dated<-gsub("-","",date)
            print(paste0(date," / ",input[i], "/ start Time: ", strTime," / spent Time: ", Sys.time()-midTime," / spent Time at first: ", Sys.time()-strTime))
            midTime<-Sys.time()
            pageUrli<-paste0("http://news.naver.com/main/search/search.nhn?query=",URLencode(input[i]),"&st=news.all&q_enc=EUC-KR&r_enc=UTF-8&r_format=xml&rp=none&sm=all.basic&ic=all&so=datetime.dsc&stDate=range:",dated,":",dated,"&detail=1&pd=4&start=1&display=25&startDate=",date,"&endDate=",date,"&categorycode=105")  
            trym<-0
            max<-try(getMaxPageNum(pageUrli), silent = T)
            while(trym<=5&&class(max)=="try-error"){
                max<-try(getMaxPageNum(pageUrli), silent = T)
                Sys.sleep(abs(rnorm(1)))
                trym<-trym+1
                print(paste0("try again max num: ",pageUrli))
            }
            if(max=="no result"){
                print("no naver news links this time")
                next
            }
            for (pageNum in 1:max){
                print(paste0(date," / ",input[i], " / ", pageNum,"/ start Time: ", strTime," / spent Time: ", Sys.time()-midTime," / spent Time at first: ", Sys.time()-strTime))
                midTime<-Sys.time()
                pageUrl<-paste0(pageUrli,"&page=",pageNum)
                tryp<-0
                newsList<-try(getUrlListByQuery(pageUrl), silent = T)
                while(tryp<=5&&class(newsList)=="try-error"){
                    newsList<-try(getUrlListByCategory(pageUrl), silent = T)
                    Sys.sleep(abs(rnorm(1)))
                    tryp<-tryp+1
                    print(paste0("try again max num: ",pageUrl))
                }
                if(newsList$news_links[1]=="no naver news"){ 
                    print("no naver news links this time")
                    next
                }
                
                pool <- new_pool()
                news_data <- list()
                sapply(newsList$news_links, function(x) curl_fetch_multi(x,success,failure))
                res <- multi_run()
                
                if( identical(news_data, list()) ){
                    pool <- new_pool()
                    news_data <- list()
                    sapply(newsList$news_links, function(x) curl_fetch_multi(x,success,failure))
                    res <- multi_run()
                }
                
                closeAllConnections()
                
                loc<-sapply(news_data, function(x) grepl("^http://news.naver",x$url))
                loc<-sapply(news_data, function(x) grepl("sid1=105",x$url))  ## IT/Science Category Filtering
                cont<-sapply(news_data, function(x) x$content)
                cont<-cont[loc]
                
                if(identical(cont,character(0))){ 
                    print("no naver news links this time")
                    next
                }
                
                titles<-unlist(lapply(cont,function(x) getContentTitle(read_html(x))))
                bodies<-unlist(lapply(cont,function(x) getContentBody(read_html(x))))
                presses<-unlist(lapply(cont,function(x) getContentPress(read_html(x))))
                datetime<-lapply(cont,function(x) getContentDatetime(read_html(x))[1])
                datetime<-sapply(datetime, function(x) (as.character(x)[1]))
                edittime<-lapply(cont,function(x) getContentDatetime(read_html(x))[2])
                edittime<-sapply(edittime, function(x) (as.character(x)[1]))
                
                urls<-sapply(news_data, function(x) x$url)
                urls<-urls[loc]
                
                datC<-data.frame(titles,urls,presses,datetime,edittime,bodies)
                datD<-rbind(datD,datC)
                
            }
            write.csv(datD, file=paste0("./data/NewsQuery_",Today, "/national news_", input[i],"_",date, ".csv"),row.names = F)
        }
    }
}
