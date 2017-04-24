shinyServer(
  
  function(input, output)  
    {

      
    
output$MAP <- renderPlot({
     {
    library(readr)
    library(readxl)
    library(ggplot2)
    library(stringr)
    library(ggthemes)
    library(plotly)
    library(RODBC)
    library(manipulate)
    library(MASS)
    library(dplyr)
    library(reshape2)   
  }
     DASconnect1<-function(a){
        hdb <- odbcConnect(dsn="DAS",uid = "sa", pwd = "@admin123")
        quary <- paste("SELECT a.REMARK, b.UNIQUE_LOT_NO,	RECORD_NO,	FAULT_YPOS,	CAMERA_NO,	FAULT_XPOS,	MAX_SIZE,	MIN_SIZE,	V_VLAUE,	DEFECT_TYPE_CODE,	INSPEC_PROD_NO,	PRODUCT_LEN,	PROD_WC_CD,	INSPEC_STATUS from LAS_DATA a , LAS_MASTER b WITH (NOLOCK) where a.IDN = b.IDN and b.UNIQUE_LOT_NO in",a)
        RAW<-sqlQuery(hdb,quary)
        quary <-"SELECT * FROM MASTER_CODE where MASTER_CODE.CODE_CLASS = '29'"
        Code <- sqlQuery(hdb,quary)
        Code<-Code[,2:5]
        start<-as.numeric(substr(unlist(levels(factor(RAW$UNIQUE_LOT_NO))),0,8))
        end<-as.numeric(substr(unlist(levels(factor(RAW$UNIQUE_LOT_NO))),0,8))+2
        PROD_WC_CD<-unlist(levels(factor(RAW$PROD_WC_CD)))
        quary <-paste("select * from QA_PARAMETER_MARKING_HIS where QA_PARAMETER_MARKING_HIS.PARAMETER_DATE between '",start, "'and'",end,"' and QA_PARAMETER_MARKING_HIS.WORK_CENTER_ID = '",PROD_WC_CD,"'", sep="")
        parameter <- sqlQuery(hdb,quary)
        close(hdb)
        parameter<-subset(parameter,PARAMETER_NAME==unlist(levels(factor(gsub("전수-","",RAW$INSPEC_STATUS)))))
        parameter<-(data.frame(names(parameter),unlist(parameter[1,])))
        names(parameter)<-c("CODE_DATA","마킹유무")
        마킹정보<-merge(Code,parameter,by=c("CODE_DATA"),all.x=TRUE)
        마킹정보<-마킹정보[,2:5]
        names(마킹정보)<-c("불량명","camerano","강약구분","마킹유무")
        마킹정보$camerano<- str_replace(마킹정보$camerano,"크로스1","크로스")
        마킹정보$마킹유무<-ifelse(마킹정보$마킹유무==1,"Y","N")  
        names(RAW)<-c("camerano","LOT이름","RECORD","Y.좌표","camera_no","X.좌표","max_size","min_size","Value","불량명code","coating_name","length","Plant","INSPEC_STATUS")
        RAW$Size <- ((as.numeric(RAW$max_size)+as.numeric(RAW$min_size))/2)
        RAW$강약구분<- ifelse(RAW$불량명code <60000 , "강" ,"약")
        camerano<-c("크로스2","크로스2","크로스2","크로스2","크로스2","미분투과","정투과","정투과","투영","투영","투영","정반사B","정반사B","정반사A","정반사A","사선경계","사선경계","사선경계","크로스1","크로스1","크로스1","크로스2","크로스2","크로스2","크로스2","크로스2","미분투과","정투과","정투과","투영","투영","투영","정반사B","정반사B","정반사A","정반사A","사선경계","사선경계","사선경계","크로스1","크로스1","크로스1")
        강약구분<-c("강","강","강","강","강","강","강","강","강","강","강","강","강","강","강","강","강","강","강","강","강","약","약","약","약","약","약","약","약","약","약","약","약","약","약","약","약","약","약","약","약","약")
        불량명code<-c("257","258","259","769","770","772","1025","1026","1281","1282","1284","1537","1538","1793","1794","2049","2050","2051","2305","2306","2307","65793","65794","65795","66305","66306","66308","66561","66562","66817","66818","66820","67073","67074","67329","67330","67585","67586","67587","67841","67842","67843")
        불량명<-c("휘점","쿠닉","군집,S/C","이물","라미눌림","S/C","점이물","선이물","백점","흑점","라인","기포(백)","이물(흑)","기포(백)","이물(흑)","백점","흑점","찍힘","휘점","쿠닉","군집,S/C","휘점","쿠닉","군집,S/C","이물","라미눌림","S/C","점이물","선이물","백점","흑점","라인","기포(백)","이물(흑)","기포(백)","이물(흑)","백점","흑점","찍힘","휘점","쿠닉","군집,S/C")
        마킹구분<-data.frame(불량명code,불량명)
        RAW$INSPEC_STATUS<-gsub("전수-","",RAW$INSPEC_STATUS)
        RAW6<-(merge((rbind((subset(RAW, 불량명code <3000)),(subset(RAW, 불량명code<69000&불량명code>=65000)))),마킹구분,by =c("불량명code"),all.x=TRUE))
        RAW7<-rbind((subset(RAW, 불량명code<65000&불량명code>=3000)),(subset(RAW, 불량명code >=69000)))
        RAW7<-mutate(RAW7,불량명=c("주기성불량")) 
        RAW<-rbind(RAW6,RAW7)
        RAW<- merge(RAW,마킹정보,by =c("camerano","강약구분","불량명"),all.x = TRUE)
        RAW$Size<-as.numeric(RAW$Size)
        RAW$Value<-as.numeric(RAW$Value)
        RAW$마킹유무<-as.character(RAW$마킹유무)
        return(RAW)
      }
     a<-paste("('",input$LOT,"')",sep="")
     a1<-data.frame(DASconnect1(a))
     a1<-subset(a1,마킹유무==input$마킹)
     a1<-subset(a1,강약구분==input$강약)
     c<-input$소자폭                                                          
     d<-input$원단길이      
     e<-input$모델TD      
     f<-input$모델MD   
     g<-input$재단열                
     h<-input$nulling            
     ggplot(data = a1,aes(x=X.좌표, y=Y.좌표/1000,colour =camerano )) + geom_point(aes(shape=마킹유무))+ geom_vline(xintercept =c(0,seq(h,(c-(c-g*e)/2),e),c))
     
    })
    
output$yield<-renderTable({
      {
    library(readr)
    library(readxl)
    library(ggplot2)
    library(stringr)
    library(ggthemes)
    library(plotly)
    library(RODBC)
    library(manipulate)
    library(MASS)
    library(dplyr)
    library(reshape2)   
  }
       DASconnect1<-function(a){
         hdb <- odbcConnect(dsn="DAS",uid = "sa", pwd = "@admin123")
         quary <- paste("SELECT a.REMARK, b.UNIQUE_LOT_NO,	RECORD_NO,	FAULT_YPOS,	CAMERA_NO,	FAULT_XPOS,	MAX_SIZE,	MIN_SIZE,	V_VLAUE,	DEFECT_TYPE_CODE,	INSPEC_PROD_NO,	PRODUCT_LEN,	PROD_WC_CD,	INSPEC_STATUS from LAS_DATA a , LAS_MASTER b WITH (NOLOCK) where a.IDN = b.IDN and b.UNIQUE_LOT_NO in",a)
         RAW<-sqlQuery(hdb,quary)
         quary <-"SELECT * FROM MASTER_CODE where MASTER_CODE.CODE_CLASS = '29'"
         Code <- sqlQuery(hdb,quary)
         Code<-Code[,2:5]
         start<-as.numeric(substr(unlist(levels(factor(RAW$UNIQUE_LOT_NO))),0,8))
         end<-as.numeric(substr(unlist(levels(factor(RAW$UNIQUE_LOT_NO))),0,8))+2
         PROD_WC_CD<-unlist(levels(factor(RAW$PROD_WC_CD)))
         quary <-paste("select * from QA_PARAMETER_MARKING_HIS where QA_PARAMETER_MARKING_HIS.PARAMETER_DATE between '",start, "'and'",end,"' and QA_PARAMETER_MARKING_HIS.WORK_CENTER_ID = '",PROD_WC_CD,"'", sep="")
         parameter <- sqlQuery(hdb,quary)
         close(hdb)
         parameter<-subset(parameter,PARAMETER_NAME==unlist(levels(factor(gsub("전수-","",RAW$INSPEC_STATUS)))))
         parameter<-(data.frame(names(parameter),unlist(parameter[1,])))
         names(parameter)<-c("CODE_DATA","마킹유무")
         마킹정보<-merge(Code,parameter,by=c("CODE_DATA"),all.x=TRUE)
         마킹정보<-마킹정보[,2:5]
         names(마킹정보)<-c("불량명","camerano","강약구분","마킹유무")
         마킹정보$camerano<- str_replace(마킹정보$camerano,"크로스1","크로스")
         마킹정보$마킹유무<-ifelse(마킹정보$마킹유무==1,"Y","N")  
         names(RAW)<-c("camerano","LOT이름","RECORD","Y.좌표","camera_no","X.좌표","max_size","min_size","Value","불량명code","coating_name","length","Plant","INSPEC_STATUS")
         RAW$Size <- ((as.numeric(RAW$max_size)+as.numeric(RAW$min_size))/2)
         RAW$강약구분<- ifelse(RAW$불량명code <60000 , "강" ,"약")
         camerano<-c("크로스2","크로스2","크로스2","크로스2","크로스2","미분투과","정투과","정투과","투영","투영","투영","정반사B","정반사B","정반사A","정반사A","사선경계","사선경계","사선경계","크로스1","크로스1","크로스1","크로스2","크로스2","크로스2","크로스2","크로스2","미분투과","정투과","정투과","투영","투영","투영","정반사B","정반사B","정반사A","정반사A","사선경계","사선경계","사선경계","크로스1","크로스1","크로스1")
         강약구분<-c("강","강","강","강","강","강","강","강","강","강","강","강","강","강","강","강","강","강","강","강","강","약","약","약","약","약","약","약","약","약","약","약","약","약","약","약","약","약","약","약","약","약")
         불량명code<-c("257","258","259","769","770","772","1025","1026","1281","1282","1284","1537","1538","1793","1794","2049","2050","2051","2305","2306","2307","65793","65794","65795","66305","66306","66308","66561","66562","66817","66818","66820","67073","67074","67329","67330","67585","67586","67587","67841","67842","67843")
         불량명<-c("휘점","쿠닉","군집,S/C","이물","라미눌림","S/C","점이물","선이물","백점","흑점","라인","기포(백)","이물(흑)","기포(백)","이물(흑)","백점","흑점","찍힘","휘점","쿠닉","군집,S/C","휘점","쿠닉","군집,S/C","이물","라미눌림","S/C","점이물","선이물","백점","흑점","라인","기포(백)","이물(흑)","기포(백)","이물(흑)","백점","흑점","찍힘","휘점","쿠닉","군집,S/C")
         마킹구분<-data.frame(불량명code,불량명)
         RAW$INSPEC_STATUS<-gsub("전수-","",RAW$INSPEC_STATUS)
         RAW6<-(merge((rbind((subset(RAW, 불량명code <3000)),(subset(RAW, 불량명code<69000&불량명code>=65000)))),마킹구분,by =c("불량명code"),all.x=TRUE))
         RAW7<-rbind((subset(RAW, 불량명code<65000&불량명code>=3000)),(subset(RAW, 불량명code >=69000)))
         RAW7<-mutate(RAW7,불량명=c("주기성불량")) 
         RAW<-rbind(RAW6,RAW7)
         RAW<- merge(RAW,마킹정보,by =c("camerano","강약구분","불량명"),all.x = TRUE)
         RAW$Size<-as.numeric(RAW$Size)
         RAW$Value<-as.numeric(RAW$Value)
         RAW$마킹유무<-as.character(RAW$마킹유무)
         return(RAW)
       }
       Yield2<-function(a,c,d,e,f,g,h){
         a <- subset(a, 마킹유무==c("Y"))  ##마킹유무구분
         a1 <-subset(a,camerano=="크로스"|camerano=="크로스1"|camerano=="정반사A"|camerano=="투영"|camerano=="슬릿반사"|camerano=="사선경계")
         잘린마킹1<- a1
         잘린마킹2<- a1
         잘린마킹3<- a1
         잘린마킹4<- a1
         잘린마킹1$Y.좌표<-잘린마킹1$Y.좌표-11
         잘린마킹2$Y.좌표<-잘린마킹2$Y.좌표-11
         잘린마킹3$Y.좌표<-잘린마킹3$Y.좌표+11
         잘린마킹4$Y.좌표<-잘린마킹4$Y.좌표+11
         잘린마킹1$X.좌표<-잘린마킹1$X.좌표-10
         잘린마킹2$X.좌표<-잘린마킹2$X.좌표+10
         잘린마킹3$X.좌표<-잘린마킹3$X.좌표-10
         잘린마킹4$X.좌표<-잘린마킹4$X.좌표+10
         잘린마킹5<-rbind(잘린마킹1,잘린마킹2,잘린마킹3,잘린마킹4)
         잘린마킹5<-mutate(잘린마킹5,마킹유무=c("C"))   
         
         a2 <-subset(a,camerano=="미분투과"|camerano=="정반사B"|camerano=="정투과"|camerano=="크로스2")
         잘린마킹1<- a2
         잘린마킹2<- a2
         잘린마킹3<- a2
         잘린마킹4<- a2
         잘린마킹1$Y.좌표<-잘린마킹1$Y.좌표-6.5
         잘린마킹2$Y.좌표<-잘린마킹2$Y.좌표-6.5
         잘린마킹3$Y.좌표<-잘린마킹3$Y.좌표+6.5
         잘린마킹4$Y.좌표<-잘린마킹4$Y.좌표+6.5
         잘린마킹1$X.좌표<-잘린마킹1$X.좌표-10
         잘린마킹2$X.좌표<-잘린마킹2$X.좌표+10
         잘린마킹3$X.좌표<-잘린마킹3$X.좌표-10
         잘린마킹4$X.좌표<-잘린마킹4$X.좌표+10
         잘린마킹6<-rbind(잘린마킹1,잘린마킹2,잘린마킹3,잘린마킹4)
         잘린마킹6<-mutate(잘린마킹6,마킹유무=c("C"))  
         
         
         Xsimulation_v1<-rbind(a,잘린마킹5,잘린마킹6)  
         
         widths<-c 
         rolllengths<-d
         modelx<-e 
         modely<-f
         lengths<-rolllengths*1000 
         col<-g
         nulling<-h
         nulling1<-(widths-col*modelx)/2
         Xsimulation_v1$ncol<-cut(Xsimulation_v1$`X.좌표`,breaks=c(0,seq(nulling,(widths-nulling1),modelx),widths),include.lowest=FALSE,right=TRUE,label=c("rnulling",as.character(seq(1,col,1)),"lnulling"))
         row<-seq(0,lengths,modely)
         length(seq(0,lengths,modely))
         Xsimulation_v1$nrow<-cut(Xsimulation_v1$`Y.좌표`,breaks=c(seq(0,lengths,modely)),include.lowest=FALSE,right=TRUE,label=c(as.character(seq(1,(length(seq(1,(lengths),modely))-1),1))))
         행<-seq(1,col,1)
         열<-seq(1,length(seq(1,(lengths),modely)),1)
         chip<-merge(행,열)
         names(chip)<-c("ncol","nrow")
         chip1 <- merge(chip,Xsimulation_v1,by =c("ncol","nrow"),all.x = TRUE)
         전체chip수량<-(col*length(seq(1,(lengths),modely)))
         수율<-sum(is.na(chip1$camerano))/(col*length(seq(1,(lengths),modely)))*100
         chip1<-mutate(chip1,마킹수율= ifelse(is.na(chip1$camerano),1,0))   
         열별수율<-data.frame(
           chip1%>%
             group_by(ncol)%>%
             summarise( 수량=length(seq(1,(lengths),modely)),
                       수율=sum(마킹수율)/length(seq(1,(lengths),modely))*100   ))
         names(열별수율)<-c("열","재단수량","수율")
         열별수율<-mutate(열별수율,양품량=(열별수율$재단수량*열별수율$수율/100))
         열별수율<-rbind(열별수율,(data.frame(열=c("총합"),재단수량=sum(열별수율$재단수량),
                                       수율=(sum(열별수율$양품량)/sum(열별수율$재단수량)*100),
                                       양품량=sum(열별수율$양품량) )))
         return(열별수율)
       }
       a<-paste("('",input$LOT,"')",sep="")
       a1<-data.frame(DASconnect1(a))
       a1<-subset(a1,마킹유무==input$마킹)
       a1<-subset(a1,강약구분==input$강약)
       c<-input$소자폭                                                          
       d<-input$원단길이      
       e<-input$모델TD      
       f<-input$모델MD   
       g<-input$재단열                
       h<-input$nulling 
       
     data.frame(Yield2(a1,c,d,e,f,g,h))     ### 위에꺼 입력 다했으면 여기 클릭 하고 Ctrl+enter 출력값(chip수량. 예상마킹수율, 열별수율)  
     
   
    })
    }
 )
  












