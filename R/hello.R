# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

aerobol <- function(day1) {
  #rm(list = ls());gc()
  pacman::p_load(tidyverse,glue,lubridate,openxlsx,leaflet.extras,terra,sf,raster,leafpop,mapview,kableExtra,kableExtra,cols4all,htmlwidgets,htmltools,common)
  day<-gsub('-','',Sys.Date())
  day0<-gsub('-','',(Sys.Date()-day1))

  station_Bol<-read.xlsx('C:/Users/Cocha/Desktop/output-cbba/AeropBolivia.xlsx')
  code_bol<-station_Bol$wmo_id
  # code_bol<-c(85284,85151,85175,85315,85214,85041,85223,85196,85032,85201,
  #             85114,85242,85293,85332,85289,85140,85043,85268,85141,85152,
  #             85153,85206,85195,85104,85247,85210,85109,85123,85245,85244,85139,
  #             85364,85154,85264,85345,85365)
  # no<-c('BAURES','MONTEAGUDO')
  # names<-c('ALCANTARI','APOLO','ASC. DE GUARAYOS','CAMIRI',
  #          'CHIMORE','COBIJA','COCHABAMBA','CONCEPCION','GUAJARAMIRIN','LA PAZ ALTO',
  #          'MAGDALENA','ORURO','POTOSI AEROPUERTO','POTOSI UYUNI',
  #          'PUERTO SUAREZ','REYES','RIBERALTA','ROBORE','RURRENABAQUE','SAN BORJA',
  #          'INS ING MOXOS','SAN ING VELASCO', 'SAN JAVIER','SAN JOAQUIN',
  #          'SAN JOSE DE CHIQUITOS','SAN MATIAS','SAN RAMON','SANTA ANA',
  #          'STA CRUZ TROMPILLO','STA CRUZ VIRU VIRU','SANTA ROSA','TARIJA',
  #          "TRINIDAD",'VALLEGRANDE','VILLAMONTES','YACUIBA')
  #code<-station_Bol$wmo_id
  date0<-as.character(seq((Sys.Date()-30),Sys.Date(),1))
  rep(NA,nrow(station_Bol)*length(date))
  dfp<-matrix(rep(NA,nrow(station_Bol)*length(date0)),ncol = nrow(station_Bol))
  dftx<-matrix(rep(NA,nrow(station_Bol)*length(date0)),ncol = nrow(station_Bol))
  dftn<-matrix(rep(NA,nrow(station_Bol)*length(date0)),ncol = nrow(station_Bol))
  #row.names(df)<-date

  code<-85223

  #df1<-data.frame(code_bol,names)
  for (z in 1:length(code_bol)) {
    code<-code_bol[z]
    print(station_Bol[which(station_Bol$wmo_id==code),2])
    a<-read.csv(glue('http://www.ogimet.com/cgi-bin/getsynop?block={code}&begin={day0}0000&end={day}2000'),sep=" ",header = F,fill = T)
    #a
    date<-paste0(gsub(',','-',substr(a$V1,7,16)),' ',substr(a$V1,18,19),':00')
    date[which(date==' :00')]<-NA
    b<-a %>% mutate(date=ymd_hm(date)-4*3600)
    b$date<-as.character(b$date)
    df0<-paste0('V',(ncol(b)+1):(ncol(b)+ncol(b)))
    df01<-rep(NA,length(df0))
    df<-rbind(df01) %>% as.data.frame()
    names(df)<-df0
    #df

    c<-b %>% cbind(df)

    id<-which(is.na(c$date))
    if(length(id)!=0){
      for (i in id) {
        c[i-1,(ncol(b)+1):ncol(c)]<-b[i,]
        d<-c[-id,]
        id0<-which(colnames(d)=='date')
        d<-d[,-id0] %>% mutate(date=d$date)
      }
    }else{
      d<-c
      id0<-which(colnames(d)=='date')
      d<-d[,-id0] %>% mutate(date=d$date)
    }
    #d
    prcp<-c()
    for (i in 1:nrow(d)) {
      if(all(na.omit((substr(d[i,7:(ncol(d)-1)],1,1)!=6))==T)){
        prcp<-append(prcp,0)
      }
      for (j in 7:(ncol(d)-1)) {
        if(is.na(substr(d[i,j],1,1)))next
        if(substr(d[i,j],1,1)==6&substr(d[i,j],5,5)==1){
          if(substr(d[i,j],2,3)==99){
            prcp<-append(prcp,paste0('0.',substr(d[i,j],4,4)))
          }else{
            prcp<-append(prcp,substr(d[i,j],2,4))
          }
        }else if(substr(d[i,j],1,1)==6&substr(d[i,j],5,5)==4){
          len<-which(substr(d[i,1:(ncol(d)-1)],1,1)==7)
          prcp<-append(prcp,paste0(substr(d[i,len[length(len)]],2,4),'.',substr(d[i,len[length(len)]],5,5)))
        }
      }
    }
    code
    #temperaturas
    tx<-c()
    tn<-c()
    #tk<-1
    for (tk in 1:nrow(d)) {

      if(length(which(substr(d[tk,],1,3)==333))==1){
        idt<-which(substr(d[tk,],1,3)==333)
        if(substr(d[tk,idt+1],1,1)==1&substr(d[tk,idt+2],1,1)==2){

          if(length(c((substr(d[tk,idt+1],1,1)==1),(substr(d[tk,idt+2],1,1)==2)))==2){
            tx<-append(tx,ifelse(substr(d[tk,idt+1],2,2)==1,paste0('-',substr(d[tk,idt+1],3,4),'.',substr(d[tk,idt+1],5,5)),
                                 paste0(substr(d[tk,idt+1],3,4),'.',substr(d[tk,idt+1],5,5))))
            tn<-append(tn,ifelse(substr(d[tk,idt+2],2,2)==1,paste0('-',substr(d[tk,idt+2],3,4),'.',substr(d[tk,idt+2],5,5)),
                                 paste0(substr(d[tk,idt+2],3,4),'.',substr(d[tk,idt+2],5,5))))
          }
        }else{
          tx<-append(tx,NA)
          tn<-append(tn,NA)
        }
      }else{
        tx<-append(tx,NA)
        tn<-append(tn,NA)
      }
    }

    dispPRCP<-substr(d[tk,4],2,2)
    dispPRCP1<-function(dispPRCP){
      dispPRCP0<-c()
      if(dispPRCP==0){
        dispPRCP0<-append(dispPRCP0,'Se incluye prcp en 1 & 3')
      }else if (dispPRCP==1){
        dispPRCP0<-append(dispPRCP0,'Se incluye prcp en 1')
      }else if (dispPRCP==2){
        dispPRCP0<-append(dispPRCP0,'Se incluye prcp en 3')
      }else if (dispPRCP==3){
        dispPRCP0<-append(dispPRCP0,'Se omite, prcp 0.0')
      }else if (dispPRCP==4){
        dispPRCP0<-append(dispPRCP0,'Se omite, No prcp')
      }else{
        dispPRCP0<-append(dispPRCP0,NA)
      }
      return(dispPRCP0)
    }
    dispnubes<-substr(d[tk,4],3,3)
    dispnubes0<-function(dispnubes){
      dispPRCP0<-c()
      if(dispnubes==0){
        dispPRCP0<-append(dispPRCP0,'0-50')
      }else if (dispnubes==1){
        dispPRCP0<-append(dispPRCP0,'50-100')
      }else if (dispnubes==2){
        dispPRCP0<-append(dispPRCP0,'100-200')
      }else if (dispnubes==3){
        dispPRCP0<-append(dispPRCP0,'200-300')
      }else if (dispnubes==4){
        dispPRCP0<-append(dispPRCP0,'300-600')
      }else if (dispnubes==5){
        dispPRCP0<-append(dispPRCP0,'600-1000')
      }else if (dispnubes==6){
        dispPRCP0<-append(dispPRCP0,'1000-1500')
      }else if (dispnubes==7){
        dispPRCP0<-append(dispPRCP0,'1500-2000')
      }else if (dispnubes==8){
        dispPRCP0<-append(dispPRCP0,'2000-2500')
      }else if (dispnubes==9){
        dispPRCP0<-append(dispPRCP0,'> 2500')
      }else{
        dispPRCP0<-append(dispPRCP0,'Se desconoce la altura')
      }
      return(dispPRCP0)
    }

    dispnubes0(dispnubes)

    visibilidad<-substr(d[tk,4],4,5)
    dispnubes0<-function(dispnubes){
      dispPRCP0<-c()
      if(dispnubes==0){
        dispPRCP0<-append(dispPRCP0,'0-50')
      }else if (dispnubes==1){
        dispPRCP0<-append(dispPRCP0,'50-100')
      }else if (dispnubes==2){
        dispPRCP0<-append(dispPRCP0,'100-200')
      }else if (dispnubes==3){
        dispPRCP0<-append(dispPRCP0,'200-300')
      }else if (dispnubes==4){
        dispPRCP0<-append(dispPRCP0,'300-600')
      }else if (dispnubes==5){
        dispPRCP0<-append(dispPRCP0,'600-1000')
      }else if (dispnubes==6){
        dispPRCP0<-append(dispPRCP0,'1000-1500')
      }else if (dispnubes==7){
        dispPRCP0<-append(dispPRCP0,'1500-2000')
      }else if (dispnubes==8){
        dispPRCP0<-append(dispPRCP0,'2000-2500')
      }else if (dispnubes==9){
        dispPRCP0<-append(dispPRCP0,'> 2500')
      }else{
        dispPRCP0<-append(dispPRCP0,'Se desconoce la altura')
      }
      return(dispPRCP0)
    }
    #prcp
    #d
    # tx
    # length(d)
    # length(tx)
    #e<-d %>% mutate(t=ifelse(substr(V6,2,2)==1,paste0('-',substr(V6,3,4),'.',substr(V6,5,5)),ifelse(substr(V6,2,2)==0,paste0(substr(V6,3,4),'.',substr(V6,5,5)),NA)))
    d$tx<-tx
    d$tn<-tn
    e<-d %>% mutate(t=ifelse(substr(V6,2,2)==1,paste0('-',substr(V6,3,4),'.',substr(V6,5,5)),ifelse(substr(V6,2,2)==0,paste0(substr(V6,3,4),'.',substr(V6,5,5)),NA)))
    #tibble(e)

    e$prcp<-prcp
    #tail(e)
    e$t<-as.numeric(e$t)
    e$tx<-as.numeric(e$tx)
    e$tn<-as.numeric(e$tn)
    e$prcp<-as.numeric(e$prcp)
    #tail(e)

    #tibble(e[,40:48])
    h<-e[,c((ncol(e)-4):ncol(e))]
    # g<-h %>% separate(date,into=c('date','time'),sep=' ') %>% group_by(date) %>% mutate(prcp0=ifelse(time=='08:00:00',prcp,NA),Temp_amb=last(t),Prcp_amb=last(prcp))
    # g
    # tail(g,15)
    # g$pamb<-NA
    # g$tamb<-NA
    # g$pamb<-g[nrow(g),]
    g<-h %>% separate(date,into=c('date','time'),sep=' ') %>% group_by(date) %>%
      summarise(prcp=prcp[(which(time=='08:00:00'))],tx=tx[(which(time=='08:00:00'))],tn=tn[(which(time=='08:00:00'))])
    g
    g[,3]<-c(g$tx[-1],NA)

    g[nrow(g),]<-g[nrow(g),] %>% mutate(tx=ifelse(h[nrow(h),4]>tn,h[nrow(h),4],NA))
    #g$tx<-c(g$tx[-1],g$t[nrow(g)])
    #g<-g[,-ncol(g)]

    dfp[date0%in%g$date,z]<-g$prcp
    row.names(dfp)<-date0
    dftx[date0%in%g$date,z]<-g$tx
    row.names(dftx)<-date0
    dftn[date0%in%g$date,z]<-g$tn
    row.names(dftn)<-date0
    #print(station_Bol[which(station_Bol$wmo_id==code),2]);g
    #tail(g)
    # length(tx)
    # length(d)
    # #print(h)

  }
  dayID<-4
  dfx<-data.frame(station_Bol,t(dftx[(nrow(dftx)-dayID):nrow(dftx),]),t(dftn[(nrow(dftn)-dayID):nrow(dftn),]),t(dfp[(nrow(dfp)-dayID):nrow(dfp),]))
  colnames(dfx)<-paste0("V",1:ncol(dfx))
  for(f in 6:20){
    dfx[,f]<-sprintf('%.1f',dfx[,f])
  }

  dfx1<-dfx %>% mutate(day1=paste(V6,'    ',V11,'    ',V16),day2=paste0(V7,'    ',V12,'    ',V17),
                       day3=paste0(V8,'    ',V13,'    ',V18),day4=paste0(V9,'    ',V14,'    ',V19),
                       day5=paste0(V10,'    ',V15,'    ',V20))


  colnames(dfx1)[(ncol(dfx1)-dayID):ncol(dfx1)]<-paste0(format(seq(as.Date((Sys.Date()-dayID)),Sys.Date(),1), format="%Y %b %d"),', ',format(seq(as.Date((Sys.Date()-dayID)),Sys.Date(),1),"%A"))
  dfx1<-dfx1[,c(1:5,21:25)]
  dfx1$VARIABLE<-'VARIABLE'
  dfx1$VARIABLE<-paste('TX (oC)','TN (oC)', 'PRCP (mm)')
  colnames(dfx1)[1:5]<-c("id","estacion",'lon','lat','alt')
  dfx1<-dfx1[,c(1:5,11,6:10)]
  df1<-as(vect(dfx1,geom=c('lon','lat'),crs="+proj=longlat +datum=WGS84"),'Spatial')
  setwd('C:/Users/Cocha/Desktop/output-cbba/')
  bol<-getData('GADM',country='BOL',level=1)

  rr <- tags$div(
    HTML('<a href="https://cran.r-project.org/"> <img border="0" alt="ImageTitle" src="/PathToImage/ImageR.jpeg" width="300" height="100"> </a>')
  )
  is.na(dfx[,10])
  sort(dfx$V10)
  pal <- colorNumeric(palette = "YlGnBu",
                      domain = as.numeric(sort(dfx$V10)) # New sort
  )

  Val = as.numeric(sort(dfx$V10))
  Pal = pal(Val)
  Labels = paste(Val, '[\u00b5g/m\u00b3]')

  m<-leaflet() %>%
    addTiles() %>% addPolygons(data=bol,fillColor = topo.colors(16, alpha = NULL),
                               weight = 0.6,
                               label = ~NAME_1,
                               group = "Departamentos") %>%
    addCircleMarkers(data = df1,
                     popup = popupTable(df1 )) %>%
    addLayersControl(overlayGroups = "Departamentos", options = layersControlOptions(collapsed = TRUE)) %>%
    addControl(rr, position = "bottomleft")
  return(m)

}
