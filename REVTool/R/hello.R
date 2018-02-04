REVTool<-function(){

  for(Programa in 1){

    setwd("C:/")

    library(xlsx)
    library(reshape2)

    midoc<-choose.files(default = "",
                        caption = "Selecciona archivo .csv de PUBMED",
                        multi = F)

    data  <- readLines(midoc)

    longitud<-length(data)

    pb<-winProgressBar(title="Articulos codificados: Barra de progreso",
                       label="0% hecho",
                       min=0,
                       max=100,
                       initial=0)

    vect<-matrix(0,0,1)

    for(i in 1:length(data)){
      prueba<-data[i]
      prueba<-gsub("\"","$",prueba)
      prueba<-gsub("$,$","$",prueba,fixed = T)
      vect<-rbind(vect,prueba)

      avance<-round(i*100/longitud)
      nfo <- sprintf("%d%% hecho",avance )
      setWinProgressBar(pb, avance, "CODIFICACION DE ARTICULOS", nfo)

    }

    mibase<-matrix(NA,length(data),11)
    colnames(mibase)<-c("Titulo","Enlace","Autores","Detalles","Revista y fecha","Base de datos","Tipo","Identificadores","Db","PMID","Fecha de creacion en Db y primer autor")
    for(i in 1:length(data)){
      if(vect[i]!="Title,URL,Description,Details,ShortDetails,Resource,Type,Identifiers,Db,EntrezUID,Properties"){
        infor<-unlist(strsplit(vect[i], "[$]"))
        mifila<-t(as.matrix(infor))
        mibase[i,]<-mifila[,c(2:12)]

        avance<-round(i*100/longitud)
        nfo <- sprintf("%d%% hecho",avance )
        setWinProgressBar(pb, avance, "RECODIFICACION DE ARTICULOS", nfo)

      }
    }

    setWinProgressBar(pb, 98, "Espere unos segundos: Creando y guardando EXCEL", "98% guardando documento")

    mibase<-na.omit(mibase)
    mibase[,"Enlace"]<-paste("https://www.ncbi.nlm.nih.gov",mibase[,"Enlace"],sep = "")
    mibase<-mibase[,-(which(colnames(mibase)%in%"Base de datos"))]

    nuevascols<-strsplit(mibase[,"Fecha de creacion en Db y primer autor"], split=" | ", fixed = T)
    nwcls<-matrix(0,0,3)

    misnomb<-colnames(mibase)[1:9]

    for(pptr in 1:nrow(mibase)){
      if(length(nuevascols[[pptr]])==2){
        nuevascols[[pptr]][1]<-gsub("create date:","",nuevascols[[pptr]][1])
        nuevascols[[pptr]][2]<-gsub("first author:","",nuevascols[[pptr]][2])
        nwcls<-rbind(nwcls,c(nuevascols[[pptr]],"OK"))
      }else{
        nwcls<-rbind(nwcls,c("","","Error"))
      }
    }

    if(nrow(mibase)==nrow(nwcls)){
      mibase<-cbind(mibase[,1:9],nwcls)
    }

    colnames(mibase)<-c(misnomb,"Fecha","Primer autor","Correccion de errores")

    nomb<-gsub(" ",Sys.info()["login"],replacement = "-")

    nombre<-paste("Busqueda_",nomb,"_",Sys.Date(),".xlsx",sep = "")

    fech<-strsplit(mibase[,"Fecha"],"/")

    options(warn=-1)
    fechasvec<-numeric(0)
    for(todos in 1:length(fech)){
      if(!is.na(as.numeric(fech[[todos]][1]))){
        fechasvec<-c(fechasvec,as.numeric(fech[[todos]][1]))
      }
    }

    options(warn=0)

    setwd(choose.dir(default = "",caption = "Donde guardar el resultado"))

    write.xlsx(mibase, nombre)
    close(pb)
    properr<-round(length(which(mibase[,ncol(mibase)]=="Error"))/
                     length(which(mibase[,ncol(mibase)]=="OK")),4)
    cat("\n","\n","Proporcion de errores = ", properr," : (",properr*100,"%",")",sep = "")

    barplot(table(fechasvec),main = "nº de articulos por año")

    rm(list=ls())

  }} ##Ejecutar programa: CTRL+R sobre esta linea

##
ileg<-function()
{for(defec    in 1){

  if(exists("pb"))close(pb)
  rm(list=ls())

  problem<-readline(prompt = "hay algun articulo ilegible? (s/n) >> ")

  if(problem=="s"){
    setwd("C:/")

    library(xlsx)
    library(reshape2)

    midoc<-choose.files(default = "",
                        caption = "Seleccione el documento .csv de PUBMED con errores",
                        multi = F)

    data  <- readLines(midoc)

    recuperar<-readline(prompt = "Numero de estos articulos con separacion en espacios >> \n(ej. 145 190 993)")
    recuperar<-strsplit(recuperar," ")
    recuperar<-as.numeric(recuperar[[1]])
    esta<-data[recuperar]
    coso<-matrix(0,0,1)

    for(i in 1:length(esta)){
      coso<-rbind(coso,esta[i])
    }
    colnames(coso)<-"informacion de los articulos"
    nomb<-gsub(" ",Sys.info()["login"],replacement = "-")
    nombre<-paste("ilegibles_",nomb,"_",Sys.Date(),".xlsx",sep = "")
    setwd(choose.dir(default = "",caption = "Donde guardar el resultado"))
    write.xlsx(coso, nombre)
    rm(list=ls())
  }
} }##CTRL+R para buscar articulos ilegibles

