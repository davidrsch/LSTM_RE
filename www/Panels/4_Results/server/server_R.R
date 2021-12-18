observe({
  #establish results tab as disabled by default
  disable(selector = '#Main_tabsetpanel a[data-value = "Results"]')
  #stablishind dragable download div hide as default
  runjs("
    document.querySelector('#dragablepanelcontent').parentElement.style.display = 'none';
          ")
  
  
})

#To store reactive values of results
r <- reactiveValues()
#To store the number of the tab to create
r$tab <- NULL
r$calculation <- 0

#To create UI when clicking repeat experimentation
observeEvent(input$repeatexperimentation,{
  
  #To enable results tab
  enable(
    selector = '#Main_tabsetpanel a[data-value = "Results"]')
  
  #To go to result tab
  updateTabsetPanel(
    session,
    'Main_tabsetpanel',
    selected = "Results"
  )
  
  if(is.null(r$tab)){
    r$tab <- 1
  }else{
    r$tab <- r$tab + 1
  }
  
  amountofts <- dim(sf$selectedtrains)[1] * length(sf$tstransform) * length(sf$tsscales)
  amountofvec <- amountofts * length(sf$inpvec)
  amountofmodels <- amountofvec * dim(sf$modelstablecopy)[1]
  
  tabname <- paste0(" ",as.character(r$tab),substright(ordinal(r$tab),2)," ")
  r$tabname <- paste0(as.character(r$tab),substright(ordinal(r$tab),2))
  
  insertTab(
    session = session,
    inputId = "results_tabset",
    tab = tabPanel(
      tabname,
      div(
        id = paste0(r$tabname,"content"),
        fluidRow(
          column(2),
          column(
            width = 8,
            wellPanel(
              #Container of model features table
              div(id = paste0(r$tabname,'htmlmodelfeatT'),
                  style = "margin-bottom: 4px"),
              
              #Container of live fit plot
              div(
                id = paste0(r$tabname,"livefitpcon"),
                
                plotlyOutput(
                  outputId = paste0(r$tabname,'liveplot'),
                  height = "290px")
              ),
              
              #Container of progress bars
              div(
                id = paste0(r$tabname,'containingpbs'),
                #Progress bar for samples
                progressBar(
                  id = paste0(r$tabname,"batchpb"),
                  value = 0,
                  total = 100,
                  title = "Samples:",
                  display_pct = F
                ),
                #Progress bar for epochs
                progressBar(
                  id = paste0(r$tabname,"epochpb"),
                  value = 0,
                  total = sf$epoch,
                  title = "Epochs:",
                  display_pct = F
                ),
                #Progress bar for models
                progressBar(
                  id = paste0(r$tabname,"modelpb"),
                  value = 0,
                  total = amountofmodels,
                  title = "Models:",
                  display_pct = F
                ),
                style = "margin:0; padding: 0; font-size:12px"
                
              ),
              
              #Style of the well panel inside the main panel
              style = "background-color: white;
              border-color:black;border-radius:0;
              height:100%; margin-top: 1%"
            )
          ),
          
          column(2),
          style = "height:480px"
        )
      )
      
    )
  )
  
  #To select the inserted tab
  updateTabsetPanel(
    session = session,
    inputId = "results_tabset",
    selected = paste0(" ",r$tabname," "))
  
  
  output[[paste0(r$tabname,'liveplot')]] <- renderPlotly({
    p <- plotly_empty() %>%
      config(displayModeBar = F) %>% 
      onRender('
        function(){
          Shiny.onInputChange("rcalculation",1);
        }'
      )
    
  })
  
})

#Store individual path directory
r$path_of_directorio <- paste0("www/",session$token)
#Create individual path directory
observe({
  dir.create(r$path_of_directorio)
})

#To calculate when clicking repeat experimentation
observeEvent(input$rcalculation,{
  
  if(input$rcalculation == 1){
    
    #Store the individual directory path
    #path_of_directorio <- paste0("www/",session$token)
    #Store experimentation directory path
    exp_directory <- paste0(r$path_of_directorio,"/",r$tabname)
    #Store models directory path
    exp_models <- paste0(exp_directory,"/models")
    #Store dashboard directory path
    exp_dashdirect <- paste0(exp_directory,"/dashboard")
    #Store dashboard data directory path
    exp_dashdata <- paste0(exp_dashdirect,"/www")
    #Store loss directory path
    loss_directory <- paste0(r$path_of_directorio,'/plotdata/')
    
    #Create individual directory
    #dir.create(path_of_directorio)
    #Create experimentation directory
    dir.create(exp_directory)
    #Create models directory
    dir.create(exp_models)
    #Create experimentation dashboard directory
    dir.create(exp_dashdirect)
    #Create dashborad data directory
    dir.create(exp_dashdata)
    #Create the directory for the loss live_plot data
    dir.create(loss_directory)
    
    amountofts <- dim(sf$selectedtrains)[1]
    amountoftf <- length(sf$tstransform)
    amountofsc <- length(sf$tsscales)
    amountofinps <- length(sf$inpvec)
    amountofmodels <- dim(sf$modelstablecopy)[1]
    amountoftotalmodels <- amountofts * amountoftf * amountofsc * amountofinps *amountofmodels
    modelbuilding <- 0
    #To update style of progress value
    html(
      paste0(r$tabname,'containingpbs'),
      "<style>.progress-number{
           position: relative;
           right:0px;
           z-index: 1;}</style>",
      add = T
    )
    
    #Experimentation
    #Setting seed
    if(sf$setseed == T){
      set.seed(sf$seed)
    }
    
    #TS
    for (i in 1:amountofts) {
      
      ts <- createts(sf$Xdata,
                     sf$EDA,
                     sf$selectedtrains,
                     i,
                     sf$testend)
      
      set <- sf$selectedtrains[i,1]
      
      #Transform TS
      for (tf in 1:amountoftf) {
        
        transfts <- createtrfts(TS =  ts,
                                trf = sf$tstransform,
                                ntrf = tf)
        
        trf <- sf$tstransform[tf]
        
        #Scale TS
        for (sc in 1:amountofsc) {
          
          scts <- createscts(transfts,
                             sf$tsscales,
                             sc)
          
          sca <- sf$tsscales[sc]
          
          #Create vectors
          for(input in 1:amountofinps){
            
            steps <- as.numeric(sf$inpvec[input]) + sf$tmph
            
            if(trf == "Second transformation"){
              tstv <- ts[,-1,drop = F]
              Date <- ts[[1]]
              tstv <- log(tstv)
              tstv <- cbind(Date,tstv)
            }else{
              tstv <- ts
            }
            
            vector <- threedvectfunc(tstv[,,drop=F],steps,c(1,dim(tstv)[1]))
            starttest <- which(tstv[[1]] == sf$teststart)
            truetestvector <- vector[(starttest-steps+1):dim(vector)[1],,-1,drop = F]
            date3dtest <- vector[(starttest-steps+1):dim(vector)[1],,1,drop = F]
            
            
            vector <- threedvectfunc(scts[,-1,drop=F], steps, c(1,dim(scts)[1]))
            starttest <- which(scts[[1]] == sf$teststart)
            trainvector <- vector[1:(starttest-steps),,,drop = F]
            testvector <- vector[(starttest-steps+1):dim(vector)[1],,,drop = F]
            inp <- sf$grid %>% filter(Inputs == 1) %>% select(Variables)
            inp <- whichequalvec(names(scts[,-1,drop=F]), inp[[1]])
            out <- sf$grid %>% filter(Outputs == 1) %>% select(Variables)
            out <- whichequalvec(names(scts[,-1,drop=F]), out[[1]])
            date3dtest <- date3dtest[,
                                     (as.numeric(sf$inpvec[input]) + 1):dim(testvector)[2],
                                     ,
                                     drop = F]
            
            for (m in 1:amountofmodels) {
              #Model that it is been build from total
              modelbuilding <- modelbuilding + 1
              
              #Creating models folders
              #Folder to store the model
              savemodelpath <- paste0(exp_models,"/model_",modelbuilding)
              dir.create(savemodelpath)
              #Folder to store the model data for dashboard
              dashdatamodelpath <- paste0(exp_dashdata,"/model_",modelbuilding)
              dir.create(dashdatamodelpath)
              
              #Obtaing structure of the model
              struct <- sf$modelstablecopy[m,,drop = F] %>%
                select_if(~ !any(is.na(.)))
              struct <- struct %>%
                mutate_if(is.factor, as.character)
              
              #Obataining amount of neurons
              amountofneurons <- struct %>%
                mutate_if(is.character, as.numeric) %>%
                mutate(Neurons = rowSums(across(where(is.numeric)))) %>%
                select(Neurons)
              
              #Model features table
              modelfeatutable <- data.frame(
                Model=modelbuilding,
                Set = set,
                Transformation = trf,
                Scale = sca,
                Inputs = sf$inpvec[input],
                Structure = gsub(" ","|",pastevec(as.character(struct))))
              htmlmodelfeat <- htmlTable(modelfeatutable)
              html(
                paste0(r$tabname,'htmlmodelfeatT'),
                htmlmodelfeat
              )
              
              #Update model progress bar
              updateProgressBar(
                session = session,
                id = paste0(r$tabname,"modelpb"),
                value = modelbuilding,
                total = amountoftotalmodels,
                status = "primary")
              
              #Build model
              model <- createmodel(
                structure = struct,
                inputvec = trainvector[,
                                       1:as.numeric(sf$inpvec[input]),
                                       inp,
                                       drop = F],
                outputvec = trainvector[,
                                        (as.numeric(sf$inpvec[input]) + 1):dim(trainvector)[2],
                                        out,
                                        drop = F])
              #Compile model
              model%>%
                compile( loss = "mse", optimizer='adam')
              #Obtain number of samples
              samples <- dim(trainvector[,
                                         1:as.numeric(sf$inpvec[input]),
                                         inp,
                                         drop = F])[1]
              #Create callback
              cd <- creatingcallback(nmodel = modelbuilding,
                                     session = session,
                                     directory = loss_directory,
                                     plotid = paste0(r$tabname,'liveplot'),
                                     batchpbid = paste0(r$tabname,"batchpb"),
                                     batchamount = samples,
                                     epochpbid = paste0(r$tabname,"epochpb"),
                                     epochamount = sf$epoch)
              
              #Fit the model
              model%>%
                fit(trainvector[,
                                1:as.numeric(sf$inpvec[input]),
                                inp,
                                drop = F],
                    trainvector[,
                                (as.numeric(sf$inpvec[input]) + 1):dim(trainvector)[2],
                                out,
                                drop = F],
                    epochs = sf$epoch,
                    batch_size = 1,
                    shuffle = F,
                    callbacks = list(cd),
                    verbose = 0)
              
              #Update model progress bar
              updateProgressBar(
                session = session,
                id = paste0(r$tabname,"modelpb"),
                value = modelbuilding,
                total = amountoftotalmodels,
                status = "warning")
              
              #Test the model
              predictions <- predictwkeras(
                Model = model,
                inputs = testvector[,
                                    1:as.numeric(sf$inpvec[input]),
                                    inp,
                                    drop = F],
                outputs = testvector[,
                                     (as.numeric(sf$inpvec[input]) + 1):dim(testvector)[2],
                                     out,
                                     drop = F],
                lastvaluesout = truetestvector[,
                                               1:as.numeric(sf$inpvec[input]),
                                               out,
                                               drop = F],
                scale = sca,
                transformation = trf,
                transfTS = transfts)
              
              #Creating model path
              model_directorio <- paste0(savemodelpath,"/model",modelbuilding,".hdf5")
              #Save model
              model%>%save_model_hdf5(model_directorio)
              #Save predictions
              #write_json(toJSON(predictions),paste0(modelpath,'/prediction.json'))
              #Calculating min. mean and max of predictions
              outputwithdate <- abind(date3dtest,predictions, along = 3)
              date2d <- unique(as.vector(outputwithdate[,,1]))
              mmmpred <- creatingplotpreddf(
                threddata = outputwithdate,
                xdata = date2d,
                colnames = names(scts[,-1,drop=F])[out]
              )
              
              #Save min, mean, max of predictions
              mmmpred <- toJSON(toJSON(mmmpred))
              mmmpred <- paste0('var modelpred = ',mmmpred,';')
              writeLines(mmmpred,paste0(dashdatamodelpath,'/mmmpred.json'), useBytes = T)
              #Save training loss
              trainloss <- fromJSON(paste0(loss_directory,'/loss.json'))
              trainloss <- paste0('var modellosses = ["',trainloss,'"];')
              write(trainloss,paste0(dashdatamodelpath,'/loss.json'))
              
              #Save loss of prediction
              Mloss <- gettingmetrics(
                truetestvector[,
                               (as.numeric(sf$inpvec[input]) + 1):dim(testvector)[2],
                               out,
                               drop = F],
                predictions)
              
              if(modelbuilding == 1){
                modelloss <- data.frame(a = Mloss)
                colnames(modelloss) <- paste0('model',modelbuilding)
                modelcharact <- cbind(modelfeatutable,amountofneurons)
              }else{
                model_loss <- data.frame(a = Mloss)
                colnames(model_loss) <- paste0('model',modelbuilding)
                model_charact <- cbind(modelfeatutable,amountofneurons)
                modelloss <- cbind(modelloss,model_loss)
                modelcharact <- rbind(modelcharact,model_charact)
              }
              
              if(modelbuilding == amountoftotalmodels){
                #Real values
                #start
                outstart <- which(ts[[1]] == date2d[1])
                #end
                outend <- which(ts[[1]] == date2d[length(date2d)])
                #Store real vales
                trueoutvalue <- ts[outstart:outend,c(1,(out + 1))]
                
                rownames(modelloss) <- c("MSE","RMSE","MAE")
                rownames(modelcharact) <- NULL
                #Save the errors of the models
                write_json(toJSON(modelloss),paste0(exp_dashdata,"/modelsloss.json"))
                #Save the characteristics of the models
                write_json(toJSON(modelcharact),paste0(exp_dashdata,"/modelcharact.json"))
                #Save real values
                write_json(toJSON(trueoutvalue),paste0(exp_dashdata,"/realvalues.json"))
                #Copy rmarkdown file
                file.copy("www/dashboard/interactivedashb.Rmd",
                          paste0(exp_dashdirect,"/interactivedashb.Rmd"))
                #Copy rmarkdown file js dependency
                file.copy("www/dashboard/plotly-latest.min.js",
                          paste0(exp_dashdata,"/plotly-latest.min.js"))
                #Render rmarkdown file
                render(paste0(exp_dashdirect,"/interactivedashb.Rmd"),
                       envir = new.env())
                #Remove rmarkdown file
                file.remove(paste0(exp_dashdirect,"/interactivedashb.Rmd"))
                #Get direction of dashboard
                htmldir <- gsub("www/","",exp_dashdirect)
                #Display dashboard
                runjs(paste0("
                  document.getElementById('",r$tabname,"content').innerHTML = ",
                             "'<embed type = \"text/html\" src = \"",htmldir,"/interactivedashb.html",
                             " \" style = \"height: 690px; width: 100%\">';
                  document.querySelector('#downloaddash').classList.remove('disabled');
                  document.querySelector('#downloaddash').removeAttribute('disabled');
                  document.querySelector('#downloaddash + span').classList.remove('spanNOT');
                  document.querySelector('#downloadmodels + button').classList.remove('disabled');
                  document.querySelector('#downloadmodels + button').removeAttribute('disabled');
                  document.querySelector('#dragablepanelcontent').parentElement.style.display = 'block';
                  
                  "))
                modelBuilded <- list.files(paste0(exp_models,"/"))
                modelBuilded <- gsub("model_","",modelBuilded)
                updatePickerInput(
                  session = session,
                  "downloadmodels",
                  choices = modelBuilded,
                  selected = NULL)
                
              }
              
              k_clear_session()
              
            }
            
          }
          
        }
        
      }
      
    }
    
    runjs("
      Shiny.onInputChange('rcalculation',0);
            ")
  }else{
    
  }
  
})

observeEvent(input$results_tabset,{
  htmldir <- paste0(r$path_of_directorio,"/",
                    gsub(" ","",input$results_tabset),
                    "/dashboard/interactivedashb.html")
  experimentstatus <- file.exists(htmldir)
  if(experimentstatus == F){
    disable('downloaddash')
    addClass(class = "spanNOT",selector = '#downloaddash + span')
    disable(selector = '#downloadmodels + button')
    disable('acceptdownload')
  }else{
    enable('downloaddash')
    removeClass(class = "spanNOT",selector = '#downloaddash + span')
    enable(selector = '#downloadmodels + button')
    modelspath <- paste0(r$path_of_directorio,"/",
                         gsub(" ","",input$results_tabset),
                         "/models/")
    modelBuilded <- list.files(modelspath)
    modelBuilded <- gsub("model_","",modelBuilded)
    updatePickerInput(
      session = session,
      "downloadmodels",
      choices = modelBuilded,
      selected = NULL)
  }
  
})

r$collapseCDIV <- 0

observeEvent(input$collapseDragDiv,{
  
  if(r$collapseCDIV == 0){
    runjs("
      document.getElementById('dragablepanelcontent').style.display = 'none';
      document.getElementById('dragablepanelcontent').parentElement.style.padding = '6px';
      document.getElementById('collapseDragDivicon').classList.add('fa-window-maximize');
      document.getElementById('collapseDragDivicon').classList.remove('fa-window-minimize');
            ")
    r$collapseCDIV <- 1
  }else{
    runjs("
      document.getElementById('dragablepanelcontent').style.display = 'block';
      document.getElementById('dragablepanelcontent').parentElement.style.padding = '2%';
      document.getElementById('collapseDragDivicon').classList.add('fa-window-minimize');
      document.getElementById('collapseDragDivicon').classList.remove('fa-window-maximize');
            ")
    r$collapseCDIV <- 0
  }
  
  if(r$dashboard == 'A' || r$modelsdir == 'A'){
    if(r$dashboard == 'A'){
      files <- r$modelsdir
    }else{
      files <- r$dashboard
    }
  }else{
    files <- c(r$modelsdir,r$dashboard)
  }
  
})

r$dashboard <- "A"
r$modelsdir <- 'A'

observeEvent(input$downloaddash,{
  if(input$downloaddash == T){
    dashdir <- paste0("www/",session$token,"/",gsub(" ","",input$results_tabset),
                      "/dashboard")
    r$dashboard <- dashdir
  }else{
    r$dashboard <- "A"
  }
  
})

observeEvent(input$downloadmodels,{
  if(!is.null(input$downloadmodels) && input$downloadmodels != ""){
    modelspath <- paste0("www/",session$token,"/",gsub(" ","",input$results_tabset),
                         "/models/")
    r$modelsdir <- c()
    for (i in 1:length(input$downloadmodels)) {
      r$modelsdir[i] <- paste0(modelspath,"model_",input$downloadmodels[i])
    }
    
  }else{
    r$modelsdir <- 'A'
  }
  
},ignoreNULL = F)

observeEvent(c(input$downloaddash,input$downloadmodels),{
  if(is.null(input$downloadmodels) && input$downloaddash ==  F){
    disable('acceptdownload')
  }else{
    if(input$downloadmodels == "" && input$downloaddash ==  F){
      disable('acceptdownload')
    }else{
      enable('acceptdownload')
    }
  }
},ignoreNULL = F)

output$acceptdownload <- downloadHandler(
  filename = function(){
    "Results.zip"
  },
  content = function(file){
    
    if(r$dashboard == 'A' || r$modelsdir == 'A'){
      if(r$dashboard == 'A'){
        files <- r$modelsdir
      }else{
        files <- r$dashboard
      }
    }else{
      files <- c(r$modelsdir,r$dashboard)
    }
    
    return(zipr(zipfile = file, files = files))
  },
  
  contentType = "application/zip"
)

session$onSessionEnded(function(){
  unlink(paste0("www/",session$token), recursive = TRUE)
})