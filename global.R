#GLOBAL OBJECTS----
#(FUNC & VARS)----
#01-Possible file extensions----
#For text files
readdelim <- c("text","csv","tsv","fwf")
#For excel files
readexcel <- c("xlsx","xls")
#All together
readall <- c(readdelim,readexcel)
#Error classes
errorclasses <- c("simpleError", "error", "condition")

#02-EDA functions----
plotedafunc <- function(data){
  library(ggplot2)
  library(GGally)
  p <- ggpairs(
    data,
    title = 'Exploratory Data Analysis',
    lower = list(
      continuous = wrap(
        "points",
        colour = "blue")),
    diag = list(
      continuous = wrap(
        'densityDiag',
        color = "black",
        fill = "blue",
        alpha = 0.5)),
    upper = list(
      continuous = wrap(
        'cor',
        size = 6))) +
    theme(
      plot.title = element_text(hjust = 0.5),
      text = element_text(size=15))
  return(p)
}

databasesum <- function(data){
  library(pastecs)
  stat.desc(data)
}

#03-Training set periods plots----
Setsvarsplots <- function(
    selectedtrains,
    EDA,
    Xdata,
    selectteststart,
    selecttestend){
  
  library(shiny)
  library(plotly)
  
  SVplots <- c()
  
  for (i1 in 1:dim(selectedtrains)[1]) {
    
    if(!(i1%%2) == 0){
      element <- tagList(
        div(
          style = "float:left; width:48%;"
        )
      )
      
    }else{
      element <- tagList(
        div(
          style = "float:right; width:48%;"
        )
      )
    }
    
    p <- plot_ly(type = "scatter", mode = "lines") %>%
      config(displayModeBar = F)
      
    for (i2 in 1:dim(EDA)[2]) {
      stn <- which(Xdata == selectedtrains[i1,])
      stt <- which(Xdata == selectteststart)
      x1 <- Xdata[stn:stt]
      y1 <- EDA[stn:stt,i2][[1]]
      p <- p %>% add_trace(
        x = x1,
        y = y1,
        mode = "lines",
        legendgroup = colnames(EDA)[i2],
        legendgrouptitle = list(text = colnames(EDA)[i2]),
        name = 'train')
        
      linegtcolr <- i2 * 2
        
      ptogtcolr <- plotly_build(p)
      nxtlinecolor <- ptogtcolr$x$data[[linegtcolr]]$line$color[1]
      ett <- which(Xdata == selecttestend)
      x2 <- Xdata[stt:ett]
      y2 <- EDA[stt:ett,i2][[1]]
        
      p <- p %>% add_trace(
        x = x2,
        y = y2,
        mode = "lines",
        legendgroup = colnames(EDA)[i2],
        name = 'test',
        line = list(dash = "dash",
                    color = nxtlinecolor))
        
    }
      
    element[[1]][[3]][[i1]] <- p
    
    if(i1 == 1){
      
      SVplots <- element
        
    }else{
        
      SVplots <- tagList(SVplots,element)
    }
    
  }
  
  return(SVplots)
}

#04-Alert when user start experimentation without all required parameters----
startalert <- tagList(
  div(
    p(
      "Check have selected at least:"
    ),
    tags$ul(
      tags$li("A time serie to use."),
      tags$li("A scale to use."),
      tags$li("Specified a temporal horizon."),
      tags$li("Specified an input amount."),
      tags$li("Specified a LSTM layers amount."),
      tags$li("Specified a neurons amount."),
      tags$li("Specified an epoch amount.")
      
    ),
    style = "text-align:left; margin-left: 20%"
  )
)

#05-Function to find models to build per vectors----
findmodels <- function(lstm, neurons){
  
  for (i in 1:length(lstm)){
    if(i == 1){
      df <- expand.grid(rep(list(neurons),lstm[i]))
      colsnames <- lapply(1:lstm[i], function(x) paste0(x,"_LSTM"))
      names(df) <- colsnames
    }else{
      df2 <- expand.grid(rep(list(neurons),lstm[i]))
      colsnames <- lapply(1:lstm[i], function(x) paste0(x,"_LSTM"))
      names(df2) <- colsnames
      df <- bind_rows(df,df2)
    }
    
  }
  
  return(df)
  
}

#06-Function to build modal of models to build----
selectmodelstobuild <- function(train, ts, sc, vec, lstm, neu){
  amountoftrain <- dim(train)[1]
  if(dim(train)[1] == 1){
    setors <- "set"
  }else{setors <- "sets"}
  if(length(ts) == 1){
    tfors <- "transformation"
  }else{tfors <- "transformations"}
  if(length(sc) == 1){
    scors <- "scale"
  }else{scors <- "scales"}
  if(length(vec) == 1){
    inputors <- "amount"
  }else{inputors <- "amounts"}
  models <- findmodels(lstm = lstm,neurons = neu)
  if(dim(models)[1] == 1){
    modelors <- "model"
  }else{
    modelors <- "models"
  }
  if(dim(models)[1] == 1){
    Modelors <- "Model"
  }else{
    Modelors <- "Models"
  }
  text <- tagList(
    div(
      style = "text-align:left; margin-left:5%;",
      p("There have been selected:",style = "color:black"),
      tags$ul(
        tags$li(as.character(amountoftrain)," ",setors," of the data.",
                style = "color:black"),
        tags$li(as.character(length(ts))," ",tfors,".",style = "color:black"),
        tags$li(as.character(length(sc))," ",scors,".",style = "color:black"),
        tags$li(as.character(length(vec))," input ",inputors,".",style = "color:black")
      ),
      uiOutput("modelstobuildtext"),
      div(style = "text-align:center",
          p(
            tags$b(paste0(Modelors," per vector")),
            style = "color: black;"
          )),
      div(id="containingmodelstable",
          DTOutput("modelestable"),
          style = "border-radius: 0;
          border: black thin solid;
          max-height: 350px;
          overflow: auto;
          text-align: center"
          ),
      div(style = "height:20px"),
      div(
        actionButton(
          "eliminatemodel",
          "Eliminate",
          style = "float: right;"
        ),
        style = "display: flow-root;"
      ),
      div(style = "height:20px"),
      div(
        actionButton(
          "acceptmodels",
          "OK",
          class = "btn-primary"),
        actionButton(
          "cancelmodels",
          "Cancel",
          class = "btn-danger"
        ),
        style = "display: flex; justify-content: space-between"
      )
      
    )
  )
  return(text)
}

#07-To stract character from right to left----
substright <- function(x, n){
  substr(x,nchar(x)-n+1,nchar(x))
}

#08-To create time series----
createts <- function(Date, variables, sttrain, ntrain, endtt){
  df <- cbind(Date, variables)
  start <- which(df[[1]] == sttrain[ntrain, ][[1]])  
  end <- which(df[[1]] == endtt)
  df <- df[start:end,]
}

#09-To create transformations----
#First transformation
firstrf <- function(TS){
  
  df <- TS[,-1, drop = F]
  
  for (variables in 1:dim(df)[2]) {
    
    #To differenciate the time series until meets stationariti
    for(i in 1:dim(df)[1]){
      
      valuedif <- diff(df[[variables]], differences = i)
      ADF <- adf.test(valuedif, alternative = "stationary")
      PP <- pp.test(valuedif, alternative = "stationary")
      
      #To stop when TS is stationary
      if(ADF$p.value <= 0.05 && PP$p.value <= 0.05){
        valuedif <- i
        
        break
      }else{}
      
    }
    
    #To store stationary difference value
    if(variables == 1){
      diffvalue <- valuedif
    }else{
      diffvalue <- c(diffvalue, valuedif)
    }
    
  }
  
  #Store max difference value
  diffvalue <- max(diffvalue)
  
  #To create the difference dataframe
  diffdf <- data.frame(diff(as.matrix(df),differences = diffvalue))
  names(diffdf) <- names(TS)[-1]
  valstrm <- 1:diffvalue
  Date <- TS[[1]][-(valstrm)]
  DifV <- rep(diffvalue,length(Date))
  diffdf <- cbind(Date,
                 diffdf,
                 DifV)
  
  
  return(diffdf)
  
}

#Second transformation
secondtrf <- function(TS){
  
  logTS <- log(TS[,-1, drop = F])
  Date <- TS[,1, drop = F]
  logdf <- cbind(Date,
                 logTS)
  logdf <- firstrf(logdf)
  return(logdf)
}

#10-Create transformed ts----
createtrfts <- function(TS, trf, ntrf){
  
  if(trf[ntrf] == "Original"){
    x <- TS
    }else{
      
      if(trf[ntrf] == "First transformation"){
        
        x <- firstrf(TS)
        
        }else{
          x <- secondtrf(TS)
        }
      
    }
  
  return(x)
  
}

#11-To create scaled TS----
#To rescale data frames
rescaledf <- function(x, to){
  
  for (variable in 1:dim(x)[2]) {
    df <- rescale(x[[variable]],to,from = c(min(x),max(x)))
    if(variable == 1){
      datfra <- data.frame(df)
    }else{
      datfra <- cbind(datfra,df)
    }
  }
  
  colnames(datfra) <- names(x)
  return(datfra)
}

#To scale the datas
createscts <- function(TS, sc, nsc){
  
  if(sc[nsc] == "Exact"){
    if(any(names(TS)=='DifV')){
      x <- TS[,-grep('DifV',names(TS)),drop = F]
    }else{
      x <- TS
    }
  }else{
    
    if(sc[nsc] == "From 0 to 1"){
      
      if(any(names(TS)=='DifV')){
        Y <- TS[,-grep('DifV',names(TS)),drop = F]
      }else{
        Y <- TS
      }
      
      dftrsc <- Y[,-1,drop = F]
      Date <- Y[1,drop = F]
      df <- rescaledf(x = dftrsc,
                      to = c(0,1))
      x <- cbind(Date,
                 df)
      
    }else{
      
      if(any(names(TS)=='DifV')){
        Y <- TS[,-grep('DifV',names(TS)),drop = F]
      }else{
        Y <- TS
      }
      
      dftrsc <- Y[,-1, drop = F]
      Date <- Y[1, drop = F]
      df <- rescaledf(dftrsc,c(-1,1))
      x <- cbind(Date,
                 df)
      
    }
    
  }
  
  return(x)
  
  
}

#12-To create vectors----
#3D vectors function
threedvectfunc <- function(data, steps, datasample){
  if(is.data.frame(data)||is.matrix(data)){
    for (column in 1:dim(data)[2]) {
      rollingwin <- runner(data[datasample[1]:datasample[2],column],k=steps,na_pad = T)
      rollingwin <- rollingwin[steps:length(datasample[1]:datasample[2])]
      rollingwin <- t(matrix(unlist(rollingwin),
                             ncol = length(datasample[1]:datasample[2]) - steps + 1,
                             nrow = steps))
      if(column == 1){
        threedrw <- abind(rollingwin, along = 3)
      }else{
        threedrw <- abind(threedrw, rollingwin, along = 3)
      }
    }
  }
  return(threedrw)
}

#13-To search for inp and out----
whichequalvec <- function(vec,equalto){
  for (i in 1:length(equalto)) {
    if(i == 1){
      x <- which(vec == equalto[i])
    }else{
      x <- c(x,which(vec == equalto[i]))
    }
  }
  x <- sort(x)
  return(x)
}

#14-To create models----
createmodel <- function(structure, inputvec, outputvec){
  #In case of single LSTM layer
  if(dim(structure)[2] == 1){
    model <- keras_model_sequential()%>%
      layer_lstm(
        units = as.numeric(structure[[1]]),
        input_shape=c(dim(inputvec)[2],dim(inputvec)[3]))
  }else{
    #In case of multiple LSTM layers
    for (layer in 1:dim(structure)[2]) {
      #Starting LSTM layer
      if(layer == 1){
        model <- keras_model_sequential()%>%
          layer_lstm(
            units = as.numeric(structure[[1]]),
            input_shape = c(dim(inputvec)[2],dim(inputvec)[3]),
            return_sequences = T)
      }else{
        #Between LSTM layers 
        if(layer > 1 && layer < dim(structure)[2]){
          model <- model %>%
            layer_lstm(
              units = as.numeric(structure[[layer]]),
              return_sequences = T
            )
          #Final LSTM layer
        }else{
          model <- model %>%
            layer_lstm(units = as.numeric(structure[[layer]]))
        }
      }
    }
  }
  
  #Addin output layers
  model <- model %>%
    layer_dense(dim(outputvec)[2]*dim(outputvec)[3])%>%
    layer_reshape(c(dim(outputvec)[2],dim(outputvec)[3]))
  
  return(model)
  
}
#15-To create table of models----
#To paste characters inside a vector
pastevec <- function(vect){
  for (i in 1:length(vect)) {
    if(i == 1){
      x <- vect[i]
    }else{
      x <- paste(x,vect[i])
    }
  }
  return(x)
}
#To create html table
htmlTable <- function(df){
  x <- '<div>
        <style>th, td {border: 1px solid black;margin:0px;padding: 5px;}
                table{
                    border-collapse:collapse;
                    font-size:12px;
                    width: 100%;
                    text-align:center}</style><table>'
  
  colnames <- names(df)
  for (col in 1:length(colnames)) {
    if(col == 1){
      x <- paste(x,'<tr><th style = "border-color:black;"><b>',colnames[col],'</b></th>')
    }else{
      if(col > 1 && col < length(colnames)){
        x <- paste(x,'<th style = "border-color:black;"><b>',colnames[col],'</b></th>')
      }else{
        x <- paste(x,'<th style = "border-color:black;"><b>',colnames[col],'</b></th></tr>')
      }
    }
  }
  
  for (r in 1:dim(df)[1]) {
    
    x <- paste(x,'<tr>')
    
    for (cl in 1:dim(df)[2]) {
      
      x <- paste(x,'<td style = "border-color:black;">',df[r,cl][[1]],'</td>')
      
    }
    
    x <- paste(x,'</tr>')
    
  }
  
  x <- paste(x,'</table></div>')
  return(x)
}

#16-Creating callback----
# Callback function:
#   - On batch and epoch begin:
updatingpg = function(session,pgbid,amount,item){
  
  updateProgressBar(
    session = session,
    id = pgbid,
    value = (item + 1),
    total = amount)
  
}
#   - On epoch end
#       * Updatelayout
updatelayoutfunc <- function(x,valuesofx,plotid){
  shinyjs::runjs(paste0('
        var graphDiv = document.getElementById("',plotid,'");
        
        var update = {
        
         "margin": {
            "b": 40,
            "l": 60,
            "t": 25,
            "r": 10
         },
         xaxis: {
           "domain": [0, 1],
           "automargin": true,
           "title": "",
           "showgrid": false,
           "linecolor": "black",
           "ticks": "outside",
           tickvals: ',toJSON(valuesofx),',
           "range": [0.9,',(x[length(x)] + 0.1),']
         },
         yaxis: {
           "domain": [0, 1],
           "automargin": true,
           "title": "",
           "showgrid": false,
           "linecolor": "black",
           rangemode: "normal",
           "ticks": "outside"
         },
         legend: {
           orientation: "h",
           x: 0.40
         },
         hovermode: "x unified",
         dragmode: false,
         showlegend: true
        };
        
        Plotly.relayout(graphDiv, update);
                     '))
}
#       * Add trace function
addtracesfunction <- function(x,loss,plotid){
  shinyjs::runjs(paste0('
        var graphDiv = document.getElementById("',plotid,'");
        
        Plotly.addTraces(graphDiv,[
          {
            mode: "lines+markers",
            alpha_stroke: 1,
            sizes: [10, 100],
            spans: [1, 20],
            type: "scatter",
            x:',toJSON(x),' ,
            y:',toJSON(loss),',
            name: "loss",
            line: {
              color: "blue"
            },
            marker: {
              color: "blue"
            },
            inherit: true
           }
    ]);
                     '))
}
#       * Extend trace function
extendtraces <- function(loss,epoch,plotid){
  shinyjs::runjs(paste0('
        var graphDiv = document.getElementById("',plotid,'");
        
        Plotly.extendTraces(graphDiv, { y: [',toJSON(loss[epoch+1]),']},[1]);'))
}
#       * Eliminate traces
eliminatetraces <- function(plotid){
  shinyjs::runjs(paste0('
  var graphDiv = document.getElementById("',plotid,'");
  
  Plotly.deleteTraces(graphDiv,[-1]);'))
}
#       * Onepochend
onepochend <- function(nmodel, directory, plotid, amountofepoch, epoch, logs){
  
  plotx <- 1:amountofepoch
  if(plotx[length(plotx)]>10){
    xticksvalues <- seq(1,plotx[length(plotx)],round(plotx[length(plotx)]/10))
  }else{
    xticksvalues <- plotx
  }
  
  
  if(nmodel == 1){
    firstepoch <- function(epoch, logs, directory, plotid, plotx) {
      updatelayoutfunc(plotx,xticksvalues,plotid)
      loss <- logs$loss
      addtracesfunction(plotx,loss,plotid)
      loss <- jsonlite::toJSON(loss)
      jsonlite::write_json(loss,paste0(directory,'loss.json'))
    }
  }else{
    firstepoch <- function(epoch, logs, directory, plotid, plotx) {
      eliminatetraces(plotid)
      updatelayoutfunc(plotx,xticksvalues,plotid)
      loss <- logs$loss
      addtracesfunction(plotx,loss,plotid)
      loss <- jsonlite::toJSON(loss)
      jsonlite::write_json(loss,paste0(directory,'loss.json'))
    }
  }
  
  if(epoch == 0){
    firstepoch(epoch,logs,directory,plotid,plotx)
  }else{
    loss <- jsonlite::fromJSON(paste0(directory,'loss.json'))
    loss <- jsonlite::fromJSON(loss)
    loss <- c(loss,logs$loss)
    extendtraces(loss,epoch,plotid)
    loss <- jsonlite::toJSON(loss)
    jsonlite::write_json(loss,paste0(directory,'loss.json'))
  }
}

# Creating callback
creatingcallback <- function(nmodel,
                             session,
                             directory,
                             plotid,
                             batchpbid,
                             batchamount,
                             epochpbid,
                             epochamount){
  
  cd <- callback_lambda(
    on_batch_begin = function(batch, logs){
      updatingpg(
        session = session,
        pgbid = batchpbid,
        amount = batchamount,
        item = batch)},
    on_epoch_begin = function(epoch, logs){
      updatingpg(
        session = session,
        pgbid = epochpbid,
        amount = epochamount,
        item = epoch)},
    on_epoch_end = function(epoch,logs){
      onepochend(
        nmodel = nmodel,
        directory = directory,
        plotid = plotid,
        amountofepoch = epochamount,
        epoch = epoch,
        logs = logs)
    })
  
}

#17-To test model----
# - Transforming 3d vectors to 2d vector by droping the first dimension
from3dto2d <-function(vec3d){
  
  for (i in 1:dim(vec3d)[1]) {
    x1 <- adrop(vec3d[i,,,drop=F],drop = 1)
    if(i == 1){
      x <- x1
    }else{
      x <- abind(x,x1,along = 1)
    }
  }
  
  return(x)
}

# - Inverting difference of 3d vector (predictions)
diffinv3d <- function(data3d, difference, lastknow3d){
  
  data2d <- from3dto2d(data3d)
  lastknow2d <- from3dto2d(lastknow3d)
  datastepspersample <- dim(data2d)[1]/dim(data3d)[1]
  lastknowsteppersample <- dim(lastknow2d)[1]/dim(lastknow3d)[1]
  
  for (sample in 1:dim(data3d)[1]) {
    datastartsample <- ((datastepspersample * sample) - datastepspersample) + 1
    dataendsample <- datastepspersample * sample
    data <- data2d[datastartsample:dataendsample,,drop=F]
    lastknowendsample <- lastknowsteppersample * sample
    lastknow <- lastknow2d[(lastknowendsample-difference+1):lastknowendsample,,drop = F]
    lastknow <- as.matrix(lastknow)
    invert2d <- diffinv(data, differences = difference, xi = lastknow)
    invert3d <- invert2d
    dim(invert3d) <- c(1,dim(invert2d)[1],dim(invert2d)[2])
    
    if(sample == 1){
      invertedarray <- invert3d
    }else{
      invertedarray <- abind(invertedarray,invert3d, along = 1)
    }
  }
  
  stepstoselect <- (dim(invertedarray)[2]-dim(data3d)[2]+1) : dim(invertedarray)[2]
  invertedarray <- invertedarray[,stepstoselect,,drop = F]
  return(invertedarray)
  
}

# - To obtain predictions
predictwkeras <- function(Model,
                          inputs,
                          outputs,
                          lastvaluesout,
                          trueoutputs,
                          scale,
                          transformation,
                          transfTS){
  
  modeltouse <- Model
  for (samples in 1:dim(inputs)[1]) {
    inputss <- inputs[samples,,,drop=F]
    predictions <- predict(modeltouse,inputss)
    #Commented to no update model
    #if(samples > dim(outputs)[2]){
      
    #  modeltouse %>% fit(inputs[(samples-dim(outputs)[2]),,,drop=F],
    #                     outputs[(samples-dim(outputs)[2]),,,drop=F],
    #                     epochs = 1,
    #                     batch_size = 1,
    #                     shuffle = F,
    #                     verbose = 0)
      
    #}
    
    
    if(scale == "Exact"){
      predictions <- predictions
    }else{
      if(scale == "From 0 to 1"){
        predictions <- rescale(predictions,
                               to = c(min(transfTS[,-1]),max(transfTS[,-1])),
                               from = c(0,1))
        
        }
      else{
        predictions <- rescale(predictions,
                               to = c(min(transfTS[,-1]),max(transfTS[,-1])),
                               from = c(-1,1))
      }
    }
    
    if(transformation == "Original"){
      predictions <- predictions
    }else{
      if(transformation == "First transformation"){
        predictions <- diffinv3d(
          data3d = predictions,
          difference = transfTS[[dim(transfTS)[2]]][1],
          lastknow3d = lastvaluesout[samples,,,drop=F])
      }else{
        predictions <- diffinv3d(
          data3d = predictions,
          difference = transfTS[[dim(transfTS)[2]]][1],
          lastknow3d = lastvaluesout[samples,,,drop=F])
        predictions <- exp(predictions)
      }
    }
    
    if(samples == 1){
      predarray <- predictions
    }else{
      predarray <- abind(predarray,predictions,along = 1)
    }
    
  }
  return(predarray)
}

# - To compare models
gettingmetrics <- function(actual, predicted){
  MSE <- mse(actual, predicted)
  RMSE <- rmse(actual,predicted)
  MAE <- mae(actual,predicted)
  x <-  c(MSE,RMSE,MAE)
  return(x)
}

# - Create table of min,mean and max of each output variable
creatingplotpreddf <- function(threddata, xdata, colnames){
  
  
  for (col in 1:length(colnames)){
    
    for(date in 1:length(xdata)){
      data3d <- as.data.frame(as.matrix(threddata[,,1]))
      rowcolar <- which(data3d == xdata[date], arr.ind = T)
      rowcolar <- as.data.frame(rowcolar)
      rowcolar <- arrange(rowcolar,row)
      
      for (combofrowcol in 1:dim(rowcolar)[1]) {
        rowcol <- as.vector(as.matrix(rowcolar[combofrowcol,]))
        pred <- as.matrix(threddata[rowcol[1],rowcol[2],1+col])[[1]]
        if(combofrowcol == 1){
          predict <- pred
        }else{
          predict <- c(predict,pred)
        }
        
      }
      if(length(predict) < dim(threddata)[2]){
        predict <- c(predict,rep(NaN,dim(threddata)[2]-length(predict)))
      }
      if(date == 1){
        predictions <- data.frame(t(predict))
      }else{
        predictions <- rbind(predictions,predict)
      }
      
    }
    
    min <- apply(
      predictions[,1:dim(predictions)[2],drop=F],
      1,
      function(predictions){min(predictions,na.rm = T)})
    mean <- apply(
      predictions[,1:dim(predictions)[2],drop=F],
      1,
      function(predictions){mean(predictions,na.rm = T)})
    max <- apply(
      predictions[,1:dim(predictions)[2],drop=F],
      1,
      function(predictions){max(predictions,na.rm = T)})
    
    datapred <- data.frame(min,mean,max)
    minname <- paste0(colnames[col],'MIN')
    meanname <- paste0(colnames[col],'MEAN')
    maxname <- paste0(colnames[col],'MAX')
    names(datapred) <- c(minname,meanname,maxname)
    if(col == 1){
      datapredfin <- datapred
    }else{
      datapredfin <- cbind(datapredfin,datapred)
    }
  }
  return(datapredfin)
}

  