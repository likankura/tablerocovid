    # Interactive plots
    
    #setwd('/home/alex/MEGA/itzamna/Aportando valor/analisis covid/')
    source("data.R") #downloads and modifies data
    library(plotly)
    library(RColorBrewer)
    library(lubridate)
    
    
    nat <- c('MEX','ECU','ESP','KOR','ITA','USA')
    knat <- data$countryterritoryCode%in%nat
    
    data <- data[knat,]
    
    knat <- data$countryterritoryCode%in%nat
    mex <- data$countryterritoryCode=='MEX'
    knat <- data$countryterritoryCode%in%nat
    
    data$date <- data$date - days(1) #La fecha es la fecha de reporte al ecdc
    
    # Modifies ranges to dates
    data$date <- as.Date(data$date)
    data$c100 <- as.Date(data$date[data$c100==0 & mex]) + data$c100
    data$cd1 <- as.Date(data$date[data$cd1==0 & mex]) + data$cd1
    data$cd10 <- as.Date(data$date[data$cd10==0 & mex]) + data$cd10
    
    cls <- brewer.pal(length(nat),'Dark2')[length(nat):1] # Colors
    
    xpos=0.05  # Menu
    ypos=0.95
    
    data$countryterritoryCode <- factor(data$countryterritoryCode, levels=nat ) #cause plotly
    
    ######################################################
    ###########     Cumulative Cases
    ######################################################
    plt1 <- plot_ly(data=data, y=~cumsum, color=~countryterritoryCode#,
                    #colors="Dark2"
                    )
    
    plt1 <- plt1%>%
        #add_segments(
        #    x=max(data$date[mex]), xend=max(data$date[mex]), y=0, yend=max(data$cumsum[knat]),
        #    line=list(color='grey', dash='dot'), showlegend=FALSE, visible=TRUE)%>%
        add_lines(x~date,   visible=TRUE) %>% 
        add_lines(x~c100,  visible=FALSE) %>%
        add_lines(x~cd1, colors=cls, visible=FALSE) %>%
        add_lines(x~cd10, colors=cls, visible=FALSE) 
    
    # Matrix for menus "visible"
    nnat <- length(nat)
    nline <- 4
    m <- diag(nline)==TRUE
    w <- apply(m,2, FUN=function(col){
        z <- sapply(col,FUN=function(x){rep(x,nnat)})
        c(z)
    })
    #w <- w==TRUE
    
    # matrices for ranges
    xrang <- data.frame(
        d1=c(min(data$date[mex & data$cumsum>0]),max(data$date[mex])),
        d2=c(min(data$date[mex & data$cumsum>=100]),max(data$c100[knat])),
        d3=c(min(data$date[mex & data$decs>=1]),max(data$cd1[knat])),
        d4=c(min(data$date[mex & data$decs>=10]),max(data$cd10[knat]))
    )
    
    yrang <- data.frame(
        d1=c(0, max(data$cumsum[mex])*3.5),
        d2=c(0, max(data$cumsum[knat & data$c100==today()])),
        d3=c(0, max(data$cumsum[knat & data$cd1==today()])),
        d4=c(0, max(data$cumsum[knat & data$cd10==today()]))
    )
    
    
    # Buttons
    diaref <- list(type='dropdown', 
                   buttons = list(
                       list(method = "update",
                            args = list(
                                list(visible=as.list(w[,1])),
                                list(xaxis.range=xrang[,1], yaxis.range=yrang[,1])
                            ),
                            label = "Fecha"),
                       list(method = "update",
                            args = list(
                                list(visible=as.list(w[,2])),
                                list(xaxis.range=xrang[,2], yaxis.range=yrang[,2])
                            ),
                            label = "100 casos"),
                       list(method = "update",
                            args = list(
                                list(visible=as.list(w[,3])),
                                list(xaxis.range=xrang[,3], yaxis.range=yrang[,3])
                            ),
                            label = "1 deceso"),
                       list(method = "update",
                            args = list(
                                list(visible=as.list(w[,4])),
                                list(xaxis.range=xrang[,4], yaxis.range=yrang[,4])
                            ),
                            label = "10 decesos")
                   ),
                   direction = "down",
                   xanchor = 'left',
                   yanchor = "top",
                   name = "",
                   bordercolor="#bf5337",
                   #font=list(color="#ffffff"),
                   pad = list('r'= 0, 't'= 0, 'b' = 0),
                   x = xpos,
                   y = ypos
    )
    
     
    
    plt1 <- plt1 %>% layout(
        updatemenus = list( diaref))
    
    
    # Abline data
    hoy <- list(type='line', x0=max(data$date[mex]), x1=max(data$date[mex]), y0=0, y1=max(data$cumsum[knat]), 
                line=list(dash='dot', width=1, color="#808080"))
    # Menu title
    annot <- list(text = "Casos acumulados según:", x=0, y=1.05, 
                  xref='paper', yref='paper', showarrow=FALSE,
                  font=list(color="#bf5337", size=16))
    
    plt1 <- plt1%>%    
        layout(shapes=hoy, #abline
               xaxis=list(title='Fecha (base México)'),# axis labels
               yaxis=list(title='Casos confirmados',
                          range=c(0,max(data$cumsum[mex]*3))),#initial range
               annotations=annot, #Menu title
               hovermode = 'compare' # Set hovermode as default
               )
    
    cumplot <- plt1 
    
    ######################################################
    ###########     Cumulative deaths
    ######################################################
    
    plt1 <- plot_ly(data=data[knat,], y=~decs, color=~countryterritoryCode)
    
    plt1 <- plt1%>%
        #add_segments(
        #    x=max(data$date[mex]), xend=max(data$date[mex]), y=0, yend=max(data$cumsum[knat]),
        #    line=list(color='grey', dash='dot'), showlegend=FALSE, visible=TRUE)%>%
        add_lines(x~date,  visible=TRUE) %>% 
        add_lines(x~c100, visible=FALSE) %>%
        add_lines(x~cd1, visible=FALSE) %>%
        add_lines(x~cd10, visible=FALSE) 
    
    # Matrix for menus "visible"
    nnat <- length(nat)
    nline <- 4
    m <- diag(nline)==TRUE
    w <- apply(m,2, FUN=function(col){
        z <- sapply(col,FUN=function(x){rep(x,nnat)})
        c(z)
    })
    #w <- w==TRUE
    
    # matrices for ranges
    xrang <- data.frame(
        d1=c(min(data$date[mex & data$cumsum>0]),max(data$date[mex])),
        d2=c(min(data$date[mex & data$cumsum>=100]),max(data$c100[knat])),
        d3=c(min(data$date[mex & data$decs>=1]),max(data$cd1[knat])),
        d4=c(min(data$date[mex & data$decs>=10]),max(data$cd10[knat]))
    )
    
    yrang <- data.frame(
        d1=c(0, max(data$decs[mex])*3.5),
        d2=c(0, max(data$decs[knat & data$c100==today()])),
        d3=c(0, max(data$decs[knat & data$cd1==today()])),
        d4=c(0, max(data$decs[knat & data$cd10==today()]))
    )
    
    
    # Buttons
    diaref <- list(type='dropdown', 
                   buttons = list(
                       list(method = "update",
                            args = list(
                                list(visible=as.list(w[,1])),
                                list(xaxis.range=xrang[,1], yaxis.range=yrang[,1])
                            ),
                            label = "Fecha"),
                       list(method = "update",
                            args = list(
                                list(visible=as.list(w[,2])),
                                list(xaxis.range=xrang[,2], yaxis.range=yrang[,2])
                            ),
                            label = "100 casos"),
                       list(method = "update",
                            args = list(
                                list(visible=as.list(w[,3])),
                                list(xaxis.range=xrang[,3], yaxis.range=yrang[,3])
                            ),
                            label = "1 deceso"),
                       list(method = "update",
                            args = list(
                                list(visible=as.list(w[,4])),
                                list(xaxis.range=xrang[,4], yaxis.range=yrang[,4])
                            ),
                            label = "10 decesos")
                   ),
                   direction = "down",
                   xanchor = 'left',
                   yanchor = "top",
                   name = "Decesos acumulados",
                   bordercolor="#bf5337",
                   #font=list(color="#ffffff"),
                   pad = list('r'= 0, 't'= 0, 'b' = 0),
                   x = xpos,
                   y = ypos
    )
    
    plt1 <- plt1 %>% layout(
        updatemenus = list( diaref))
    
    
    # Abline data
    hoy <- list(type='line', x0=max(data$date[mex]), x1=max(data$date[mex]), y0=0, y1=max(data$decs[knat]), 
                line=list(dash='dot', width=1, color="#808080"))
    # Menu title
    annot <- list(text = "Decesos acumulados según:", x=0, y=1.05, 
                  xref='paper', yref='paper', showarrow=FALSE,
                  font=list(color="#bf5337", size=16))
    
    plt1 <- plt1%>%    
        layout(shapes=hoy, #abline
               xaxis=list(title='Fecha (base México)'),# axis labels
               yaxis=list(title='Decesos por Covid',
                          range=c(0,max(data$decs[mex]*3))),#initial range
               annotations=annot, #Menu title
               hovermode = 'compare' # Set hovermode as default
        )
    
    decsplot <- plt1 
    
    ######################################################
    ###########     dayly deaths
    ######################################################
    
    plt1 <- plot_ly(data=data[knat,], y=~deaths, color=~countryterritoryCode)
    
    plt1 <- plt1%>%
        #add_segments(
        #    x=max(data$date[mex]), xend=max(data$date[mex]), y=0, yend=max(data$cumsum[knat]),
        #    line=list(color='grey', dash='dot'), showlegend=FALSE, visible=TRUE)%>%
        add_lines(x~date,  visible=TRUE) %>% 
        add_lines(x~c100, visible=FALSE) %>%
        add_lines(x~cd1, visible=FALSE) %>%
        add_lines(x~cd10, visible=FALSE) 
    
    # Matrix for menus "visible"
    nnat <- length(nat)
    nline <- 4
    m <- diag(nline)==TRUE
    w <- apply(m,2, FUN=function(col){
        z <- sapply(col,FUN=function(x){rep(x,nnat)})
        c(z)
    })
    #w <- w==TRUE
    
    # matrices for ranges
    xrang <- data.frame(
        d1=c(min(data$date[mex & data$cumsum>0]),max(data$date[mex])),
        d2=c(min(data$date[mex & data$cumsum>=100]),max(data$c100[knat])),
        d3=c(min(data$date[mex & data$decs>=1]),max(data$cd1[knat])),
        d4=c(min(data$date[mex & data$decs>=10]),max(data$cd10[knat]))
    )
    
    yrang <- data.frame(
        d1=c(0, max(data$deaths[mex])*3.5),
        d2=c(0, max(data$deaths[knat & data$c100==today()])),
        d3=c(0, max(data$deaths[knat & data$cd1==today()])),
        d4=c(0, max(data$deaths[knat & data$cd10==today()]))
    )
    
    # Buttons
    diaref <- list(type='dropdown', 
                   buttons = list(
                       list(method = "update",
                            args = list(
                                list(visible=as.list(w[,1])),
                                list(xaxis.range=xrang[,1], yaxis.range=yrang[,1])
                            ),
                            label = "Fecha"),
                       list(method = "update",
                            args = list(
                                list(visible=as.list(w[,2])),
                                list(xaxis.range=xrang[,2], yaxis.range=yrang[,2])
                            ),
                            label = "100 casos"),
                       list(method = "update",
                            args = list(
                                list(visible=as.list(w[,3])),
                                list(xaxis.range=xrang[,3], yaxis.range=yrang[,3])
                            ),
                            label = "1 deceso"),
                       list(method = "update",
                            args = list(
                                list(visible=as.list(w[,4])),
                                list(xaxis.range=xrang[,4], yaxis.range=yrang[,4])
                            ),
                            label = "10 decesos")
                   ),
                   direction = "down",
                   xanchor = 'left',
                   yanchor = "top",
                   name = "Referencia",
                   bordercolor="#bf5337",
                   #font=list(color="#ffffff"),
                   pad = list('r'= 0, 't'= 0, 'b' = 0),
                   x = xpos,
                   y = ypos
    )
    
    plt1 <- plt1 %>% layout(
        updatemenus = list( diaref))
    
    
    # Abline data
    hoy <- list(type='line', x0=max(data$date[mex]), x1=max(data$date[mex]), y0=0, y1=max(data$deaths[knat]), 
                line=list(dash='dot', width=1, color="#808080"))
    # Menu title
    annot <- list(text = "Decesos diarios según:", x=0, y=1.05, 
                  xref='paper', yref='paper', showarrow=FALSE,
                  font=list(color="#bf5337", size=16))
    
    plt1 <- plt1%>%    
        layout(shapes=hoy, #abline
               xaxis=list(title='Fecha (base México)'),# axis labels
               yaxis=list(title='Fallecimientos por Covid-19',
                          range=c(0,max(data$deaths[mex]*3))),#initial range
               annotations=annot, #Menu title
               hovermode = 'compare' # Set hovermode as default
        )
    
    deathsplot <- plt1 
    
    ######################################################
    ###########     dayly cases
    ######################################################
    
    plt1 <- plot_ly(data=data[knat,], y=~cases, color=~countryterritoryCode)
    
    plt1 <- plt1%>%
        #add_segments(
        #    x=max(data$date[mex]), xend=max(data$date[mex]), y=0, yend=max(data$cumsum[knat]),
        #    line=list(color='grey', dash='dot'), showlegend=FALSE, visible=TRUE)%>%
        add_lines(x~date,  visible=TRUE) %>% 
        add_lines(x~c100, visible=FALSE) %>%
        add_lines(x~cd1, visible=FALSE) %>%
        add_lines(x~cd10, visible=FALSE) 
    
    # Matrix for menus "visible"
    nnat <- length(nat)
    nline <- 4
    m <- diag(nline)==TRUE
    w <- apply(m,2, FUN=function(col){
        z <- sapply(col,FUN=function(x){rep(x,nnat)})
        c(z)
    })
    #w <- w==TRUE
    
    # matrices for ranges
    xrang <- data.frame(
        d1=c(min(data$date[mex & data$cumsum>0]),max(data$date[mex])),
        d2=c(min(data$date[mex & data$cumsum>=100]),max(data$c100[knat])),
        d3=c(min(data$date[mex & data$decs>=1]),max(data$cd1[knat])),
        d4=c(min(data$date[mex & data$decs>=10]),max(data$cd10[knat]))
    )
    
    yrang <- data.frame(
        d1=c(0, max(data$cases[mex])*3.5),
        d2=c(0, max(data$cases[knat & data$c100==today()])),
        d3=c(0, max(data$cases[knat & data$cd1==today()])),
        d4=c(0, max(data$cases[knat & data$cd10==today()]))
    )
    
    # Buttons
    diaref <- list(type='dropdown', 
                   buttons = list(
                       list(method = "update",
                            args = list(
                                list(visible=as.list(w[,1])),
                                list(xaxis.range=xrang[,1], yaxis.range=yrang[,1])
                            ),
                            label = "Fecha"),
                       list(method = "update",
                            args = list(
                                list(visible=as.list(w[,2])),
                                list(xaxis.range=xrang[,2], yaxis.range=yrang[,2])
                            ),
                            label = "100 casos"),
                       list(method = "update",
                            args = list(
                                list(visible=as.list(w[,3])),
                                list(xaxis.range=xrang[,3], yaxis.range=yrang[,3])
                            ),
                            label = "1 deceso"),
                       list(method = "update",
                            args = list(
                                list(visible=as.list(w[,4])),
                                list(xaxis.range=xrang[,4], yaxis.range=yrang[,4])
                            ),
                            label = "10 decesos")
                   ),
                   direction = "down",
                   xanchor = 'left',
                   yanchor = "top",
                   name = "Referencia",
                   bordercolor="#bf5337",
                   #font=list(color="#ffffff"),
                   pad = list('r'= 0, 't'= 0, 'b' = 0),
                   x = xpos,
                   y = ypos
    )
    
    plt1 <- plt1 %>% layout(
        updatemenus = list( diaref))
    
    
    # Abline data
    hoy <- list(type='line', x0=max(data$date[mex]), x1=max(data$date[mex]), y0=0, y1=max(data$cases[knat]), 
                line=list(dash='dot', width=1, color="#808080"))
    # Menu title
    annot <- list(text = "Casos diarios según:", x=0, y=1.05, 
                  xref='paper', yref='paper', showarrow=FALSE,
                  font=list(color="#bf5337", size=16))
    
    plt1 <- plt1%>%    
        layout(shapes=hoy, #abline
               xaxis=list(title='Fecha (base México)'),# axis labels
               yaxis=list(title='Casos confirmados',
                          range=c(0,max(data$cases[mex]*3))),#initial range
               annotations=annot, #Menu title
               hovermode = 'compare' # Set hovermode as default
        )
    
    casesplot <- plt1 
    
    
    ######################################################
    ###########     Save plots
    ######################################################
    
    save(cumplot, decsplot, casesplot, deathsplot, file='plots.Rdata')
        
    
