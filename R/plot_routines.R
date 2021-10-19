
#' set margin for a fit
#'
#' @param fit_range list of two number tmin and tmax
#' @param  T total time extend of the plot (T/2)

set_xmargin<- function(fit_range, T){
  xmin <-fit_range[1]-7
  xmax <-fit_range[2]+7
  if (xmin<0 ){ xmin<-0}
  if (xmax>T ){ xmax<-T}
  c(xmin,xmax)
}




#' plot a plateaux
#' @param d data frame
#' @param  T total time extend of the plot (T/2)

myggplot<-function(d,fit, fit_range,T,logscale="no"){

  gg <- ggplot2::ggplot(d, aes(x=d[,1], y=d[,2])) + geom_point()
  #gg  <- gg + xlim(set_xmargin(fit_range,T) ) + ylim(fit[1,1]-15*fit[1,2], fit[1,1]+15*fit[1,2])
  gg <- gg +ggplot2::geom_errorbar(aes(ymin=d[,2]-d[,3], ymax=d[,2]+d[,3]),  width = 1)
  gg <- gg+ labs(x = 't', y= 'y')
  # plot orizontal line with fit
  gg <- gg+ ggplot2::geom_segment(aes(x = fit_range[1], y = fit[1,1], xend = fit_range[2], yend = fit[1,1]) , linetype="dashed", color = "red")
  gg <- gg+ ggplot2::geom_segment(aes(x = fit_range[1], y = fit[1,1]-fit[1,2], xend = fit_range[2], yend = fit[1,1]-fit[1,2]) , linetype="solid", color = "red")
  gg <- gg+ ggplot2::geom_segment(aes(x = fit_range[1], y = fit[1,1]+fit[1,2], xend = fit_range[2], yend = fit[1,1]+fit[1,2]) , linetype="solid", color = "red")
  gg <- gg+theme_bw()
  s<- sprintf("%.6f",fit[1,1])
  err<- sprintf("%.6f",fit[1,2])


  pander(paste0("  fit: $m_{eff}=",s,"\\pm",err,"$"))
  #plot(gg)
  return(gg)
}



#####################################################################
#####################################################################

#' plot a fit to a data
#' @param d data frame
#' @param  T total time extend of the plot (T/2)


my_fit_ggplot<-function(d,fit_par, fit_range,T, logscale="no"){

  l<- length(d[1,])
  fit_precision<- 2 #(l -2)/3  # the number of x of the fits
  mydf <-data.frame('x'=c(0), 'y'=c(0), 'err'=c(0)
                    ,'xfit'=c(0), 'fit'=c(0), 'errfit'=c(0) )
  mydf<- mydf[-1,]
  #
  colx <- c(1,c(1:fit_precision*3))[-2] # 1, 6, 9, 12,..#columns of the x
  colf <- c(4,c(1:fit_precision*3+1))[-2]# 4, 7, 10, 13,..#columns of the fit
  colferr <- c(5,c(1:fit_precision*3+2))[-2]# 5, 8, 11, 14,..#columns of the fit
  count<-1
  for(i in c(1:fit_precision)) {
    for (t in c(1: length(d[,1])) ){
      mylist  <-  list(d[t,1],d[t,2], d[t,3]  )
      mylist  <- append(mylist, list( d[t,colx[i]],d[t,colf[i]], d[t,colferr[i]]  ) )
      mydf[count,]<- mylist
      count<-count+1
    }
  }
  if (logscale=="yes"){
    mydf[,3]<- mydf[,3]/mydf[,2]
    mydf[,6]<- mydf[,6]/mydf[,5]
    mydf<-dplyr::mutate_at(mydf,c(2,5) ,function(x) log10(x))
  }
  #gg <- gg+ scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #            labels = trans_format("log10", math_format(10^.x)))

  gg <- ggplot2::ggplot(mydf, aes(x=x, y=y)) + ggplot2::geom_point()

  gg <- gg +ggplot2::geom_errorbar(data=mydf, mapping=aes(x=x, ymin=y-err, ymax=y+err),
                                   width = 0.3,inherit.aes = FALSE)
  #

  gg <- gg +ggplot2::geom_ribbon( mapping=aes(x=xfit, ymin=fit-errfit,ymax=fit+errfit ),
                                  color="darkgreen",alpha=0.3,fill="darkgreen",
                                  inherit.aes = FALSE)
  gg <- gg+ ggplot2::geom_line( aes(x=fit_range[1]), color="red", linetype="dashed")
  gg <- gg+ ggplot2::geom_line( aes(x=fit_range[2]), color="red", linetype="dashed")


  #
  #gg <- gg+ labs(x = TeX('x_0/a'), y= TeX('$c(x_0/a)$'))
  #
  #
  # gg <- gg+theme_bw()
  # len<-length(fit_par[1,])  /2-1
  # for(i in c(1:len )  ){
  #   if(! is.na(fit_par[1,i*2])) {
  #     s<- sprintf("P[%d]=%.6f ", i,fit_par[1,i*2-1])
  #     err<- sprintf("%.6f",fit_par[1,i*2])
  #     pander(paste0("$",s,"\\pm ",err,"$ "))
  #   }
  # }


  return(gg)
}

#####################################################################
#####################################################################
#'  add to a ggplot() points with symmetric error
#'  @param gg a ggplot() object where to add the layers
#'  @param x  coordinate
#'  @param y  coordinate
#'  @param x error on y coordinate
#'  @param ... add the other aes options: color, size, fill, width
geom_error<-function(gg,x,y, dy, ... ){
  gg<- gg +  geom_point(aes(x=x, y=y,...) )
  gg<- gg +  geom_errorbar(aes(x=x, ymin=y-dy, ymax=y+dy,...))
  return(gg)
}

#####################################################################
#####################################################################
#' from a data frame of the form
#' t   data err fit err  t+h fit  err t+2h fit err
#' return a data frame of the form
#'  t   data err fit err
#'  t+h   data err fit err
#'  t+2h   data err fit err
#'  @param d data frame
reshape_df_analysis_to_ggplot<-function(d){

  l<- length( which( d[1,]!="NA" ) )
  fit_precision<-   (l -2)/3  # the number of x of the fits

  mydf <-data.frame('x'=c(0), 'y'=c(0), 'err'=c(0)
                    ,'xfit'=c(0), 'fit'=c(0), 'errfit'=c(0) )
  mydf<- mydf[-1,]
  #
  colx <- c(1,c(1:fit_precision*3))[-2] # 1, 6, 9, 12,..#columns of the x
  colf <- c(4,c(1:fit_precision*3+1))[-2]# 4, 7, 10, 13,..#columns of the fit
  colferr <- c(5,c(1:fit_precision*3+2))[-2]# 5, 8, 11, 14,..#columns of the fit
  count<-1
  for(i in c(1:fit_precision)) {
    for (t in c(1: length(d[,1])) ){
      mylist  <-  list(d[t,1],d[t,2], d[t,3]  )
      mylist  <- append(mylist, list( d[t,colx[i]],d[t,colf[i]], d[t,colferr[i]]  ) )
      mydf[count,]<- mylist
      count<-count+1
    }
  }

  return(mydf)
}


#####################################################################
#####################################################################
#' plot data minus fit appending to the existing ggplot
#' @param g a ggplot object created with ggplot()
#' @param d data frame
#' @param  T total time extend of the plot (T/2)
#' @import ggplot2
residual_ggplot<-function(d,fit_par, fit_range,T, logscale="no", g, mylabel){
  mydf<-reshape_df_analysis_to_ggplot(d)

  if (logscale=="yes"){
    mydf[,3]<- mydf[,3]/mydf[,2]
    mydf[,6]<- mydf[,6]/mydf[,5]
    mydf<-dplyr::mutate_at(mydf,c(2,5) ,function(x) log10(x))
  }
  mylabel<-mylabel
  gg <- g + ggplot2::geom_point(data=d,mapping=aes(x=d[,1], y=d[,2]-d[,4],colour=mylabel),inherit.aes = FALSE)

  gg <- gg +ggplot2::geom_errorbar(data=d, mapping=aes(x=d[,1], ymin=d[,2]-d[,3]-d[,4], ymax=d[,2]+d[,3]-d[,4],color=mylabel),
                                   width = 0.3,inherit.aes = FALSE)


  gg <- gg +ggplot2::geom_ribbon( data=mydf,
                                  mapping=aes(x=xfit, ymin=-errfit,ymax=+errfit ,fill=mylabel),color=NA
                                  ,alpha=0.2      ,inherit.aes = FALSE, show.legend = FALSE)
  fit_range<-fit_range

  gg <- gg+ ggplot2::geom_line(data=d, aes(x=fit_range[1],y=d[,2]-d[,4]+d[,3],  color=mylabel),alpha=0.3, linetype="dashed",)
  gg <- gg+ ggplot2::geom_line( data=d ,aes(x=fit_range[2],y=d[,2]-d[,4]+d[,3], color=mylabel),alpha=0.3, linetype="dashed")


  gg <- gg+ggplot2::theme_bw()

  return(gg)
}

#' plot a fit to some data appending to the existing ggplot
#' @param fit_par the output of read_fit()

print_fit<- function(fit_par){
  len<-length(fit_par[1,])  /2-1
  for(i in c(1:len )  ){
    if(! is.na(fit_par[1,i*2])) {
      s<- sprintf("P[%d]=%.6f ", i,fit_par[1,i*2-1])
      err<- sprintf("%.6f",fit_par[1,i*2])
      pander(paste0("$",s,"\\pm ",err,"$ "))
    }
  }

}

#####################################################################
#####################################################################
#' plot a fit to some data appending to the existing ggplot
#' @param g a ggplot object created with ggplot()
#' @param d data frame
#' @param  Th total time extend of the plot (T/2)
#' @param logscale can be "yes", everithig else means no
#' @param extrax  extra time space to give to the plot after the plateau range default c(2,4)
#' @param extrax extra yrange in unit of the yerror , default c(5,5)
#' @import ggplot2
scale_fit_ggplot<-function(d, fit_range,Th, logscale="no", g,extrax=c(2,4), extray=c(5,5) ){

  rowmin=d[fit_range[1],1]
  rowmax=d[fit_range[2],1]
  ymin=(min(d[rowmin,2]-extray[1]*d[rowmin,3]  ,d[rowmax,2]-extray[1]*d[rowmax,3]))
  ymax=(max(d[rowmin,2]+extray[2]*d[rowmin,3]  ,d[rowmax,2]+extray[2]*d[rowmax,3]))

  if (logscale=="yes"){
    ymin=log10(ymin)
    ymax=log10(ymax)
  }

  xmin=fit_range[1]-extrax[1]
  if (xmin <0)
    xmin=0
  xmax=fit_range[2]+extrax[2]
  if (xmax >Th)
    xmax=Th


  gg<- g+ggplot2::coord_cartesian(xlim=c(xmin,xmax), ylim=c(ymin,ymax ))
  return(gg)
}



#####################################################################
#####################################################################
#' adjust and print the plot to the output format, it return a ggplot if pdf output
#' or a plot_ly if html
#' @param gg a ggplot object created with ggplot()
#' @param title title sting, it can contain tex
#' @param xlabel total time extend of the plot (T/2)
#' @param ylabel can be "yes", everithig else means no
#' @param xrange range of x axis, e.g. c(0,10)
#' @param yrange range of y axis, e.g. c(0,10)
#' @param to_print if you want to print now the plot now
#' @param save_pdf name of the file if you want to save (without .pdf)
#' @param legend_position e.g c(0.5,0.5) middle, c(0,0) bottom left
#' @param legend_title if NULL (default)  eliminate the legend title
#' @import ggplot2, plotly, tikzDevice
myplotly<-function(gg, title="",xlabel="", ylabel="",
                   xrange=NULL, yrange=NULL,
                   output="AUTO", to_print=TRUE, save_pdf=NULL,
                   legend_position=NULL, legend_title=NULL, width=680, height=480){
  gg<- gg+ theme_bw()
  if(is.null(legend_title))gg<- gg+theme(legend.title = element_blank())

  HTML=FALSE
  PDF=FALSE
  if (output=="AUTO"){
    if(knitr::is_html_output()){
      HTML=TRUE
    }
    else if(knitr::is_latex_output()){
      PDF=TRUE
    }
    else {
      HTML=TRUE
      PDF=TRUE
    }

  }
  else if(output=="HTML"){
    HTML=TRUE
    PDF=FALSE
  }
  else if(output=="PDF"){
    PDF=TRUE
    HTML=FALSE
  }


  if(!is.null(save_pdf) ){
    HTML=FALSE
    PDF=TRUE
  }


  if(HTML) {
    if(is.null(xrange)){autox=TRUE; rangex=xrange }else {autox=FALSE; rangex=xrange }
    if(is.null(yrange)){autoy=TRUE; rangey=yrange }else {autoy=FALSE; rangey=yrange }
    fig<- plotly::ggplotly(gg, dynamicTicks = TRUE, width=width, height = height)%>%
      layout(title=title,
        xaxis = list(title = xlabel,showexponent = "all", exponentformat = "e",
                     autorange = autox , range=rangex ) ,
        yaxis = list(title = ylabel,showexponent = "all", exponentformat = "e",
                     autorange = autoy , range=rangey )
      )

    if(!is.null(legend_position))
      fig<-fig%>% layout(legend = list(x = legend_position[1],
                                       y = legend_position[2]))
    if(to_print) print(htmltools::tagList(fig))
  }
  else if (PDF  ){
    #if(!is.null(save_pdf) ) pdf(save_pdf)
    #knitr::opts_chunk$set(  dev='tikz')
    if (stringr::str_detect(xlabel,"\\$")){labelx=xlabel} else {labelx=paste0("\\verb|",xlabel,"|")}
    if (stringr::str_detect(ylabel,"\\$")){labely=ylabel} else {labely=paste0("\\verb|",ylabel,"|")}
    if (stringr::str_detect(title,"\\$")){mytitle=title} else {mytitle=paste0("\\verb|",title,"|")}

    fig<-gg
    if(!(is.null(xrange) && is.null(yrange))) fig<-fig +ggplot2::coord_cartesian(xlim= xrange,ylim= yrange)
    else if(is.null(yrange)) fig<-fig +ggplot2::coord_cartesian(xlim= xrange)
    else if(is.null(xrange)) fig<-fig +ggplot2::coord_cartesian(ylim= yrange)
    if(!is.null(title)) fig<- fig +ggplot2::ggtitle(mytitle)
    if(!is.null(ylabel))fig<- fig +ggplot2::xlab(labelx)
    if(!is.null(xlabel))fig<- fig +ggplot2::ylab(labely)

    if(!is.null(legend_position)) fig<-fig+theme( legend.position =legend_position)

    if(to_print) plot(fig)
    if(!is.null(save_pdf) ) {
      texfile=paste0(save_pdf,".tex")
      tikzDevice::tikz(texfile,standAlone=TRUE, width = width/100, height = height/100)
      plot(fig)
      dev.off()
      tools::texi2dvi(paste0(save_pdf,".tex"),pdf=TRUE)
    }

  }
  else {
    print("not output selected")
    fig<- gg
  }

  return(fig)
}


#' set margin for a fit
#'
#' @param fit_range list of two number tmin and tmax
#' @param  T total time extend of the plot (T/2)

set_xmargin<- function(fit_range, T){
  xmin <-fit_range[1]-7
  xmax <-fit_range[2]+7
  if (xmin<0 ){ xmin<-0}
  if (xmax>T ){ xmax<-T}
  c(xmin,xmax)
}






#####################################################################
#####################################################################
#'  add to a ggplot() points with symmetric error
#'  @param gg a ggplot() object where to add the layers
#'  @param x  coordinate
#'  @param y  coordinate
#'  @param x error on y coordinate
#'  @param ... add the other aes options: color, size, fill, width
geom_error<-function(gg,x,y, dy, ... ){
  gg<- gg +  geom_point(aes(x=x, y=y,...) )
  gg<- gg +  geom_errorbar(aes(x=x, ymin=y-dy, ymax=y+dy,...))
  return(gg)
}

#####################################################################
#####################################################################
#' from a data frame of the form
#' t   data err fit err  t+h fit  err t+2h fit err
#' return a data frame of the form
#'  t   data err fit err
#'  t+h   data err fit err
#'  t+2h   data err fit err
#'  @param d data frame
reshape_df_analysis_to_ggplot<-function(d){

  l<- length( which( d[1,]!="NA" ) )
  fit_precision<-   (l -2)/3  # the number of x of the fits

  mydf <-data.frame('x'=c(0), 'y'=c(0), 'err'=c(0)
                    ,'xfit'=c(0), 'fit'=c(0), 'errfit'=c(0) )
  mydf<- mydf[-1,]
  #
  colx <- c(1,c(1:fit_precision*3))[-2] # 1, 6, 9, 12,..#columns of the x
  colf <- c(4,c(1:fit_precision*3+1))[-2]# 4, 7, 10, 13,..#columns of the fit
  colferr <- c(5,c(1:fit_precision*3+2))[-2]# 5, 8, 11, 14,..#columns of the fit
  count<-1
  for(i in c(1:fit_precision)) {
    for (t in c(1: length(d[,1])) ){
      mylist  <-  list(d[t,1],d[t,2], d[t,3]  )
      mylist  <- append(mylist, list( d[t,colx[i]],d[t,colf[i]], d[t,colferr[i]]  ) )
      mydf[count,]<- mylist
      count<-count+1
    }
  }

  return(mydf)
}

#####################################################################
#####################################################################
#' plot a fit to some data appending to the existing ggplot
#' @param g a ggplot object created with ggplot()
#' @param d data frame
#' @param  T total time extend of the plot (T/2)
#' @param  logscale default "no"
#' @param  fit_range  e.g. c(5,6)
#' @param  fit_par unused
#' @param  mylabel a label that goes in both color and fill
#' @param  nudge  shift the point on the x axes, default 0
#' @import ggplot2
many_fit_ggplot<-function(d,fit_par, fit_range,T, logscale="no", g, mylabel, nudge=0.0){

  mydf<-reshape_df_analysis_to_ggplot(d)
  if (logscale=="yes"){
    mydf[,3]<- mydf[,3]/mydf[,2]
    mydf[,6]<- mydf[,6]/mydf[,5]
    # for (i in c(1:length(mydf[,1]))){
    #    if (mydf[i,2]>0)  mydf[i,2]=log10(mydf[i,2])
    #    else              mydf[i,2]=-20
    #    if (mydf[i,5]>0)  mydf[i,5]=log10(mydf[i,5])
    #    else              mydf[i,5]=-20
    # }

    mydf<-dplyr::mutate_at(mydf,c(2,5) ,function(x) log10(x) )
  }
  #gg <- gg+ scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #            labels = trans_format("log10", math_format(10^.x)))
  mylabel<-mylabel
  gg <- g + ggplot2::geom_point(data=mydf,mapping=aes(x=x, y=y,
                                                      colour=mylabel, fill=mylabel),
                                position =position_nudge(x=nudge),
                                                  inherit.aes = FALSE)

  gg <- gg +ggplot2::geom_errorbar(data=mydf, mapping=aes(x=x, ymin=y-err, ymax=y+err,
                                                      color=mylabel, fill=mylabel),
                                   position =position_nudge(x=nudge),
                                   width = 0.3,inherit.aes = FALSE)


  gg <- gg +ggplot2::geom_ribbon( data=mydf,
                mapping=aes(x=xfit, ymin=fit-errfit,ymax=fit+errfit ,
                            color=mylabel, fill=mylabel),
                                  alpha=0.2      ,inherit.aes = FALSE, show.legend = FALSE)
  fit_range<-fit_range

  gg <- gg+ ggplot2::geom_line(data=mydf, aes(x=fit_range[1],y=y,
                                              color=mylabel, fill=mylabel),
                               alpha=0.3, linetype="dashed",position =position_nudge(x=-0.1))
  gg <- gg+ ggplot2::geom_line( data=mydf ,aes(x=fit_range[2],y=y,
                                               color=mylabel,  fill=mylabel),
                                alpha=0.3, linetype="dashed",position =position_nudge(x=0.1))

  #gg  <- gg + xlim(set_xmargin(fit_range,128/2) ) + ylim(-2e+4, 1e+4)
  #gg<- gg +geom_text(data=mydf, aes(x=x,y=y), label=mylabel)
  #gg <- gg+ labs(x = TeX('x_0/a'), y= TeX('$c(x_0/a)$'))


  gg <- gg+ggplot2::theme_bw()
  # len<-length(fit_par[1,])  /2-1
  # for(i in c(1:len )  ){
  #   if(! is.na(fit_par[1,i*2])) {
  #     s<- sprintf("P[%d]=%.6f ", i,fit_par[1,i*2-1])
  #     err<- sprintf("%.6f",fit_par[1,i*2])
  #     pander(paste0("$",s,"\\pm ",err,"$ "))
  #   }
  # }
  #

  return(gg)
}


#####################################################################
#####################################################################
#' print result of a fit
#' @param label a ggplot object created with ggplot()
#' @param fit c(P0, dP0, P1, DP1, ... )name of the string to find
#' @param  all_obs the outcome of Rose::get_all_corr()
#' @param  l logscale default "no"
#' @import ggplot2
print_fit_res<-function(label,fit,all_obs,l){
  str2<-paste(label,"=")
  for( i in c(1:(length(fit[1,])/2))*2-1 ){
    if (!is.na(fit[1,i]))
      str2 <- paste(str2,"  ",mean_print(fit[1,i],fit[1,i+1]) )
  }
  str2 <- paste(str2,"  $\\chi^2/dof=$",all_obs[l,4])
  cat(str2,'\n\n')
}

#####################################################################
#####################################################################
#' plot a fit to some data appending to the existing ggplot
#' @param gg a ggplot object created with ggplot()
#' @param string name of the string to find
#' @param  all_obs the outcome of Rose::get_all_corr()
#' @param  log logscale default "no"
#' @param  number if set it will ignore the string and plot the block selected by
#' number
#' @param  nudge  shift the point on the x axes, default 0
#' @param  print_res  if you want the result of the fit to be printed
#' default TRUE
#' @import ggplot2
plot_corr<-function(string,all_obs,mt, L,T ,gg ,log,number=NULL, nudge=0.0, print_res=TRUE){
  string=sprintf("\\b%s\\b",string)# need to put the delimiters on the word to grep
  label<-paste0(gsub('\\\\b','',string),"(L",L,"T",T,")")
  if (is.null(number)){
    l<-grep(string,all_obs[,"corr"])
    n<-all_obs[l,"n"]
  }
  else {
    n=number
    l=n
  }

  d<- get_block_n(mt,n)
  fit<- get_fit_n(mt,n)
  fit_range<- get_plateaux_range(mt,n)
  gg<- many_fit_ggplot(d,fit,fit_range,T/2,log,gg,  label ,nudge )
  if(print_res)  print_fit_res(label,fit,all_obs,l)

  return(gg)
}
