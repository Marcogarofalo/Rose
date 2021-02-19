
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
#' plot a fit to some data appending to the existing ggplot
#' @param g a ggplot object created with ggplot()
#' @param d data frame
#' @param  T total time extend of the plot (T/2)
#' @import ggplot2
many_fit_ggplot<-function(d,fit_par, fit_range,T, logscale="no", g, mylabel){

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

  mylabel<-mylabel
  gg <- g + ggplot2::geom_point(data=mydf,mapping=aes(x=x, y=y,colour=mylabel),inherit.aes = FALSE)

  gg <- gg +ggplot2::geom_errorbar(data=mydf, mapping=aes(x=x, ymin=y-err, ymax=y+err,color=mylabel),
                                   width = 0.3,inherit.aes = FALSE)


  gg <- gg +ggplot2::geom_ribbon( data=mydf,
                mapping=aes(x=xfit, ymin=fit-errfit,ymax=fit+errfit ,color=mylabel,fill=mylabel)
                                  ,alpha=0.3      ,inherit.aes = FALSE)
  fit_range<-fit_range
  gg <- gg+ ggplot2::geom_line(data=mydf, aes(x=fit_range[1],y=y,  color=mylabel), linetype="dashed")
  gg <- gg+ ggplot2::geom_line( data=mydf ,aes(x=fit_range[2],y=y, color=mylabel), linetype="dashed")

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
