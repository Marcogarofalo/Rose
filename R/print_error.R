# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' return a string like 1.32(11)
#'
#' This function allows you read a file in gnuplot style,
#'  Each block is a chunk of data separated by an empty line
#' @param mean mean value
#' @param err error
#' @param digit number of digit in the error, default=2
#' @details  Important it allows only for 20 columns
#' @export
#' @examples mean_print(1.32,0.11)

mean_print<-function(ave,err,digit=2,mean_exp=TRUE){
  stopifnot(digit>0)
  if( !is.numeric(ave) | is.na(ave)  ){
    s=sprintf("NA")
    return(s)
  }
  if(!is.numeric(err) | is.na(err) | err==0 ){
    s=sprintf("%g",ave)
    return(s)
  }

  if(err<0){
    s=sprintf("%g(%g)",ave,err)
    return(s)
  }
  if(ave<0)
    ave1<- -ave
  else
    ave1<-ave

  a<-(log10(ave1))
  e<-(log10(err+err/1000))
  if ( a<4  & a>-4){
    we<-(err/10^(as.integer(e-digit)))
    if(e<0){
      e<-as.integer(e)
      format=sprintf("%%.%df(%%.0f)",-e+digit)
      s=sprintf(format,ave1,we);

    }
    else if(e>1){
      format=sprintf("%%.%df(%%.%df)",digit-2,digit-2)
      s=sprintf(format,ave1,err);

    }
    else{
      format=sprintf("%%.%df(%%.%df)",digit-1, digit-1 )
      s=sprintf(format,ave1,err);

    }

  }
  else{
    if(mean_exp ){
      a<-as.integer(a)
      if (a<1) a<- a-1
      e<-as.integer(e)
      if (e<1) e<- e-1
      wm<-( ave1/10^(a))
      we<-(err/10^(a))
      #format=sprintf("%%.%df(%%.%.df)e%%+-d",a-e+digit-1,a-e+digit-1)

      e1<-(log10(we))

      if (e1<0 ){
        e1<-as.integer(e1)
        we<- we/ 10^(e1-digit)
        format=sprintf("%%.%df(%%.0f)e%%+-d",a-e+digit-1)
        s=sprintf(format,wm,we,a)
      }
      else if (e1<1) {
        format=sprintf("%%.%df(%%.%df)e%%+-d",digit-1, digit-1 )
        s=sprintf(format,wm,we,a)
      }
      else {
        dig_error=as.integer(e1)
        dig_mean<-(digit-2)
        if (dig_mean<0) dig_mean<-0
        format=sprintf("%%.%df(%%.%df)e%%+-d",dig_mean,dig_mean)
        s=sprintf(format,wm,we,a)
      }

    }
    else{
      a<-as.integer(a)
      e<-as.integer(e-digit)
      wm<-( ave1/10^(e))
      we<-(err/10^(e))
      s=sprintf("%.0f(%.0f)e%+-d",wm,we,e);
      #
      #s=sprintf("%.1f(%.1f)e%+-d",wm,we,e);
    }
  }
  if(ave<0)
    s=sprintf("-%s",s);

  return(s)

}
