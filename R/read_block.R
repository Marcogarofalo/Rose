
#' read a data frame from a file with maximum 20 column
#'
#' This function allows you read a file in gnuplot style,
#'  it enumerates the file with  blocks.
#'  Each block is a chunk of data separated by an empty line
#' @param file connection to a file or string with the name .
#' @details  Important it allows only for 20 columns

read_df<- function(file){
  columns_file<-c(1:20)
  lc<- length(columns_file)+1

  all_file <- read.table(file,header=FALSE,fill = TRUE ,
                         blank.lines.skip=FALSE,skip=0, comment.char = "",
                         col.names = columns_file)
  # get the blank lines
  #add to the list an extra fictitious blank line
  ix <- c(which(all_file[,1]==""),nrow(all_file) )
  # get the blank line after one blank line
  ixp <- c(which(all_file[ix+1,1]=="")  )
  #ixp <- c(0,which(all_file[ix+1,1]=="") )
  ixm<- c(0,ix[-ixp])
  # enumerate  with blocks
  iblock<-rep(1:length(diff(ixm)),diff(ixm))
  #m <- cbind(all_file,rep(1:length(diff(ix)),diff(ix)))
  #remove blank lines
  #m <- m[!(m[,1]==""),]

  mt<-cbind("index"=iblock, all_file)
  mt <- mt[!(mt[,2]==""),]
  if (mt[1,1]!=1)
    mt[,1]<-mt[,1]-mt[1,1]+1
  return(mt)
}




#' get a block of data
#'
#' This function allows to read a file in gnuplot style,
#'  it enumerates the file with  blocks.
#'  Each block is a chunk of data separated by an empty line
#' @param df data frame constructed with read_df
#' @param n index of the block of data
#' @details  Important it allows only for 20 columns

get_block_n<- function(df,n){
  bo<-which(df[,1]==(n*2-1))
  data <- df[bo,-1]
  if (!rlang::is_empty(grep("^#", data[,1]) ) ) {
    data<-data[-grep("^#", data[,1])  ,  ]
  }
  #data<-dplyr::mutate_all(data, function(x) as.numeric(as.character(x)))
  data<-dplyr::mutate_all(data, function(x) as.numeric(x))
  return(data)
}


#' get fit result
#'
#' This function allows you read a file in gnuplot style,
#'  Each block is a chunk of data separated by an empty line
#' @param df data frame constructed with read_df
#' @param n index of the block of data
#' @details  Important it allows only for 20 columns

get_fit_n<- function(df,n){
  be<-which(df[,1]==(n*2))
  data <- df[be,-1]
  if (!rlang::is_empty(grep("^#", data[,1]) ) ){
    data<-data[-grep("^#", data[,1])  ,  ]
  }
  data<-dplyr::mutate_all(data, function(x) as.numeric(as.character(x)))
  return(data)
}


#' get plateaux range result
#'
#' This function allows you read a file in gnuplot style,
#'  Each block is a chunk of data separated by an empty line
#' @param df data frame constructed with read_df
#' @param n index of the block of data
#' @details  Important it allows only for 20 columns

get_plateaux_range<-function(df,n){
  l<-grep("fit",df[,3])
  a1<-gsub("\\[","c\\(", df[l,5][n])
  a2<-gsub("\\]","\\)", a1)
  fit_range <- eval(parse(text=a2))

  return(fit_range)
}


#' get all the correlator name in a file
#'
#' This function allows you read a file in gnuplot style,
#'  Each block is a chunk of data separated by an empty line
#' @param df data frame constructed with read_df
#' @return  a data frame with columns "n" an integer integer,"corr" the name of
#' the correlator, "fit_Range"   , "chi2/dof" chi2 value,
#' @details  Important it allows only for 20 columns

get_all_corr<-function(df){

  #Nobs<-df[length(df[,1]),1]/2
  Nobs<-  df[length(df[,1]),1]/2

  corr<- data.frame("n"= c(1:Nobs),"corr"= c(1:Nobs),"fit_range"= c(1:Nobs),"chi2.dof"= c(1:Nobs))

  # for (n in c(1:Nobs)){
  #   l<-grep("fit",df[,3])
  #   a1<-gsub("#","", df[l,2][n])
  #   fit_range<- df[l,5][n]
  #   chi2<-gsub("chi2=","", df[l,6][n])
  #   corr[n,]<- list(n, a1,fit_range,chi2)
  #
  # }

  l<-grep("fit",df[,3])
  corr$corr<- gsub("#","", df[l,2])
  corr$chi2.dof<- gsub("chi2=","", df[l,6])
  corr$fit_range<- df[l,5]
  return(corr)
}
