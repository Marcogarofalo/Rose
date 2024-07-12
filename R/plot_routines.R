#' set margin for a fit
#'
#' @param fit_range list of two number tmin and tmax
#' @param  T total time extend of the plot (T/2)

set_xmargin <- function(fit_range, T) {
  xmin <- fit_range[1] - 7
  xmax <- fit_range[2] + 7
  if (xmin < 0) {
    xmin <- 0
  }
  if (xmax > T) {
    xmax <- T
  }
  c(xmin, xmax)
}





################################################################################
################################################################################
#' plot a plateaux
#' @param color bool
#' @param  shape bool
#' @param fill bool
myggplot <- function(color = TRUE, shape = TRUE, fill = TRUE, repeat_color = 1) {
  gg <- ggplot2::ggplot() +
    ggplot2::theme_bw()
  colorlist <- c(
    "#404040", "#4863A0", "#C04000",
    "#228B22", "#8B008B", "#00CCFF",
    "#996600", "#999999", "#FFCC33",
    "#FF6600", "#6633FF", "#9966FF",
    "#006666", "#FFCCFF"
  )
  colorlist <- rep(colorlist, each = repeat_color)

  if (color) gg <- gg + scale_color_manual(values = colorlist)
  # ggplot2::scale_color_manual(values=seq(1,50))
  if (fill) gg <- gg + ggplot2::scale_fill_manual(values = colorlist)
  if (shape) {
    gg <- gg + ggplot2::scale_shape_manual(
      values = c(0, 1, 2, 4, 5, 6, 7, 8, 11, 15, 16, 17, 18, 21, 22, 23, 24, 25)
    )
  }
  return(gg)
}

#####################################################################
#####################################################################

#' plot a fit to a data
#' @param d data frame
#' @param  T total time extend of the plot (T/2)
my_fit_ggplot <- function(d, fit_par, fit_range, T, logscale = "no") {
  l <- length(d[1, ])
  fit_precision <- 2 # (l -2)/3  # the number of x of the fits
  mydf <- data.frame(
    "x" = c(0), "y" = c(0), "err" = c(0),
    "xfit" = c(0), "fit" = c(0), "errfit" = c(0)
  )
  mydf <- mydf[-1, ]
  #
  colx <- c(1, c(1:fit_precision * 3))[-2] # 1, 6, 9, 12,..#columns of the x
  colf <- c(4, c(1:fit_precision * 3 + 1))[-2] # 4, 7, 10, 13,..#columns of the fit
  colferr <- c(5, c(1:fit_precision * 3 + 2))[-2] # 5, 8, 11, 14,..#columns of the fit
  count <- 1
  for (i in c(1:fit_precision)) {
    for (t in c(1:length(d[, 1]))) {
      mylist <- list(d[t, 1], d[t, 2], d[t, 3])
      mylist <- append(mylist, list(d[t, colx[i]], d[t, colf[i]], d[t, colferr[i]]))
      mydf[count, ] <- mylist
      count <- count + 1
    }
  }
  if (logscale == "yes") {
    mydf[, 3] <- mydf[, 3] / mydf[, 2]
    mydf[, 6] <- mydf[, 6] / mydf[, 5]
    mydf <- dplyr::mutate_at(mydf, c(2, 5), function(x) log10(x))
  }
  # gg <- gg+ scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #            labels = trans_format("log10", math_format(10^.x)))

  gg <- ggplot2::ggplot(mydf, aes(x = x, y = y)) +
    ggplot2::geom_point()

  gg <- gg + ggplot2::geom_errorbar(
    data = mydf, mapping = aes(x = x, ymin = y - err, ymax = y + err),
    width = 0.3, inherit.aes = FALSE
  )
  #

  gg <- gg + ggplot2::geom_ribbon(
    mapping = aes(x = xfit, ymin = fit - errfit, ymax = fit + errfit),
    color = "darkgreen", alpha = 0.3, fill = "darkgreen",
    inherit.aes = FALSE
  )
  gg <- gg + ggplot2::geom_line(aes(x = fit_range[1]), color = "red", linetype = "dashed")
  gg <- gg + ggplot2::geom_line(aes(x = fit_range[2]), color = "red", linetype = "dashed")


  #
  # gg <- gg+ labs(x = TeX('x_0/a'), y= TeX('$c(x_0/a)$'))
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
geom_error <- function(gg, x, y, dy, ...) {
  gg <- gg + geom_point(aes(x = x, y = y, ...))
  gg <- gg + geom_errorbar(aes(x = x, ymin = y - dy, ymax = y + dy, ...))
  return(gg)
}

#####################################################################
#####################################################################
#' from a data frame of the form
#' t   data err fit err  t+h fit  err t+2h fit err
#' return a data frame of the form
#'  t     data err t    fit err
#'  t+h   data err t+h  fit err
#'  t+2h  data err t+2h fit err
#'  @param d data frame
reshape_df_analysis_to_ggplot <- function(d) {
  l <- length(which(d[1, ] != "NA"))
  fit_precision <- (l - 2) / 3 # the number of x of the fits

  size <- fit_precision * length(d[, 1])
  # mydf<- mydf[-1,]
  #
  colx <- c(1, c(1:fit_precision * 3))[-2] # 1, 6, 9, 12,..#columns of the x
  colf <- c(4, c(1:fit_precision * 3 + 1))[-2] # 4, 7, 10, 13,..#columns of the fit
  colferr <- c(5, c(1:fit_precision * 3 + 2))[-2] # 5, 8, 11, 14,..#columns of the fit
  mydf <- data.frame(
    "x" = rep(NA, size), "y" = rep(NA, size), "err" = rep(NA, size),
    "xfit" = rep(NA, size), "fit" = rep(NA, size), "errfit" = rep(NA, size)
  )
  count <- 1
  for (i in c(1:fit_precision)) {
    for (t in c(1:length(d[, 1]))) {
      # mylist  <-  list(d[t,1],d[t,2], d[t,3]  )
      # mylist  <- append(mylist, list( d[t,colx[i]],d[t,colf[i]], d[t,colferr[i]]  ) )
      mydf[count, ] <- list(
        d[t, 1], d[t, 2], d[t, 3],
        d[t, colx[i]], d[t, colf[i]], d[t, colferr[i]]
      )
      count <- count + 1
    }
  }
  # mydf <-data.frame('x'=rep(d[,1],fit_precision),
  #                   'y'=rep(d[,2],fit_precision),
  #                   'err'=rep(d[,3],fit_precision),
  #                   'xfit'=rep(NA,size),
  #                   'fit'=rep(NA,size),
  #                   'errfit'=rep(NA,size)
  #                   )


  return(mydf)
}


#####################################################################
#####################################################################
#' plot data minus fit appending to the existing ggplot
#' @param g a ggplot object created with ggplot()
#' @param d data frame
#' @param  T total time extend of the plot (T/2)
#' @import ggplot2
residual_ggplot <- function(d, fit_par, fit_range, T, logscale = "no", g, mylabel) {
  mydf <- reshape_df_analysis_to_ggplot(d)

  if (logscale == "yes") {
    mydf[, 3] <- mydf[, 3] / mydf[, 2]
    mydf[, 6] <- mydf[, 6] / mydf[, 5]
    mydf <- dplyr::mutate_at(mydf, c(2, 5), function(x) log10(x))
  }
  mylabel <- mylabel
  gg <- g + ggplot2::geom_point(data = d, mapping = aes(x = d[, 1], y = d[, 2] - d[, 4], colour = mylabel), inherit.aes = FALSE)

  gg <- gg + ggplot2::geom_errorbar(
    data = d, mapping = aes(x = d[, 1], ymin = d[, 2] - d[, 3] - d[, 4], ymax = d[, 2] + d[, 3] - d[, 4], color = mylabel),
    width = 0.3, inherit.aes = FALSE
  )


  gg <- gg + ggplot2::geom_ribbon(
    data = mydf,
    mapping = aes(x = xfit, ymin = -errfit, ymax = +errfit, fill = mylabel), color = NA,
    alpha = 0.2, inherit.aes = FALSE, show.legend = FALSE
  )
  fit_range <- fit_range

  gg <- gg + ggplot2::geom_line(data = d, aes(x = fit_range[1], y = d[, 2] - d[, 4] + d[, 3], color = mylabel), alpha = 0.3, linetype = "dashed", )
  gg <- gg + ggplot2::geom_line(data = d, aes(x = fit_range[2], y = d[, 2] - d[, 4] + d[, 3], color = mylabel), alpha = 0.3, linetype = "dashed")


  gg <- gg + ggplot2::theme_bw()

  return(gg)
}

#' plot a fit to some data appending to the existing ggplot
#' @param fit_par the output of read_fit()

print_fit <- function(fit_par) {
  len <- length(fit_par[1, ]) / 2 - 1
  for (i in c(1:len)) {
    if (!is.na(fit_par[1, i * 2])) {
      s <- sprintf("P[%d]=%.6f ", i, fit_par[1, i * 2 - 1])
      err <- sprintf("%.6f", fit_par[1, i * 2])
      pander(paste0("$", s, "\\pm ", err, "$ "))
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
scale_fit_ggplot <- function(d, fit_range, Th, logscale = "no", g, extrax = c(2, 4), extray = c(5, 5)) {
  rowmin <- d[fit_range[1], 1]
  rowmax <- d[fit_range[2], 1]
  ymin <- (min(d[rowmin, 2] - extray[1] * d[rowmin, 3], d[rowmax, 2] - extray[1] * d[rowmax, 3]))
  ymax <- (max(d[rowmin, 2] + extray[2] * d[rowmin, 3], d[rowmax, 2] + extray[2] * d[rowmax, 3]))

  if (logscale == "yes") {
    ymin <- log10(ymin)
    ymax <- log10(ymax)
  }

  xmin <- fit_range[1] - extrax[1]
  if (xmin < 0) {
    xmin <- 0
  }
  xmax <- fit_range[2] + extrax[2]
  if (xmax > Th) {
    xmax <- Th
  }


  gg <- g + ggplot2::coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax))
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
myplotly <- function(gg, title = "", xlabel = "", ylabel = "",
                     xrange = NULL, yrange = NULL,
                     output = "AUTO", to_print = TRUE, save_pdf = NULL,
                     legend_position = c(0, 1), legend_title = NULL, width = 680, height = 480,
                     latex_engine = "pdflatex", to_webgl = FALSE, restyle = TRUE) {
  # gg<- gg+ ggplot2::theme_bw()

  if (restyle) {
    gg <- gg + ggplot2::labs(
      color = legend_title,
      fill = legend_title,
      shape = legend_title,
      linewidth = legend_title
    )

    gg <- gg + ggplot2::theme(
      panel.background     = ggplot2::element_rect(fill = "white", color = NA),
      legend.key           = element_rect(fill = NA, color = NA),
      legend.background    = element_rect(fill = "#ffffff80", color = NA, linewidth = 1),
      legend.justification = c(1, 1),
      legend.box.margin    = margin(1, 0, 0, 0),
    )
  }
  HTML <- FALSE
  PDF <- FALSE
  if (output == "AUTO") {
    if (knitr::is_html_output()) {
      HTML <- TRUE
    } else if (knitr::is_latex_output()) {
      PDF <- TRUE
    } else {
      HTML <- TRUE
      PDF <- TRUE
    }
  } else if (output == "HTML") {
    HTML <- TRUE
    PDF <- FALSE
  } else if (output == "PDF") {
    PDF <- TRUE
    HTML <- FALSE
  }


  if (!is.null(save_pdf)) {
    HTML <- FALSE
    PDF <- TRUE
  }

  if (HTML) {
    if (is.null(xrange)) {
      autox <- TRUE
      rangex <- xrange
    } else {
      autox <- FALSE
      rangex <- xrange
    }
    if (is.null(yrange)) {
      autoy <- TRUE
      rangey <- yrange
    } else {
      autoy <- FALSE
      rangey <- yrange
    }
    fig <- plotly::ggplotly(gg, dynamicTicks = TRUE, width = width, height = height) %>%
      layout(
        title = title,
        xaxis = list(
          title = xlabel, showexponent = "all", exponentformat = "e",
          autorange = autox, range = rangex
        ),
        yaxis = list(
          title = ylabel, showexponent = "all", exponentformat = "e",
          autorange = autoy, range = rangey
        )
      )

    if (!is.null(legend_position)) {
      fig <- fig %>% layout(legend = list(
        x = legend_position[1],
        y = legend_position[2]
      ))
    }
    #     partial_bundle()
    #    fig<- toWebGL(fig)
    # fig<- fig %>% config(modeBarButtonsToAdd =
    #                       list("drawine",  "eraseshape" ) )
    # if(to_print) print(htmltools::tagList(fig%>%config(mathjax = "cdn")))
    if (to_webgl) fig <- fig %>% toWebGL()

    if (to_print) {
      fig <- widgetframe::frameableWidget(fig %>% config(
        mathjax = "cdn",
        displayModeBar = FALSE
      ))
      # print(widgetframe::frameableWidget(fig))
      # widgetframe::frameableWidget(fig)
      # htmltools::frameWidget(fig)
      print(htmltools::tagList(fig, "<\br >"))
    }
  } else if (PDF) {
    # if(!is.null(save_pdf) ) pdf(save_pdf)
    # knitr::opts_chunk$set(  dev='tikz')
    if (stringr::str_detect(xlabel, "\\$")) {
      labelx <- xlabel
    } else {
      labelx <- paste0("\\verb|", xlabel, "|")
    }
    if (stringr::str_detect(ylabel, "\\$")) {
      labely <- ylabel
    } else {
      labely <- paste0("\\verb|", ylabel, "|")
    }
    if (stringr::str_detect(title, "\\$")) {
      mytitle <- title
    } else {
      mytitle <- paste0("\\verb|", title, "|")
    }

    fig <- gg
    if (!is.null(xrange) && !is.null(yrange)) {
      fig <- fig + ggplot2::coord_cartesian(xlim = xrange, ylim = yrange)
    } else if (!is.null(xrange)) {
      fig <- fig + ggplot2::coord_cartesian(xlim = xrange)
    } else if (!is.null(yrange)) fig <- fig + ggplot2::coord_cartesian(ylim = yrange)
    # if(!is.null(xrange)) fig<-fig + xlim(xrange[1],xrange[2])
    # if(!is.null(yrange)) fig<-fig + ylim(yrange[1],yrange[2])

    if (!title == "") fig <- fig + ggplot2::ggtitle(mytitle)
    if (!xlabel == "") fig <- fig + ggplot2::xlab(labelx)
    if (!ylabel == "") fig <- fig + ggplot2::ylab(labely)

    if (!is.null(legend_position)) fig <- fig + theme(legend.position = legend_position + c(0.18, 0))

    if (to_print) plot(fig)
    if (!is.null(save_pdf)) {
      texfile <- paste0(save_pdf, ".tex")
      tikzDevice::tikz(texfile,
        standAlone = TRUE,
        width = width / 100,
        height = height / 100
      )
      plot(fig)
      dev.off()
      tools::texi2dvi(paste0(save_pdf, ".tex"), texi2dvi = latex_engine, pdf = TRUE)
    }
  } else {
    print("not output selected")
    fig <- gg
  }

  return(fig)
}


#' set margin for a fit
#'
#' @param fit_range list of two number tmin and tmax
#' @param  T total time extend of the plot (T/2)

set_xmargin <- function(fit_range, T) {
  xmin <- fit_range[1] - 7
  xmax <- fit_range[2] + 7
  if (xmin < 0) {
    xmin <- 0
  }
  if (xmax > T) {
    xmax <- T
  }
  c(xmin, xmax)
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
# reshape_df_analysis_to_ggplot<-function(d){
#
#   l<- length( which( d[1,]!="NA" ) )
#   fit_precision<-   (l -2)/3  # the number of x of the fits
#
#   # mydf <-data.frame('x'=c(0), 'y'=c(0), 'err'=c(0)
#   #                   ,'xfit'=c(0), 'fit'=c(0), 'errfit'=c(0) )
#   # mydf<- mydf[-1,]
#   #
#   colx <- c(1,c(1:fit_precision*3))[-2] # 1, 6, 9, 12,..#columns of the x
#   colf <- c(4,c(1:fit_precision*3+1))[-2]# 4, 7, 10, 13,..#columns of the fit
#   colferr <- c(5,c(1:fit_precision*3+2))[-2]# 5, 8, 11, 14,..#columns of the fit
#
#
#   size <- fit_precision*length(d[,1])
#   mydf <-data.frame('x'=rep(1:size), 'y'=rep(1:size), 'err'=rep(1:size)
#                      ,'xfit'=rep(1:size), 'fit'=rep(1:size),
#                     'errfit'=rep(1:size) )
#
#   count<-1
#   for(i in c(1:fit_precision)) {
#     for (t in c(1: length(d[,1])) ){
#       mylist  <-  list(d[t,1],d[t,2], d[t,3]  )
#       mylist  <- append(mylist, list( d[t,colx[i]],d[t,colf[i]], d[t,colferr[i]]  ) )
#       mydf[count,]<- mylist
#       count<-count+1
#     }
#   }
#   # browser()
#   # mydf[,1]<-rep(d[,1], fit_precision)
#   # mydf[,2]<-rep(d[,2], fit_precision)
#   # mydf[,3]<-rep(d[,3], fit_precision)
#   # mydf[,4]<-stack(d,select = colx)[,1]
#   # mydf[,5]<-stack(d,select = colf)[,1]
#   # mydf[,6]<-stack(d,select = colferr)[,1]
#
#   return(mydf)
# }

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
many_fit_ggplot <- function(d, fit_par, fit_range, T, logscale = "no", g, mylabel, nudge = 0.0,
                            noerror = FALSE, noribbon = FALSE) {
  defaultW <- getOption("warn")

  options(warn = -1)


  mydf <- reshape_df_analysis_to_ggplot(d)
  if (logscale == "yes") {
    mydf[, 3] <- mydf[, 3] / mydf[, 2]
    mydf[, 6] <- mydf[, 6] / mydf[, 5]
    # for (i in c(1:length(mydf[,1]))){
    #    if (mydf[i,2]>0)  mydf[i,2]=log10(mydf[i,2])
    #    else              mydf[i,2]=-20
    #    if (mydf[i,5]>0)  mydf[i,5]=log10(mydf[i,5])
    #    else              mydf[i,5]=-20
    # }

    mydf <- dplyr::mutate_at(mydf, c(2, 5), function(x) log10(x))
  }
  # gg <- gg+ scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #            labels = trans_format("log10", math_format(10^.x)))
  mylabel <- mylabel
  if (is.null(g$scales$scales)) {
    gg <- g + scale_shape_manual(values = seq(0, 50))
  }

  gg <- g + ggplot2::geom_point(
    data = mydf, mapping = aes(
      x = x, y = y,
      colour = mylabel, fill = mylabel, shape = mylabel
    ),
    position = position_nudge(x = nudge),
    stroke = 0.3,
    inherit.aes = FALSE
  )
  if (!noerror) {
    gg <- gg + ggplot2::geom_errorbar(
      data = mydf, mapping = aes(
        x = x, ymin = y - err, ymax = y + err,
        color = mylabel, fill = mylabel, shape = mylabel
      ),
      position = position_nudge(x = nudge),
      width = 0.3, stroke = 0.3, inherit.aes = FALSE
    )
  }

  if (!noribbon) {
    gg <- gg + ggplot2::geom_ribbon(
      data = mydf,
      mapping = aes(
        x = xfit, ymin = fit - errfit, ymax = fit + errfit,
        color = mylabel, fill = mylabel, shape = mylabel
      ),
      alpha = 0.2, stroke = 0.3, inherit.aes = FALSE, show.legend = FALSE
    )
  }
  fit_range <- fit_range
  yr1 <- mydf$fit[fit_range[1]] - 2 * mydf$errfit[fit_range[1]]
  yr2 <- mydf$fit[fit_range[1]] + 2 * mydf$errfit[fit_range[1]]
  gg <- gg + ggplot2::geom_line(
    aes(
      x = fit_range[1], y = c(yr1, yr2),
      color = mylabel, fill = mylabel, shape = mylabel
    ),
    alpha = 0.3, stroke = 0.2, linetype = "dashed", position = position_nudge(x = -0.1)
  )

  yr1 <- mydf$fit[fit_range[2]] - 2 * mydf$errfit[fit_range[2]]
  yr2 <- mydf$fit[fit_range[2]] + 2 * mydf$errfit[fit_range[2]]
  gg <- gg + ggplot2::geom_line(
    aes(
      x = fit_range[2],
      y = c(yr1, yr2),
      color = mylabel, fill = mylabel, shape = mylabel
    ),
    alpha = 0.3, stroke = 0.2, linetype = "dashed", position = position_nudge(x = 0.1)
  )

  # gg  <- gg + xlim(set_xmargin(fit_range,128/2) ) + ylim(-2e+4, 1e+4)
  # gg<- gg +geom_text(data=mydf, aes(x=x,y=y), label=mylabel)
  # gg <- gg+ labs(x = TeX('x_0/a'), y= TeX('$c(x_0/a)$'))


  gg <- gg + ggplot2::theme_bw()
  # if (gg$scales$scales[1] == NULL)
  # if(is.null(gg$scales$scales))
  # gg<- gg+scale_shape_manual(values=seq(0,50))

  # len<-length(fit_par[1,])  /2-1
  # for(i in c(1:len )  ){
  #   if(! is.na(fit_par[1,i*2])) {
  #     s<- sprintf("P[%d]=%.6f ", i,fit_par[1,i*2-1])
  #     err<- sprintf("%.6f",fit_par[1,i*2])
  #     pander(paste0("$",s,"\\pm ",err,"$ "))
  #   }
  # }
  #
  options(warn = defaultW)

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
print_fit_res <- function(label, fit, all_obs, l) {
  str2 <- paste(label, "=")
  for (i in c(1:(length(fit[1, ]) / 2)) * 2 - 1) {
    if (!is.na(fit[1, i])) {
      str2 <- paste(str2, "  ", mean_print(fit[1, i], fit[1, i + 1]))
    }
  }
  str2 <- paste(str2, "  $\\chi^2/dof=$", all_obs[l, 4])
  cat(str2, "\n\n")
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
plot_corr <- function(string, all_obs, mt, L, T, gg, log = "no", number = NULL,
                      nudge = 0.0, print_res = TRUE) {
  string <- sprintf("\\b%s\\b", string) # need to put the delimiters on the word to grep
  label <- paste0(gsub("\\\\b", "", string))
  if (is.null(number)) {
    l <- grep(string, all_obs[, "corr"])
    if (purrr::is_empty(l)) stop("correlator not found")
    n <- all_obs[l, "n"]
  } else {
    n <- number
    l <- n
  }

  d <- get_block_n(mt, n)
  fit <- get_fit_n(mt, n)
  fit_range <- get_plateaux_range(mt, n)
  gg <- many_fit_ggplot(d, fit, fit_range, T / 2, log, gg, label, nudge)
  if (print_res) print_fit_res(label, fit, all_obs, l)


  return(gg)
}



#####################################################################
#####################################################################
#' create or append a data frame with a fit
#'
#' create or append a data frame with a fit
#' @param string name of the string to find
#' @param all_obs the outcome of Rose::get_all_corr()
#' @param mt result of Rose::read_df()
#' @param df if specified append result to df
#' @param log logscale default FALSE
#' @param  number if set it will ignore the string and plot the block selected by
#' number
#' @param  nudge  shift the point on the x axes, default 0
#' @param  print_res  if you want the result of the fit to be printed
#' @param  required  stop if not found
#' default TRUE
#' @export
add_corr_to_df <- function(string = NULL, all_obs, mt, df = NULL, log = FALSE, number = NULL,
                           nudge = 0.0, print_res = TRUE, rename = NULL, reshape = TRUE, logx = 0) {
  # string=sprintf("\\b%s\\b",string)# need to put the delimiters on the word to grep
  # label<-paste0(gsub('\\\\b','',string) )
  if (is.null(number)) {
    if (is.null(string)) {
      stop("add_corr_to_df need at least one of the two arguments: sting or number ")
    }
    # l<-grep(string,all_obs[,"corr"])
    # if (purrr::is_empty(l)) stop("correlator not found")
    # n<-all_obs[l,"n"]
    n <- which(all_obs[, "corr"] == string)
    if (length(n) != 1) {
      stop("correlator ", string, "not found")
    }
    l <- n
    label <- paste0(string)
  } else {
    n <- number
    l <- n
    label <- paste0(all_obs[n, "corr"])
  }

  d <- get_block_n(mt, n)

  fit_range <- get_plateaux_range(mt, n)

  if (reshape) {
    mydf <- reshape_df_analysis_to_ggplot(d)
  } else {
    mydf <- data.frame(
      "x" = d[, 1], "y" = d[, 2], "err" = d[, 3],
      "xfit" = d[, 4], "fit" = d[, 5], "errfit" = d[, 6]
    )
  }

  if (log) {
    mydf[, 3] <- mydf[, 3] / mydf[, 2]
    mydf[, 6] <- mydf[, 6] / mydf[, 5]
    mydf <- dplyr::mutate_at(mydf, c(2, 5), function(x) log10(x))
  }
  if (is.null(rename)) {
    mydf$label <- label
  } else {
    mydf$label <- rename
  }
  mydf$tmin <- fit_range[1]
  mydf$tmax <- fit_range[2]
  if (logx == 2) {
    mydf$x <- log2(mydf$x)
    mydf$xfit <- log2(mydf$xfit)
    mydf$tmin <- log2(mydf$tmin)
    mydf$tmax <- log2(mydf$tmax)
  } else if (logx == 10) {
    mydf$x <- log10(mydf$x)
    mydf$xfit <- log10(mydf$xfit)
    mydf$tmin <- log10(mydf$tmin)
    mydf$tmax <- log10(mydf$tmax)
  }
  mydf$x <- mydf$x + nudge
  mydf$xfit <- mydf$xfit + nudge
  mydf$tmin <- mydf$tmin + nudge
  mydf$tmax <- mydf$tmax + nudge
  if (print_res) {
    fit <- get_fit_n(mt, n)
    print_fit_res(mydf$label[1], fit, all_obs, l)
  }

  if (!is.null(df)) mydf <- rbind(df, mydf)
  return(mydf)
}


#####################################################################
#####################################################################
#' plot a fit to some data
#' @param df data.frame created with Rose::add_corr_to_df()
#' @param  noerror default FALSE
#' @param  noribbon default FALSE
#' @import ggplot2
plot_df_corr_ggplot <- function(df, noerror = FALSE, noribbon = FALSE, gg = NULL,
                                width = 0.3, alpha = 0.5, stroke = 0.3, size_error = 0.1,
                                extra_length_plateau = 0.4) {
  defaultW <- getOption("warn")
  if (is.null(gg)) {
    gg <- myggplot()
  }
  l <- which(!stringr::str_detect(df$label, "\\$"))
  df$label[l] <- paste0("\\verb|", df$label[l], "|")
  df$label <- factor(df$label, levels = unique(df$label))
  gg <- gg + ggplot2::geom_point(
    data = df, mapping = aes(
      x = x, y = y,
      color = label, fill = label, shape = label
    ),
    stroke = stroke,
    inherit.aes = FALSE
  )
  if (!noerror) {
    xrange <- layer_scales(gg)$x$range$range
    gg <- gg + ggplot2::geom_errorbar(
      data = df,
      mapping = aes(
        x = x, ymin = y - err, ymax = y + err,
        color = label
      ),
      # width = (xrange[2]-xrange[1])*width,
      width = width,
      linewidth = size_error, inherit.aes = TRUE
    )
  }
  if (!noribbon) {
    dfp <- filter(df, xfit >= tmin, xfit <= tmax)
    df1 <- filter(df, xfit == tmin, xfit == tmax)
    df1 <- rbind(df1, df1)
    df1$xfit <- df1$xfit + c(-extra_length_plateau, +extra_length_plateau)
    dfp <- rbind(dfp, df1)
    gg <- gg + ggplot2::geom_ribbon(
      data = dfp,
      mapping = aes(
        x = xfit, ymin = fit - errfit, ymax = fit + errfit,
        color = label, fill = label
      ),
      alpha = alpha, inherit.aes = TRUE, show.legend = FALSE
    )
  }
  gg <- gg + guides(fill = "none", shape = "none")


  options(warn = defaultW)

  return(gg)
}

#' create a table with the fit result
#' @param df data.frame created with Rose::read_fit_P_file("filename")
make_table_fit_result <- function(df, namefit=NULL) {
  df1 <- data.frame("P" = df$P[, 1], "value" = mapply(mean_print, df$P[, 2], df$P[, 3]))
  # cap<-paste0("$\\chi^2/dof=$",df$chi2)
  # kable(df1, caption=cap)
  if (!is.null(namefit)){
    cat(namefit,"\n")
  }
  cat("$\\chi^2/dof=$ ", df$chi2, "\n\n")
  kable(df1)
}

#' plot the result of a fit
plot_fit <- function(basename, var, data_type = NULL, gg = NULL, noribbon = FALSE,
                     labelfit = "fit", width = 0.02, size = 1,
                     id_color = NULL, id_shape = NULL,
                     single_name_for_fit = NULL,
                     nolabel_for_fit = FALSE,
                     nudge = 0) {
  filed <- paste0(basename, "_fit_data.txt")
  df <- read.table(filed, header = FALSE, fill = TRUE)

  if (is.null(gg)) gg <- myggplot()
  idy <- ncol(df) - 2

  if (is.null(id_color)) {
    color_type <- as.factor(df[, idy + 2])
  } else {
    color_type <- as.factor(df[, id_color])
  }

  if (is.null(id_shape)) {
    shape_type <- as.factor(df[, idy + 2])
  } else {
    shape_type <- as.factor(df[, id_shape])
  }

  lastr <- nrow(df)
  Nfits <- c(df[1, idy + 2]:df[lastr, idy + 2])
  if (!is.null(data_type)) {
    if (length(data_type) == 1) {
      color_type <- data_type
      shape_type <- data_type
    } else {
      N <- length(which(df[, idy + 2] == 0))

      color_type <- rep(data_type, each = N)
      shape_type <- rep(data_type, each = N)
    }
  }

  gg <- gg + geom_point(
    data = df,
    mapping = aes(
      x = df[, 1] + nudge, y = df[, idy],
      color = color_type,
      shape = shape_type,
      fill = color_type
    ),
    size = size
  )

  gg <- gg + geom_errorbar(
    data = df,
    mapping = aes(
      x = df[, 1] + nudge, y = df[, idy],
      ymin = df[, idy] - df[, idy + 1],
      ymax = df[, idy] + df[, idy + 1],
      color = color_type,
      shape = shape_type,
      fill = color_type
    ),
    width = width, size = size
  )


  datalist <- list()
  mycol <- unique(paste0(labelfit, color_type))
  if (length(mycol) != length(Nfits)) {
    mycol <- paste0(labelfit, Nfits)
  }

  if (!is.null(single_name_for_fit)) {
    mycol <- rep(single_name_for_fit, length(Nfits))
  }

  if (nolabel_for_fit) {
    mycol <- unique(paste0(color_type))
    if (length(mycol) != length(Nfits)) {
      mycol <- rep(color_type, length(Nfits))
    }
  }

  if (!noribbon) {
    for (n in Nfits) {
      file <- sprintf("%s_fit_out_n%d_%s.txt", basename, n, var)
      # browser()
      n1 <- n + 1
      datalist[[n1]] <- read.table(file,
        header = FALSE, fill = TRUE,
        col.names = c(paste0("x", n), paste0("fit", n), paste0("fiterr", n))
      )
      gg <- gg + geom_ribbon(
        mapping = aes_string(
          x = datalist[[n1]][, 1] + nudge,
          ymin = datalist[[n1]][, 2] - datalist[[n1]][, 3],
          ymax = datalist[[n1]][, 2] + datalist[[n1]][, 3],
          fill = as.factor(mycol[n1]),
          color = as.factor(mycol[n1]),
          shape = as.factor(mycol[n1])
        ),
        alpha = 0.5
      )
      gg <- gg + geom_line(
        mapping = aes_string(
          x = datalist[[n1]][, 1] + nudge,
          y = datalist[[n1]][, 2],
          fill = as.factor(mycol[n1]),
          color = as.factor(mycol[n1]),
          shape = as.factor(mycol[n1])
        )
      )
    }
  }
  return(gg)
}
