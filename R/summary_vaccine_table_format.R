#' format flextable to make the header and the first column bold, all the columns except the first column align to right
#'
#' @param dataset any flextable
#'
#' @export
#' @import dplyr
#' @return returns formatted flextable
#' @export
#'
#' @examples
#'
align_bold<-function(dataset){
    n<-ncol_keys(dataset)
    dataset<-align(dataset,j=2:n,align="right",part="body")
    dataset<-align(dataset,j=2:n,align="right",part="header")
    dataset<-bold(dataset,j=1,bold=TRUE,part="body")
    dataset<-bold(dataset,bold=TRUE,part="header")
    return (dataset)
}
#' Title format summary table in the exsum
#'
#' @param dataset the flextable of summary table in the exsum
#'
#' @return formatted summary table
#' @export
#'
#' @examples
summary_table_format<-function(dataset){
    dataset<-hrule(dataset,rule="exact",part="body")
    dataset<-height(dataset,height=0.18,part="body")
    dataset<-width(dataset,j=1,width=1.6)
    dataset<-width(dataset,j=2,width=1.3)
    dataset<-width(dataset,j=3,width=1.1)
    dataset<-width(dataset,j=4,width=1.0)
    dataset<-width(dataset,j=5:6,width=1.7)
    dataset<-compose(dataset,part="body",i=1,j=1,value=as_paragraph("Cases",as_sup("1")))
    dataset<-compose(dataset,part="body",i=2,j=1,value=as_paragraph("Hospital Admissions",as_sup("2")))
    dataset<-compose(dataset,part="body",i=3,j=1,value=as_paragraph("Deaths",as_sup("1")))
    dataset<-compose(dataset,part="body",i=4,j=1,value=as_paragraph("Test Volume",as_sup("3")))
    dataset<-compose(dataset,part="body",i=5,j=1,value=as_paragraph("Test Positivity",as_sup("3")))
    dataset<-compose(dataset,part="header",j=2,value=as_paragraph("Cumulative Total"))
    dataset<-compose(dataset,part="header",j=3,value=as_paragraph("Weekly Total"))
    dataset<-compose(dataset,part="header",j=4,value=as_paragraph("7-Day Average"))
    dataset<-compose(dataset,part="header",j=5,value=as_paragraph("Change from Prior Period"))
    dataset<-compose(dataset,part="header",j=6,value=as_paragraph("Cumulative 7-Day Rate per 100K"))
    dataset<-color(dataset,color="white",j=1,part="header")
    dataset<-color(dataset,color="grey",i=5,j=6,part="body")
    dataset<-color(dataset,color="grey",i=4:5,j=3,part="body")
    dataset<-align(dataset,align="center",part="header")
    dataset<-align(dataset,align="center",part="body")
    dataset<-align(dataset,j=1,align="left",part="body")
    dataset<-bold(dataset,j=1,part="body")
    dataset<-bold(dataset,part="header")
    return (dataset)
}
#' Title format vaccine age table in the exsum
#'
#' @param dataset the flextable of vaccine age table
#'
#' @return formatted vaccine age table
#' @export
#'
#' @examples
vaccine_age_table_format<-function(dataset){
    dataset<-compose(dataset,part="header",j=5,value=as_paragraph("Received 2nd Booster/Additional Dose",as_sup("5")))

#set height and width
    dataset<-hrule(dataset,rule="exact",part="body")
    dataset<-height(dataset,height=0.18,part="body")
    dataset<-width(dataset,j=1,width=1.1)
    dataset<-width(dataset,j=2:3,width=1.7)
    dataset<-width(dataset,j=4:5,width=2.0)
#rename the first column
    dataset<-compose(dataset,part="body",i=1,j=1,value=as_paragraph("US Overall"))
    dataset<-compose(dataset,part="body",i=2,j=1,value=as_paragraph("5-11 Years"))
    dataset<-compose(dataset,part="body",i=3,j=1,value=as_paragraph("12-17 Years"))
    dataset<-compose(dataset,part="body",i=4,j=1,value=as_paragraph("\U2265 5 Years"))
    dataset<-compose(dataset,part="body",i=5,j=1,value=as_paragraph("\U2265 12 Years"))
    dataset<-compose(dataset,part="body",i=6,j=1,value=as_paragraph("\U2265 18 Years"))
    dataset<-compose(dataset,part="body",i=7,j=1,value=as_paragraph("\U2265 65 Years"))
    dataset<-color(dataset,color="grey",i=2,j=4:5,part="body")
    dataset<-color(dataset,color="grey",i=3:4,j=5,part="body")
    return (dataset)
}
#' Title format vaccine trend table in exsum
#'
#' @param dataset the flextable of vaccine trend talbe
#'
#' @return formatted vaccine trend table
#' @export
#'
#' @examples
vaccine_trend_table_format<-function(dataset){
    dataset<-compose(dataset,part="header",j=3,value=as_paragraph("Daily(% of Doses Administered)",as_sup("6")))
    #set height and width
    dataset<-hrule(dataset,rule="exact",part="body")
    dataset<-height(dataset,height=0.18,part="body")
    dataset<-width(dataset,j=1,width=1.5)
    dataset<-width(dataset,j=2:6,width=1.4)
    #color of the cell contents
    dataset<-color(dataset,color="white",j=1,part="header")
    return (dataset)
}

#define metrics format function
#' Title format metrics table in the White House Slides
#'
#' @param dataset the flextable of metrics table
#'
#' @return formatted metrics table
#' @export
#'
#' @examples
metrics_format<-function(dataset){
    std_border=fp_border(color="white")
    dataset<-hrule(dataset,rule="exact",part="body")
    dataset<-height(dataset,height=0.48,part="body")
    dataset<-width(dataset,j=1,width=2.0)
    dataset<-width(dataset,j=2,width=1.6)
    dataset<-width(dataset,j=3,width=1.0)
    dataset<-width(dataset,j=4,width=1.0)
    dataset<-width(dataset,j=5,width=1.2)
    dataset<-add_header_row(dataset,top=TRUE,values=c("Key Metrics"),colwidths=c(5))
    dataset<-compose(dataset,part="body",i=1,j=1,value=as_paragraph("Cases"))
    dataset<-compose(dataset,part="body",i=2,j=1,value=as_paragraph("Hospital Admissions"))
    dataset<-compose(dataset,part="body",i=3,j=1,value=as_paragraph("Deaths"))
    dataset<-compose(dataset,part="body",i=4,j=1,value=as_paragraph("Test Positivity"))
    dataset<-compose(dataset,part="header",i=2,j=2,value=as_paragraph("Total to date"))
    dataset<-compose(dataset,part="header",i=2,j=3,value=as_paragraph("Most recent week"))
    dataset<-compose(dataset,part="header",i=2,j=4,value=as_paragraph("7-Day average"))
    dataset<-compose(dataset,part="header",i=2,j=5,value=as_paragraph("Period-on-period change"))
    dataset<-color(dataset,color="#000036",i=1,part="header")
    dataset<-color(dataset,color="white",i=2,part="header")
    dataset<-color(dataset,color="#041690",part="body")
    dataset<-align(dataset,align="center",part="header")
    dataset<-align(dataset,align="center",part="body")
    dataset<-bold(dataset,j=1,part="body")
    dataset<-bold(dataset,part="header")
    dataset<-fontsize(dataset,size=20,i=1,part="header")
    dataset<-fontsize(dataset,size=10,i=2,part="header")
    dataset<-fontsize(dataset,size=9,j=1,part="body")
    dataset<-fontsize(dataset,size=10,j=2:5,part="body")
    dataset<-font(dataset,font="Georgia",part="body")
    dataset<-font(dataset,font="Georgia",part="header")
    dataset<-border_remove(dataset)
    dataset<-bg(dataset,i=2,j=2:5,bg="#000036",part="header")
    dataset<-bg(dataset,j=2:4,bg="#F5F5F5",part="body")
    dataset<-hline(dataset,i=2,j=2:5,border=std_border,part="header")
    dataset<-vline(dataset,i=2,j=2:5,border=std_border,part="header")
    dataset<-hline(dataset,j=2:5,border=std_border,part="body")
    dataset<-vline(dataset,j=2:5,border=std_border,part="body")
    dataset<-bg(dataset,~change>=0,~change,bg="#8fd400",part="body")
    dataset<-bg(dataset,~change<0,~change,bg="#F08000",part="body")
    return (dataset)
}

#' Title format title table in the exsum
#'
#' @param dataset the flextable of title table
#'
#' @return formatted title table
#' @export
#'
#' @examples
title_format_exsum<-function(dataset){
    dataset<-hrule(dataset,rule="exact",part="body")
    dataset<-height(dataset,height=0.25)
    dataset<-width(dataset,width=53)
    dataset<-fontsize(x=dataset,i=1,size=15,part="body")
    dataset<-fontsize(x=dataset,i=2,size=10,part="body")
    dataset<-bold(x=dataset,i=1,part="body")
    dataset<-color(x=dataset,i=1,j=1,color="white",part="header")
    dataset<-align(x=dataset,align="center",part="body")
    dataset<-valign(x=dataset,valign="top",part="body")
    dataset<-border_remove(dataset)
    return(dataset)  }

#' Title format title table in the White House Slides
#'
#' @param dataset the flextable of title table
#'
#' @return formatted title table
#' @export
#'
#' @examples
title_format_wh<-function(dataset){
    dataset<-hrule(dataset,rule="exact",part="body")
    dataset<-height(dataset,height=0.25)
    dataset<-width(dataset,width=158)
    dataset<-fontsize(x=dataset,i=1,size=27,part="body")
    dataset<-fontsize(x=dataset,i=2,size=18,part="body")
    dataset<-font(x=dataset,i=2,font="Georgia",part="body")
    dataset<-bold(x=dataset,j=1,part="body")
    dataset<-color(x=dataset,i=1,j=1,color="white",part="header")
    dataset<-color(x=dataset,i=1,color="#041690",part="body")
    dataset<-color(x=dataset,i=2,color="black",part="body")
    dataset<-align(x=dataset,align="left",part="body")
    dataset<-border_remove(dataset)
    return(dataset)  }
