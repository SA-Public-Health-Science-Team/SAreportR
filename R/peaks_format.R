
#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
casepeaks_format<-function(dataset){
    dataset<-add_header_row(x=dataset,top=TRUE,values=c("s","Weekly Total","s","21-Day Daily Average"),colwidths=c(1,2,1,3))
    dataset<-add_header_row(x=dataset,top=TRUE,values=c("Peaks in Weekly Total and 21-Day Average of Daily Cases"),colwidths=c(7))
    dataset<-hrule(dataset,rule="exact",part="body")
    dataset<-hrule(dataset,rule="exact",part="header")
    dataset<-width(dataset,j=4,width=1)
    dataset<-width(dataset,j=1,width=3.8)
    dataset<-width(dataset,j=2:3,width=3)
    dataset<-width(dataset,j=5:7,width=3)
    dataset<-height(dataset,part="header",i=1:3,height=0.3)
    dataset<-height(dataset,part="body",height=0.3)
    dataset<-compose(dataset,part="header",i=3,j=1,value=as_paragraph("Peak"))
    dataset<-compose(dataset,part="header",i=3,j=c(3,6),value=as_paragraph("Reporting Week End"))
    dataset<-compose(dataset,part="header",i=3,j=2,value=as_paragraph("New Cases"))
    dataset<-compose(dataset,part="header",i=3,j=5,value=as_paragraph("Average"))
    dataset<-compose(dataset,part="header",i=3,j=7,value=as_paragraph("% Change From Current"))
    dataset<-color(x=dataset,i=3,color="black",part="header")
    dataset<-color(x=dataset,i=2,j=c(1,4),color="white",part="header")
    dataset<-color(x=dataset,i=3,j=4,color="white",part="header")
    dataset<-color(x=dataset,j=4,color="white",part="body")
    dataset<-align(x=dataset,align="center",part="body")
    dataset<-align(x=dataset,align="center",part="header")
    dataset<-bold(x=dataset,i=1:2,part="header")
    dataset<-fontsize(x=dataset,i=1,size=12,part="header")
    dataset<-fontsize(x=dataset,i=2,size=11,part="header")
    dataset<-fontsize(x=dataset,i=3,size=10,part="header")
    dataset<-fontsize(x=dataset,size=10,part="body")
    dataset<-border_remove(dataset)
    std_border<-fp_border(color="grey",width=0.5)
    dataset<-hline(x=dataset,i=2,j=2:3,border=std_border,part="header")
    dataset<-hline(x=dataset,i=2,j=5:7,border=std_border,part="header")
    small_border<-fp_border(color="grey",width=0.3)
    dataset<-hline(x=dataset,i=3,j=1:7,border=small_border,part="header")
    tiny_border<-fp_border(color="grey",width=0.2)
    dataset<-hline(x=dataset,i=1:8,j=1:7,border=tiny_border,part="body")
    dataset<-hline(x=dataset,i=3,j=1:7,border=tiny_border,part="header")
      return (dataset)
}
#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
hosppeaks_format_juris<-function(dataset){
    dataset<-add_header_row(x=dataset,top=TRUE,values=c("s"),colwidths=c(6))
    dataset<-add_header_row(x=dataset,top=TRUE,values=glue("August 01, 2020 - {hosp_Bdate}"),colwidths=c(6))
    dataset<-add_header_row(x=dataset,top=TRUE,values=glue("{params$timeframe} Admissions Change of Patients with Confirmed COVID-19, {params$juris}"),colwidths=c(6))
    dataset<-fontsize(x=dataset,i=1,size=16,part="header")
    dataset<-fontsize(x=dataset,i=2,size=8,part="header")
    dataset<-color(x=dataset,i=3,color="white",part="header")
    dataset<-hrule(dataset,rule="exact",part="body")
    dataset<-hrule(dataset,rule="exact",part="header")
    dataset<-width(dataset,j=c(2,5),width=2.0)
    dataset<-width(dataset,j=4,width=0.2)
    dataset<-width(dataset,j=c(1,3,6),width=1.0)
    dataset<-height(dataset,part="header",i=1:6,height=0.16)
    dataset<-height(dataset,part="body",height=0.12)
    dataset<-compose(dataset,part="header",i=6,j=1,value=as_paragraph("Peak"))
    dataset<-compose(dataset,part="header",i=6,j=c(2,5),value=as_paragraph("New Admissions"))
    dataset<-compose(dataset,part="header",i=6,j=c(3,6),value=as_paragraph("Date"))
    dataset<-color(x=dataset,i=5,j=c(1,4),color="white",part="header")
    dataset<-color(x=dataset,i=6,j=4,color="white",part="header")
    dataset<-color(x=dataset,j=4,color="white",part="body")
    dataset<-align(x=dataset,align="center",part="body")
    dataset<-align(x=dataset,align="center",part="header")
    dataset<-bold(x=dataset,i=c(1,4,5,6),part="header")
    dataset<-fontsize(x=dataset,i=4,size=10,part="header")
    dataset<-fontsize(x=dataset,i=5:6,size=8,part="header")
    dataset<-fontsize(x=dataset,size=7,part="body")
    dataset<-border_remove(dataset)
    std_border <- fp_border(color="black",width=1.0)
    dataset<-hline(x=dataset,i=5,j=2:3,border=std_border,part="header")
    dataset<-hline(x=dataset,i=5,j=5:6,border=std_border,part="header")
    small_border<-fp_border(color="grey")
    dataset<-hline(x=dataset,i=6,j=1:6,border=small_border,part="header")
    tiny_border<-fp_border(color="grey",width=0.5)
    dataset<-hline(x=dataset,i=1:3,j=1:6,border=tiny_border,part="body")
    return (dataset)
}
#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
hosppeaks_format_reg<-function(dataset){
    dataset<-add_header_row(x=dataset,top=TRUE,values=c("s"),colwidths=c(6))
    dataset<-add_header_row(x=dataset,top=TRUE,values=glue("August 01, 2020 - {hosp_Bdate}"),colwidths=c(6))
    dataset<-add_header_row(x=dataset,top=TRUE,values=glue("{params$timeframe} Admissions Change of Patients with Confirmed COVID-19, Region {params$region}"),colwidths=c(6))
    dataset<-fontsize(x=dataset,i=1,size=16,part="header")
    dataset<-fontsize(x=dataset,i=2,size=8,part="header")
    dataset<-color(x=dataset,i=3,color="white",part="header")
    dataset<-hrule(dataset,rule="exact",part="body")
    dataset<-hrule(dataset,rule="exact",part="header")
    dataset<-width(dataset,j=c(2,5),width=2.0)
    dataset<-width(dataset,j=4,width=0.2)
    dataset<-width(dataset,j=c(1,3,6),width=1.0)
    dataset<-height(dataset,part="header",i=1:6,height=0.16)
    dataset<-height(dataset,part="body",height=0.12)
    dataset<-compose(dataset,part="header",i=6,j=1,value=as_paragraph("Peak"))
    dataset<-compose(dataset,part="header",i=6,j=c(2,5),value=as_paragraph("New Admissions"))
    dataset<-compose(dataset,part="header",i=6,j=c(3,6),value=as_paragraph("Date"))
    dataset<-color(x=dataset,i=5,j=c(1,4),color="white",part="header")
    dataset<-color(x=dataset,i=6,j=4,color="white",part="header")
    dataset<-color(x=dataset,j=4,color="white",part="body")
    dataset<-align(x=dataset,align="center",part="body")
    dataset<-align(x=dataset,align="center",part="header")
    dataset<-bold(x=dataset,i=c(1,4,5,6),part="header")
    dataset<-fontsize(x=dataset,i=4,size=10,part="header")
    dataset<-fontsize(x=dataset,i=5:6,size=8,part="header")
    dataset<-fontsize(x=dataset,size=7,part="body")
    dataset<-border_remove(dataset)
    std_border <- fp_border(color="black",width=1.0)
    dataset<-hline(x=dataset,i=5,j=2:3,border=std_border,part="header")
    dataset<-hline(x=dataset,i=5,j=5:6,border=std_border,part="header")
    small_border<-fp_border(color="grey")
    dataset<-hline(x=dataset,i=6,j=1:6,border=small_border,part="header")
    tiny_border<-fp_border(color="grey",width=0.5)
    dataset<-hline(x=dataset,i=1:3,j=1:6,border=tiny_border,part="body")
    return (dataset)
}
#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
hosppeaks_format_nat<-function(dataset){

    dataset<-add_header_row(x=dataset,top=TRUE,values=c("s"),colwidths=c(6))
    dataset<-add_header_row(x=dataset,top=TRUE,values=glue("August 01, 2020 - {hosp_Bdate}"),colwidths=c(6))
    dataset<-add_header_row(x=dataset,top=TRUE,values=glue("{params$timeframe} Admissions Change of Patients with Confirmed COVID-19, US"),colwidths=c(6))
    dataset<-fontsize(x=dataset,i=1,size=16,part="header")
    dataset<-fontsize(x=dataset,i=2,size=8,part="header")
    dataset<-color(x=dataset,i=3,color="white",part="header")
    dataset<-hrule(dataset,rule="exact",part="body")
    dataset<-hrule(dataset,rule="exact",part="header")
    dataset<-width(dataset,j=c(2,5),width=2.0)
    dataset<-width(dataset,j=4,width=0.2)
    dataset<-width(dataset,j=c(1,3,6),width=1.0)
    dataset<-height(dataset,part="header",i=1:6,height=0.16)
    dataset<-height(dataset,part="body",height=0.12)
    dataset<-compose(dataset,part="header",i=6,j=1,value=as_paragraph("Peak"))
    dataset<-compose(dataset,part="header",i=6,j=c(2,5),value=as_paragraph("New Admissions"))
    dataset<-compose(dataset,part="header",i=6,j=c(3,6),value=as_paragraph("Date"))
    dataset<-color(x=dataset,i=5,j=c(1,4),color="white",part="header")
    dataset<-color(x=dataset,i=6,j=4,color="white",part="header")
    dataset<-color(x=dataset,j=4,color="white",part="body")
    dataset<-align(x=dataset,align="center",part="body")
    dataset<-align(x=dataset,align="center",part="header")
    dataset<-bold(x=dataset,i=c(1,4,5,6),part="header")
    dataset<-fontsize(x=dataset,i=4,size=10,part="header")
    dataset<-fontsize(x=dataset,i=5:6,size=8,part="header")
    dataset<-fontsize(x=dataset,size=7,part="body")
    dataset<-border_remove(dataset)
    std_border <- fp_border(color="black",width=1.0)
    dataset<-hline(x=dataset,i=5,j=2:3,border=std_border,part="header")
    dataset<-hline(x=dataset,i=5,j=5:6,border=std_border,part="header")
    small_border<-fp_border(color="grey")
    dataset<-hline(x=dataset,i=6,j=1:6,border=small_border,part="header")
    tiny_border<-fp_border(color="grey",width=0.5)
    dataset<-hline(x=dataset,i=1:3,j=1:6,border=tiny_border,part="body")
    return (dataset)
}
#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
hosppeaks_format<-function(dataset){
  dataset<-hrule(dataset,rule="exact",part="body")
  dataset<-hrule(dataset,rule="exact",part="header")
  dataset<-width(dataset,j=c(2,5),width=2.0)
  dataset<-width(dataset,j=4,width=0.2)
  dataset<-width(dataset,j=c(1,3,6),width=1.0)
  dataset<-height(dataset,part="header",i=1:3,height=0.16)
  dataset<-height(dataset,part="body",height=0.12)
  dataset<-compose(dataset,part="header",i=3,j=1,value=as_paragraph("Peak"))
  dataset<-compose(dataset,part="header",i=3,j=c(2,5),value=as_paragraph("New Admissions"))
  dataset<-compose(dataset,part="header",i=3,j=c(3,6),value=as_paragraph("Date"))
  dataset<-color(x=dataset,i=2,j=c(1,4),color="white",part="header")
  dataset<-color(x=dataset,i=3,j=4,color="white",part="header")
  dataset<-color(x=dataset,j=4,color="white",part="body")
  dataset<-align(x=dataset,align="center",part="body")
  dataset<-align(x=dataset,align="center",part="header")
  dataset<-bold(x=dataset,i=1:3,part="header")
  dataset<-fontsize(x=dataset,i=2,size=10,part="header")
  dataset<-fontsize(x=dataset,i=2:3,size=8,part="header")
  dataset<-fontsize(x=dataset,size=7,part="body")
  dataset<-border_remove(dataset)
  std_border <- fp_border(color="black",width=1.0)
  dataset<-hline(x=dataset,i=2,j=2:3,border=std_border,part="header")
  dataset<-hline(x=dataset,i=2,j=5:6,border=std_border,part="header")
  small_border<-fp_border(color="grey")
  dataset<-hline(x=dataset,i=3,j=1:6,border=small_border,part="header")
  tiny_border<-fp_border(color="grey",width=0.5)
  dataset<-hline(x=dataset,i=1:3,j=1:6,border=tiny_border,part="body")
  return (dataset)
}

#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
labpeaks_format_juris<-function(dataset){
    dataset<-add_header_row(x=dataset,top=TRUE,values=c("s"),colwidths=c(15))
    dataset<-add_header_row(x=dataset,top=TRUE,values=glue("March 01, 2020 - {lab_Bdate}"),colwidths=c(15))
    dataset<-add_header_row(x=dataset,top=TRUE,values=glue("{params$timeframe} SARS-CoV-2 NAAT Percent Positivity and Test Volume, {params$juris}"),colwidths=c(15))
    dataset<-fontsize(x=dataset,i=1,size=16,part="header")
    dataset<-fontsize(x=dataset,i=2,size=8,part="header")
    dataset<-color(x=dataset,i=3,color="white",part="header")
    dataset<-hrule(dataset,rule="exact",part="body")
    dataset<-hrule(dataset,rule="exact",part="header")
    dataset<-width(dataset,j=c(2,5),width=0.6)
    dataset<-width(dataset,j=c(4,8,12),width=0.2)
    dataset<-width(dataset,j=c(3,6,10,11,13,14),width=0.6)
    dataset<-width(dataset,j=c(1,9),width=0.6)
    dataset<-width(dataset,j=c(7,15),width=0.6)
    dataset<-height(dataset,part="header",i=4:5,height=0.12)
    dataset<-height(dataset,part="header",i=6,height=0.4)
    dataset<-height(dataset,part="body",height=0.10)
    dataset<-compose(dataset,part="header",i=6,j=c(1,9),value=as_paragraph("Peak"))
    dataset<-compose(dataset,part="header",i=6,j=c(2,5),value=as_paragraph("% Positivity"))
    dataset<-compose(dataset,part="header",i=6,j=c(3,6,11,14),value=as_paragraph("Date"))
    dataset<-compose(dataset,part="header",i=6,j=c(10,13),value=as_paragraph("Test Vol"))
    dataset<-compose(dataset,part="body",i=1,j=c(1,9),value=as_paragraph("1st Peak"))
    dataset<-compose(dataset,part="body",i=2,j=c(1,9),value=as_paragraph("2nd Peak"))
    dataset<-compose(dataset,part="body",i=3,j=c(1,9),value=as_paragraph("3rd Peak"))
    dataset<-compose(dataset,part="body",i=4,j=c(1,9),value=as_paragraph("Latest Peak"))
    dataset<-color(x=dataset,i=4,j=8,color="white",part="header")
    dataset<-color(x=dataset,i=5,j=c(1,4,8,9,12),color="white",part="header")
    dataset<-color(x=dataset,i=6,j=c(4,8,12),color="white",part="header")
    dataset<-color(x=dataset,i=6,j=c(1,2,3,5,6,7,9,10,11),color="black",part="header")
    dataset<-color(x=dataset,j=c(4,8,12),color="white",part="body")
    dataset<-align(x=dataset,align="center",part="body")
    dataset<-align(x=dataset,align="center",part="header")
    dataset<-bold(x=dataset,i=c(1,4,5,6),part="header")
    dataset<-fontsize(x=dataset,i=4,size=8,part="header")
    dataset<-fontsize(x=dataset,i=5,size=7,part="header")
    dataset<-fontsize(x=dataset,i=6,size=6,part="header")
    dataset<-fontsize(x=dataset,size=6,part="body")
    dataset<-border_remove(dataset)
    std_border=fp_border(color="black")
    dataset<-hline(x=dataset,i=5,j=2:3,border=std_border,part="header")
    dataset<-hline(x=dataset,i=5,j=5:7,border=std_border,part="header")
    dataset<-hline(x=dataset,i=5,j=10:11,border=std_border,part="header")
    dataset<-hline(x=dataset,i=5,j=13:15,border=std_border,part="header")
    dataset<-hline(x=dataset,i=4,j=1:7,border=std_border,part="header")
    dataset<-hline(x=dataset,i=4,j=9:15,border=std_border,part="header")
    small_border<-fp_border(color="grey")
    dataset<-hline(x=dataset,i=6,j=1:7,border=small_border,part="header")
    dataset<-hline(x=dataset,i=6,j=9:15,border=small_border,part="header")
    tiny_border<-fp_border(color="grey",width=0.5)
    dataset<-hline(x=dataset,i=1:4,j=1:7,border=tiny_border,part="body")
    dataset<-hline(x=dataset,i=1:4,j=9:15,border=tiny_border,part="body")
    return (dataset)
}
#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
labpeaks_format_reg<-function(dataset){
    dataset<-add_header_row(x=dataset,top=TRUE,values=c("s"),colwidths=c(15))
    dataset<-add_header_row(x=dataset,top=TRUE,values=glue("March 01, 2020 - {lab_Bdate}"),colwidths=c(15))
    dataset<-add_header_row(x=dataset,top=TRUE,values=glue("{params$timeframe} SARS-CoV-2 NAAT Percent Positivity and Test Volume, Regioin {params$region}"),colwidths=c(15))
    dataset<-fontsize(x=dataset,i=1,size=16,part="header")
    dataset<-fontsize(x=dataset,i=2,size=8,part="header")
    dataset<-color(x=dataset,i=3,color="white",part="header")
    dataset<-hrule(dataset,rule="exact",part="body")
    dataset<-hrule(dataset,rule="exact",part="header")
    dataset<-width(dataset,j=c(2,5),width=0.6)
    dataset<-width(dataset,j=c(4,8,12),width=0.2)
    dataset<-width(dataset,j=c(3,6,10,11,13,14),width=0.6)
    dataset<-width(dataset,j=c(1,9),width=0.6)
    dataset<-width(dataset,j=c(7,15),width=0.6)
    dataset<-height(dataset,part="header",i=4:5,height=0.12)
    dataset<-height(dataset,part="header",i=6,height=0.4)
    dataset<-height(dataset,part="body",height=0.10)
    dataset<-compose(dataset,part="header",i=6,j=c(1,9),value=as_paragraph("Peak"))
    dataset<-compose(dataset,part="header",i=6,j=c(2,5),value=as_paragraph("% Positivity"))
    dataset<-compose(dataset,part="header",i=6,j=c(3,6,11,14),value=as_paragraph("Date"))
    dataset<-compose(dataset,part="header",i=6,j=c(10,13),value=as_paragraph("Test Vol"))
    dataset<-compose(dataset,part="body",i=1,j=c(1,9),value=as_paragraph("1st Peak"))
    dataset<-compose(dataset,part="body",i=2,j=c(1,9),value=as_paragraph("2nd Peak"))
    dataset<-compose(dataset,part="body",i=3,j=c(1,9),value=as_paragraph("3rd Peak"))
    dataset<-compose(dataset,part="body",i=4,j=c(1,9),value=as_paragraph("Latest Peak"))
    dataset<-color(x=dataset,i=4,j=8,color="white",part="header")
    dataset<-color(x=dataset,i=5,j=c(1,4,8,9,12),color="white",part="header")
    dataset<-color(x=dataset,i=6,j=c(4,8,12),color="white",part="header")
    dataset<-color(x=dataset,i=6,j=c(1,2,3,5,6,7,9,10,11),color="black",part="header")
    dataset<-color(x=dataset,j=c(4,8,12),color="white",part="body")
    dataset<-align(x=dataset,align="center",part="body")
    dataset<-align(x=dataset,align="center",part="header")
    dataset<-bold(x=dataset,i=c(1,4,5,6),part="header")
    dataset<-fontsize(x=dataset,i=4,size=8,part="header")
    dataset<-fontsize(x=dataset,i=5,size=7,part="header")
    dataset<-fontsize(x=dataset,i=6,size=6,part="header")
    dataset<-fontsize(x=dataset,size=6,part="body")
    dataset<-border_remove(dataset)
    std_border=fp_border(color="black")
    dataset<-hline(x=dataset,i=5,j=2:3,border=std_border,part="header")
    dataset<-hline(x=dataset,i=5,j=5:7,border=std_border,part="header")
    dataset<-hline(x=dataset,i=5,j=10:11,border=std_border,part="header")
    dataset<-hline(x=dataset,i=5,j=13:15,border=std_border,part="header")
    dataset<-hline(x=dataset,i=4,j=1:7,border=std_border,part="header")
    dataset<-hline(x=dataset,i=4,j=9:15,border=std_border,part="header")
    small_border<-fp_border(color="grey")
    dataset<-hline(x=dataset,i=6,j=1:7,border=small_border,part="header")
    dataset<-hline(x=dataset,i=6,j=9:15,border=small_border,part="header")
    tiny_border<-fp_border(color="grey",width=0.5)
    dataset<-hline(x=dataset,i=1:4,j=1:7,border=tiny_border,part="body")
    dataset<-hline(x=dataset,i=1:4,j=9:15,border=tiny_border,part="body")
    return (dataset)
}
#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
labpeaks_format_nat<-function(dataset){
    dataset<-add_header_row(x=dataset,top=TRUE,values=c("s"),colwidths=c(15))
    dataset<-add_header_row(x=dataset,top=TRUE,values=glue("March 01, 2020 - {lab_Bdate}"),colwidths=c(15))
    dataset<-add_header_row(x=dataset,top=TRUE,values=glue("{params$timeframe} SARS-CoV-2 NAAT Percent Positivity and Test Volume, US"),colwidths=c(15))
    dataset<-fontsize(x=dataset,i=1,size=16,part="header")
    dataset<-fontsize(x=dataset,i=2,size=8,part="header")
    dataset<-color(x=dataset,i=3,color="white",part="header")
    dataset<-hrule(dataset,rule="exact",part="body")
    dataset<-hrule(dataset,rule="exact",part="header")
    dataset<-width(dataset,j=c(2,5),width=0.6)
    dataset<-width(dataset,j=c(4,8,12),width=0.2)
    dataset<-width(dataset,j=c(3,6,10,11,13,14),width=0.6)
    dataset<-width(dataset,j=c(1,9),width=0.6)
    dataset<-width(dataset,j=c(7,15),width=0.6)
    dataset<-height(dataset,part="header",i=4:5,height=0.12)
    dataset<-height(dataset,part="header",i=6,height=0.4)
    dataset<-height(dataset,part="body",height=0.10)
    dataset<-compose(dataset,part="header",i=6,j=c(1,9),value=as_paragraph("Peak"))
    dataset<-compose(dataset,part="header",i=6,j=c(2,5),value=as_paragraph("% Positivity"))
    dataset<-compose(dataset,part="header",i=6,j=c(3,6,11,14),value=as_paragraph("Date"))
    dataset<-compose(dataset,part="header",i=6,j=c(10,13),value=as_paragraph("Test Vol"))
    dataset<-compose(dataset,part="body",i=1,j=c(1,9),value=as_paragraph("1st Peak"))
    dataset<-compose(dataset,part="body",i=2,j=c(1,9),value=as_paragraph("2nd Peak"))
    dataset<-compose(dataset,part="body",i=3,j=c(1,9),value=as_paragraph("3rd Peak"))
    dataset<-compose(dataset,part="body",i=4,j=c(1,9),value=as_paragraph("Latest Peak"))
    dataset<-color(x=dataset,i=4,j=8,color="white",part="header")
    dataset<-color(x=dataset,i=5,j=c(1,4,8,9,12),color="white",part="header")
    dataset<-color(x=dataset,i=6,j=c(4,8,12),color="white",part="header")
    dataset<-color(x=dataset,i=6,j=c(1,2,3,5,6,7,9,10,11),color="black",part="header")
    dataset<-color(x=dataset,j=c(4,8,12),color="white",part="body")
    dataset<-align(x=dataset,align="center",part="body")
    dataset<-align(x=dataset,align="center",part="header")
    dataset<-bold(x=dataset,i=c(1,4,5,6),part="header")
    dataset<-fontsize(x=dataset,i=4,size=8,part="header")
    dataset<-fontsize(x=dataset,i=5,size=7,part="header")
    dataset<-fontsize(x=dataset,i=6,size=6,part="header")
    dataset<-fontsize(x=dataset,size=6,part="body")
    dataset<-border_remove(dataset)
    std_border=fp_border(color="black")
    dataset<-hline(x=dataset,i=5,j=2:3,border=std_border,part="header")
    dataset<-hline(x=dataset,i=5,j=5:7,border=std_border,part="header")
    dataset<-hline(x=dataset,i=5,j=10:11,border=std_border,part="header")
    dataset<-hline(x=dataset,i=5,j=13:15,border=std_border,part="header")
    dataset<-hline(x=dataset,i=4,j=1:7,border=std_border,part="header")
    dataset<-hline(x=dataset,i=4,j=9:15,border=std_border,part="header")
    small_border<-fp_border(color="grey")
    dataset<-hline(x=dataset,i=6,j=1:7,border=small_border,part="header")
    dataset<-hline(x=dataset,i=6,j=9:15,border=small_border,part="header")
    tiny_border<-fp_border(color="grey",width=0.5)
    dataset<-hline(x=dataset,i=1:4,j=1:7,border=tiny_border,part="body")
    dataset<-hline(x=dataset,i=1:4,j=9:15,border=tiny_border,part="body")
    return (dataset)
}
#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
labpeaks_format<-function(dataset){

  dataset<-hrule(dataset,rule="exact",part="body")
  dataset<-hrule(dataset,rule="exact",part="header")
  dataset<-width(dataset,j=c(2,5),width=0.6)
  dataset<-width(dataset,j=c(4,8,12),width=0.2)
  dataset<-width(dataset,j=c(3,6,10,11,13,14),width=0.6)
  dataset<-width(dataset,j=c(1,9),width=0.6)
  dataset<-width(dataset,j=c(7,15),width=0.6)
  dataset<-height(dataset,part="header",i=1:2,height=0.12)
  dataset<-height(dataset,part="header",i=3,height=0.4)
  dataset<-height(dataset,part="body",height=0.10)
  dataset<-compose(dataset,part="header",i=3,j=c(1,9),value=as_paragraph("Peak"))
  dataset<-compose(dataset,part="header",i=3,j=c(2,5),value=as_paragraph("% Positivity"))
  dataset<-compose(dataset,part="header",i=3,j=c(3,6,11,14),value=as_paragraph("Date"))
  dataset<-compose(dataset,part="header",i=3,j=c(10,13),value=as_paragraph("Test Vol"))
  dataset<-compose(dataset,part="body",i=1,j=c(1,9),value=as_paragraph("1st Peak"))
  dataset<-compose(dataset,part="body",i=2,j=c(1,9),value=as_paragraph("2nd Peak"))
  dataset<-compose(dataset,part="body",i=3,j=c(1,9),value=as_paragraph("3rd Peak"))
  dataset<-compose(dataset,part="body",i=4,j=c(1,9),value=as_paragraph("Latest Peak"))
  dataset<-color(x=dataset,i=1,j=8,color="white",part="header")
  dataset<-color(x=dataset,i=2,j=c(1,4,8,9,12),color="white",part="header")
  dataset<-color(x=dataset,i=3,j=c(4,8,12),color="white",part="header")
  dataset<-color(x=dataset,i=3,j=c(1,2,3,5,6,7,9,10,11),color="black",part="header")
  dataset<-color(x=dataset,j=c(4,8,12),color="white",part="body")
  dataset<-align(x=dataset,align="center",part="body")
  dataset<-align(x=dataset,align="center",part="header")
  dataset<-bold(x=dataset,i=1:3,part="header")
  dataset<-fontsize(x=dataset,i=1,size=8,part="header")
  dataset<-fontsize(x=dataset,i=2,size=7,part="header")
  dataset<-fontsize(x=dataset,i=3,size=6,part="header")
  dataset<-fontsize(x=dataset,size=6,part="body")
  dataset<-border_remove(dataset)
  std_border=fp_border(color="black")
  dataset<-hline(x=dataset,i=2,j=2:3,border=std_border,part="header")
  dataset<-hline(x=dataset,i=2,j=5:7,border=std_border,part="header")
  dataset<-hline(x=dataset,i=2,j=10:11,border=std_border,part="header")
  dataset<-hline(x=dataset,i=2,j=13:15,border=std_border,part="header")
  dataset<-hline(x=dataset,i=1,j=1:7,border=std_border,part="header")
  dataset<-hline(x=dataset,i=1,j=9:15,border=std_border,part="header")
  small_border<-fp_border(color="grey")
  dataset<-hline(x=dataset,i=3,j=1:7,border=small_border,part="header")
  dataset<-hline(x=dataset,i=3,j=9:15,border=small_border,part="header")
  tiny_border<-fp_border(color="grey",width=0.5)
  dataset<-hline(x=dataset,i=1:4,j=1:7,border=tiny_border,part="body")
  dataset<-hline(x=dataset,i=1:4,j=9:15,border=tiny_border,part="body")
  return (dataset)
}
