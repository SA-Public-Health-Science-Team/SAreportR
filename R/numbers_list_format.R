#' format the flextable of vertical number list of case or death data or hospital data
#'
#' @param dataset could be case or death data or hospital data flextable
#'
#' @export
#' @import dplyr
#' @return formatted vertical flextable
#' @export
#'
#' @examples
#'
numbers_list_format_exsum<-function(dataset){
    dataset<-hrule(dataset,rule="exact",part="body")
    dataset<-height(dataset,i=c(2,3,5,6,8,9,11,12,14),height=0.4)
    dataset<-height(dataset,i=c(1,4,7,10,13),height=0.5)
    dataset<-width(dataset,width=4.5)
    dataset<-fontsize(x=dataset,i=c(1,4,7,10,13),size=24,part="body")
    dataset<-fontsize(x=dataset,i=c(2,5,8,11,14),size=18,part="body")
    dataset<-fontsize(x=dataset,i=c(3,6,9,12),size=17,part="body")
    dataset<-color(x=dataset,i=1,j=1,color="white",part="header")
    dataset<-color(x=dataset,i=c(1,4),color="orange",part="body")
    dataset<-color(x=dataset,i=c(7,10,13),color="black",part="body")
    dataset<-color(x=dataset,i=c(2,3,5,6,8,9,11,12,14),color="#666666",part="body")
    dataset<-bold(x=dataset,i=1:14,part="body")
    dataset<-align(x=dataset,align="left",part="body")
    dataset<-align(x=dataset,i=4,align="left",part="body")
    dataset<-border_remove(dataset)
    return (dataset)
}

#' Title create horizontal list of cumulative, new, average, change numbers and peak numbers table for case or death data
#'
#' @param indicator could be "Cases" or "Deaths"
#' @param dataset1 the last date of case or death data to show cumulative, new, average, and change numbers
#' @param dataset2 case or death peak number data
#'
#' @return formatted horizontal table combined with cumulative, new, average, change numbers and peak numbers
#' @export
#'
#' @examples
numbers_list_new_format_exsum<-function(dataset1,dataset2,indicator){

    date_col<-dataset1 |>
        select(contains("date")) |>
        select(1) |>
        colnames()
    cum_col<-dataset1 |>
        select(contains("cum")) |>
        select(1) |>
        colnames()
    new_col<-dataset1 |>
        select(contains("new")) |>
        select(1) |>
        colnames()
    avg_col<-dataset1 |>
        select(contains("avg")) |>
        select(1) |>
        colnames()
    prior_avg_col<-dataset1 |>
        select(contains("prior_avg")) |>
        select(1) |>
        colnames()
    change_col<-dataset1 |>
        select(contains("change")) |>
        select(1) |>
        colnames()
    dataset1 <-  dataset1 |>
        rename(date  := {{date_col}},
               cum   := {{cum_col}},
               new   := {{new_col}},
               avg   := {{avg_col}},
               prior_avg   := {{prior_avg_col}},
               change   := {{change_col}})
    #get dates
    date_raw<-dataset1%>%pull(date)
    date<-format(date_raw,format="%b %d, %Y")
    #get the case and death data current week start date
    date_6<- format(date_raw-6, format="%b %d, %Y")
    date_7<- format(date_raw-7, format="%b %d, %Y")
    date_13<- format(date_raw-13, format="%b %d, %Y")
    n<-nrow(dataset2)
    dataset<-dataset2%>%flextable()

    dataset<-compose(dataset,part="header",j=1:10,value=as_paragraph("s"))
    dataset<-add_header_row(x=dataset,values=c(glue("Total {indicator} Reported"),glue("New Weekly {indicator}"),"s","Weekly Total","s","7-Day Daily Average"),colwidths=c(1,1,2,2,1,3))
    dataset<-add_header_row(x=dataset,values=c(dataset1$cum,dataset1$new,"s",glue("Peaks in Weekly Total and Weekly Average of Daily {indicator}")),colwidths=c(1,1,1,7))
    dataset<-hrule(dataset,rule="exact",part="body")
    dataset<-hrule(dataset,rule="exact",part="header")
    dataset<-width(dataset,j=1:3,width=2.6)
    dataset<-width(dataset,j=7,width=0.1)
    dataset<-height(dataset,part="body",height=0.15)
    dataset<-compose(dataset,part="header",j=2,i=3,value=as_paragraph(glue("{date_6} - {date}")))
    dataset<-fontsize(dataset,size=5,j=2,part="header")
    dataset<-compose(dataset,part="header",j=4,i=3,value=as_paragraph("Peak"))
    dataset<-compose(dataset,part="header",j=5,i=3,value=as_paragraph(glue("New {indicator}")))
    dataset<-compose(dataset,part="header",j=c(6,9),i=3,value=as_paragraph("Reporting Week End"))
    dataset<-compose(dataset,part="header",j=8,i=3,value=as_paragraph("Average"))
    dataset<-compose(dataset,part="header",j=10,i=3,value=as_paragraph("%Change From Current"))
    dataset<-fontsize(dataset,size=11,i=1,j=1:3,part="header")
    dataset<-fontsize(dataset,size=11,i=3,j=1:3,part="body")
    dataset<-fontsize(dataset,size=8,i=2,j=1:2,part="header")
    dataset<-fontsize(dataset,size=8,i=5,j=1:3,part="body")
    dataset<-fontsize(dataset,size=6,i=6,j=1:3,part="body")
    dataset<-fontsize(dataset,size=6,j=4:10,i=1:3,part="header")
    dataset<-fontsize(dataset,size=5,j=4:10,part="body")
    dataset<-fontsize(dataset,size=6,i=2,j=4:10,part="header")
    dataset<-fontsize(dataset,size=7,i=1,j=4:10,part="header")
    dataset<-color(x=dataset,i=1,j=1:2,color="orange",part="header")
    dataset<-color(x=dataset,i=3,j=1,color="white",part="header")
    dataset<-color(x=dataset,i=1:3,j=3,color="white",part="header")
    dataset<-color(x=dataset,i=2:3,j=7,color="white",part="header")
    dataset<-color(x=dataset,j=1,i=c(1,2),color="white",part="body")
    dataset<-color(x=dataset,j=2,i=c(1,2),color="white",part="body")
    dataset<-color(x=dataset,j=3,i=c(1,2),color="white",part="body")
    dataset<-color(x=dataset,j=7,color="white",part="body")
    dataset<-bold(x=dataset,i=1,part="header")
    dataset<-bold(x=dataset,i=2,j=4:10,part="header")
    dataset<-bold(x=dataset,i=3:4,j=1:3,part="body")
    dataset<-align(dataset,align="center",j=4:10,part="header")
    dataset<-valign(dataset,valign="top",j=2,i=3,part="header")
    dataset<-align(dataset,align="center",j=4:10,part="body")
    dataset<-merge_at(x=dataset,i=3:4,j=1,part="body")
    dataset<-merge_at(x=dataset,i=3:4,j=2,part="body")
    dataset<-merge_at(x=dataset,i=3:4,j=3,part="body")
    dataset<-border_remove(dataset)
    std_border<-fp_border(color="black")
    dataset<-hline(x=dataset,i=1,j=4:10,border=std_border,part="header")
    dataset<-hline(x=dataset,i=2,j=5:6,border=std_border,part="header")
    dataset<-hline(x=dataset,i=2,j=8:10,border=std_border,part="header")
    small_border<-fp_border(color="grey")
    dataset<-hline(x=dataset,i=3,j=4:10,border=small_border,part="header")
    tiny_border<-fp_border(color="grey",width=0.5)
    dataset<-hline(x=dataset,i=1:n,j=4:10,border=tiny_border,part="body")

    return(dataset)}

#' Title format flextable of vertical list of cumulative, average, change numbers of lab data in the exsum
#'
#' @param dataset lab data flextable
#'
#' @return formatted flextable with vertical list of cumulative, average, change numbers of lab data
#' @export
#'
#' @examples
lab_numbers_list_format_exsum<-function(dataset){
    dataset<-hrule(dataset,rule="exact",part="body")
    dataset<-height(dataset,i=c(2,4,5,7,8,10,12,13,15,16,18,20),height=0.6)
    dataset<-height(dataset,i=c(1,3,6,9,11,14,17,19),height=0.7)
    dataset<-width(dataset,width=8.5)
    dataset<-fontsize(x=dataset,i=c(1,3,6,9,11,14,17,19),size=38,part="body")
    dataset<-fontsize(x=dataset,i=c(2,4,7,10,12,15,18,20),size=30,part="body")
    dataset<-fontsize(x=dataset,i=c(5,8,13,16),size=28,part="body")
    dataset<-color(x=dataset,i=1,j=1,color="white",part="header")
    dataset<-color(x=dataset,i=c(1,3,6,9),color="orange",part="body")
    dataset<-color(x=dataset,i=c(2,4,5,7,8,10,12,13,15,16,18,20),color="#666666",part="body")
    dataset<-bold(x=dataset,i=1:20,part="body")
    dataset<-align(x=dataset,align="left",part="body")
    dataset<-align(x=dataset,i=4,align="left",part="body")
    dataset<-border_remove(dataset)

    return(dataset)}
#' Title format vertical list of cumulative, new, average, change numbers of case data in the White House Slides
#'
#' @param dataset case data flextable
#'
#' @return formatted flextable with vertical list of cumulative, new, average, change numbers of case data
#' @export
#'
#' @examples
case_numbers_list_format_wh<-function(dataset){
    dataset<-hrule(dataset,rule="exact",part="body")
    dataset<-height(dataset,i=c(1,3,5,7,9),height=0.9)
    dataset<-height(dataset,i=c(2,4,6,8,10),height=1.1)
    dataset<-width(dataset,width=112)
    dataset<-fontsize(x=dataset,i=c(1,3,5,7,9),size=29,part="body")
    dataset<-fontsize(x=dataset,i=c(2,4,6,8,10),size=36,part="body")
    dataset<-color(x=dataset,i=1,j=1,color="white",part="header")
    dataset<-color(x=dataset,i=c(1,3,5,7,9),color="#062a78",part="body")
    dataset<-bold(x=dataset,j=1,part="body")
    dataset<-align(x=dataset,align="left",part="body")
    dataset<-border_remove(dataset)
    dataset<-bg(x=dataset,j=1,bg="#fff8dc",part="body")
    std_border<-fp_border(color="#ba8759",width=2)
    dataset<-hline(x=dataset,i=4,border=std_border,part="body")
    return (dataset)
}
#' Title format vertical list of cumulative, new, average, change numbers of hospital data in the White House Slides
#'
#' @param dataset hospital or death flextable
#'
#' @return formatted flextable with vertical list of cumulative, new, average, change numbers of hospital or death data
#' @export
#'
#' @examples
hosp_numbers_list_format_wh<-function(dataset){

    dataset<-width(dataset,width=64)
    dataset<-height(dataset,i=c(1,3,5,7,9,11),height=0.7)
    dataset<-height(dataset,i=c(2,4,6,8,10,12),height=0.9)
    dataset<-fontsize(x=dataset,i=c(1,3,5,7,9,11),size=23.5,part="body")
    dataset<-fontsize(x=dataset,i=c(2,4,6,8,10,12),size=36,part="body")
    dataset<-hrule(dataset,rule="exact",part="body")
    dataset<-color(x=dataset,i=1,j=1,color="white",part="header")
    dataset<-color(x=dataset,i=c(1,3,5,7,9,11),color="#062a78",part="body")
    dataset<-bold(x=dataset,j=1,part="body")
    dataset<-align(x=dataset,align="left",part="body")
    dataset<-border_remove(dataset)
    dataset<-bg(x=dataset,j=1,bg="#fff8dc",part="body")
    std_border<-fp_border(color="#ba8759",width=2)
    dataset<-hline(x=dataset,i=4,border=std_border,part="body")
    return (dataset)
}

