#' Load, clean, and analyze state hospital data
#'
#' @param dataset
#'
#' @export
#' @import dplyr
#' @return returns aggregate hospital table with metrics
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
#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
case_numbers_list_format_exsum<-function(dataset){
    dataset<-hrule(dataset,rule="exact",part="body")
    dataset<-height(dataset,i=c(2,3,4,6,7),height=0.5)
    dataset<-height(dataset,i=c(1,5),height=0.5)
    dataset<-width(dataset,width=10.5)
    dataset<-width(dataset,j=1,width=3)
    dataset<-width(dataset,j=2,width=3)
    dataset<-width(dataset,j=3,width=3.5)
    dataset<-fontsize(x=dataset,i=c(1,5),size=18,part="body")
    dataset<-fontsize(x=dataset,i=c(2,6),size=16,part="body")
    dataset<-fontsize(x=dataset,i=c(3,7),size=14,part="body")
    dataset<-color(x=dataset,i=1,color="orange",part="body")
    dataset<-color(x=dataset,i=5,color="black",part="body")
    dataset<-color(x=dataset,i=c(2,3,6,7),color="#666666",part="body")
    dataset<-bold(x=dataset,i=1:7,part="body")
    dataset<-color(x=dataset,i=1,color="white",part="header")
    dataset<-color(x=dataset,i=1:3,j=3,color="white",part="body")
    dataset<-color(x=dataset,i=4,color="white",part="body")
    dataset<-color(x=dataset,i=3,j=1,color="white",part="body")
    dataset<-color(x=dataset,i=7,j=3,color="white",part="body")
    dataset<-align(x=dataset,align="left",part="body")
    dataset<-align(x=dataset,i=4,align="left",part="body")
    dataset<-border_remove(dataset)
    return (dataset)
}
#' Title
#'
#' @param indicator
#' @param dataset1
#' @param dataset2
#'
#' @return
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
#' Title
#'
#' @param dataset1
#' @param dataset2
#' @param indicator
#'
#' @return
#' @export
#'
#' @examples
numbers_list_new_format_exsum2<-function(dataset1,dataset2,indicator){

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
    dataset1 <-  dataset1 |>
        rename(date  := {{date_col}},
               cum   := {{cum_col}},
               new   := {{new_col}})
    #get dates
    date_raw<-dataset1%>%pull(date)
    date<-format(date_raw,format="%b %d, %Y")
    #get the case and death data current week start date
    date_6<- format(date_raw-6, format="%b %d, %Y")

    dataset2<-compose(dataset2,part="header",j=1:10,value=as_paragraph("s"))
    dataset2<-add_header_row(x=dataset2,values=c(glue("Total {indicator} Reported"),glue("New Weekly {indicator}"),"s","Weekly Total","s","7-Day Daily Average"),colwidths=c(1,1,2,2,1,3))
    dataset2<-add_header_row(x=dataset2,values=c(dataset1$cum,dataset1$new,"s",glue("Peaks in Weekly Total and Weekly Average of Daily {indicator}")),colwidths=c(1,1,1,7))
    dataset2<-hrule(dataset2,rule="exact",part="body")
    dataset2<-hrule(dataset2,rule="exact",part="header")
    dataset2<-width(dataset2,j=1:3,width=2.6)
    dataset2<-width(dataset2,j=7,width=0.1)
    dataset2<-height(dataset2,part="body",height=0.15)
    dataset2<-compose(dataset2,part="header",j=2,i=3,value=as_paragraph(glue("{date_6} - {date}")))
    dataset2<-fontsize(dataset2,size=5,j=2,part="header")
    dataset2<-compose(dataset2,part="header",j=4,i=3,value=as_paragraph("Peak"))
    dataset2<-compose(dataset2,part="header",j=5,i=3,value=as_paragraph(glue("New {indicator}")))
    dataset2<-compose(dataset2,part="header",j=c(6,9),i=3,value=as_paragraph("Reporting Week End"))
    dataset2<-compose(dataset2,part="header",j=8,i=3,value=as_paragraph("Average"))
    dataset2<-compose(dataset2,part="header",j=10,i=3,value=as_paragraph("%Change From Current"))
    dataset2<-fontsize(dataset2,size=11,i=1,j=1:3,part="header")
    dataset2<-fontsize(dataset2,size=11,i=2,j=1:3,part="body")
    dataset2<-fontsize(dataset2,size=8,i=2,j=1:2,part="header")
    dataset2<-fontsize(dataset2,size=8,i=4,j=1:3,part="body")
    dataset2<-fontsize(dataset2,size=6,i=5,j=1:3,part="body")
    dataset2<-fontsize(dataset2,size=6,j=4:10,i=1:3,part="header")
    dataset2<-fontsize(dataset2,size=5,j=4:10,part="body")
    dataset2<-fontsize(dataset2,size=6,i=2,j=4:10,part="header")
    dataset2<-fontsize(dataset2,size=7,i=1,j=4:10,part="header")
    dataset2<-color(x=dataset2,i=1,j=1:2,color="orange",part="header")
    dataset2<-color(x=dataset2,i=3,j=1,color="white",part="header")
    dataset2<-color(x=dataset2,i=1:3,j=3,color="white",part="header")
    dataset2<-color(x=dataset2,i=2:3,j=7,color="white",part="header")
    dataset2<-color(x=dataset2,j=1,i=c(1,6,7),color="white",part="body")
    dataset2<-color(x=dataset2,j=2,i=c(1,6,7),color="white",part="body")
    dataset2<-color(x=dataset2,j=3,i=c(1,5,6,7),color="white",part="body")
    dataset2<-color(x=dataset2,j=7,color="white",part="body")
    dataset2<-bold(x=dataset2,i=1,part="header")
    dataset2<-bold(x=dataset2,i=2,j=4:10,part="header")
    dataset2<-bold(x=dataset2,i=2:3,j=1:3,part="body")
    dataset2<-align(dataset2,align="center",j=4:10,part="header")
    dataset2<-valign(dataset2,valign="top",j=2,i=3,part="header")
    dataset2<-align(dataset2,align="center",j=4:10,part="body")
    dataset2<-merge_at(x=dataset2,i=2:3,j=1,part="body")
    dataset2<-merge_at(x=dataset2,i=2:3,j=2,part="body")
    dataset2<-merge_at(x=dataset2,i=2:3,j=3,part="body")
    dataset2<-border_remove(dataset2)
    std_border<-fp_border(color="black")
    dataset2<-hline(x=dataset2,i=1,j=4:10,border=std_border,part="header")
    dataset2<-hline(x=dataset2,i=2,j=5:6,border=std_border,part="header")
    dataset2<-hline(x=dataset2,i=2,j=8:10,border=std_border,part="header")
    small_border<-fp_border(color="grey")
    dataset2<-hline(x=dataset2,i=3,j=4:10,border=small_border,part="header")
    tiny_border<-fp_border(color="grey",width=0.5)
    dataset2<-hline(x=dataset2,i=1:7,j=4:10,border=tiny_border,part="body")

    return(dataset2)}
#' Title
#'
#' @param dataset
#'
#' @return
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
#' Title
#'
#' @param dataset
#'
#' @return
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
#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
hosp_numbers_list_format_wh<-function(dataset){

    dataset<-width(dataset,width=62)
    dataset<-height(dataset,i=c(1,3,5,7,9,11),height=0.7)
    dataset<-height(dataset,i=c(2,4,6,8,10,12),height=0.9)
    dataset<-fontsize(x=dataset,i=c(1,3,5,7,9,11),size=23,part="body")
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

