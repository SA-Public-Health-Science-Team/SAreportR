
#' Title plot epicurve for case, death, hospital, lab or any dataset with date and new
#'
#' @param dataset case,death, hospital, lab or any dataset
#' @param indicator could be "Cases", "Deaths", "Admissions", "Test Volume" or any
#' @param timeframe could be "Daily", "Weekly", or "Monthly"
#' @param color_of_column could be "#E1BE6A"
#'
#' @return epicurve
#' @export
#'
#' @examples
plot_exsum<-function(dataset,indicator,timeframe,color_of_column){
    date_col<-dataset |>
        select(contains("date")) |>
        select(1) |>
        colnames()
    new_col<-dataset |>
        select(contains("new")) |>
        select(1) |>
        colnames()
    dataset <-  dataset |>
        rename(date  := {{date_col}},
               new   := {{new_col}}
        )

    #plot weekly cases
    plot<-ggplot(dataset,aes(date))+
        #add column weekly cases
        geom_col(aes(y=new,fill=glue(" New {timeframe} {indicator}")),color =color_of_column) +
        #add 21 day avg line
        #geom_line(aes(y=avg*7,group=1,color=glue("21-Day Average {indicator}    ")),size=1.3)+
         #set color of 21 day avg line
       # scale_color_manual(" ",values=color_of_avg_line)+
        #set color of column
        scale_fill_manual(" ",values=color_of_column)+
        #set maximal value of y axis and set the secondary y axis
        scale_y_continuous(limits=c(0,max(dataset$new)*1.3),labels = label_number(big.mark = ","))+
        #set axis title
        labs(x="Reporting End Date",y=glue("{timeframe} New {indicator}"))+
        #set theme
        theme(axis.title = element_text(size = rel(1.1),face="bold"),
              axis.text = element_text(size = rel(1.1)),
              axis.line = element_line(color = "black"),
              # La légende set the format of the legend
              legend.text = element_text(size = rel(1.1), face = "bold"),
              legend.key.size = unit(0.9, "lines"),
              legend.position="top")
    return(plot)
}




#' Title plot weekly epicurve in the White House Slides
#'
#' @param dataset could be case, death, or hospital data
#' @param indicator could be "Cases", "Deaths", or "Admissions"
#'
#' @return a weekly epicurve with mark of lowest average
#' @export
#'
#' @examples
weekly_plot_wh<-function(dataset,indicator){
    date_col<-dataset |>
        select(contains("date")) |>
        select(1) |>
        colnames()

    new_col<-dataset |>
        select(contains("new")) |>
        select(1) |>
        colnames()
    avg_col<-dataset |>
        select(contains("avg")) |>
        select(1) |>
        colnames()

    dataset <-  dataset |>
        rename(date  := {{date_col}},
               new   := {{new_col}},
               avg   := {{avg_col}}
        )

    #get the latest date
    latest_date<-max(dataset$date,na.rm=TRUE)
    #get the 21 day avg case on the latest date
    latest_average<-dataset%>%filter(date==max(date,na.rm=TRUE))%>%head(1)%>%pull(avg)
    #format the 21 day avg case on the latest date
    latest_avg_format<-latest_average%>%format(justify="right",big.mark= ",",digits=0,scientific=FALSE)
    #get the lowest 21 day avg of 2021
    lowestavg2021<-dataset%>%filter(year(date)==2021)%>%filter(avg==min(avg,na.rm=TRUE))%>%head(1)%>%pull(avg)
    #get the date of the lowest 21 day avg of 2021
    lowestavg2021date<-dataset%>%filter(year(date)==2021)%>%filter(avg==min(avg,na.rm=TRUE))%>%head(1)%>%pull(date)
    #format the lowest 21 day avg of 2021
    lowestavg2021_format<-lowestavg2021%>%format(justify = "right",big.mark  = ",",digits   = 0,scientific=FALSE)
    #get the lowest 21 day avg of 2022
    lowestavg2022<-dataset%>%filter(year(date)==2022)%>%filter(avg==min(avg,na.rm=TRUE))%>%head(1)%>%pull(avg)
    #get the date of the lowest 21 day avg of 2022
    lowestavg2022date<-dataset%>%filter(year(date)==2022)%>%filter(avg==min(avg,na.rm=TRUE))%>%head(1)%>%pull(date)
    #format the lowest 21 day avg of 2022
    lowestavg2022_format<-lowestavg2022%>%format(big.mark  = ",",digits   = 0,scientific=FALSE)
    #get the maximal weekly cases
    maxcum<-max(dataset$new,na.rm=TRUE)
    #plot weekly case curve
    plot<-ggplot(dataset )+
        geom_col(aes(y=new,x=date,fill=glue(" Weekly New {indicator}")),color="#B4C7E7")+
        #add the black line of the lowest 21 day avg of 2021
        geom_col(aes(y={lowestavg2021}/17,x={lowestavg2021date}),color="black")+
        #add the black line of the lowest 21 day avg of 2022
        geom_col(aes(y={lowestavg2022}/17,x={lowestavg2022date}),color="black")+
        #add the red line of the latest 21 day avg
        geom_col(aes(y={latest_average}/27,x={latest_date}),color="red")+
        #add the 21 day avg line
        geom_line(aes(y=avg*7,x=date,group=1,color=glue("7-Day Average {indicator}    ")))+
        #set the color of 21 day avg line
        scale_color_manual(" ",values=c("#757575","black"))+
        #set the color of weekly cases column
        scale_fill_manual(" ",values="#B4C7E7")+
        #set the axis title
        labs(y     = glue("Weekly New {indicator}"),
             x     = "Date of Report"  ) +
        #add annotation of the lowest 21 day avg of 2021
        annotate("text",x={lowestavg2021date},y={maxcum}/2,label=glue("2021:\n LOWEST\n AVERAGE\n \n{lowestavg2021_format}"),size=3)+
        #add annotation of the lowest 21 day avg of 2021
        annotate("text",x={lowestavg2022date},y={maxcum}/1,label=glue("2022:\n LOWEST\n AVERAGE\n \n \n{lowestavg2022_format}"),size=3)+
        #add annotation of the latest 21 day avg
        annotate("text",x={latest_date},y={maxcum}/2,label=glue("LATEST\n AVERAGE\n \n{latest_avg_format}"),size=3)+
        #set the maximal of y axis
        scale_y_continuous(limits=c(0,max(dataset$new,na.rm=TRUE)*1.3),labels=label_number(big.mark =","))+
        #set the theme
        theme(axis.title=element_text(size=rel(1.1),face="bold"),
              axis.text=element_text(size = rel(1.1), face = "bold"),
              legend.key.size = unit(0.9, "lines"),
              legend.text = element_text(size = rel(1.1), face = "bold"),
              legend.position="top")

    return(plot)
}


#' Title plot monthly epicurve in the White House Slides
#'
#' @param dataset could be case, death, or hospital data
#' @param indicator could be "Cases", "Deaths", or "Admissions"
#'
#' @return a monthly epicurve with mark of lowest average
#' @export
#'
#' @examples
monthly_plot_wh<-function(dataset,indicator){

    cummonth_col<-dataset |>
        select(contains("cummonth")) |>
        select(1) |>
        colnames()
    dataset <-  dataset |>
        rename(cummonth   := {{cummonth_col}})
    #get the latest month
    latestnew_date<-max(dataset$month,na.rm=TRUE)
    #get the montly cases in the latest month
    latest_new<-dataset%>%tail(1)%>%pull(cummonth)
    #format the montly cases in the latest month
    latest_new_format<-latest_new%>%format(justify="right",big.mark= ",",digits=0,scientific=FALSE)
    #get the lowest monthly cases in 2021
    lowestnew2021min<-dataset%>%filter(year(month)==2021)%>%
        filter(cummonth==min(cummonth,na.rm=TRUE))%>%head(1)%>%
        pull(cummonth)
    #get the month of the lowest monthly cases in 2021
    lowestnew2021date<-dataset%>%filter(year(month)==2021)%>%filter(cummonth==lowestnew2021min,na.rm=TRUE)%>%head(1)%>%pull(month)
    #format the lowest monthly cases in 2021
    lowestnew2021_format<-lowestnew2021min%>%format(justify = "right",big.mark  = ",",digits   = 0,scientific=FALSE)
    #get the lowest monthly cases in 2022
    lowestnew2022min<-dataset%>%filter(year(month)==2022)%>%filter(cummonth==min(cummonth,na.rm=TRUE))%>%head(1)%>%pull(cummonth)
    #get the month of the lowest monthly cases in 2022
    lowestnew2022date<-dataset%>%filter(year(month)==2022)%>%filter(cummonth==lowestnew2022min,na.rm=TRUE)%>%head(1)%>%pull(month)
    #format the lowest monthly cases in 2022
    lowestnew2022_format<-lowestnew2022min%>%format(justify = "right",big.mark  = ",",digits   = 0,scientific=FALSE)
    #get the maximal monthly cases
    maxcum<-max(dataset$cummonth,na.rm=TRUE)
    #plot monthly case curve
    plot<-ggplot(dataset)+
        geom_col(aes(x=month,y=cummonth),fill="#B4C7E7")+
        #add the black line of the lowest monthly cases of 2021
        geom_col(aes(y={lowestnew2021min}/38,x={lowestnew2021date}),color="black")+
        #add the black line of the lowest monthly cases of 2022
        geom_col(aes(y={lowestnew2022min}/38,x={lowestnew2022date}),color="black")+
        #add the red line of the latest monthly cases
        geom_col(aes(y={latest_new}/35,x={latestnew_date}),color="red")+
        #set the axis title
        labs(y     = glue("Monthly New {indicator}"),
             x     = "Month of Report" ) +
        #set the color of the monthly cases column
        scale_fill_manual("",values=c("#B4C7E7"))+
        #add the annotation of the lowest monthly cases of 2021
        annotate("text",x={lowestnew2021date},y={maxcum}/2,label=glue("2021:\n LOWEST\n {indicator}\n \n{lowestnew2021_format}"),size=2)+
        #add the annotation of the lowest monthly cases of 2022
        annotate("text",x={lowestnew2022date},y={maxcum}/1.2,label=glue("2022:\n LOWEST\n {indicator}\n \n \n{lowestnew2022_format}"),size=2)+
        #add the annotation of the latest monthly cases
        annotate("text",x={latestnew_date},y={maxcum}/2,label=glue("LATEST\n {indicator}\n \n{latest_new_format}"),size=2)+
        #set the maximal value of y axis
        scale_y_continuous(limits=c(0,max(dataset$cummonth,na.rm=TRUE)*1.3),labels=label_number(big.mark =","))+
        #set theme
        theme(axis.title=element_text(size=rel(0.6),face="bold"),
              axis.text=element_text(size = rel(0.6), face = "bold"),
              legend.key.size = unit(0.5, "lines"),
              legend.text = element_text(size = rel(0.60), face = "bold"),
              legend.position="top")
    return(plot)
}
#' Title plot daily epicurve for hospital data in the White House Slides
#'
#' @param dataset hospital data
#'
#' @return a daily epicurve with mark of lowest average
#' @export
#'
#' @examples
daily_hosp_plot_wh<-function(dataset){
    #get the latest date
    latesthosp_date<-max(dataset$hosp_date,na.rm=TRUE)
    #get the 7 day avg admission on the latest date
    latesthosp_average<-dataset%>%filter(hosp_date==max(hosp_date,na.rm=TRUE))%>%head(1)%>%pull(hosp_avg)
    #format the 7 day avg admission on the latest date
    latest_hospavg_format<-latesthosp_average%>%format(justify = "right",big.mark  = ",",digits   = 0,scientific=FALSE)
    #get the lowest 7 day avg admission of 2021
    hosplowestavg2021<-dataset%>%filter(year(hosp_date)==2021)%>%filter(hosp_avg==min(hosp_avg,na.rm=TRUE))%>%head(1)%>%pull(hosp_avg)
    #get the date of the lowest 7 day avg admission of 2021
    hosplowestavg2021date<-dataset%>%filter(year(hosp_date)==2021)%>%filter(hosp_avg==min(hosp_avg,na.rm=TRUE))%>%head(1)%>%pull(hosp_date)
    #format the lowest 7 day avg admission of 2021
    hosplowestavg2021_format<-hosplowestavg2021%>%format(justify = "right",big.mark  = ",",digits   = 0,scientific=FALSE)
    #get the lowest 7 day avg admission of 2022
    hosplowestavg2022<-dataset%>%filter(year(hosp_date)==2022)%>%filter(hosp_avg==min(hosp_avg,na.rm=TRUE))%>%head(1)%>%pull(hosp_avg)
    #get the date of the lowest 7 day avg admission of 2022
    hosplowestavg2022date<-dataset%>%filter(year(hosp_date)==2022)%>%filter(hosp_avg==min(hosp_avg,na.rm=TRUE))%>%head(1)%>%pull(hosp_date)
    #format the lowest 7 day avg admission of 2022
    hosplowestavg2022_format<-hosplowestavg2022%>%format(justify = "right",big.mark  = ",",digits   = 0,scientific=FALSE)
    #get the maximal daily admissions
    hospmaxnew<-max(dataset$hosp_new,na.rm=TRUE)
    #plot daily admission curve
    hospplot<-ggplot(dataset,aes(hosp_date)) +
        geom_col(aes(y=hosp_new, fill = " New Admissions"),color="#B4C7E7") +
        #add the black line of the lowest 7 day avg of 2021
        geom_col(aes(y={hosplowestavg2021}/700,x={hosplowestavg2021date}),color="black")+
        #add the black line of the lowest 7 day avg of 2022
        geom_col(aes(y={hosplowestavg2022}/700,x={hosplowestavg2022date}),color="black")+
        #add the red line of the latest 7 day avg
        geom_col(aes(y={latesthosp_average}/700,x={latesthosp_date}),color="red")+
        #add 7 day avg admission line
        geom_line(aes(y=hosp_avg, color = "7-Day Moving Average - New Admissions"))+
        #set color of 7 day avg admission line
        scale_color_manual("",values=c("7-Day Moving Average- New Admissions  "="#757575"))+
        #set color of daily admission column
        scale_fill_manual("",values=c("#B4C7E7"))+
        #set axis title
        labs(y     = "New Admissions",
             x     = "Date of Report" ) +
        #add annotation of the lowest 7 day avg of 2021
        annotate("text",x={hosplowestavg2021date},y={hospmaxnew}/1.5,label=glue("2021:\n LOWEST\n AVERAGE\n \n{hosplowestavg2021_format}"),size=3)+
        #add annotation of the lowest 7 day avg of 2022
        annotate("text",x={hosplowestavg2022date},y={hospmaxnew}/1.2,label=glue("2022:\n LOWEST\n AVERAGE\n \n \n{hosplowestavg2022_format}"),size=3)+
        #add annotation of the latest 7 day avg
        annotate("text",x={latesthosp_date},y={hospmaxnew}/2,label=glue("LATEST\n AVERAGE\n \n \n{latest_hospavg_format}"),size=3)+
        #set the maximal value of y axis
        scale_y_continuous(limits=c(0,max(dataset$hosp_new,na.rm=TRUE)*1.3),labels = label_number(big.mark = ","))+
        #set theme
        theme(axis.title=element_text(size=rel(1.1),face="bold"),
              axis.text=element_text(size = rel(1.1), face = "bold"),
              legend.key.size = unit(0.9, "lines"),
              legend.text = element_text(size = rel(1.1), face = "bold"),
              legend.position="top")
    return(hospplot)
}
