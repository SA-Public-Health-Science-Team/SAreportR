
#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
weekly_case_plot_exsum<-function(dataset){
    cases_new=dataset$cases_new
    #plot weekly cases
    caseplot<-ggplot(dataset,aes(cases_date ))+
        #add column weekly cases
        geom_col(aes(y=cases_new,fill=" New Weekly Cases"),color ="#573B92") +
        #add 21 day avg line
        geom_line(aes(y=cases_avg,group=1,color="21-Day Average Case    "),size=1.3)+
        #add dash line for each peak
        geom_segment(aes(x={casepeakline1$date},xend={casepeakline1$date},y=0,yend=max(cases_new)*1.3),linetype="dashed",color="grey")+
        geom_segment(aes(x={casepeakline2$date},xend={casepeakline2$date},y=0,yend=max(cases_new)*1.3),linetype="dashed",color="grey")+
        geom_segment(aes(x={casepeakline3$date},xend={casepeakline3$date},y=0,yend=max(cases_new)*1.3),linetype="dashed",color="grey")+
        geom_segment(aes(x={casepeakline4$date},xend={casepeakline4$date},y=0,yend=max(cases_new)*1.3),linetype="dashed",color="grey")+
        geom_segment(aes(x={casepeakline5$date},xend={casepeakline5$date},y=0,yend=max(cases_new)*1.3),linetype="dashed",color="grey")+
        geom_segment(aes(x={casepeakline6$date},xend={casepeakline6$date},y=0,yend=max(cases_new)*1.3),linetype="dashed",color="grey")+
        geom_segment(aes(x={casepeakline7$date},xend={casepeakline7$date},y=0,yend=max(cases_new)*1.3),linetype="dashed",color="grey")+
        #set color of 21 day avg line
        scale_color_manual(" ",values="#E1C233")+
        #set color of column
        scale_fill_manual(" ",values="#573B92")+
        #set maximal value of y axis and set the secondary y axis
        scale_y_continuous(limits=c(0,max(dataset$cases_new)*1.3),labels = label_number(big.mark = ","),sec.axis=sec_axis(~./7,name="21-Day Average of Daily Cases",labels = label_number(big.mark = ",")))+
        #set axis title
        labs(x="Reporting Week End Date",y="Weekly New Cases")+
        #add the date mark for each peak
        annotate("text",x={casepeakline1$date}+40,y=max(cases_new),label=glue("{casepeakline1$dateformat}"),size=3.5)+
        annotate("text",x={casepeakline2$date}+40,y=max(cases_new),label=glue("{casepeakline2$dateformat}"),size=3.5)+
        annotate("text",x={casepeakline3$date}+40,y=max(cases_new),label=glue("{casepeakline3$dateformat}"),size=3.5)+
        annotate("text",x={casepeakline4$date}+40,y=max(cases_new),label=glue("{casepeakline4$dateformat}"),size=3.5)+
        annotate("text",x={casepeakline5$date}+40,y=max(cases_new),label=glue("{casepeakline5$dateformat}"),size=3.5)+
        annotate("text",x={casepeakline6$date}+40,y=max(cases_new),label=glue("{casepeakline6$dateformat}"),size=3.5)+
        annotate("text",x={casepeakline7$date}+40,y=max(cases_new),label=glue("{casepeakline7$dateformat}"),size=3.5)+
        #set theme
        theme(axis.title = element_text(size = rel(1.1),face="bold"),
              axis.text = element_text(size = rel(1.1)),
              axis.line = element_line(color = "black"),
              # La légende set the format of the legend
              legend.text = element_text(size = rel(1.1), face = "bold"),
              legend.key.size = unit(0.9, "lines"),
              legend.position="top")
    return(caseplot)
}
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
#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
monthly_case_plot_exsum<-function(dataset){
    #plot monthly cases curve
    caseplot<-ggplot(dataset,aes(month ))+
        #add monthly cases column
        geom_col(aes(y=cases_cummonth,fill=" Monthly New Cases"),color ="#573B92") +
        #set color of column
        scale_fill_manual(" ",values="#573B92")+
        #set the maximal value of y axis and set the secondary y axis
        scale_y_continuous(limits=c(0,max(dataset$cases_cummonth)*1.3),labels = label_number(big.mark = ","))+
        #set axis title
        labs(x="Month of Report",y="Monthly New Cases")+
        #set theme
        theme(axis.title = element_text(size = rel(1.1),face="bold"),
              axis.text = element_text(size = rel(1.1)),
              axis.line = element_line(color = "black"),
              # La légende set the format of the legend
              legend.text = element_text(size = rel(1.1), face = "bold"),
              legend.key.size = unit(0.9, "lines"),
              legend.position="top")
    return(caseplot)
}
#' Title
#'
#' @param dataset
#' @param indicator
#' @param color
#'
#' @return
#' @export
#'
#' @examples
monthly_plot_exsum<-function(dataset,indicator,color){
    cummonth_col<-dataset |>
        select(contains("cummonth")) |>
        select(1) |>
        colnames()
    dataset <-  dataset |>
        rename(cummonth   := {{cummonth_col}})
    #plot monthly cases curve
    plot<-ggplot(dataset,aes(month))+
        #add monthly cases column
        geom_col(aes(y=cummonth,fill=glue(" Monthly New {indicator}")),color =color) +
        #set color of column
        scale_fill_manual(" ",values=color)+
        #set the maximal value of y axis and set the secondary y axis
        scale_y_continuous(limits=c(0,max(dataset$cummonth)*1.3),labels = label_number(big.mark = ","))+
        #set axis title
        labs(x="Month of Report",y=glue("Monthly New {indicator}"))+
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
#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
daily_hosp_plot_exsum<-function(dataset){
    #plot daily admission curve
    hospplot<-ggplot(dataset,aes(hosp_date ))+
        #add column of daily admissions
        geom_col(aes(y=hosp_new,fill=" Daily New Admissions"),color ="#F5793A") +
        #add 7 day avg admission line
        geom_line(aes(y=hosp_avg,group=1,color="7-Day Average Admission         "),size=1.3)+
        #set color of 7 day avg admission line
        scale_color_manual(" ",values=c("7-Day Average Admission         "="#12239E"))+
        #set color of column
        scale_fill_manual(" ",values="#F5793A")+
        #set maximal value of y axis and set the secondary y axis
        scale_y_continuous(limits=c(0,max(dataset$hosp_new)*1.3),labels = label_number(big.mark = ","),sec.axis=sec_axis(~.,name="7-Day Average Admission",labels = label_number(big.mark = ",")))+
        #set axis title
        labs(x="Date of Report",y="Daily New Admissions")+
        #set theme
        theme(axis.title = element_text(size = rel(1.1),face="bold"),
              axis.text = element_text(size = rel(1.1)),
              axis.line = element_line(color = "black"),
              # La légende set the format of the legend
              legend.text = element_text(size = rel(1.1), face = "bold"),
              legend.key.size = unit(0.9, "lines"),
              legend.position="top")
    return(hospplot)
}
#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
weekly_hosp_plot_exsum<-function(dataset){
    #plot weekly admission curve
    hospplot<-ggplot(dataset,aes(hosp_date ))+
        #add column of weekly admissions
        geom_col(aes(y=hosp_cum7,fill=" Weekly New Admissions"),color ="#F5793A") +
        #add line of 21 day avg admission
        geom_line(aes(y=hosp_avg21*7,group=1,color="21-Day Average Admission       "),size=1.3)+
        #set color of 21 day avg admission
        scale_color_manual(" ",values=c("21-Day Average Admission       "="#12239E"))+
        #set color of column
        scale_fill_manual(" ",values="#F5793A")+
        #set maximal value of y axis and set the secondary y axis
        scale_y_continuous(limits=c(0,max(dataset$hosp_cum7)*1.3),labels = label_number(big.mark = ","),sec.axis=sec_axis(~./7,name="21-Day Average Admission",labels = label_number(big.mark = ",")))+
        #set axis title
        labs(x="Week End Date",y="Weekly New Admissions")+
        #set theme
        theme(axis.title = element_text(size = rel(1.1),face="bold"),
              axis.text = element_text(size = rel(1.1)),
              axis.line = element_line(color = "black"),
              # La légende set the format of the legend
              legend.text = element_text(size = rel(1.1), face = "bold"),
              legend.key.size = unit(0.9, "lines"),
              legend.position="top")
    return(hospplot)
}
#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
monthly_hosp_plot_exsum<-function(dataset){
    #plot monthly admissions curve
    hospplot<-ggplot(dataset,aes(month))+
        #add column of monthly admissions
        geom_col(aes(y=hosp_cummonth,fill=" Monthly New Admissions"),color ="#F5793A") +
        #set color of column
        scale_fill_manual(" ",values="#F5793A")+
        #set the maximal value of y axis and set the secondary y axis
        scale_y_continuous(limits=c(0,max(dataset$hosp_cummonth)*1.3),labels = label_number(big.mark = ","))+
        #set axis title
        labs(x="Month of Report",y="Monthly New Admissions")+
        #set theme
        theme(axis.title = element_text(size = rel(1.1),face="bold"),
              axis.text = element_text(size = rel(1.1)),
              axis.line = element_line(color = "black"),
              # La légende set the format of the legend
              legend.text = element_text(size = rel(1.1), face = "bold"),
              legend.key.size = unit(0.9, "lines"),
              legend.position="top")
    return(hospplot)
}

#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
weekly_death_plot_exsum<-function(dataset){
    deaths_new=dataset$deaths_new
    #plot weekly deaths curve
    deathplot<-ggplot(dataset,aes(deaths_date ))+
        #add the column of weekly deaths
        geom_col(aes(y=deaths_new,fill=" Weekly New Deaths"),color ="#08519C") +
        #add dash line for each peak
        geom_segment(aes(x={deathpeakline1$date},xend={deathpeakline1$date},y=0,yend=max(deaths_new)*1.3),linetype="dashed",color="grey")+
        geom_segment(aes(x={deathpeakline2$date},xend={deathpeakline2$date},y=0,yend=max(deaths_new)*1.3),linetype="dashed",color="grey")+
        geom_segment(aes(x={deathpeakline3$date},xend={deathpeakline3$date},y=0,yend=max(deaths_new)*1.3),linetype="dashed",color="grey")+
        geom_segment(aes(x={deathpeakline4$date},xend={deathpeakline4$date},y=0,yend=max(deaths_new)*1.3),linetype="dashed",color="grey")+
        geom_segment(aes(x={deathpeakline5$date},xend={deathpeakline5$date},y=0,yend=max(deaths_new)*1.3),linetype="dashed",color="grey")+
        geom_segment(aes(x={deathpeakline6$date},xend={deathpeakline6$date},y=0,yend=max(deaths_new)*1.3),linetype="dashed",color="grey")+
        #add the line of 21 day avg death
        geom_line(aes(y=deaths_avg,group=1,color="21-Day Average Death    "),size=1.3)+
        #set the color of the line of 21 day avg death
        scale_color_manual(" ",values=c("21-Day Average Death    "="#E66C37"))+
        #set the color of column
        scale_fill_manual(" ",values="#08519C")+
        #set the maximal value of y axis and set secondary y axis
        scale_y_continuous(limits=c(0,max(dataset$deaths_new)*1.3),labels = label_number(big.mark = ","),sec.axis=sec_axis(~./7,name="21-Day Average Death",labels = label_number(big.mark = ",")))+
        #set axis title
        labs(x="Week End Date",y="Weekly New Deaths")+
        #add the date mark for each peak
        annotate("text",x={deathpeakline1$date}+40,y=max(deaths_new),label=glue("{deathpeakline1$dateformat}"),size=3.5)+
        annotate("text",x={deathpeakline2$date}+40,y=max(deaths_new),label=glue("{deathpeakline2$dateformat}"),size=3.5)+
        annotate("text",x={deathpeakline3$date}+40,y=max(deaths_new),label=glue("{deathpeakline3$dateformat}"),size=3.5)+
        annotate("text",x={deathpeakline4$date}+40,y=max(deaths_new),label=glue("{deathpeakline4$dateformat}"),size=3.5)+
        annotate("text",x={deathpeakline5$date}+40,y=max(deaths_new),label=glue("{deathpeakline5$dateformat}"),size=3.5)+
        annotate("text",x={deathpeakline6$date}+40,y=max(deaths_new),label=glue("{deathpeakline6$dateformat}"),size=3.5)+
        #set theme
        theme(axis.title = element_text(size = rel(1.1),face="bold"),
              axis.text = element_text(size = rel(1.1)),
              axis.line = element_line(color = "black"),
              # La légende set the format of the legend
              legend.text = element_text(size = rel(1.1), face = "bold"),
              legend.key.size = unit(0.9, "lines"),
              legend.position="top")
    return(deathplot)
}
#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
monthly_death_plot_exsum<-function(dataset){
    #plot monthly deaths curve
    deathplot<-ggplot(dataset,aes(month ))+
        #add the column of monthly deaths
        geom_col(aes(y=deaths_cummonth,fill=" Monthly New Deaths"),color ="#08519C") +
        #set the color of the column
        scale_fill_manual(" ",values="#08519C")+
        #set the maximal value of y axis and set secondary y axis
        scale_y_continuous(limits=c(0,max(dataset$deaths_cummonth)*1.3),labels = label_number(big.mark = ","))+
        #set the axis title
        labs(x="Month of Report",y="Monthly New Deaths")+
        #set theme
        theme(axis.title = element_text(size = rel(1.1),face="bold"),
              axis.text = element_text(size = rel(1.1)),
              axis.line = element_line(color = "black"),
              # La légende set the format of the legend
              legend.text = element_text(size = rel(1.1), face = "bold"),
              legend.key.size = unit(0.9, "lines"),
              legend.position="top")
    return(deathplot)
}
#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
daily_lab_plot_exsum<-function(dataset){
    #get the maximal daily test volume
    dailytestvolumemax<-max(dataset$new_test_vol,na.rm=TRUE)
    #get the maximal daily percent positivity
    dailytestposmax<-max(dataset$testpos_average,na.rm=TRUE)
    #get the ratio of the maximal daily test volume over the maximal daily percent positivity
    dailyyaxisratio<-dailytestvolumemax/dailytestposmax
    #plot daily test volume and percent positivity curve
    labplot<-ggplot(dataset,aes(testpos_date )) +
        #add column of daily test volume
        geom_col(aes(y=new_test_vol,fill = " Daily Test Volume"),color="#E1BE6A") +
        #add 7 day avg test volume line
        geom_line(aes(y=testvol_avg, x=testpos_date,color = "7-Day Avg Daily Test Volume    ", group=1),size=1.3)+
        #add 7 day avg percent positivity line
        geom_line(aes(y=testpos_average*dailyyaxisratio, color = "7-Day Avg. Percent Positivity   ",group=1),size=1)+
        #set the color of column
        scale_fill_manual("",values="#E1BE6A")+
        #set the color of 7 day avg test volume line and 7 day avg percent positivity line
        scale_color_manual(" ",values=c("7-Day Avg Daily Test Volume    "="#666666","7-Day Avg. Percent Positivity   "="#40B0A6"))+
        #set the maximal value of y axis and set the secondary y axis
        scale_y_continuous(limits=c(0,max(dataset$new_test_vol)*1.3),labels = label_number(big.mark = ","),sec.axis=sec_axis(~./dailyyaxisratio,name="7-Day Avg. Percent Positivity",labels = label_percent(decimal.mark=".",accuracy = 1)))+
        #set axis title
        labs(y = "Daily Test Volume",x = "Date of Report",color="Legend")+
        #set theme
        theme(axis.title = element_text(size = rel(1.1),face="bold"),
              axis.text = element_text(size = rel(1.1)),
              axis.line = element_line(color = "black"),
              # La légende set the format of the legend
              legend.text = element_text(size = rel(1.1), face = "bold"),
              legend.key.size = unit(0.9, "lines"),
              legend.position="top")
    return(labplot)
}
#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
weekly_lab_plot_exsum<-function(dataset){
    #get the maximal of weekly test volume
    weeklytestvolumemax<-max(dataset$test_cum7,na.rm=TRUE)
    #get the maximal of weekly percent positivity
    weeklytestposmax<-max(dataset$testpos_avg21,na.rm=TRUE)
    #get the ratio of the maximal of weekly test volume over the maximal of weekly percent positivity
    weeklyyaxisratio<-weeklytestvolumemax/weeklytestposmax
    #plot weekly test volume and percent positivity
    labplot<-ggplot(dataset,aes(testpos_date ))+
        #add column of weekly test volume
        geom_col(aes(y=test_cum7,fill = " Weekly Test Volume"),color="#E1BE6A") +
        #add 21 day avg test volume line
        geom_line(aes(y=testvol_avg21*7, x=testpos_date,color = "21-Day Avg Test Volume   ", group=1),size=1.3)+
        #add 21 day avg percent positivity line
        geom_line(aes(y=testpos_avg21*weeklyyaxisratio, color = "21-Day Avg. Percent Positivity   ",group=1),size=1)+
        #set color of column
        scale_fill_manual("",values="#E1BE6A")+
        #set the color of 21 day avg test volume line and 21 day avg percent positivity line
        scale_color_manual(" ",values=c("21-Day Avg Test Volume   "="#666666","21-Day Avg. Percent Positivity   "="#40B0A6"))+
        #set maximal value of y axis and set the secondary y axis
        scale_y_continuous(limits=c(0,max(dataset$test_cum7)*1.3),labels = label_number(big.mark = ","),sec.axis=sec_axis(~./weeklyyaxisratio,name="21-Day Avg. Percent Positivity",labels = label_percent(decimal.mark=".",accuracy = 1)))+
        #set axis title
        labs(y = "Weekly Test Volume",x = "Week End Date",color="Legend")+
        #set theme
        theme(axis.title = element_text(size = rel(1.1),face="bold"),
              axis.text = element_text(size = rel(1.1)),
              axis.line = element_line(color = "black"),
              # La légende set the format of the legend
              legend.text = element_text(size = rel(1.1), face = "bold"),
              legend.key.size = unit(0.9, "lines"),
              legend.position="top")
    return(labplot)
}
#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
monthly_lab_plot_exsum<-function(dataset){
    #get the maximal monthly test volume
    monthlytestvolumemax<-max(dataset$lab_cummonth,na.rm=TRUE)
    #get the maximal percent positivity
    monthlytestposmax<-max(dataset$testpos_avg30,na.rm=TRUE)
    #get the ratio of the maximal monthly test volume over the maximal percent positivity
    monthlyyaxisratio<-monthlytestvolumemax/monthlytestposmax
    #plot monthly lab curve
    labplot<-ggplot(dataset,aes(month))+
        #add monthly test volume column
        geom_col(aes(y=lab_cummonth,fill = " Monthly Test Volume"),color="#E1BE6A") +
        #add 30 day avg percent positivity line
        geom_line(aes(y=testpos_avg30*monthlyyaxisratio, color = "30-Day Avg. Percent Positivity   ",group=1),size=1)+
        #set the color of column
        scale_fill_manual("",values="#E1BE6A")+
        #set the color of 30 day avg percent positivity line
        scale_color_manual(" ",values=c("30-Day Avg. Percent Positivity   "="#40B0A6"))+
        #set the maximal value of y axis and set the secondary y axis
        scale_y_continuous(limits=c(0,max(dataset$lab_cummonth)*1.3),labels = label_number(big.mark = ","),sec.axis=sec_axis(~./monthlyyaxisratio,name="30-Day Avg. Percent Positivity",labels = label_percent(decimal.mark=".",accuracy = 1)))+
        #set axis title
        labs(y = "Monthly Test Volume",x = "Month of Report",color="Legend")+
        #set theme
        theme(axis.title = element_text(size = rel(1.1),face="bold"),
              axis.text = element_text(size = rel(1.1)),
              axis.line = element_line(color = "black"),
              # La légende set the format of the legend
              legend.text = element_text(size = rel(1.1), face = "bold"),
              legend.key.size = unit(0.9, "lines"),
              legend.position="top")
    return(labplot)
}


#' Title
#'
#' @param dataset
#' @param indicator
#'
#' @return
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
        geom_col(aes(y={latest_average}/17,x={latest_date}),color="red")+
        #add the 21 day avg line
        geom_line(aes(y=avg*7,x=date,group=1,color=glue("7-Day Average {indicator}    ")),size=1.3)+
        #set the color of 21 day avg line
        scale_color_manual(" ",values=c("#757575","black"))+
        #set the color of weekly cases column
        scale_fill_manual(" ",values="#B4C7E7")+
        #set the axis title
        labs(y     = glue("Weekly New {indicator}"),
             x     = "Date of Report"  ) +
        #add annotation of the lowest 21 day avg of 2021
        annotate("text",x={lowestavg2021date},y={lowestavg2021}+{maxcum}/2,label=glue("2021:\n LOWEST\n AVERAGE\n \n{lowestavg2021_format}"),size=2)+
        #add annotation of the lowest 21 day avg of 2021
        annotate("text",x={lowestavg2022date},y={lowestavg2022}+{maxcum}/1,label=glue("2022:\n LOWEST\n AVERAGE\n \n \n{lowestavg2022_format}"),size=1.8)+
        #add annotation of the latest 21 day avg
        annotate("text",x={latest_date},y={latest_average}+{maxcum}/2,label=glue("LATEST\n AVERAGE\n \n{latest_avg_format}"),size=2)+
        #set the maximal of y axis
        scale_y_continuous(limits=c(0,max(dataset$new,na.rm=TRUE)*1.3),labels=label_number(big.mark =","))+
        #set the theme
        theme(axis.title=element_text(size=rel(0.6),face="bold"),
              axis.text=element_text(size = rel(0.6), face = "bold"),
              legend.key.size = unit(0.5, "lines"),
              legend.text = element_text(size = rel(0.60), face = "bold"),
              legend.position="top")

    return(plot)
}


#' Title
#'
#' @param dataset
#' @param indicator
#'
#' @return
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
        annotate("text",x={lowestnew2021date},y={lowestnew2021min}+{maxcum}/2,label=glue("2021:\n LOWEST\n {indicator}\n \n{lowestnew2021_format}"),size=2)+
        #add the annotation of the lowest monthly cases of 2022
        annotate("text",x={lowestnew2022date},y={lowestnew2022min}+{maxcum}/1.2,label=glue("2022:\n LOWEST\n {indicator}\n \n \n{lowestnew2022_format}"),size=2)+
        #add the annotation of the latest monthly cases
        annotate("text",x={latestnew_date},y={latest_new}+{maxcum}/2,label=glue("LATEST\n {indicator}\n \n{latest_new_format}"),size=2)+
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
#' Title
#'
#' @param dataset
#'
#' @return
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
        annotate("text",x={hosplowestavg2021date},y={hosplowestavg2021}+{hospmaxnew}/1.5,label=glue("2021:\n LOWEST\n AVERAGE\n \n{hosplowestavg2021_format}"),size=2)+
        #add annotation of the lowest 7 day avg of 2022
        annotate("text",x={hosplowestavg2022date},y={hosplowestavg2022}+{hospmaxnew}/1.2,label=glue("2022:\n LOWEST\n AVERAGE\n \n \n{hosplowestavg2022_format}"),size=2)+
        #add annotation of the latest 7 day avg
        annotate("text",x={latesthosp_date},y={latesthosp_average}+{hospmaxnew}/2,label=glue("LATEST\n AVERAGE\n \n \n{latest_hospavg_format}"),size=2)+
        #set the maximal value of y axis
        scale_y_continuous(limits=c(0,max(dataset$hosp_new,na.rm=TRUE)*1.3),labels = label_number(big.mark = ","))+
        #set theme
        theme(axis.title=element_text(size=rel(0.6),face="bold"),
              axis.text=element_text(size = rel(0.6), face = "bold"),
              legend.key.size = unit(0.5, "lines"),
              legend.text = element_text(size = rel(0.60), face = "bold"),
              legend.position="top")
    return(hospplot)
}
#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
weekly_hosp_plot_wh<-function(dataset){
    #get the latest date
    latesthosp_date<-max(dataset$hosp_date,na.rm=TRUE)
    #get the 21 day avg admission on the latest date
    latesthosp_average<-dataset%>%filter(hosp_date==max(hosp_date,na.rm=TRUE))%>%head(1)%>%pull(hosp_avg21)
    #format the 21 day avg admission on the latest date
    latest_hospavg_format<-latesthosp_average%>%format(justify = "right",big.mark  = ",",digits   = 0,scientific=FALSE)
    #get the lowest 21 day avg admission of 2021
    hosplowestavg2021<-dataset%>%filter(year(hosp_date)==2021)%>%filter(hosp_avg21==min(hosp_avg21,na.rm=TRUE))%>%head(1)%>%pull(hosp_avg21)
    #get the date of the lowest 21 day avg admission of 2021
    hosplowestavg2021date<-dataset%>%filter(year(hosp_date)==2021)%>%filter(hosp_avg21==min(hosp_avg21,na.rm=TRUE))%>%head(1)%>%pull(hosp_date)
    #format the lowest 21 day avg admission of 2021
    hosplowestavg2021_format<-hosplowestavg2021%>%format(justify = "right",big.mark  = ",",digits   = 0,scientific=FALSE)
    #get the lowest 21 day avg admission of 2022
    hosplowestavg2022<-dataset%>%filter(year(hosp_date)==2022)%>%filter(hosp_avg21==min(hosp_avg21,na.rm=TRUE))%>%head(1)%>%pull(hosp_avg21)
    #get the date of the lowest 21 day avg admission of 2022
    hosplowestavg2022date<-dataset%>%filter(year(hosp_date)==2022)%>%filter(hosp_avg21==min(hosp_avg21,na.rm=TRUE))%>%head(1)%>%pull(hosp_date)
    #format the lowest 21 day avg admission of 2022
    hosplowestavg2022_format<-hosplowestavg2022%>%format(justify = "right",big.mark  = ",",digits   = 0,scientific=FALSE)
    #get the maximal weekly admissions
    hospmaxcum<-max(dataset$hosp_cum7,na.rm=TRUE)
    #plot weekly admissions
    hospplot<-ggplot(dataset,aes(hosp_date))+
        geom_col(aes(y=hosp_cum7,fill=" Weekly New Admissions"),color ="#B4C7E7")+
        #add the black line of the lowest 21 day avg admission of 2021
        geom_col(aes(y={hosplowestavg2021}/21,x={hosplowestavg2021date}),color="black")+
        #add the black line of the lowest 21 day avg admission of 2022
        geom_col(aes(y={hosplowestavg2022}/21,x={hosplowestavg2022date}),color="black")+
        #add the red line of the latest 21 day avg admission
        geom_col(aes(y={latesthosp_average}/15,x={latesthosp_date}),color="red")+
        #add the 21 day avg line
        geom_line(aes(y=hosp_avg21*7,group=1,color="21-Day Average Admission    "),size=1.3)+
        #set color of 21 day avg line
        scale_color_manual(" ",values=c("21-Day Average Admission    "="#757575"))+
        #set color of weekly admissions column
        scale_fill_manual(" ",values="#B4C7E7")+
        #set the maximal value of y axis
        scale_y_continuous(limits=c(0,max(dataset$hosp_cum7,na.rm=TRUE)*1.3),labels = label_number(big.mark = ","))+
        labs(y     = "Weekly New Admissions",
             x     = "Date of Report" ) +
        #add the annotation of the lowest 21 day avg of 2021
        annotate("text",x={hosplowestavg2021date},y={hosplowestavg2021}+{hospmaxcum}/1.5,label=glue("2021:\n LOWEST\n AVERAGE\n \n{hosplowestavg2021_format}"),size=2)+
        #add the annotation of the lowest 21 day avg of 2021
        annotate("text",x={hosplowestavg2022date},y={hosplowestavg2022}+{hospmaxcum}/1.1,label=glue("2022:\n LOWEST\n AVERAGE\n \n \n{hosplowestavg2022_format}"),size=2)+
        #add the annotation of the latest 21 day avg
        annotate("text",x={latesthosp_date},y={latesthosp_average}+{hospmaxcum}/1.5,label=glue("LATEST\n AVERAGE\n \n \n{latest_hospavg_format}"),size=2)+
        #set theme
        theme(axis.title=element_text(size=rel(0.6),face="bold"),
              axis.text=element_text(size = rel(0.6), face = "bold"),
              legend.key.size = unit(0.5, "lines"),
              legend.text = element_text(size = rel(0.60), face = "bold"),
              legend.position="top")

    return(hospplot)
}
#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
monthly_hosp_plot_wh<-function(dataset){
    #get the latest month
    latestnew_date<-max(dataset$month,na.rm=TRUE)
    #get the monthly admission on the latest month
    latesthosp_new<-dataset%>%tail(1)%>%pull(hosp_cummonth)
    #format the monthly admission on the latest month
    latest_hospnew_format<-latesthosp_new%>%format(justify="right",big.mark= ",",digits=0,scientific=FALSE)
    #get the lowest monthly admissions of 2021
    lowestnew2021min<-dataset%>%filter(year(month)==2021)%>%filter(hosp_cummonth==min(hosp_cummonth,na.rm=TRUE))%>%head(1)%>%pull(hosp_cummonth)
    #get the month of the lowest monthly admissions of 2021
    lowestnew2021date<-dataset%>%filter(year(month)==2021)%>%filter(hosp_cummonth==lowestnew2021min)%>%head(1)%>%pull(month)
    #format the lowest monthly admissions of 2021
    lowestnew2021_format<-lowestnew2021min%>%format(justify = "right",big.mark  = ",",digits   = 0,scientific=FALSE)
    #get the lowest monthly admissions of 2021
    lowestnew2022min<-dataset%>%filter(year(month)==2022)%>%filter(hosp_cummonth==min(hosp_cummonth,na.rm=TRUE))%>%head(1)%>%pull(hosp_cummonth)
    #get the month of the lowest monthly admissions of 2022
    lowestnew2022date<-dataset%>%filter(year(month)==2022)%>%filter(hosp_cummonth==lowestnew2022min)%>%head(1)%>%pull(month)
    #format the lowest monthly admissions of 2022
    lowestnew2022_format<-lowestnew2022min%>%format(justify = "right",big.mark  = ",",digits   = 0,scientific=FALSE)
    #get the maximal monthly admissions
    maxcum<-max(dataset$hosp_cummonth,na.rm=TRUE)
    #plot the monthly admission
    hospplot<-ggplot(dataset)+
        geom_col(aes(x=month,y=hosp_cummonth),fill="#B4C7E7")+
        #add the black line of the lowest monthly admission of 2021
        geom_col(aes(y={lowestnew2021min}/28,x={lowestnew2021date}),color="black")+
        #add the black line of the lowest monthly admission of 2022
        geom_col(aes(y={lowestnew2022min}/28,x={lowestnew2022date}),color="black")+
        #add the red line of the latest monthly admission
        geom_col(aes(y={latesthosp_new}/28,x={latestnew_date}),color="red")+
        #set axis title
        labs(y     = "Monthly New Admissions",
             x     = "Month of Report" ) +
        #set color of monthly admission column
        scale_fill_manual("",values=c("#B4C7E7"))+
        #add annotation of the lowest monthly admission of 2021
        annotate("text",x={lowestnew2021date},y={lowestnew2021min}+{maxcum}/1.3,label=glue("2021:\n LOWEST\n ADMISSIONS\n \n{lowestnew2021_format}"),size=2)+
        #add annotation of the lowest monthly admission of 2022
        annotate("text",x={lowestnew2022date},y={lowestnew2022min}+{maxcum}/1.2,label=glue("2022:\n LOWEST\n ADMISSIONS\n \n \n{lowestnew2022_format}"),size=2)+
        #add annotation of the latest monthly admission
        annotate("text",x={latestnew_date},y={latesthosp_new}+{maxcum}/2,label=glue("LATEST\n ADMISSIONS\n \n{latest_hospnew_format}"),size=2)+
        #set the mximal value of y axis
        scale_y_continuous(limits=c(0,max(dataset$hosp_cummonth,na.rm=TRUE)*1.3),labels=label_number(big.mark =","))+
        #set theme
        theme(axis.title=element_text(size=rel(0.6),face="bold"),
              axis.text=element_text(size = rel(0.6), face = "bold"),
              legend.key.size = unit(0.5, "lines"),
              legend.text = element_text(size = rel(0.60), face = "bold"),
              legend.position="top")

    return(hospplot)
}
