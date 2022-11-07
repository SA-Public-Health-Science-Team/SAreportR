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
daily_hospital_numbers_exsum<-function(dataset){
    #get hospital dates
    hosp_date_raw<-dataset%>%pull(hosp_date)
    hosp_date<-format(hosp_date_raw,format="%b %d, %Y")
    hosp_date_6<-format(hosp_date_raw-6, format="%b %d, %Y")
    hosp_date_7<-format(hosp_date_raw-7, format="%b %d, %Y")
    hosp_date_13<-format(hosp_date_raw-13, format="%b %d, %Y")
    #create hospital numbers dataframe
    hosp_numbers_list<-data.frame(
        totalnumber=dataset$hosp_cum,
        totalname="Total New Admissions",
        daterange=glue("Jan 23, 2020 - {hosp_date}"),
        newnumber=dataset$hosp_new,
        newname="New Admissions",
        date={hosp_date},
        averagenumber=dataset$hosp_avg,
        averagename="Current 7-Day Average",
        daterange3=glue("{hosp_date_6} - {hosp_date}"),
        prioraveragenumber=dataset$hosp_prior_avg7,
        prioraveragename="Prior 7-Day Average",
        daterange4=glue("{hosp_date_13} - {hosp_date_7}"),
        change=dataset$hosp_change,
        changename="Change in 7-Day Average")%>%
        pivot_longer( cols = everything(),
                      names_to = "names",
                      values_to = "val",
                      values_transform = list(val = as.character))%>%select(val)%>%flextable()

    return(hosp_numbers_list)
}
#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
weekly_hospital_numbers_exsum<-function(dataset){
    #get hospital dates
    hosp_date_raw<-dataset%>%pull(hosp_date)
    hosp_date<-format(hosp_date_raw,format="%b %d, %Y")
    hosp_date_6<-format(hosp_date_raw-6, format="%b %d, %Y")
    hosp_date_20<-format(hosp_date_raw-20, format="%b %d, %Y")
    hosp_date_21<-format(hosp_date_raw-21, format="%b %d, %Y")
    hosp_date_41<-format(hosp_date_raw-41, format="%b %d, %Y")
    #create hospital numbers dataframe
    hosp_numbers_list<-data.frame(
            totalnumber=dataset$hosp_cum,
            totalname="Total New Admissions",
            daterange=glue("Aug 01, 2020 - {hosp_date}"),
            newnumber=dataset$hosp_cum7,
            newname="Weekly New Admissions Reported",
            daterange2=glue("{hosp_date_6} - {hosp_date}"),
            averagenumber=dataset$hosp_avg21,
            averagename="Current 21-Day Average",
            daterange3=glue("{hosp_date_20} - {hosp_date}"),
            prioraveragenumber=dataset$hosp_prior_avg21,
            prioraveragename="Prior 21-Day Average",
            daterange4=glue("{hosp_date_41} - {hosp_date_21}"),
            change=dataset$hosp_change21,
            changename="Change in 21-Day Average")%>%
        pivot_longer( cols = everything(),
                      names_to = "names",
                      values_to = "val",
                      values_transform = list(val = as.character))%>%select(val)%>%flextable()

    return(hosp_numbers_list)
}

#' Title
#'
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
daily_hospital_numbers_wh<-function(dataset){
    hospend_date_raw<-dataset%>%pull(hosp_date)
    hospend_date<-format(hospend_date_raw,format="%m/%d/%y")
    hospend_date_6<- format(hospend_date_raw-6, format="%m/%d/%y")
    hospend_date_7<- format(hospend_date_raw-7, format="%m/%d/%y")
    hospend_date_13<- format(hospend_date_raw-13, format="%m/%d/%y")
    #get the maximal admissions
    hosp_newpeak<-max(hosp_epicurve$hosp_new,na.rm=TRUE)
    #get the date of the maximal admissions
    hosp_newpeakdate<-hosp_epicurve%>%filter(hosp_new==hosp_newpeak)%>%head(1)%>%pull(hosp_date)
    #format the maximal admissions
    hosp_newpeakformat<-hosp_newpeak%>%format(justify="right",big.mark=",")
    #format the date of the maximal admissions
    hosp_newpeakdate<-format(hosp_newpeakdate,"%m/%d/%y")
    hosp_numbers_list<-data.frame(
        patientsname=glue("Patients Currently Hospitalized with COVID on {hospend_date}"),
        patientnumber=dataset$total_patients,
        newname=glue("New Admissions on {hospend_date}"),
        newnumber=dataset$hosp_new,
        peakname=glue("Peak in New Admissions({hosp_newpeakdate})"),
        admissionpeak=hosp_newpeakformat,
        changename="Change in 7-Day Average of New Admissioins",
        change=dataset$hosp_change,
        averagename=glue("Current 7-Day Average of New Admissions ({hospend_date_6} - {hospend_date})"),
        averagenumber=dataset$hosp_avg,
        prioraveragename=glue("Prior 7-Day Average of New Admissions ({hospend_date_13} - {hospend_date_7})"),
        prioraveragenumber=dataset$hosp_prior_avg7
    )%>%
        pivot_longer( cols = everything(),
                      names_to = "names",
                      values_to = "val",
                      values_transform = list(val = as.character))%>%select(val)%>%flextable()
    return(hosp_numbers_list)
}
weekly_hospital_numbers_wh<-function(dataset){
    hospend_date_raw<-dataset%>%pull(hosp_date)
    hospend_date<-format(hospend_date_raw,format="%m/%d/%y")
    hospend_date_20<- format(hospend_date_raw-20, format="%m/%d/%y")
    hospend_date_21<- format(hospend_date_raw-21, format="%m/%d/%y")
    hospend_date_41<- format(hospend_date_raw-41, format="%m/%d/%y")
    #get the maximal admissions
    hosp_newpeak<-max(hosp_epicurve$hosp_new,na.rm=TRUE)
    #get the date of the maximal admissions
    hosp_newpeakdate<-hosp_epicurve%>%filter(hosp_new==hosp_newpeak)%>%head(1)%>%pull(hosp_date)
    #format the maximal admissions
    hosp_newpeakformat<-hosp_newpeak%>%format(justify="right",big.mark=",")
    #format the date of the maximal admissions
    hosp_newpeakdate<-format(hosp_newpeakdate,"%m/%d/%y")
    hosp_numbers_list<-data.frame(
        patientsname=glue("Patients Currently Hospitalized with COVID on {hospend_date}"),
        patientnumber=dataset$total_patients,
        newname=glue("New Admissions on {hospend_date}"),
        newnumber=dataset$hosp_new,
        peakname=glue("Peak in New Admissions({hosp_newpeakdate})"),
        admissionpeak=hosp_newpeakformat,
        changename="Change in 21-Day Average of New Admissioins",
        change=dataset$hosp_change21,
        averagename=glue("Current 21-Day Average of New Admissions ({hospend_date_20} - {hospend_date})"),
        averagenumber=dataset$hosp_avg21,
        prioraveragename=glue("Prior 21-Day Average of New Admissions ({hospend_date_41} - {hospend_date_21})"),
        prioraveragenumber=dataset$hosp_prior_avg21
    )%>%
        pivot_longer( cols = everything(),
                      names_to = "names",
                      values_to = "val",
                      values_transform = list(val = as.character))%>%select(val)%>%flextable()

    return(hosp_numbers_list)
}
