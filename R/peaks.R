
#' Title create peak table for case or death data
#'
#' @param dataset case or death data
#' @param startdate peak start date could be "2020-03-19"
#' @param enddate peak end date could be "2020-06-19"
#'
#' @return one row of case or death peak data frame with formatted new, average numbers and date
#' @export
#'
#' @examples
peak_format<-function(dataset,startdate,enddate){
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
    #get current case data
    current_average<-dataset%>%
        filter(date==max(date,na.rm=TRUE))%>%head(1)%>%pull(avg)
    #filter the start date and the end date of date range
    peak<-dataset%>%
        filter(date>=as.Date(startdate,format("%Y-%m-%d")))%>%
        filter(date<=as.Date(enddate,format("%Y-%m-%d")))
    #get the maximal weekly cases
    peakcases=max(peak$new,na.rm=TRUE)
    #get the date of the maximal weekly cases
    peakdate<-peak%>%filter(new==peakcases)%>%
        mutate(date=min(date,na.rm=TRUE))%>%head(1)%>%pull(date)
    #get the maximal 21 day avg case
    peak_average=max(peak$avg,na.rm=TRUE)
    #get the date of the maximal 21 day avg case
    peakavgdate<-peak%>%filter(avg==peak_average)%>%
        mutate(date=min(date,na.rm=TRUE))%>%head(1)%>%pull(date)
    #get the percent change of the maximal 21 day avg and the current 21 day avg
    peakchange=(current_average-peak_average)/peak_average
    #format numbers
    Cases=label_number(big.mark=",",accuracy=1)(peakcases)
    Week=format(peakdate,format="%b %d, %Y")
    Case=label_number(big.mark=",",accuracy=1)(peak_average)
    Date=format(peakavgdate,format="%b %d, %Y")
    Change=label_percent(decimal.mark = ".",accuracy = .1)(peakchange)
    return(data.frame("New Cases"=Cases,"Reporting Week End"=Week,"1"="1","Average"=Case,"Reporting Week End"=Date,"% Change From Current"=Change))
}
#' Title create peak numbers for peak mark in the epicurve of case/death data
#'
#' @param dataset case or death data
#' @param startdate peak start date could be "2020-03-19"
#' @param enddate peak end date could be "2020-06-19"
#'
#' @return a data frame of peak number and formatted date
#' @export
#'
#' @examples
peak_number<-function(dataset,startdate,enddate){
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

    #filter the start date and the end date of date range
    peak<-dataset%>%
        filter(date>=as.Date(startdate,format("%Y-%m-%d")))%>%
        filter(date<=as.Date(enddate,format("%Y-%m-%d")))
    #get the maximal weekly cases
    peakcases=max(peak$new,na.rm=TRUE)
    #get the date of the maximal weekly cases
    peakdate<-peak%>%filter(new==peakcases)%>%
        mutate(date=min(date,na.rm=TRUE))%>%head(1)%>%pull(date)

    Date=format(peakdate,format="%b %d, %Y")

    return(data.frame("date"=peakdate,"dateformat"=Date))
}

#' Title create peak table for hospital data
#'
#' @param dataset hospital data
#' @param startdate peak start date could be "2021-01-01"
#' @param enddate peak end date could be "2021-06-30"
#'
#' @return one row of hospital peak data frame with formatted new, average numbers and date
#' @export
#'
#' @examples
hosp_peak<-function(dataset,startdate,enddate){
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

        #filter the start date and the end date of date range
        peak<-dataset%>%
            filter(date>=as.Date(startdate,format("%Y-%m-%d")))%>%
            filter(date<=as.Date(enddate,format("%Y-%m-%d")))
        #get the maximal daily admissions
        peakadmissions=max(peak$new,na.rm=TRUE)
        #get the date of the maximal daily admissions
        peakdate<-peak%>%filter(new==peakadmissions)%>%
            mutate(hosp_date=min(date,na.rm=TRUE))%>%head(1)%>%pull(date)
        #get the maximal 7 day avg admission
        peak_average=max(peak$avg,na.rm=TRUE)
        #get the date of the maximal 7 day avg admission
        peakavgdate<-peak%>%filter(avg==peak_average)%>%
            mutate(date=min(date,na.rm=TRUE))%>%head(1)%>%pull(date)

        #format numbers
        Admissions=label_number(big.mark=",",accuracy=1)(peakadmissions)
        Dates=format(peakdate,format="%d-%b-%y")
        Admission=label_number(big.mark=",",accuracy=1)(peak_average)
        Date=format(peakavgdate,format="%d-%b-%y")

        return(data.frame("Admissions"=Admissions,"Dates"=Dates,"1"="1","Admission"=Admission,"Date"=Date))
}



#' Title create peak table for lab data
#'
#' @param dataset lab data
#' @param startdate peak start date could be "2020-06-01"
#' @param enddate peak end date could be "2020-09-01"
#'
#' @return one row of lab peak data frame with formatted new, average numbers and date
#' @export
#'
#' @examples
lab_peak<-function(dataset,startdate,enddate){
    date_col<-dataset |>
        select(contains("testpos_date")) |>
        select(1) |>
        colnames()

    new_pos_col<-dataset |>
        select(contains("new_pos")) |>
        select(1) |>
        colnames()
    new_test_col<-dataset |>
        select(contains("new_test")) |>
        select(1) |>
        colnames()
    avg_col<-dataset |>
        select(contains("avg")) |>
        select(1) |>
        colnames()

    average_col<-dataset |>
        select(contains("average")) |>
        select(1) |>
        colnames()

    dataset <-  dataset |>
        rename(date  := {{date_col}},
               new_pos   := {{new_pos_col}},
               new_test   := {{new_test_col}},
               avg   := {{avg_col}},
               average   := {{average_col}}

        )
    #get current lab data
    lab_current_average<-dataset%>%
        filter(date==max(date,na.rm=TRUE))
    #filter the date
    peak<-dataset%>%
        filter(date>=as.Date(startdate,format("%Y-%m-%d")))%>%
        filter(date<=as.Date(enddate,format("%Y-%m-%d")))
    #get the maximal daily percent positivity
    peakpos=max(peak$new_pos,na.rm=TRUE)
    #get the date of the maximal daily percent positivity
    peakposdate<-peak%>%filter(new_pos==peakpos)%>%
        mutate(date=min(date,na.rm=TRUE))%>%head(1)%>%pull(date)
    #get the maximal 7 day avg percent positivity
    peakposavg=max(peak$average,na.rm=TRUE)
    #get the date of the maximal 7 day avg percent positivity
    peakposavgdate<-peak%>%filter(average==peakposavg)%>%
        mutate(date=min(date,na.rm=TRUE))%>%head(1)%>%pull(date)
    #percent change of the maximal 7 day avg percent positivity and current 7 day avg percent positivity
    peakposchange=(lab_current_average$average-peakposavg)/peakposavg
    #get the maximal daily test volume
    peaktestvol=max(peak$new_test,na.rm=TRUE)
    #get the date of the maximal daily test volume
    peaktestvoldate<-peak%>%filter(new_test==peaktestvol)%>%
        mutate(date=min(date,na.rm=TRUE))%>%head(1)%>%pull(date)
    #get the maximal 7 day avg test volume
    peaktestvolavg=max(peak$avg,na.rm=TRUE)
    #get the date of the maximal 7 day avg test volume
    peaktestvolavgdate<-peak%>%filter(avg==peaktestvolavg)%>%
        mutate(date=min(date,na.rm=TRUE)-4)%>%head(1)%>%
        pull(date)
    #percent change of the maximal 7 day avg test volume and current 7 day avg test volume
    peaktestvolchange=(lab_current_average$avg-peaktestvolavg)/peaktestvolavg
    #format numbers
    pos=label_percent(decimal.mark = ".",accuracy = .1)(peakpos/100)
    posdate=format(peakposdate,format="%b-%d-%y")
    posavg=label_percent(decimal.mark = ".",accuracy = .1)(peakposavg)
    posavgdate=format(peakposavgdate,format="%b-%d-%y")
    poschange=label_percent(decimal.mark = ".",accuracy = .1)(peakposchange)
    testvol=label_number(big.mark=",",accuracy=1)(peaktestvol)
    testvoldate=format(peaktestvoldate,format="%b-%d-%y")
    testvolavg=label_number(big.mark=",",accuracy=1)(peaktestvolavg)
    testvolavgdate=format(peaktestvolavgdate,format="%b-%d-%y")
    testvolchange=label_percent(decimal.mark = ".",accuracy = .1)(peaktestvolchange)
    return(data.frame("Peak"="Peak","Positivity"=pos,"Date"=posdate,"1"="1","Positivity"=posavg,"Date"=posavgdate,"Change vs. Current 7-Day Average"=poschange,"2"="2","Peak"="Peak","Test Vol"=testvol,"Date"=testvoldate,"3"="3","Test Vol"=testvolavg,"Date"=testvolavgdate,"Change vs. Current 7-Day Average"=testvolchange))

}
