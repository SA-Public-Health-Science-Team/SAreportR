
#' Title
#'
#' @param dataset
#' @param startdate
#' @param enddate
#'
#' @return
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
#' Title
#'
#' @param dataset
#' @param startdate
#' @param enddate
#'
#' @return
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
    #get the maximal weekly cases
    peakcases=max(peak$new,na.rm=TRUE)
    #get the date of the maximal weekly cases
    peakdate<-peak%>%filter(new==peakcases)%>%
        mutate(date=min(date,na.rm=TRUE))%>%head(1)%>%pull(date)

    Date=format(peakdate,format="%b %d, %Y")

    return(data.frame("date"=peakdate,"dateformat"=Date))
}

#' Title
#'
#' @param dataset
#' @param startdate
#' @param enddate
#'
#' @return
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
#' Title
#'
#' @param dataset
#' @param startdate
#' @param enddate
#'
#' @return
#' @export
#'
#' @examples
weekly_hosp_peak<-function(dataset,startdate,enddate){

        #filter the date
        peak<-dataset%>%
            filter(hosp_date>=as.Date(startdate,format("%Y-%m-%d")))%>%
            filter(hosp_date<=as.Date(enddate,format("%Y-%m-%d")))
        #get the maximal weekly admissions
        peakadmission=max(peak$hosp_cum7,na.rm=TRUE)
        #get the date of the maximal weekly admissions
        peakdate<-peak%>%filter(hosp_cum7==peakadmission)%>%
            mutate(hosp_date=min(hosp_date,na.rm=TRUE))%>%head(1)%>%pull(hosp_date)
        #get the maximal 21 day avg admission
        peak_21_average=max(peak$hosp_avg21,na.rm=TRUE)
        #get the date of the maximal 21 day avg admission
        peak21avgdate<-peak%>%filter(hosp_avg21==peak_21_average)%>%
            mutate(hosp_date=min(hosp_date,na.rm=TRUE))%>%head(1)%>%pull(hosp_date)
        #format numbers
        Admissions=label_number(big.mark=",",accuracy=1)(peakadmission)
        Dates=format(peakdate,format="%d-%b-%y")
        Admission=label_number(big.mark=",",accuracy=1)(peak_21_average)
        Date=format(peak21avgdate,format="%d-%b-%y")

        return(data.frame("Admissions"=Admissions,"Dates"=Dates,"1"="1","Admission"=Admission,"Date"=Date))

}


#' Title
#'
#' @param dataset
#' @param startdate
#' @param enddate
#'
#' @return
#' @export
#'
#' @examples
daily_lab_peak<-function(dataset,startdate,enddate){
    #get current lab data
    lab_current_7_average<-dataset%>%
        filter(testpos_date==max(testpos_date,na.rm=TRUE))
    #filter the date
    peak<-dataset%>%
        filter(testpos_date>=as.Date(startdate,format("%Y-%m-%d")))%>%
        filter(testpos_date<=as.Date(enddate,format("%Y-%m-%d")))
    #get the maximal daily percent positivity
    peakpos=max(peak$new_pos,na.rm=TRUE)
    #get the date of the maximal daily percent positivity
    peakposdate<-peak%>%filter(new_pos==peakpos)%>%
        mutate(testpos_date=min(testpos_date,na.rm=TRUE))%>%head(1)%>%pull(testpos_date)
    #get the maximal 7 day avg percent positivity
    peakposavg=max(peak$testpos_avg,na.rm=TRUE)
    #get the date of the maximal 7 day avg percent positivity
    peakposavgdate<-peak%>%filter(testpos_avg==peakposavg)%>%
        mutate(testpos_date=min(testpos_date,na.rm=TRUE))%>%head(1)%>%pull(testpos_date)
    #percent change of the maximal 7 day avg percent positivity and current 7 day avg percent positivity
    peakposchange=(lab_current_7_average$testpos_avg-peakposavg)/peakposavg
    #get the maximal daily test volume
    peaktestvol=max(peak$new_test_vol,na.rm=TRUE)
    #get the date of the maximal daily test volume
    peaktestvoldate<-peak%>%filter(new_test_vol==peaktestvol)%>%
        mutate(testpos_date=min(testpos_date,na.rm=TRUE))%>%head(1)%>%pull(testpos_date)
    #get the maximal 7 day avg test volume
    peaktestvolavg=max(peak$testvol_avg,na.rm=TRUE)
    #get the date of the maximal 7 day avg test volume
    peaktestvolavgdate<-peak%>%filter(testvol_avg==peaktestvolavg)%>%
        mutate(testpos_date=min(testpos_date,na.rm=TRUE))%>%head(1)%>%
        pull(testpos_date)
    #percent change of the maximal 7 day avg test volume and current 7 day avg test volume
    peaktestvolchange=(lab_current_7_average$testvol_avg-peaktestvolavg)/peaktestvolavg
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
#' Title
#'
#' @param dataset
#' @param startdate
#' @param enddate
#'
#' @return
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
#' Title
#'
#' @param dataset
#' @param startdate
#' @param enddate
#'
#' @return
#' @export
#'
#' @examples
weekly_lab_peak<-function(dataset,startdate,enddate){
    #get current lab data
    lab_current_21_average<-lab_weekly_epicurve%>%
        filter(testpos_date==max(testpos_date,na.rm=TRUE))
    #filter the date
    peak<-lab_weekly_epicurve%>%
        filter(testpos_date>=as.Date(startdate,format("%Y-%m-%d")))%>%
        filter(testpos_date<=as.Date(enddate,format("%Y-%m-%d")))
    #get the maximal weekly percent positivity
    peakpos=max(peak$testpos_avg,na.rm=TRUE)
    #get the date of the maximal weekly percent positivity
    peakposdate<-peak%>%filter(testpos_avg==peakpos)%>%
        mutate(testpos_date=min(testpos_date,na.rm=TRUE))%>%head(1)%>%pull(testpos_date)
    #get the maximal 21 day avg percent positivity
    peakposavg=max(peak$testpos_avg21,na.rm=TRUE)
    #get the date of the maximal 21 day avg percent positivity
    peakposavgdate<-peak%>%filter(testpos_avg21==peakposavg)%>%
        mutate(testpos_date=min(testpos_date,na.rm=TRUE))%>%head(1)%>%pull(testpos_date)
    #percent change of the maximal 21 day avg percent positivity and current 21 day avg percent positivity
    peakposchange=(lab_current_21_average$testpos_avg21-peakposavg)/peakposavg
    #get the maximal of weekly test volume
    peaktestvol=max(peak$test_cum7,na.rm=TRUE)
    #get the date of the maximal of weekly test volume
    peaktestvoldate<-peak%>%filter(test_cum7==peaktestvol)%>%
        mutate(testpos_date=min(testpos_date,na.rm=TRUE))%>%head(1)%>%pull(testpos_date)
    #get the maximal 21 day avg test volume
    peaktestvolavg=max(peak$testvol_avg21,na.rm=TRUE)
    #get the date of the maximal 21 day avg test volume
    peaktestvolavgdate<-peak%>%filter(testvol_avg21==peaktestvolavg)%>%
        mutate(testpos_date=testpos_date)%>%head(1)%>%pull(testpos_date)
    #percent change of the maximal 21 day avg test volume and current 21 day avg test volume
    peaktestvolchange=(lab_current_21_average$testvol_avg21-peaktestvolavg)/peaktestvolavg
    #format number
    pos=label_percent(decimal.mark = ".",accuracy = .1)(peakpos)
    posdate=format(peakposdate,format="%b-%d-%y")
    posavg=label_percent(decimal.mark = ".",accuracy = .1)(peakposavg)
    posavgdate=format(peakposavgdate,format="%b-%d-%y")
    poschange=label_percent(decimal.mark = ".",accuracy = .1)(peakposchange)
    testvol=label_number(big.mark=",",accuracy=1)(peaktestvol)
    testvoldate=format(peaktestvoldate,format="%b-%d-%y")
    testvolavg=label_number(big.mark=",",accuracy=1)(peaktestvolavg)
    testvolavgdate=format(peaktestvolavgdate,format="%b-%d-%y")
    testvolchange=label_percent(decimal.mark = ".",accuracy = .1)(peaktestvolchange)

    return(data.frame("Peak"="Peak","Positivity"=pos,"Date"=posdate,"1"="1","Positivity"=posavg,"Date"=posavgdate,"Change vs. Current 14-Day Average"=poschange,"2"="2","Peak"="Peak","Test Vol"=testvol,"Date"=testvoldate,"3"="3","Test Vol"=testvolavg,"Date"=testvolavgdate,"Change vs. Current 14-Day Average"=testvolchange))
}
