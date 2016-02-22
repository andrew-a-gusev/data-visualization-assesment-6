
san <- read.csv("sanfrancisco_incidents_summer_2014.csv")
sea <- read.csv("seattle_incidents_summer_2014.csv")

library(dplyr)

# Normalize coords and Crime types
san_norm <- mutate(san,
        nx = (X-min(X))/(max(X)-min(X)),
        ny = (Y-min(Y))/(max(Y)-min(Y)),
        type = ifelse(grepl('DRUG/NARCOTIC',Category),'DRUGS',
                ifelse(grepl('PROSTITUTION',Category),'PROSTITUTION','OTHER')
                ),
        a = ifelse(grepl('DRUG/NARCOTIC',Category),1,
                    ifelse(grepl('PROSTITUTION',Category),1,0.1)
            ),
        time_of_day =as.POSIXct(Time, format='%H:%M'),
        night = ifelse(time_of_day>as.POSIXct('05:00', format='%H:%M') && time_of_day<as.POSIXct('19:00', format='%H:%M'),FALSE, TRUE)
        )

sea_norm <- filter(sea, Latitude != 0, Longitude !=0 ) %>%
        mutate(
            ny = (Latitude-min(Latitude))/(max(Latitude)-min(Latitude)),
            nx = (Longitude-min(Longitude))/(max(Longitude)-min(Longitude)),
            type = ifelse(grepl('^NARC',Offense.Type),'DRUGS',
                    ifelse(grepl('^PROSTITUTION',Offense.Type),'PROSTITUTION','OTHER')
                    ),
            a = ifelse(grepl('^NARC',Offense.Type),1,
                    ifelse(grepl('^PROSTITUTION',Offense.Type),1,0.1)
                ),
            dt = parse_date_time(Occurred.Date.or.Date.Range.Start, orders="mdy hms")+{ifelse(grepl('PM$', Occurred.Date.or.Date.Range.Start), 12*3600, 0) },
            time_of_day =as.POSIXct(strftime(dt, format='%H:%M'), format='%H:%M'),
            night = ifelse(time_of_day>as.POSIXct('05:00', format='%H:%M') && time_of_day<as.POSIXct('19:00', format='%H:%M'),FALSE, TRUE)
        )

library(ggplot2)

#plot(select(san_norm,nx,ny))
#plot(select(sea_norm,nx,ny))

ggplot(data=sea_norm, mapping=aes(x=nx,y=ny)) +   geom_jitter(aes(alpha=a, color=type))
ggplot(data=san_norm, mapping=aes(x=nx,y=ny)) +   geom_jitter(aes(alpha=a, color=type))
