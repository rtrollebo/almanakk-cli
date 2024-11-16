# Manual

*almanakk-cli* is a command-line program for calculation of solar ephemeris (sunrise and sunset for a given geographical location and date), celestial phases (phases of the moon) and calendar events derived from celestial phase events. 

Almanakk (Norwegian for almanac) is an astronomical calendar, containing ephemeris of celestial objects.

*almanakk-cli* is a client program for the *almanakk-lib* library (currently, the library is not in a public repo). 

# Ephemeris

Ephemeris of astronomical bodies 

* sunset and sunrise

## signature

    almanakk-cli ephemeris -h
    Usage: almanakk-cli ephemeris lat lon [--timezone timezone]

    Calculate the ephemeris at the specified latitude and longitude.

    Available options:
      -h,--help                Show this help text
      lat                      latitude
      lon                      latitude
      --timezone timezone      Specify the timezone (default: "default")

## Input Parameters

| Parameter | Description                                              |
| :---      | :---                                                     |
| lat       | Latitude of the geographical location (decimal degrees)  |
| lon       | Longitude of the geographical location (decimal degrees) |
| timezone  | Timezone (optional) for output time (integer)            |
|           |                                                          |

The current date (and timezone if not provided) is determined from the system clock. 

## Ephemeris Data Product

| Parameter | Description                                 |
| :---      | :---                                        |
| sunrise   | Time of sunrise, in unit of hour and minute |
| sunset    | Time of sunset, in unit of hour and minute  |
| Δ rise    | Daily change in sunrise time, in seconds.   |
| Δ set     | Daily change in sunset time, in seconds.    |

`Δ rise` is < 0 between winter and summer solstice (approximately), whereas `Δ set` is vice versa.


# Phase

## signature

    almanakk-cli phase -h
    Usage: almanakk-cli phase [--timezone timezone]

      Calculate the lunar phase.

    Available options:
      -h,--help                Show this help text
      --timezone timezone      Specify the timezone (default: "default")

## Input Parameters

| Parameter | Description                                              |
| :---      | :---                                                     |
| timezone  | Timezone (optional) for output time (integer)            |
|           |                                                          |

## Celestial Phase Data Product

### Celestial Phase 

The current phase of the moon. The phases are the time durations between each 4 phase events. From the start of the lunar cycle to the end, they are as follows: 
Waxing crescent, Waxing gibbous, Waning gibbous, Waning crescent.

### Celestial Phase Events

Date and time of next phase event. Output minutes are rounded to the nearest 5.
Currently, only the phases of the Moon are calculated. But, phases of other celestial bodies such as planets may be added in the future.
The phase cycle contains 4 events:  New, first quarter, full and last quarter. `almanakk-cli` displays the lunar phase events, ordered by date of occurrence. 

# Calendar

## Signature

    almanakk-cli calendar -h
    Usage: almanakk-cli calendar [--timezone timezone]

      Calculate the astronomical calendar.

    Available options:
      -h,--help                Show this help text
      --timezone timezone      Specify the timezone (default: "default")

## Calendar Data Product

### Equinox

Date and time of the equinoxes:

* Northward equinox, or the *March equinox* (Spring equinox in the northern hepmispher, autumn equinox in the southern hemisphere)
* Southward equinox, or the *September equinox*.

### Solstice

Date and time of the solstices:

* Northern solstice, or the *June solstice* (Summer solstice in the northern hemisphere, winter solstice in the southern hemisphere).
* Southern solstice, or the *December solstice*.

### Christian Holidays

A list of holidays in the Christian Liturgical Year. By default, upcoming holidays from current day to the end of the current year, are shown. 


# Examples

Ephemeris examples for three geographical locations, on 2024-11-16

Sunrise and sunset for London, UK (lat. 51.5 and lon. 0.1)

    almanakk-cli ephemeris 51.5 0.1

    Observer data
      Date                        2024-11-16                
      Time zone                   CET                       
      Location                    lat 51.5 lon 0.1          

    Solar celestial rising and setting
      Sunrise                     8h20m                     
      Sunset                      17h5m                     
      Δ rise                      101.5s                    
      Δ set                       -77.5s  

Sunrise and sunset for Oslo, Norway (lat 59.9 and lon. 10.75)

    almanakk-cli ephemeris 59.9 10.75

    Observer data
      Date                        2024-11-16                
      Time zone                   CET                       
      Location                    lat 59.9 lon 10.75        

    Solar celestial rising and setting
      Sunrise                     8h15m                     
      Sunset                      15h40m                    
      Δ rise                      148.5s                    
      Δ set                       -124s 

Sunrise and sunset for  Lima, Peru (lat -12.05 and lon -77.03), with specified time zone. (for positional command line arguments in `ephemeris`, add a leading white space to prevent almanakk from interpreting it as an option.)

    almanakk-cli ephemeris " -12.05" " -77.03" --timezone=-5
        
    Observer data
      Date                        2024-11-16                
      Time zone                   -0500                     
      Location                    lat -12.05 lon -77.03     

    Solar celestial rising and setting
      Sunrise                     5h30m                     
      Sunset                      18h10m                    
      Δ rise                      -3s                       
      Δ set                       26.5s    

Current celestial phase, on 2024-11-16

    almanakk-cli phase

    Current celestial phases
      Lunar phase                 Waning Gibbous            

    Next celestial phase events
      2024-11-23 2h25m            Moon                        Last quarter              
      2024-12-1 7h20m             Moon                        New                       
      2024-12-8 16h25m            Moon                        First quarter             
      2024-12-15 10h0m            Moon                        Full 

Christian calendar and astronomical events (upcoming calendar events at 2024-05-14)

    almanakk calendar

    Christian holidays and astronomical events   

    2024-05-19                  Pentecost
    2024-05-26                  Trinity Sunday
    2024-06-20                  Solstice Northern 22h50m    
    2024-09-22                  Equinox southward 14h40m    
    2024-12-21                  Solstice Southern 11h20m 

# Reference

Builds for aarch64-osx are available from [rtrollebo.github.io/documentation](https://rtrollebo.github.io/documentation/)



