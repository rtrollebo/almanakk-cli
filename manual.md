# Manual

*almanakk-cli* is a command-line program for calculation of solar ephemeris (sunrise and sunset for a given geographical location and date), and celestial phases (phases of the moon). 

Almanakk (Norwegian for almanac) is an astronomical calendar, containing ephemeris of celestial objects.

*almanakk-cli* is a client program for the *almanakk-lib* library (currently, the library is not in a public repo). 

# signature

`almanakk <lat> <lon> [timezone]`

# Input Parameters

| Parameter | Description                                              |
| :---      | :---                                                     |
| lat       | Latitude of the geographical location (decimal degrees)  |
| lon       | Longitude of the geographical location (decimal degrees) |
| timezone  | Timezone (optional) for output time (integer)            |
|           |                                                          |

The current date (and timezone if not provided) is determined from the system clock. 

# Output Parameters

## Solar Ephemeris

| Parameter | Description                                 |
| :---      | :---                                        |
| sunrise   | Time of sunrise, in unit of hour and minute |
| sunset    | Time of sunset, in unit of hour and minute  |
| Δ rise    | Daily change in sunrise time, in seconds.   |
| Δ set     | Daily change in sunset time, in seconds.    |

`Δ rise` is < 0 between winter and summer solstice (approximately), whereas `Δ set` is vice versa.

## Celestial Phase 

The current phase of the moon. The phases are the time durations between each 4 phase events. From the start of the lunar cycle to the end, they are as follows: 
Waxing crescent, Waxing gibbous, Waning gibbous, Waning crescent.

## Celestial Phase Events

Date and time of next phase event. Output minutes are rounded to the nearest 5.
Currently, only the phases of the Moon are calculated. But, phases of other celestial bodies such as planets may be added in the future.
The phase cycle contains 4 events:  New, first quarter, full and last quarter. almanakk-cli displays the lunar phase events, ordered by date of occurrence. 


# Examples

Sunrise and sunset for London, UK (lat. 51.5 and lon. 0.1)

    almanakk-cli 51.5 0.1
    
    Observer data
    Date                  2024-1-3            
    Time zone             CET                 
    Location              lat 51.5 lon 0.1    
    
    Solar celestial rising and setting
    Sunrise               9h5m                
    Sunset                17h0m               
    Δ rise                -11.5s              
    Δ set                 68s                 
    
    Celestial phase data
    Lunar phase           Waning Gibbous      
    
    Next celestial phase events
    2024-1-4 4h30m        Moon                  Last quarter        
    2024-1-11 12h55m      Moon                  New                 
    2024-1-18 4h50m       Moon                  First quarter       
    2024-1-25 18h55m      Moon                  Full  

Sunrise and sunset for Oslo, Norway (lat 59.9 and lon. 10.75)

    almanakk-cli 59.9 10.75 
    
    Observer data
    Date                  2024-1-3            
    Time zone             CET                 
    Location              lat 59.9 lon 10.75  
    
    Solar celestial rising and setting
    Sunrise               9h15m               
    Sunset                15h25m              
    Δ rise                -36.5s              
    Δ set                 94s                 
    
    Celestial phase data
    Lunar phase           Waning Gibbous      
    
    Next celestial phase events
    2024-1-4 4h30m        Moon                  Last quarter        
    2024-1-11 12h55m      Moon                  New                 
    2024-1-18 4h50m       Moon                  First quarter       
    2024-1-25 18h55m      Moon                  Full 


Sunrise and sunset for  Lima, Peru (lat -12.05 and lon -77.03), with specified time zone. 

    almanakk-cli -12.05 -77.03 -5
        
    Observer data
    Date                  2024-1-3            
    Time zone             -0500               
    Location              lat -12.05 lon -77.0
    
    Solar celestial rising and setting
    Sunrise               5h45m               
    Sunset                18h35m              
    Δ rise                33.5s               
    Δ set                 21.5s               
    
    Celestial phase data
    Lunar phase           Waning Gibbous      
    
    Next celestial phase events
    2024-1-3 22h30m       Moon                  Last quarter        
    2024-1-11 6h55m       Moon                  New                 
    2024-1-17 22h50m      Moon                  First quarter       
    2024-1-25 12h55m      Moon                  Full    

# Reference

Builds for aarch64-osx are available from [rtrollebo.github.io/documentation](https://rtrollebo.github.io/documentation/)



