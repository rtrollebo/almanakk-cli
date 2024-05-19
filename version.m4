define(`MODULE_HEADER', 
module Almanakk.Application.Version (getVersion) where)



define(`CMD_OUTPUT', esyscmd(date +"%Y-%m-%d %H:%M" | tr -d '\n'))
define(`GET_VERSION', 

getVersion :: String
getVersion = "VERSION (built `CMD_OUTPUT')")

MODULE_HEADER
GET_VERSION

