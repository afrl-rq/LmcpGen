# Python template listing 



ONCE               py/LMCPObject.py            lmcp/LMCPObject.py
ONCE               py/LMCPFactory.py           lmcp/LMCPFactory.py
ONCE               py/LMCPServer.py            lmcp/LMCPServer.py
ONCE               py/LMCPClient.py            lmcp/LMCPClient.py
ONCE               py/__init__.py              lmcp/__init__.py
ONCE               py/__init__.py              __init__.py
#ONCE               py/Readme.html              Readme.html

PER_STRUCT 	   py/SeriesObject.py	       -<series_dir>-/-<datatype_name>-.py
PER_ENUM           py/enum.py	               -<series_dir>-/-<enum_name>-.py
#PER_MDM            py/SeriesFactory.py         -<series_dir>-/SeriesFactory.py
PER_MDM            py/SeriesEnum.py            -<series_dir>-/SeriesEnum.py
PER_MDM            py/series__init__.py        -<series_dir>-/__init__.py
PER_MDM            py/Readme.html              Readme.html

