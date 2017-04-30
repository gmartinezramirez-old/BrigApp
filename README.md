# BrigApp
NASA Hackathon 2017


## To use the calcute route module
Use command to mount server in the port 8989. Accesible by localhost:8989

'''
make run
'''

or use the command (included in Makefile):

'''
./td.sh datasource=your-osm-file.pbf
'''

In this case (Chile), we must use:

'''
./td.sh datasource=chile-latest-osm.pbf
'''

### Send weigths nodes
Send json data that contains the weigth of the nodes
curl -H "Content-Type: application/json" --data @traffic.json http://localhost:8989/datafeed

### Send points of beginning and end
Send points of the start and end points to calculate the safest and fastest route.
http://localhost:8989/route?point=BEGINNING-LATITUDE%2CBEGINNIG-LONGITUDE&point=ENDING-LATITUDE%2CENDING-LONGITUDE

Example:
V-46, Fresia, X Región, Chile
Latitude: -41.152994 | Longitude: -73.424678

http://localhost:8989/route?point=-41.152841%2C-73.426094&point=-41.157106%2C-73.423358

note: %2C indicate a comma in the URL
