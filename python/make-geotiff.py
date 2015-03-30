# Import libs
import numpy, os
from osgeo import osr, gdal
import eavdb
import math
import uav_maths
import uav_geotiff

db = eavdb.open('../data2/uav-toolkit.db')
print eavdb.get_all_entity_types(db,'stream')




def process(entities):
    for c,e in enumerate(entities):
        filename = eavdb.ktv_get(e,"photo")

        gps = uav_maths.geo(eavdb.ktv_get(e,"gps-1"), eavdb.ktv_get(e,"gps-0"))

        magnet = uav_maths.vec3(eavdb.ktv_get(e,"magnetic-field-0"),
                                eavdb.ktv_get(e,"magnetic-field-1"),
                                eavdb.ktv_get(e,"magnetic-field-2"))

        magnet.z=0 # collapse z
        magnet.normalise()
        #magnet.pretty_print()

        #    gps.pretty_print()

        # move to top left corner
        offset_to_tl = uav_maths.vec3(-20,-20,0)
        offset_to_br = uav_maths.vec3(20,20,0)
        rotmat = uav_maths.mat44()
        rotmat.aim(magnet,uav_maths.vec3(0,0,1))
        offset_to_tl = rotmat.transform(offset_to_tl)
        offset_to_br = rotmat.transform(offset_to_br)


        tl_gps = gps.add_metres(offset_to_tl.x,offset_to_tl.y)
        br_gps = gps.add_metres(offset_to_br.x,offset_to_br.y)

        w = 0.0002
        h = 0.0002

        uav_geotiff.geo_convert("../data2/",filename,gps,magnet,"../data2/out/")

        #cmd = 'gdal_translate -a_srs "+proj=latlong +datum=WGS84 +alpha=-45" -of GTiff '+\
            #      '-co "INTERLEAVE=PIXEL" -a_ullr '+str(tl_gps.lat)+" "+str(tl_gps.lon)+" "+str(br_gps.lat)+" "+str(br_gps.lon)+" "\
            #      +filename+' out/'+filename+'.tif'

        #print cmd
        #os.system(cmd)


entities = eavdb.filter_entities(db,"stream",
                                 [eavdb.eavdb_filter("time","varchar",">","2015-03-10 03:19:00")])


process(entities)
