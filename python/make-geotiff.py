# Import libs
import numpy, os
from osgeo import osr, gdal
import eavdb
import math
import uav_maths
import uav_geotiff
from decimal import *

def convert_images(entities):
    for e in entities:
        filename = eavdb.ktv_get(e,"photo")
        cmd = "convert -resize 20% ../data6/"+filename+" ../data6/reduce/"+filename
        print cmd
        os.system(cmd)


def process(entities):
    for c,e in enumerate(entities):
        filename = eavdb.ktv_get(e,"photo")
        if True or filename == "files/photo-1428500285-993071.jpg" or filename == "files/photo-1428500247-372799.jpg":

            gps = uav_maths.geo(eavdb.ktv_get(e,"gps-1"), eavdb.ktv_get(e,"gps-0"))

            magnet = uav_maths.vec3(eavdb.ktv_get(e,"orientation-0"),
                                    eavdb.ktv_get(e,"orientation-1"),
                                    eavdb.ktv_get(e,"orientation-2"))

            a = magnet.x

            iw = 842
            ih = 624
            ratio = iw/float(ih)

            # move to top left corner
            offset_to_tl = uav_maths.vec3(-30*ratio,-30,0)
            offset_to_br = uav_maths.vec3(30*ratio,30,0)
            offset_to_tr = uav_maths.vec3(30*ratio,-30,0)
            offset_to_bl = uav_maths.vec3(-30*ratio,30,0)
            rotmat = uav_maths.mat44()
            #rotmat.aim(magnet,uav_maths.vec3(0,0,1))
            rotmat.rotxyz(0,0,a+180);

            offset_to_tl = rotmat.transform(offset_to_tl)
            offset_to_br = rotmat.transform(offset_to_br)
            offset_to_tr = rotmat.transform(offset_to_tr)
            offset_to_bl = rotmat.transform(offset_to_bl)

            #offset_to_tl.pretty_print()
            #offset_to_br.pretty_print()

            tl_gps = gps.add_metres(offset_to_tl.x,offset_to_tl.y)
            br_gps = gps.add_metres(offset_to_br.x,offset_to_br.y)
            tr_gps = gps.add_metres(offset_to_tr.x,offset_to_tr.y)
            bl_gps = gps.add_metres(offset_to_bl.x,offset_to_bl.y)

            w = 0.0002
            h = 0.0002


            ##tl_gps = gps
            #br_gps = gps.add_metres(100,100)

            #uav_geotiff.geo_convert("../data6/",filename,gps,magnet,"../data6/out/")


            cmd = 'gdal_translate -a_srs "+proj=latlong +datum=WGS84 +alpha=-45" -of GTiff '+\
                  '-co "INTERLEAVE=PIXEL" '+\
                  "-gcp 0 0 "+str(tl_gps.lat)+" "+str(tl_gps.lon)+" "+\
                  "-gcp 0 "+str(ih)+" "+str(bl_gps.lat)+" "+str(bl_gps.lon)+" "+\
                  "-gcp "+str(iw)+" 0 "+str(tr_gps.lat)+" "+str(tr_gps.lon)+" "+\
                  "-gcp "+str(iw)+" "+str(ih)+" "+str(br_gps.lat)+" "+str(br_gps.lon)+" "+\
                  "../data6/reduce/"+filename+' ../data6/out/'+filename+'.tif'

            print cmd
            os.system(cmd)

            cmd = 'gdalwarp -r bilinear -dstalpha ../data6/out/'+filename+'.tif ../data6/out/'+filename+'oo2.tif'
            os.system(cmd)
            #cmd = 'gdalwarp -t_srs EPSG:3857 ../data6/out/'+filename+'oo2.tif ../data6/out/'+filename+'oo3.tif'
            #os.system(cmd)
            cmd = 'rm ../data6/out/'+filename+'.tif ' #'../data6/out/'+filename+'oo2.tif'
            os.system(cmd)



db = eavdb.open('../data6/uav-toolkit.db')
print eavdb.get_all_entity_types(db,'stream')

#entities = eavdb.filter_entities(db,"stream",
#                                 [eavdb.eavdb_filter("time","varchar",">","2015-04-07 03:19:00")])
entities = eavdb.filter_entities(db,"stream",
                                 [eavdb.eavdb_filter("name","varchar","=","camera-new-location")])


#print entities
#convert_images(entities)
process(entities)
