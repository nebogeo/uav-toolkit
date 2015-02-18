# Import libs
import numpy, os
from osgeo import osr, gdal
import eavdb
import math
import uav_maths

def make_geotiff(src):
    # Set file vars
    output_file = "out.tif"
    # Create gtif
    driver = gdal.GetDriverByName("GTiff")
    dst_ds = src
    # top left x, w-e pixel resolution, rotation,
    # top left y, rotation, n-s pixel resolution
    dst_ds.SetGeoTransform( [ 14.97, 0.11, 0,
                              -34.54, 0, 0.11 ] )
    # set the reference info[ 14.97, 0.11, 0, -34.54, 0, 0.11 ]
    srs = osr.SpatialReference()
    srs.SetWellKnownGeogCS("WGS84")
    dst_ds.SetProjection( srs.ExportToWkt() )
    return dst_ds


def geo_read(filename):
    filehandle = gdal.Open(str(filename))
    band1 = filehandle.GetRasterBand(1)
    xsize = filehandle.RasterXSize
    ysize = filehandle.RasterYSize
    return xsize,ysize,[band1.ReadAsArray()]


def geo_write(filename,geotransform,geoprojection,data):
    (x,y) = data[0].shape
    format = "GTiff"
    driver = gdal.GetDriverByName(format)
    dst_datatype = gdal.GDT_Byte
    dst_ds = driver.Create(str(filename),y,x,1,dst_datatype)
    dst_ds.GetRasterBand(1).WriteArray(data[0])
    dst_ds.SetGeoTransform(geotransform)
    dst_ds.SetProjection(geoprojection)
    return 1

def geo_convert(filename,gps,north,out_prepend):
    # top left x, w-e pixel resolution, rotation,
    # top left y, rotation, n-s pixel resolution

    a = math.atan2(north.y,north.x)

    m = uav_maths.mat44();
    m.rotxyz(0,0,-a*uav_maths.degconv);
    m.scale(0.0000001,0.0000001,0.0000001);
    print "ANGLE:",a*uav_maths.degconv

    gt = [ gps.lat, m.m[0][0], m.m[1][0],
           gps.lon, m.m[0][1], m.m[1][1] ]
    w,h,data = geo_read(filename)
    srs = osr.SpatialReference()
    srs.ImportFromEPSG(4326)
    #srs.SetWellKnownGeogCS("WGS84")
    geo_write(out_prepend+filename+".tif",gt,srs.ExportToWkt(),data)




db = eavdb.open('uav-toolkit.db')

print eavdb.get_all_entity_types(db,'stream')
#print get_attribute_ids_types(db, 'stream', 'group-interaction')

#table = 'stream'

#for i in get_all_entity_types(db,table):
#    with open(i+'.csv','w') as f:
#        f.write(csv(db,table,i))

########################################################


for c,i in enumerate(eavdb.all_entities(db,"stream","timed cam")[100:120]):
    print c+10
    e = eavdb.get_entity(db,"stream",i[0])
    filename = eavdb.ktv_get(e,"photo")

    gps = uav_maths.geo(eavdb.ktv_get(e,"gps-1"), eavdb.ktv_get(e,"gps-0"))

    magnet = uav_maths.vec3(eavdb.ktv_get(e,"magnetic-field-0"),
                            eavdb.ktv_get(e,"magnetic-field-1"),
                            eavdb.ktv_get(e,"magnetic-field-2"))

    magnet.z=0 # collapse z
    magnet.normalise()
    magnet.pretty_print()

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

    geo_convert(filename,gps,magnet,"out/")

    #cmd = 'gdal_translate -a_srs "+proj=latlong +datum=WGS84 +alpha=-45" -of GTiff '+\
    #      '-co "INTERLEAVE=PIXEL" -a_ullr '+str(tl_gps.lat)+" "+str(tl_gps.lon)+" "+str(br_gps.lat)+" "+str(br_gps.lon)+" "\
    #      +filename+' out/'+filename+'.tif'

    #print cmd
    #os.system(cmd)
