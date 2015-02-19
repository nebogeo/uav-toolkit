import numpy, os
from osgeo import osr, gdal
import math
import uav_maths

def geo_read(filename):
    filehandle = gdal.Open(str(filename))
    band1 = filehandle.GetRasterBand(1)
    band2 = filehandle.GetRasterBand(2)
    band3 = filehandle.GetRasterBand(3)

    xsize = filehandle.RasterXSize
    ysize = filehandle.RasterYSize
    return xsize,ysize,[band1.ReadAsArray(),band2.ReadAsArray(),band3.ReadAsArray()]


def geo_write(filename,geotransform,geoprojection,data):
    (x,y) = data[0].shape
    format = "GTiff"
    driver = gdal.GetDriverByName(format)
    dst_datatype = gdal.GDT_Byte
    dst_ds = driver.Create(str(filename),y,x,3,dst_datatype)
    dst_ds.GetRasterBand(1).WriteArray(data[0])
    dst_ds.GetRasterBand(2).WriteArray(data[1])
    dst_ds.GetRasterBand(3).WriteArray(data[2])
    dst_ds.SetGeoTransform(geotransform)
    dst_ds.SetProjection(geoprojection)
    return 1

def geo_convert(in_prepend,filename,gps,north,out_prepend):
    # top left x, w-e pixel resolution, rotation,
    # top left y, rotation, n-s pixel resolution

    a = math.atan2(north.y,north.x)

    m = uav_maths.mat44();
    m.rotxyz(0,0,-a*uav_maths.degconv);
    m.scale(0.0000001,0.0000001,0.0000001);
    print "ANGLE:",a*uav_maths.degconv

    gt = [ gps.lat, m.m[0][0], m.m[1][0],
           gps.lon, m.m[0][1], m.m[1][1] ]
    w,h,data = geo_read(in_prepend+filename)
    srs = osr.SpatialReference()
    srs.ImportFromEPSG(4326)
    #srs.SetWellKnownGeogCS("WGS84")
    geo_write(out_prepend+filename+".tif",gt,srs.ExportToWkt(),data)
