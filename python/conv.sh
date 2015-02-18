gdal_translate -a_srs "+proj=latlong +datum=WGS84" -of GTiff\
   -co "INTERLEAVE=PIXEL" -a_ullr -5.3447589874 50.2347602844\
   -5.3448590874 50.2348603844 files/photo-1421927888-277330.jpg\
   myfile.tif
