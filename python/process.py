import os

path = "../data4/reduced/"
dest = "../data4/reduce/"

i = 0
for (path, dirs, files) in os.walk(path):
    for filename in files:
        cmd = "convert -resize 20% "+path+filename+" "+dest+filename
        print cmd
        os.system(cmd)
