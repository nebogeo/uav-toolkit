import math

###########################################################

class vec3:
    def __init__(self,x,y,z):
        self.x = x
        self.y = y
        self.z = z

    def pretty_print(self):
        print(str(self.x)+" "+str(self.y)+" "+str(self.z))

    def dot(self,rhs):
        return self.x*rhs.x+self.y*rhs.y+self.z*rhs.z

    def cross(self,rhs):
        return vec3(self.y*rhs.z - self.z*rhs.y,
                    self.z*rhs.x - self.x*rhs.z,
                    self.x*rhs.y - self.y*rhs.x)

    def mag(self):
        return math.sqrt(self.x*self.x+self.y*self.y+self.z*self.z)

    def distance(self,a):
        return mag(a-b)

    def normalised(self):
        m = self.mag()
        if m>0:
            return vec3(self.x/m, self.y/m, self.z/m)
        else:
            return self

    def normalise(self):
        m = self.mag()
        if m>0:
            self.x/=m
            self.y/=m
            self.z/=m

###########################################################

class mat44:
    def __init__(self):
        self.zero();
        self.m[0][0]=1
        self.m[1][1]=1
        self.m[2][2]=1
        self.m[3][3]=1;

    def zero(self):
        self.m=[[0,0,0,0],
                [0,0,0,0],
                [0,0,0,0],
                [0,0,0,0]]

    def mul(self,rhs):
        t=mat44()
        for i in range(0,4):
            for j in range(0,4):
                t.m[i][j]=self.m[i][0]*rhs.m[0][j]+self.m[i][1]*rhs.m[1][j]+self.m[i][2]*rhs.m[2][j]+self.m[i][3]*rhs.m[3][j];
        return t

    def translate(self, x, y, z):
        t=mat44()
        t.m[3][0]=x;
        t.m[3][1]=y;
        t.m[3][2]=z;
        self.m=self.mul(t).m

    def rotxyz(self, x, y, z):
        if x!=0:
            t=mat44()
            x*=0.017453292
            sx=math.sin(x)
            cx=math.cos(x)
            t.m[1][1]=cx
            t.m[2][1]=-sx
            t.m[1][2]=sx
            t.m[2][2]=cx
            self.m=self.mul(t).m
        if y!=0:
            t=mat44()
            y*=0.017453292
            sy=math.sin(y)
            cy=math.cos(y)
            t.m[0][0]=cy
            t.m[2][0]=sy
            t.m[0][2]=-sy
            t.m[2][2]=cy
            self.m=self.mul(t).m
        if z!=0:
            t=mat44()
            z*=0.017453292
            sz=math.sin(z)
            cz=math.cos(z)
            t.m[0][0]=cz
            t.m[1][0]=-sz
            t.m[0][1]=sz
            t.m[1][1]=cz
            self.m=self.mul(t).m

    def scale(self, x, y, z):
        t = mat44()
        t.m[0][0]=x
        t.m[1][1]=y
        t.m[2][2]=z
        self.m=self.mul(t).m


    def transform(self,p):
        t = vec3(0,0,0)
        t.x=p.x*self.m[0][0] + p.y*self.m[1][0] + p.z*self.m[2][0]
        t.y=p.x*self.m[0][1] + p.y*self.m[1][1] + p.z*self.m[2][1]
        t.z=p.x*self.m[0][2] + p.y*self.m[1][2] + p.z*self.m[2][2]
        return t

    def aim(self, v, up):
        v.normalise();
        l=v.cross(up);
        u=v.cross(l);
        l.normalise();
	u.normalise();
        self.m[0][0]=v.x; self.m[0][1]=v.y; self.m[0][2]=v.z
        self.m[1][0]=l.x; self.m[1][1]=l.y; self.m[1][2]=l.z
        self.m[2][0]=u.x; self.m[2][1]=u.y; self.m[2][2]=u.z



###########################################################

earth_radius_metres = 6371000.0
radconv = 3.141592653589793/180.0
degconv = 180/3.141592653589793

def to_radians(a): return a*radconv

class geo:
    def __init__(self,lat,lon):
        self.lat = lat
        self.lon = lon

    def pretty_print(self):
        print(str(self.lat)+" "+str(self.lon))

    def distance_metres(self,a):
        lata = to_radians(a.lat)
        lona = to_radians(a.lon)
        latb = to_radians(self.lat)
        lonb = to_radians(self.lon)
        cosang = math.cos(lata)*math.cos(latb)*math.cos(lonb-lona) +\
                 math.sin(lata)*math.sin(latb)
        return math.acos(cosang)*earth_radius_metres

    def add_metres(self,dx,dy):
        return geo(self.lat+(float(dy)/earth_radius_metres)*degconv,
                   self.lon+(float(dx)/earth_radius_metres)*degconv/math.cos(self.lat*radconv));
