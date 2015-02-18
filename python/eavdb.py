import sqlite3

class ktv:
    def __init__(self,key,type,value):
        self.key = key
        self.type = type
        self.value = value

    def pretty_print(self):
        print(self.key+":"+self.type+"="+str(self.value))

def ktv_get(ktv_list,key):
    for i in ktv_list:
        if i.key==key: return i.value
    return False

def get_attribute_ids_types(db, table, entity_type):
    c = db.cursor()
    c.execute('select attribute_id, attribute_type from '+table+'_attribute where entity_type=?', (entity_type,))
    return c.fetchall()

def get_all_entity_types(db, table):
    c = db.cursor()
    c.execute('select distinct entity_type from '+table+'_entity;')
    return map(lambda a: a[0], c.fetchall())

def get_entity_type(db, table, entity_id):
    c = db.cursor()
    c.execute('select entity_type from '+table+'_entity where entity_id = ?',
              (entity_id,))
    t=c.fetchone()
    if t: return t[0]
    else: return False

def get_value(db, table, entity_id, kt):
    c = db.cursor()
    c.execute('select value, dirty from '+table+'_value_'+kt.type+
              ' where entity_id = ? and attribute_id = ?',
              (entity_id, kt.key))
    return c.fetchone()

def safe_build_ktv(key,type,v):
    if v:
        return ktv(key, type, v[0])
    else:
        return ktv(key, type, -1)


def get_entity_plain(db, table, entity_id):
    entity_type = get_entity_type(db,table,entity_id)
    return map(lambda idtype:
               safe_build_ktv(idtype[0], idtype[1], get_value(db,table,entity_id,ktv(idtype[0],idtype[1],-1))),
               get_attribute_ids_types(db,table,entity_type))

def get_unique_id(db, table, entity_id):
    c = db.cursor()
    c.execute('select unique_id from '+table+'_entity where entity_id = ?',
              (entity_id,))
    t = c.fetchone()
    if t: return t[0]
    else: return False

def get_entity(db, table, entity_id):
  return [ktv("unique_id", "varchar", get_unique_id(db, table, entity_id))] +\
      get_entity_plain(db, table, entity_id)

def all_entities(db, table, entity_type):
    c = db.cursor()
    c.execute('select e.entity_id from '+table+'_entity as e '\
              'join '+table+'_value_varchar '\
              ' as n on n.entity_id = e.entity_id and n.attribute_id = ? '\
              'left join '+table+'_value_int '\
              'as d on d.entity_id = e.entity_id and d.attribute_id = ? '\
              'where e.entity_type = ? '\
              'and (d.value=\'NULL\' or d.value is NULL or d.value = 0) '\
              'order by n.value',
              ('name', 'deleted', entity_type,))
    return c.fetchall()

def get_entity_id(db, table, unique_id):
    c = db.cursor()
    c.execute('select entity_id from '+table+'_entity where unique_id = ?',
              (unique_id,))
    t = c.fetchone()
    if t: return t[0]
    else: return False

def get_entity_by_unique(db, table, unique_id):
    return get_entity(db, table, get_entity_id(db, table, unique_id))

def get_entity_name(db, table, unique_id):
    return ktv_get(get_entity_by_unique(db, table, unique_id), "name")


def get_entity_names(db, table, id_list):
    def _(r,uid):
        name = get_entity_name(db, table, uid)
        if name: return r+name+":"
        else:
            print(uid)
            return r+"Name not found:"

    if id_list==-1 or id_list=="NULL" or id_list=="": return ""
    return reduce(_,id_list.split(","), "")

def csv_titles(db, table, entity_type):
    return reduce(lambda r,kt:
                  r+"\""+kt[0]+"\",",
                  get_attribute_ids_types(db, table, entity_type),
                  "")

def csv(db, table, entity_type):
    c = db.cursor()
    c.execute('select e.entity_id from '+table+'_entity as e '\
              'where e.entity_type = ?',
              (entity_type,))
    return csv_titles(db,table,entity_type)+"\n"+\
        reduce(lambda r,entity_id: r+conv_csv(get_entity_plain(db, table, entity_id))+"\n",
               map(lambda i: i[0], c.fetchall()), "")

def conv_csv_ktv(k):
    if k==None:
        return ""
    elif k.key[0:8]=="id-list-" or\
         k.key=="pregnant" or\
         k.key=="present" or\
         k.key=="baby-byelim" or\
         k.key=="baby-seen":
        if type(k.value) is int: return str(k.value)

        names = get_entity_names(db,"sync",k.value)
        if not names:
            return "Name not found"
        else:
            return "\""+names+"\""
    elif k.key[0:3]=="id-" or k.key=="pack":
        name = get_entity_name(db,"sync",k.value)
        if not name:
            return "Name not found"
        else:
            return "\""+name+"\""
    else:
        if type(k.value)=="String":
            return "\""+k.value+"\""
        else: return str(k.value)

def conv_csv(data):
    return reduce(lambda r,elem:
                  r+conv_csv_ktv(elem)+", ",
                  data, "")

def open(filename):
    return sqlite3.connect(filename)
