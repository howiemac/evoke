"""
evoke definition class for Data objects

schema syntax:

class Widget:
  table='widgets'             #optional table name - will default to the class name (lowercased). Can provide a database override eg 'mydb.widgets'
  name=TAG                    #first attribute / column name....
  number=INT,100,KEY          #optional default . KEY will generate an index on this field
  date=DATE,KEY,'20000101'    #default and KEY can be swapped, but TYPE must come first
  comment=STR,KEY             #KEY on a STR or TEXT field will generate a text index (ie FULLTEXT)
  ... etc                     #as many as you like
  insert=[dict(name='whatever',number=123),dict(name="something",number=456)] #seed data (columns) for the table

The above definition implies (and requires) that there is a class called Widget in a module called Widget.py in the app code directory, or in the evoke directory.

A schema class can be subclassed, eg to give a different class which uses the same database table (or indeed a different table with the same schema, or a modified version of it)

Note that column names can even be mysqsl keywords, as they are always `quoted`.

IHM April 2007

TEXTKEY: for defining multi-column fulltext indices -  CURRENTLY MODIFICATIONS ARE NOT SUPPORTED (should use mysql definitions in "show keys from ...")

"""

if __name__ == '__main__':
    import sys, os
    sys.path.append(os.path.abspath('..'))
    from lib import TAG, STR, CHAR, TEXT, INT, SMALLINT, TINYINT, FLOAT, DATE, FLAG, MONEY, TIME, REL, BLOB, sql_list, Error
else:
    from evoke.lib import TAG, STR, CHAR, TEXT, INT, SMALLINT, TINYINT, FLOAT, DATE, FLAG, MONEY, TIME, REL, BLOB, sql_list, Error
    from .DB import execute


class SchemaMismatchError(Error):
    "SCHEMA MISMATCH: '%s' column `%s` in table %s is defined in schema as '%s'"


KEY = 'KEY'
now = DATE().sql().strip("'")  #convenience shorthand for initilaising dates


class TEXTKEY(object):
    "for defining multi-column fulltext indices"

    def __init__(self, *columns):
        self.columns = columns


class Schema(object):
    """
  each instance requires:

  table='tablename'
  `fieldname`=TYPE,default,KEY' # for each field, where TYPE in TYPES , KEY and default are optional, and can be swapped in  order
  """
    TYPES = (TAG, STR, CHAR, TEXT, INT, SMALLINT, TINYINT, FLOAT, DATE, FLAG,
             MONEY, TIME, REL, BLOB)
    _v_built = []

    ################################ database maintenance ##########################

    @classmethod
    def build_database(self, database):
        " create or append defined table in MySQL db"
        self.table=getattr(self,'table',self.__name__.lower())
        if self.table.find(".") >= 0:
            self.database, self.table = self.table.split(
                ".", 1)  #allow for database override in table spec
        else:
            self.database = database
        self.tablesql = "`%s`.`%s`" % (self.database, self.table)
        tables = [
            t["Tables_in_%s" % database]
            for t in execute("show tables from `%s`" % database)
        ]
        #    print ">>>>>>>>>>>>>>>>TABLES>>>>>>>>>>>>>>>>>",tables
        self._v_columns, self._v_keys, self._v_textkeys, self._v_multikeys = self.get_columns_and_indices(
        )
        self._v_schema = dict([('uid', INT)] + [
            (k, v[0]) for (k, v) in list(self._v_columns.items())
        ])  # this is for use by data.py
        # create the table or update the table  - unless there are no columns
        if self._v_columns:
            if self.table in tables:
                self.update_table(database)
            # if the table has not already been created (by a parent class), then create it
            elif (("%s.%s") % (database, self.table)) not in self._v_built:
                self.create_table(database)
            self._v_built.append("%s.%s" % (database, self.table))


#      print '>>>>>>>>>>>>v_built=',database,self._v_built

    @classmethod
    def get_columns_and_indices(self):
        ""
        columns = {}
        indices = []
        textindices = []
        multikeys = []
        #    print "NAME================",self.__name__
        #    print "items============",self.__dict__.items()
        #    for k,v in  self.__dict__.items():
        for k in dir(self):
            v = getattr(self, k, None)
            if not v is self.TYPES:
                if isinstance(v, TEXTKEY):
                    multikeys.append((k, v.columns))
                else:
                    if not isinstance(v, tuple):
                        v = [v]
                    else:  #v is a tuple...
                        v = list(v)
                    if v[0] in self.TYPES:
                        if 'KEY' in v:  #we need an index
                            if v[0]._v_mysql_type == "mediumtext":
                                textindices.append(k)
                            else:
                                indices.append(k)
                            v.remove('KEY')
                        if len(v) == 1:  #we have no default
                            v.append(v[0]._v_default)  #put a dummy there
                        columns[k] = v
        return columns, indices, textindices, multikeys

    @classmethod
    def update_table(self, database):
        """
    FOR SAFETY REASONS WE WON'T DELETE OR MODIFY ANY DATA, except keys
    - add any new columns and keys to the table
    - generate a warning if there are any database columns no longer defined in the schema
    - throw an error if there is a mismatch between any database column type and the schema definition
    NOTE - self.insert changes are IGNORED
         - changes to defaults are ignored (O/S - fix this)
         - changes and aditions to TEXTKEY multikeys are IGNORED  (O/S - fix this)
         - dropping of TEXTKEYs doesn't work, as key names are not correct (should use keys from self._v_multikey)  (O/S - fix this)
         - ***** should use "select KEYS from <table>" to get the key data to fix the above. This assumes we have mysql v4.0.2 or better.
           i.e. Key_name, Column_name, and Index_type (BTREE or FULLTEXT):
           keys= Index_type!='FULLTEXT'
           textkeys= Key_name==Column_name and Index_type=='FULLTEXT'
           multikeys= Key_name!=Column_name and Index_type=='FULLTEXT'
    """
        columns = self._v_columns
        keys = self._v_keys
        textkeys = self._v_textkeys
        multikeys = [
            k[1][0] for k in self._v_multikeys
        ]  #mysql just shows the first of the multiple keys in the 'show columns' result
        #    print ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>",multikeys
        columnnames = list(columns.keys())
        tabledata = execute("show columns from %s" % self.tablesql)
        tablecols = []
        tablekeys = []
        tabletextkeys = []
        for i in tabledata:
            name = i['Field']
            if name != 'uid':
                tablecols.append(name)
                if i['Key'] == 'MUL':
                    if i['Type'] == 'mediumtext':
                        tabletextkeys.append(name)
                    else:
                        tablekeys.append(name)
                if name not in columnnames:  #don't delete the column - safety first....
                    print(
                        "WARNING: column `%s` in table %s is not defined in schema"
                        % (name, self.tablesql))
                elif i['Type'] != columns[name][0]._v_mysql_type:
                    raise SchemaMismatchError(i['Type'], name, self.tablesql,
                                              columns[name][0]._v_mysql_type)
        sql = []
        for i in columnnames:
            if i not in tablecols:
                v = columns[i]
                sql.append("add `%s` %s default %s" %
                           (i, v[0]._v_mysql_type,
                            v[1] is None and "NULL" or ('"%s"' % v[1])))
        for i in keys:
            if i not in tablekeys:
                sql.append("add KEY (`%s`)" % i)
        for i in textkeys:
            if i not in tabletextkeys:
                sql.append("add FULLTEXT (`%s`)" % i)
        for i in tablekeys:
            if i not in (keys + multikeys):
                sql.append("drop KEY `%s`" % i)
        for i in tabletextkeys:
            if i not in (textkeys + multikeys):
                sql.append("drop KEY `%s`" % i)
        if sql:
            sql = "ALTER TABLE %s\n%s" % (self.tablesql, ',\n'.join(sql))
            print(sql)
            for i in sql.split(';'):
                if i:
                    execute(i)

    @classmethod
    def create_table(self, database):
        """
    generate SQL code to create the table for this schema, and seed it with initial row inserts

    will simply ignore any invalidly-specified attributes
    """

        def quoted(keys):
            ks = [('`%s`' % k) for k in keys]
            #      print ">>>>>>>>>>>>>>>>>>>>>>>>>>>>",ks
            return len(ks) > 1 and str(tuple(ks)).replace("'", "").replace(
                '"', "") or ('(%s)' % ks[0])

        columns = self._v_columns
        keys = self._v_keys
        textkeys = self._v_textkeys
        multikeys = self._v_multikeys
        data = {
            'table':
            self.tablesql,
            'columns':
            ",\n".join(("`%s` %s default %s" % (k, v[
                0]._v_mysql_type, v[1] is None and "NULL" or ('"%s"' % v[1])))
                       for (k, v) in list(columns.items())),
            'keys':
            ",\n".join(
                ['PRIMARY KEY (uid)'] + [("KEY (`%s`)" % k) for k in keys] +
                [("FULLTEXT (`%s`)" % k) for k in textkeys] +
                [("FULLTEXT %s %s" % (k, quoted(v))) for (k, v) in multikeys])
        }
        sql = "CREATE TABLE %(table)s(\nuid int(11) NOT NULL auto_increment,\n%(columns)s,\n%(keys)s\n) ENGINE=InnoDB CHARSET=utf8;" % data
        if hasattr(self, 'insert'):
            if isinstance(self.insert, type({})):  #allow for single item
                self.insert = [self.insert]  #convert to list
            for row in self.insert:
                sql += "\nINSERT INTO %s %s VALUES %s;" % (
                    self.tablesql,
                    str(sql_list(list(row.keys()))).replace("'", "`"),
                    sql_list(list(row.values())))
        if __name__ == '__main__':
            return sql
        print(sql)
        for i in sql.split(';'):
            if i:
                execute(i)


def create_database(database):
    "called by app.py, for each app"
    databases = [d['Database'] for d in execute("show databases")]
    if database not in databases:
        sql = "CREATE DATABASE `%s` CHARSET=utf8" % database
        print(sql)
        execute(sql)


def test():
    class Test(Schema):
        table = 'goods'
        amount = MONEY, 0
        ok = FLAG
        name = TAG, ""
        code = TAG, "", KEY
        ref = INT, KEY
        year = INT, 2000, KEY
        when = DATE
        took = TIME
        stat = INT, 1
        txt = STR, KEY
        insert = [
            dict(uid=3, amount=500, ok='Y'),
            dict(amount=500, when="20070707")
        ]

    print(Test.create())


if __name__ == '__main__':
    test()
