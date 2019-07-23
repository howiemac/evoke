"""
evoke MySQL database interface: 

(Christopher J Hurst 2003 onwards)  
(modified Ian Howie Mackenzie Nov 2005 onwards)
"""

from .schema import *
from evoke.lib import Permit
from time import time
from pickle import loads, dumps


class DataObject(object):
    "Basic Model of a Persistent Data Object"
    __implements__ = 'DataObjectInterface'  # apart from the class methods

    def __init__(self, fields):
        "fields - [track changes for these names]"
        self._v_changed = {}
        self._v_fields = fields

    def __setattr__(self, name, value):
        "keep track of changed atts"
        object.__setattr__(self, name, value)
        if '_v_' not in name and name in self._v_fields:
            self._v_changed[name] = True

    def __delattr__(self, name):
        "keep track of changed atts"
        object.__delattr__(self, name)
        if name in self._v_changed:
            del self._v_changed[name]

    def update(self, args):
        "Update Object Fields"
        x = [
            setattr(self, k, v) for k, v in list(args.items())
            if k in self._v_fields
        ]

    def flush(self, only=[]):
        "return dictionary of changes, then reset self._v_changed"
        #print "flush2"
        res = {}
        changed = self._v_changed
        # filter by only if present
        if only:
            #      print ">>>>>>>>>>>>>> flushing only ",only
            keys = [i for i in list(changed.keys()) if i in only]
            d = {}
            for k in keys:
                d[k] = changed[k]
            changed = d
        # set object atts
        for i in changed:
            res[i] = getattr(self, i)
        self._v_changed = {}
        return res


class SQLDataObject(DataObject):
    "MySQL backed persistence"
    _v_schema = {}
    _v_fields = []

    # Initialisation
    def __init__(self):
        self._v_changed = {}

    def __setattr__(self, name, value):
        "for object attributes, create a new instance with the new value, thus converting it as required"
        if name in self._v_schema:
            #      print "CLASS",self._v_schema[name].__name__
            #      print "SET====",name,value
            value = self._v_schema[name](
                value
            )  #create an instance of the relevant type, thus processing value where required

            # include reference back to self
            # initially so we can create a back reference to TEXT objects
            # so they can be identified at translation time
            value._v_instance = self
#      print "  TO:",value
        DataObject.__setattr__(self, name, value)

    def __getattribute__(self, name):
        ""
        value = object.__getattribute__(self, name)
        if value and isinstance(value, REL):
            value = self.__class__.__dict__[name.capitalize()].get(value)
            object.__setattr__(
                self, name, value
            )  #bypass DataObject here, as we are not changing anything
        return value

    def sql(self):
        "for use by REL attributes that have been substituted (by __getattribute__ above) with evoke objects (which have RemoteDataObject as a mix-in)"
        return self.uid

    def flush(self, only=[]):
        "save changes to db, filtered by only"
        changes = DataObject.flush(self, only=only)
        if not changes:
            return
#    print ">>>> changes >>>>>",changes.items(),
#    for (k,v) in changes.items():
#      print ">>>> change >>>>>",k,v,type(v),repr(v),str(v)
#fields = self.quoted_pairs(changes.items())
# fix order of changes dict by converting to a list of tuples

# handle REL objects
        for k in changes:
            if isinstance(changes[k], REL):
                changes[k] = int(changes[k])
            if hasattr(changes[k], 'uid'):
                changes[k] = changes[k].uid
            # and DATE objects
            if isinstance(changes[k], DATE):
                changes[k] = changes[k].sql(quoted=False)

        items = list(changes.items())

        fields = ', '.join(("`%s`=%%s" % k) for k, v in items)
        # list of fields ready for MySQLdb parameters.
        # self.uid always last in the list
        values = tuple([v for k, v in items] + [self.uid])

        sql = "update %s set %s where uid=%%s" % (self.table, fields)
        #    print ">>>> SQL >>>>>",r"%s" % sql
        #print sql
        execute(sql, values)

    def quoted_pairs(self, items, op='='):
        "quote key,value pairs according to _v_schema "
        fields = []
        for k, v in items:
            if not isinstance(v, (Schema, DataObject)):
                v = self._v_schema[k](v)
            fields.append("`%s` %s %s" % (k, op, v.sql()))
        return ", ".join(fields)

    @classmethod
    def sql_quoted_pairs(self, items, op='=', link=', '):
        "return sql and list of args suitable for mySQLdb arg substitution"
        fields = []
        sqlargs = []
        for k, v in items:
            if not isinstance(v, (Schema, DataObject)):
                v = self._v_schema[k](v)
            fields.append("`%s` %s %%s" % (k, op))
            sqlargs.append(v.sql(quoted=False))
        sql = (" %s " % link).join(fields)
        return sql, sqlargs


# Error Classes for MassProducedSQLDataObject
class RecordNotFoundError(Error):
    "%s with uid=%d not found"


class RecordInUseError(Error):
    "%s with uid=%d is still in use"


class ListParameterConflictError(Error):
    "You shouldn't call list with a non-default what parameter when asObjects is true."


class SQLArgumentMismatchError(Error):
    "Number of arguments does not match number required by SQL statement."


class InvalidOrderFieldError(Error):
    "Field in ORDER BY clause not found for this object."


class InvalidOrderDirectionError(Error):
    "Direction in ORDER BY clause not valid."


class Subscriptable(type):
    """metaclass to allow a class to be subscripted"""

    def __getitem__(cls, x):
        return cls.get(x)


class MassProducedSQLDataObject(SQLDataObject, metaclass=Subscriptable):
    "SQL data object for use with makeDataClass"

    @classmethod
    def ns_table(cls):
        ""
        db, tbl = cls.table.replace('`', '').split('.', 1)
        if hasattr(cls, 'ns'):
            s = '`%s`.`%s`' % (db + '_' + cls.ns, tbl)
            #print 'ns=', s
        else:
            s = cls.table
        return s

    @classmethod
    def exists(cls, uid):
        "return true if a record with this uid exists"
        sql = 'select * from %s where uid=%%s' % (cls.table, )
        _data = execute(sql, (uid, ))
        return len(_data) > 0

    def get(cls, uid, data={}):
        "get database record and return it as an object"
        if not data:
            # send uid as a proper MySQLdb parameter
            sql = 'select * from %s where uid=%%s' % (cls.table, )
            _data = execute(sql, (uid, ))
            if not _data:
                raise RecordNotFoundError(cls.table, uid)
            data = _data[0]
        ob = cls()
        #create the instance attributes
        #    ob.update(data)
        for k, v in list(data.items()
                         ):  # as per update(data) but allow extra data fields
            setattr(ob, k, v)
        # clear the change queue, so the defaults are not needlessly updated
        ob._v_changed = {}

        # call __init__
        if hasattr(cls.__bases__[0], '__init__'):
            cls.__bases__[0].__init__(ob)

        return ob

    __getitem__ = classmethod(get)  #### works for instance, but not class :s
    get = classmethod(get)
    # allow for get overrides
    __get__ = get

    @classmethod
    def tryget(cls, uid):
        "a bombproof get()"
        try:
            return get(cls, uid)
        except:
            return None

    @classmethod
    def new(cls):
        "create a new database record and return it as a an object"
        id = execute('insert into %s() values()' % cls.table)
        return cls.get(id)

    def delete(self, uid=0):
        "remove self (or the database record with given uid: uid is for retro compat.)"
        sql = 'delete from %s where uid=%%s' % (self.table, )
        execute(sql, (uid or self.uid, ))

    def clone(self):
        "create a clone of self, flush it,  and return it"
        ob = self.new()
        for k in (i for i in self._v_fields if i != 'uid'):
            setattr(ob, k, getattr(self, k))
        ob.flush()
        return ob

    def all_change(self):
        "mark all fields as changed where their value is not None"
        changed = dict((i, True) for i in self._v_fields
                       if getattr(self, i) is not None)
        self._v_changed.update(changed)

    def pickle(self):
        "return pickled representation of the object's fields"
        return dumps(dict((k, getattr(self, k)) for k in self._v_fields))

    def pickle_update(self, pkl):
        "update object with pickled dict"
        d = loads(pkl)
        self.update(d)

    #### Test Purposes
    @classmethod
    def X_list(self, *a, **k):
        "Testing purposes - run old and new variations, compare results and time taken"
        k['_debug'] = 1
        # old version
        oldstart = time()
        oldres = self.old_list(*a, **k)
        oldtime = time() - oldstart

        # new version
        newstart = time()
        newres = self.old_list(*a, **k)
        newtime = time() - newstart

        #print "LIST: old=%d / %.4f new=%d / %.4f" % (len(oldres), oldtime, len(newres), newtime)
        #assert oldres==newres, "Old and new list give different results"
        return newres

    @classmethod
    @Permit('no way')
    def old_list(cls,
                 asObjects=True,
                 sql='',
                 like={},
                 isin={},
                 orderby='',
                 where='',
                 limit='',
                 pager=-1,
                 pagelength=20,
                 what='*',
                 _debug=False,
                 **criteria):
        """return list of objects (if obs) or data filtered by these criteria
       sql with asObjects=True requires 'select *' to give fully valid objects, so 'what' should not be used with asObjects=False  
    """
        ob = cls()  # we need an instance of our class for quoted_pairs
        # sql overrides all other parameters
        if not sql:
            pairs = ob.quoted_pairs(list(criteria.items())).replace(
                ',', ' and ')
            likes = ob.quoted_pairs(
                list(like.items()), op='like').replace(',', ' and ')
            orderby = orderby and 'order by %s' % orderby or ''
            limit = limit and 'limit %s' % limit or ''
            # page overrides limit
            if pager != -1:
                start = int(pager) * int(pagelength)
                limit = 'limit %d,%d' % (start, int(pagelength))

            # for in clauses we wildly assume that this will be passed a list of strings or ints...
            ins = ' and '.join('`%s` in %s' % (k, sql_list(v))
                               for k, v in list(isin.items()))
            sql = "select %s from %s %s %s %s %s" % (
                what, cls.table,
                (pairs or likes or ins or where) and 'where' or '',
                ' and '.join(i for i in (pairs, likes, ins, where)
                             if i), orderby, limit)
        if _debug:
            #print "LIST:", sql
            pass
        data = execute(sql)
        if asObjects:
            data = [cls.get(i['uid'], data=i) for i in data]
        del ob, cls
        return data

    #list.permit='no way'  # disable direct web access
    #list=classmethod(list)

    @classmethod
    @Permit('no way')
    def list(cls,
             asObjects=True,
             sql='',
             sqlargs=(),
             like={},
             isin={},
             orderby='',
             where='',
             limit='',
             pager=-1,
             pagelength=20,
             what='*',
             _debug=False,
             **criteria):
        """return list of objects (if obs) or data filtered by these criteria
       sql with asObjects=True requires 'select *' to give fully valid objects, so 'what' should not be used with asObjects=False  
   
    Parameters:
    cls = class of current object
    asObjects 
      = False: return a list of dictionaries 
      = True: return list of objects of the current class
    sql = full sql query. Parameters to be sent as tuple via sqlargs  
    sqlargs = tuple containing values to substitute into sql/where parameters.
    like = dict of form {fieldname:matchpattern}
      maps to sql 'fieldname LIKE "matchpattern" and otherfieldname LIKE "othermatchpattern"'
    isin = dict of form {fieldname:list-of-values}
    orderby = sql ORDER BY clause
    where = sql WHERE clause. Parameters to be sent as tuple via sqlargs  
    limit = sql LIMIT clause
    pager = Start page of results divided by page length. Overrides limit parameter
    pageLength = length of pages - use in combination with pager parameter
    what = fields to be returned by query  DEPRECATED CJH 20130408
      we can't use sql args with field names so this is in practise a minute risk..
    _debug = if True print prepared sql statement 
    **criteria = remaining field value pairs map to WHERE statement assersions which must all be true

      eg.  self.list(x=5,y='something') -> 'WHERE x=5 and y="something"'
    """
        # make sure we don't combine non-default 'what' with asObjects=True
        if asObjects and what != '*':
            raise ListParameterConflictError

    #  # `what` parameter DEPRECATED CJH 20130408  ( WHY? IHM 20141111)
    #  if what !='*':
    #    print "DEPRECATED: evoke.data.list `what` parameter is deprecated"

    # sql overrides all other parameters except sqlargs
        if sql:
            # no further action required
            pass
        else:
            # build up sql from criteria
            if where:
                sqlparts = [where]
                sqlargs = list(sqlargs)
            else:
                sqlparts = []
                sqlargs = []

            # criteria
            if criteria:
                #       print">>>>>> criteria items >>>>>"
                #       for k,v in criteria.items():
                #         print k,v
                criteria_sql, criteria_args = cls.sql_quoted_pairs(
                    list(criteria.items()), link='and')
                sqlparts.append(criteria_sql)
                sqlargs += criteria_args

            # like
            if like:
                like_sql, like_args = cls.sql_quoted_pairs(
                    list(like.items()), op='like', link='and')
                like_sql = like_sql.replace("%", "%%").replace(
                    '%%s', '%s')  # double the % wildcard (but not any %s)
                sqlparts.append(like_sql)
                sqlargs += like_args

            # isin
            for isin_field, isin_values in list(isin.items()):
                if not isin_values:
                    continue
                isin_sql = ' %s  in (%s)' % (
                    isin_field, ', '.join(['%s'] * len(isin_values)))
                isin_args = list(isin_values)
                sqlparts.append(isin_sql)
                sqlargs += isin_args

            # orderby - as field names can't be used in mySQLdb argument
            # substitution we make sure this only includes field names
            # optionally quoted with ``, separated by commas and optionally
            # including desc.
            orderby = cls.parse_orderby(orderby)
            if orderby:
                orderby = " ORDER BY %s" % orderby

            # limit and paging
            if pager == -1:
                limit = limit and 'LIMIT %s' % limit or ''
            else:
                start = int(pager) * int(pagelength)
                limit = 'LIMIT %d,%d' % (start, int(pagelength))

            # We should have a complete query.  Convert args into a tuple
            # and test we have the right number of substitutions
            where = sqlparts and 'where' or ''
            whereclauses = (' and '.join(sqlparts)).replace("%", "%%").replace(
                '%%s', '%s')  # double the % wildcard (but not any %s)
            sql = 'select %s from %s %s %s %s %s' % (what, cls.ns_table(),
                                                     where, whereclauses,
                                                     orderby, limit)
            sqlargs = tuple(sqlargs)
            try:
                nowt = sql % sqlargs
            #except TypeError:
            except:
                print("data.list unmatched arguments. There are %d arguments."
                      % len(sqlargs))
                print(sql)
                print(sqlargs)
                raise SQLArgumentMismatchError

        # ready to go
        # optionally show our query.
        if _debug:
            print("LIST:", sql)
            print("LIST:", sqlargs)
            pass
        # execute query
        data = execute(sql, sqlargs)

        #    if _debug:
        #      print "DATA COUNT:", len(data)

        # convert to objects if required
        if asObjects:
            data = [cls.get(i['uid'], data=i) for i in data]

        return data

    @classmethod
    def parse_orderby(self, orderby):
        "sanitise sql order by clause - can't use normal mySQLdb parameter substitution as it quotes field names"
        #    print "parse_orderby not implemented"
        return orderby

        parts = [i.strip().split(' ', 1) for i in orderby.split(',')]
        clauses = []

        for part in parts:
            field = part[0].strip().replace('`', '')
            if not field:
                continue
            direction = (len(part) > 1 and part[1] or '').strip().upper()

            # field should be in self._v_fields
            if field not in self._v_fields:
                print("invalid field", field)
                raise InvalidOrderFieldError

            # direction should be empty or one of ASC|DESC
            if direction not in ('', 'ASC', 'DESC'):
                raise InvalidOrderDirectionError

            clauses.append('`%s`%s%s' % (field, direction and ' ' or '',
                                         direction))
        return ', '.join(clauses)

    @classmethod
    def count(cls,
              like={},
              isin={},
              orderby='',
              where='',
              limit='',
              **criteria):
        """return count of data filtered by these criteria
    """
        return cls.list(
            asObjects=False,
            like=like,
            isin=isin,
            orderby=orderby,
            where=where,
            limit=limit,
            what="count(uid)",
            **criteria)[0]["count(uid)"]

    @classmethod
    def list_int(cls,
                 item='uid',
                 like={},
                 isin={},
                 orderby='',
                 where='',
                 limit='',
                 **criteria):
        """return list of integers from item (column) filtered by these criteria
    """
        return [
            int(i[item])
            for i in cls.list(
                asObjects=False,
                sql="",
                like=like,
                isin=isin,
                orderby=orderby,
                where=where,
                limit=limit,
                what="`%s`" % item,
                **criteria)
        ]

    @classmethod
    def max(cls,
            item='uid',
            like={},
            isin={},
            orderby='',
            where='',
            limit='',
            **criteria):
        """return max of item (column) filtered by these criteria
    """
        what = 'max(%s)' % item
        return cls.list(
            asObjects=False,
            sql="",
            like=like,
            isin=isin,
            orderby=orderby,
            where=where,
            limit=limit,
            what=what,
            **criteria)[0][what]

    @classmethod
    def min(cls,
            item='uid',
            like={},
            isin={},
            orderby='',
            where='',
            limit='',
            **criteria):
        """return min of item (column) filtered by these criteria
    """
        what = 'min(%s)' % item
        return cls.list(
            asObjects=False,
            sql="",
            like=like,
            isin=isin,
            orderby=orderby,
            where=where,
            limit=limit,
            what=what,
            **criteria)[0][what]

    @classmethod
    def sum(cls,
            item='uid',
            like={},
            isin={},
            orderby='',
            where='',
            limit='',
            **criteria):
        """return min of item (column) filtered by these criteria
    """
        what = 'sum(%s)' % item
        return cls.list(
            asObjects=False,
            sql="",
            like=like,
            isin=isin,
            orderby=orderby,
            where=where,
            limit=limit,
            what=what,
            **criteria)[0][what]


# export / import

    def for_export(self, extras=[]):
        "convert object to a dictionary (for pickling) - include only standard fields, plus given extras"
        return dict((k, v) for k, v in list(self.__dict__.items())
                    if k in self._v_fields or k in extras)

    # Dummy methods for leak testing
    @classmethod
    def mundane(self, *a, **k):
        return 'OK'


#renote removed for now - see data_remote.py


def makeDataClass(Schema):
    "Set up a custom SQL data class"
    #dbc = Connection('makeDataClass')
    #db = Cursor(dbc)
    db = None
    # we only need to get the field info once per class
    data = { #'db':db
       'table':Schema.tablesql
      , '_v_schema': Schema._v_schema
      , '_v_fields': list(Schema._v_schema.keys())
      , '_v_textkeys': Schema._v_textkeys
      , '_v_multikeys': Schema._v_multikeys
      }
    #    return type(Schema.table.capitalize(), (RemoteSQLDataObject,), data)
    return type(Schema.table.capitalize(), (MassProducedSQLDataObject, ),
                data)  #remote disabled for now


# TESTS
if __name__ == '__main__':
    # Data Object
    c = DataObject(['camelid', 'ox'])
    # let's start with a clear run
    assert c.flush() == {}, "Flush should be empty"
    # Check for a valid change
    c.camelid = 'llama'
    assert c.flush() == {'camelid': 'llama'}, "setattr noworks"
    assert c.flush() == {}, "Flush should be empty"
    # _v_ attributes don't register change
    c._v_gonewiththewind = 'Frankly'
    assert c.flush() == {}, "Flush should be empty"
    c.gnu = 'GNU'
    assert c.flush() == {}, "Flush should be empty"
    c.update({'ox': 'OXEN'})
    assert c.flush() == {'ox': 'OXEN'}, "update noworks"
    c.update({'ox': 'OXEN'})
    del c.ox
    assert c.flush() == {}, "Flush should be empty"

    # filter flush by only
    c.camelid = 'Alpaca'
    res = c.flush(only=['bison'])
    assert res == {}, "Flush should be empty"
