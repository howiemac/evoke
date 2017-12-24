"""
evoke library type interface


(IHM April 2017)
"""

#make everything visible as evoke.lib.types
from .INT import INT, SMALLINT, TINYINT
from .DOUBLE import DOUBLE, HUGEDECIMAL
from .FLOAT import FLOAT
from .STR import TAG, STR, CHAR
from .DATE import DATE
from .TEXT import TEXT
from .REL import REL
from .FLAG import FLAG
from .BLOB import BLOB
from .library import sql_list, safeint, httpDate, page, prev, next, url_safe, elapsed, delf
from .email import email
from .error import Error
from .permit import Permit
from .bug import send_error
