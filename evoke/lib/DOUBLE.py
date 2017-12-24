""" Large number handling
"""
from .FLOAT import FLOAT


class DOUBLE(FLOAT):
    """double width float"""
    _v_mysql_type = "double"


class HUGEDECIMAL(FLOAT):
    """Numbers large enough for CR.  Maximum is 65 digits.  A Trillion is 13 digits 33 digits . 32 digits is enough"""
    _v_mysql_type = "DECIMAL(33,32)"

