from typing import Union
from enum import Enum


class JTypes(Enum):
    STRING = "string"
    NUMBER = "number"
    INTEGER = "integer"
    BOOLEAN = "boolean"
    OBJECT = "object"
    ARRAY = "array"
    ONEOF = "oneOf"
    ANYOF = "anyof"
    ALLOF = "allof"
    CONST = "const"
    ENUM = "enum"


JsonType = Union[
    str,
    int,
    float,
    dict,
    bool,
    list
]


_SIMPLE_TYPES = [
    JTypes.STRING,
    JTypes.INTEGER,
    JTypes.NUMBER,
    JTypes.BOOLEAN,
    JTypes.OBJECT,
    JTypes.ARRAY
]


def simple_type(_type: JTypes) -> bool:
    return (_type in _SIMPLE_TYPES)
