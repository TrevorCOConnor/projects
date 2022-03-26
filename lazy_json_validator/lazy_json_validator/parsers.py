# Standard Modules
from typing import Optional, Union, Any, get_args, cast

# Local Modules
from .json_types import JTypes, JsonType
from .validation_errors import SchemaError, ValidationError

_PYTHON_JSON_TYPE_MAP = {
    "dict": "object",
    "list": "array",
    "str": "string",
    "int": "number",
    "float": "number",
    "bool": "boolean"
}


def schema_enum(schema_obj: dict) -> Optional[list]:
    """ If present, retrieves and validates enum list from schema object. """
    enums = schema_obj.get("enum")
    # No enums
    if enums is None:
        return enums
    # List
    elif isinstance(enums, list):
        # Validate individual items
        for item in enums:
            if not isinstance(item, get_args(JsonType)):
                raise SchemaError(
                    message=("'enum' values must be valid json types. "
                             f"Type '{type(item).__name__}' is not valid.")
                )
        return enums
    # Invalid type
    else:
        raise SchemaError(
            message="'enum' must be of type 'list'."
        )


def schema_const(schema_obj: dict) -> Optional[JsonType]:
    """ If present, retrieves and validates const value in schema object. """
    const = schema_obj.get("const")
    if isinstance(const, get_args(JsonType)):
        return const
    else:
        raise SchemaError(
            message=(f"'const' of type '{type(const).__name__}' is not "
                     "an acceptable json type.")
        )


def schema_min(schema_obj: dict) -> Optional[Union[int, float]]:
    """ Retrieves and validates minimum value from schema object. """
    optional_min = schema_obj.get("minimum")
    if (
        isinstance(optional_min, int) or
        isinstance(optional_min, float) or
        optional_min is None
    ):
        return optional_min
    else:
        raise SchemaError(
            message="Minimum must be a 'number'."
        )


def schema_max(schema_obj: dict) -> Optional[Union[int, float]]:
    """ Retrieves and validates maximum value from schema object. """
    optional_max = schema_obj.get("maximum")
    if (
        isinstance(optional_max, int) or
        isinstance(optional_max, float) or
        optional_max is None
    ):
        return optional_max
    else:
        raise SchemaError(
            message="Minimum must be a 'number'."
        )


def schema_required_fields(schema_obj: dict) -> list[str]:
    """ Fetches required fields with a default of an empty list.
        Raises errors if "required" is not a list of strings.
    """
    required_fields = schema_obj.get("required", [])
    if isinstance(required_fields, list):
        non_strings = [x for x in required_fields if not isinstance(x, str)]
        count = len(non_strings)
        if count == 0:
            return required_fields
        elif count == 1:
            raise SchemaError(
                message=(f"{non_strings[0]} in 'required' "
                         "is not of type 'string'.")
            )
        else:
            raise SchemaError(
                message=(f"{non_strings} in 'required' "
                         "are not of type 'string'.")
            )
    else:
        raise SchemaError(
            message="'required' should be of type 'list'."
        )


def validate_property(prop: tuple[Any, Any]):
    key, value = prop
    if not isinstance(key, str):
        raise SchemaError(
            message=f"Property name {key} should be of type 'string'."
        )
    if not isinstance(value, dict):
        raise SchemaError(
            message=("Property must be defined in object format. "
                     f"Type {type(value)} not valid.")
        )


def schema_properties(schema_obj: dict) -> list[tuple[str, dict]]:
    """ Fetches properties from schema object. """
    properties = schema_obj.get("properties", dict())
    if isinstance(properties, dict):
        props = list(properties.items())
        for item in props:
            validate_property(item)
        valid_props = cast(list[tuple[str, dict]], props)
        return valid_props
    else:
        raise SchemaError(
            message="'properties' should be of type 'dict'"
        )


def schema_items(schema_obj: dict) -> dict:
    """ Fetches items schema from schema object. """
    items = schema_obj.get("items", dict())
    if isinstance(items, dict):
        return items
    else:
        raise SchemaError(
            message="'items' should be of type 'dict'."
        )


def schema_type(schema_obj: dict) -> JTypes:
    """ Fetches type from schema object. """
    if "type" in schema_obj:
        _type = schema_obj["type"]
        if isinstance(_type, str):
            upper_type = _type.upper()
            if upper_type in JTypes.__members__:
                return JTypes[upper_type]
            else:
                raise SchemaError(
                    message=f"Type '{_type}' is not supported."
                )
        else:
            raise SchemaError(
                message="'type' should be a string value."
            )
    elif "oneOf" in schema_obj:
        return JTypes.ONEOF
    elif "anyOf" in schema_obj:
        return JTypes.ANYOF
    elif "allOf" in schema_obj:
        return JTypes.ALLOF
    elif "const" in schema_obj:
        return JTypes.CONST
    elif "enum" in schema_obj:
        return JTypes.ENUM

    raise SchemaError(
        message="No type found in schema object"
    )


def json_type(json_obj: JsonType) -> JTypes:
    """ Fetches type from JsonType. """
    python_type = type(json_obj).__name__
    if python_type in _PYTHON_JSON_TYPE_MAP:
        json_type = _PYTHON_JSON_TYPE_MAP[python_type]
        return JTypes[str.upper(json_type)]
    else:
        raise ValidationError(
            message=f"Type '{python_type}' is not a valid json type."
        )

    json_type = _PYTHON_JSON_TYPE_MAP[python_type]

    return JTypes[str.upper(json_type)]
