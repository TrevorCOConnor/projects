# Standard modules
from typing import Optional, Iterable, cast, Callable
from functools import partial
from itertools import chain
import operator as op

# Local modules
from .json_types import JTypes, JsonType
from .validation_errors import (
    InternalError, SchemaError,
    ValidationError, TypeValidationError, OneOfValidationError,
    AllOfValidationError, AnyOfValidationError
)
from .aux import compose, filter_out_empty_lists, lift_optional
from . import parsers


Validator = Callable[[dict, JsonType], list[ValidationError]]


def wrap_validate(
    outer_validate: Validator
) -> Callable[[Validator], Validator]:
    """ Composes a validation function with another validator function via a
        wrapper.
    """
    def inner(inner_validate: Validator):
        def eval_validators(schema_obj: dict, json_obj: JsonType):
            outer_errors = outer_validate(schema_obj, json_obj)
            inner_errors = inner_validate(schema_obj, json_obj)

            return outer_errors + inner_errors
        return eval_validators

    return inner


def validate_enum(
    schema_obj: dict, payload_obj: JsonType
) -> list[ValidationError]:
    """ Validates against enum list, if present. """
    enum = parsers.schema_enum(schema_obj)
    # No enum
    if enum is None:
        return []
    # Acceptable enum
    elif payload_obj in enum:
        return []
    # Not accepted value
    else:
        return [
            ValidationError(
                message=f"Value '{payload_obj}' is not one of {enum}."
            )
        ]


def validate_const(
    schema_obj: dict, payload_obj: JsonType
) -> list[ValidationError]:
    const_value = parsers.schema_const(schema_obj)
    if payload_obj == const_value:
        return []
    else:
        return [
            ValidationError(
                message=(f"Value '{payload_obj}' does not equal constant "
                         f"'{const_value}'.")
            )
        ]


@wrap_validate(validate_enum)
def validate_oneOf(
    schema_obj: dict, payload_obj: JsonType
) -> list[ValidationError]:
    """ Validates object of type 'oneOf' by validating against all nested
        types and comparing results to see that exactly one object had no
        errors.
    """
    schema_objs = schema_obj.get("oneOf", [])
    error_lists = list(map(
        lambda x: validate_by_type(x, payload_obj),
        schema_objs
    ))
    num_errors = len(filter_out_empty_lists(error_lists))
    num_objs = len(schema_objs)

    # validates
    if num_errors == (num_objs - 1):
        return []
    # no matching
    elif num_errors == num_objs:
        validation_error = OneOfValidationError(
            error_lists=error_lists
        )
        return [validation_error]
    # multiple matches
    else:
        indices = list(map(
            op.itemgetter(0),
            filter(compose(partial(op.eq, 0), len, op.itemgetter(1)),
                   enumerate(error_lists))
        ))
        validation_error = OneOfValidationError(
            indices=indices
        )
        return [validation_error]


@wrap_validate(validate_enum)
def validate_anyOf(
    schema_obj: dict, payload_obj: JsonType
) -> list[ValidationError]:
    """ Validates object of type 'anyOf' by validating against all nested
        types and comparing results to see that at least one object had no
        errors.
    """
    schema_objs = schema_obj.get("anyOf", [])
    error_iter = map(
        lambda x: validate_by_type(x, payload_obj),
        schema_objs
    )
    error_lists = filter_out_empty_lists(error_iter)
    num_errors = len(error_lists)

    if num_errors == len(schema_objs):
        validation_error = AnyOfValidationError(
            error_lists=error_lists
        )

        return [validation_error]
    else:
        return []


@wrap_validate(validate_enum)
def validate_allOf(
    schema_obj: dict, payload_obj: JsonType
) -> list[ValidationError]:
    """ Validates object of type 'allOf' by validating against all nested
        types and comparing results to see that no object had any errors.
    """
    schema_objs = schema_obj.get("allOf", [])
    error_iter = map(
        lambda x: validate_by_type(x, payload_obj),
        schema_objs
    )
    error_lists = filter_out_empty_lists(error_iter)

    if len(error_lists) > 0:
        validation_error = AllOfValidationError(
            error_lists=error_lists
        )

        return [validation_error]
    else:
        return []


def validate_by_type(
    schema_obj: dict, payload_obj: JsonType
) -> list[ValidationError]:
    """ Determines type and calls corresponding type function. """
    _type = parsers.schema_type(schema_obj)
    if _type == JTypes.OBJECT:
        return validate_object(schema_obj, payload_obj)
    elif _type == JTypes.ARRAY:
        return validate_array(schema_obj, payload_obj)
    elif _type == JTypes.STRING:
        return validate_string(schema_obj, payload_obj)
    elif _type == JTypes.NUMBER:
        return validate_number(schema_obj, payload_obj)
    elif _type == JTypes.INTEGER:
        return validate_number(schema_obj, payload_obj)
    elif _type == JTypes.BOOLEAN:
        return validate_boolean(schema_obj, payload_obj)
    elif _type == JTypes.ONEOF:
        return validate_oneOf(schema_obj, payload_obj)
    elif _type == JTypes.ALLOF:
        return validate_allOf(schema_obj, payload_obj)
    elif _type == JTypes.ANYOF:
        return validate_anyOf(schema_obj, payload_obj)
    elif _type == JTypes.ENUM:
        return validate_enum(schema_obj, payload_obj)
    elif _type == JTypes.CONST:
        return validate_const(schema_obj, payload_obj)
    else:
        raise InternalError(message=f"Type {_type} not handled.")


def simple_validate_type(
    schema_obj: dict, payload_obj: JsonType
) -> Optional[ValidationError]:
    """ Simple validation to see if payload type matches json type. """
    stype = parsers.schema_type(schema_obj)
    try:
        payload_type = parsers.json_type(payload_obj)
    except ValidationError as e:
        return e
    if payload_type is not stype:
        type_error = TypeValidationError(
            schema_type=stype,
            payload_type=payload_type
        )
        return type_error

    return None


def validate_required_field(
     payload_dict: dict, field_name: str
) -> Optional[ValidationError]:
    """ Validates field name is found in payload dict. """
    if field_name not in payload_dict:
        return ValidationError(
            message=f"Required field '{field_name}' not found."
        )
    else:
        return None


def validate_required_field_list(
    schema_obj: dict, payload_obj: dict
) -> list[ValidationError]:
    """ Valiades required fields are found in payload dict. """
    required_fields = parsers.schema_required_fields(schema_obj)
    error_iter: Iterable[Optional[ValidationError]] = map(
        partial(validate_required_field, payload_obj), required_fields
    )
    optional_errors: list[ValidationError] = list(filter(
        compose(op.not_, partial(op.is_, None)), error_iter
    ))
    errors = cast(list[ValidationError], optional_errors)

    return errors


def validate_property(
    payload_dict: dict, prop: tuple[str, dict]
) -> list[ValidationError]:
    """ Validates payload dict against invidual property. Returns error if the
        property exists, but does not match the specified type.
    """
    name, obj = prop
    if name in payload_dict:
        try:
            errors = validate_by_type(obj, payload_dict[name])
        except SchemaError as e:
            e.update_path(name)
            raise e
        updated_errors = list(map(
            op.methodcaller("update_path", name), errors
        ))
        return updated_errors
    else:
        return []


def validate_property_list(
    schema_obj: dict, payload_dict: dict
) -> list[ValidationError]:
    """ Validates payload dict against properties. """
    properties = parsers.schema_properties(schema_obj)
    error_iter = map(partial(validate_property, payload_dict), properties)
    errors = list(chain.from_iterable(error_iter))

    return errors


@wrap_validate(validate_enum)
def validate_object(
    schema_obj: dict, payload_obj: JsonType
) -> list[ValidationError]:
    """ Validates json against schema of type "object". """
    type_error = simple_validate_type(schema_obj, payload_obj)
    if type_error:
        return [type_error]
    else:
        payload_dict = cast(dict, payload_obj)
        req_errs = validate_required_field_list(schema_obj, payload_dict)
        prop_errs = validate_property_list(schema_obj, payload_dict)

        return (req_errs + prop_errs)


@wrap_validate(validate_enum)
def validate_array(
    schema_obj: dict, payload_obj: JsonType
) -> list[ValidationError]:
    """ Validates json against schema of type "array". """
    type_error = simple_validate_type(schema_obj, payload_obj)
    if type_error:
        return [type_error]
    else:
        # validate items
        payload_array = cast(list, payload_obj)
        schema_item = parsers.schema_items(schema_obj)
        final_errors = []
        for e, item in enumerate(payload_array):
            errors = validate_by_type(schema_item, item)
            index = f"[{e}]"
            updated_errors = list(map(
                op.methodcaller("update_path", index), errors
            ))
            final_errors += updated_errors

        return final_errors


@wrap_validate(validate_enum)
def validate_string(
    schema_obj: dict, payload_obj: JsonType
) -> list[ValidationError]:
    """ Validates json against schema of type "string". """
    type_error = simple_validate_type(schema_obj, payload_obj)
    if type_error:
        return [type_error]
    else:
        validate_enum(schema_obj, payload_obj)
        return []


@wrap_validate(validate_enum)
def validate_number(
    schema_obj: dict, payload_obj: JsonType
) -> list[ValidationError]:
    """ Validates json against schema of type "number" or "integer". """
    type_error = simple_validate_type(schema_obj, payload_obj)
    if type_error:
        return [type_error]
    else:
        optional_min = parsers.schema_min(schema_obj)
        optional_max = parsers.schema_max(schema_obj)
        # MAX < MIN
        if lift_optional(op.lt)(optional_max, optional_min):
            raise SchemaError(
                message=(f"Maximum ({optional_max}) cannot be less than "
                         f"minimum ({optional_min}).")
            )
        # VALUE < MIN
        elif lift_optional(op.lt)(payload_obj, optional_min):
            min_error = ValidationError(
                message=f"{payload_obj} is less than min {optional_min}."
            )

            return [min_error]
        # VALUE > MAX
        elif lift_optional(op.gt)(payload_obj, optional_max):
            max_error = ValidationError(
                message=f"{payload_obj} is greater than max {optional_max}."
            )

            return [max_error]
        else:
            return []


@wrap_validate(validate_enum)
def validate_boolean(
    schema_obj: dict, payload_obj: JsonType
) -> list[ValidationError]:
    """ Validates json against schema of type "boolean". """
    type_error = simple_validate_type(schema_obj, payload_obj)
    if type_error:
        return [type_error]
    else:
        return []
