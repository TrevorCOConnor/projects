# Standard modules
import unittest
from unittest.mock import Mock, patch

# Local modules
import lazy_json_validator.validators as TEST
from lazy_json_validator.validation_errors import (
    InternalError, TypeValidationError, ValidationError,
    SchemaError
)
from lazy_json_validator.json_types import JTypes

""" TODOS:
    - Add enum tests for all wrapped validators.
    - Add test for enum
    - Add tests for invalid schema
    - Update tests to have extensive error expectations
"""


class TestValidateEnum(unittest.TestCase):
    def test_match(self):
        # set up
        schema_obj = {
            "enum": ["Hi", 7]
        }
        payload_obj1 = "Hi"
        payload_obj2 = 7

        # run
        errors = TEST.validate_enum(schema_obj, payload_obj1)
        errors += TEST.validate_enum(schema_obj, payload_obj2)

        # asserts
        self.assertEqual(len(errors), 0)

    def test_mismatch(self):
        # set up
        schema_obj = {
            "enum": ["Hi", 7]
        }
        payload_obj = 8

        # run
        errors = TEST.validate_enum(schema_obj, payload_obj)

        # expectations
        expected_error = ValidationError(
            message="Value '8' is not one of ['Hi', 7]."
        )

        # asserts
        self.assertEqual(errors, [expected_error])

    def test_list_error(self):
        # set up
        schema_obj = {
            "enum": (1, 2)
        }
        payload_obj = 7

        # run
        with self.assertRaises(SchemaError) as context:
            TEST.validate_enum(schema_obj, payload_obj)

        # expectations
        expected_error = SchemaError(
            message="'enum' must be of type 'list'."
        )

        # asserts
        self.assertEqual(context.exception, expected_error)

    def test_item_error(self):
        # set up
        schema_obj = {
            "enum": [1, 2, (1, 2)]
        }
        payload_obj = 7

        # run
        with self.assertRaises(SchemaError) as context:
            TEST.validate_enum(schema_obj, payload_obj)

        # expectations
        expected_error = SchemaError(
            message=("'enum' values must be valid json types. Type 'tuple' is "
                     "not valid.")
        )

        # asserts
        self.assertEqual(context.exception, expected_error)


class TestValidateConst(unittest.TestCase):
    def test_match(self):
        # set up
        schema_obj = {
            "const": "Hi"
        }
        payload_obj = "Hi"

        # run
        errors = TEST.validate_const(schema_obj, payload_obj)

        # asserts
        self.assertEqual(len(errors), 0)

    def test_mismatch(self):
        # set up
        schema_obj = {
            "const": "Hi"
        }
        payload_obj = 7

        # run
        errors = TEST.validate_const(schema_obj, payload_obj)

        # expectations
        expected_error = ValidationError(
            message="Value '7' does not equal constant 'Hi'."
        )

        # asserts
        self.assertEqual(errors, [expected_error])

    def test_schema_error(self):
        # set up
        schema_obj = {
            "const": (1, 2)
        }
        payload_obj = 7

        # run
        with self.assertRaises(SchemaError) as context:
            TEST.validate_const(schema_obj, payload_obj)

        # expectations
        expected_error = SchemaError(
            message="'const' of type 'tuple' is not an acceptable json type."
        )

        # asserts
        self.assertEqual(context.exception, expected_error)


class TestValidateOneOf(unittest.TestCase):
    def setUp(self):
        """Define schema"""
        self.schema = {
            "oneOf": [
                {
                    "type": "object",
                    "properties": {
                        "name": {
                            "type": "string"
                        }
                    },
                    "required": [
                        "name"
                    ]
                },
                {
                    "type": "object",
                    "properties": {
                        "id": {
                            "type": "number"
                        }
                    },
                    "required": [
                        "id"
                    ]
                }
            ]
        }

    def test_valid(self):
        # set up
        json = {
            "name": "value"
        }

        # run
        errors = TEST.validate_oneOf(self.schema, json)

        # asserts
        self.assertEqual(len(errors), 0)

    def test_more_than_one_valid(self):
        # set up
        json = {
            "name": "value",
            "id": 1
        }

        # run
        errors = TEST.validate_oneOf(self.schema, json)

        # expectations
        expected_indices = [0, 1]

        # asserts
        self.assertEqual(len(errors), 1)
        error = errors[0]
        self.assertEqual(error.indices, expected_indices)

    def test_none_valid(self):
        # set up
        json = {
            "name": 7
        }

        # run
        errors = TEST.validate_oneOf(self.schema, json)

        # asserts
        self.assertEqual(len(errors), 1)
        error = errors[0]
        # assert nested errors
        self.assertEqual(len(error.error_lists), 2)


class TestValidateAllOf(unittest.TestCase):
    def setUp(self):
        """Define schema"""
        self.schema = {
            "allOf": [
                {
                    "type": "object",
                    "properties": {
                        "name": {
                            "type": "string"
                        }
                    },
                    "required": [
                        "name"
                    ]
                },
                {
                    "type": "object",
                    "properties": {
                        "id": {
                            "type": "number"
                        }
                    },
                    "required": [
                        "id"
                    ]
                }
            ]
        }

    def test_valid(self):
        # set up
        json = {
            "name": "value",
            "id": 1
        }

        # run
        errors = TEST.validate_allOf(self.schema, json)

        # asserts
        self.assertEqual(len(errors), 0)

    def test_one_not_valid(self):
        # set up
        json = {
            "name": "John Doe"
        }

        # run
        errors = TEST.validate_allOf(self.schema, json)

        # asserts
        self.assertEqual(len(errors), 1)
        error = errors[0]
        # assert nested errors
        self.assertEqual(len(error.error_lists), 1)


class TestValidateAnyOf(unittest.TestCase):
    def setUp(self):
        """Define schema"""
        self.schema = {
            "anyOf": [
                {
                    "type": "object",
                    "properties": {
                        "name": {
                            "type": "string"
                        }
                    },
                    "required": [
                        "name"
                    ]
                },
                {
                    "type": "object",
                    "properties": {
                        "id": {
                            "type": "number"
                        }
                    },
                    "required": [
                        "id"
                    ]
                }
            ]
        }

    def test_none_valid(self):
        # set up
        json = {
            "name": 7
        }

        # run
        errors = TEST.validate_anyOf(self.schema, json)

        # asserts
        self.assertEqual(len(errors), 1)
        error = errors[0]
        self.assertEqual(len(error.error_lists), 2)

    def test_one_not_valid(self):
        # set up
        json = {
            "name": "John Doe"
        }

        # run
        errors = TEST.validate_anyOf(self.schema, json)

        # asserts
        self.assertEqual(len(errors), 0)

    def test_all_valid(self):
        # set up
        json = {
            "name": "John Doe",
            "id": 1
        }

        # run
        errors = TEST.validate_anyOf(self.schema, json)

        # asserts
        self.assertEqual(len(errors), 0)


class TestValidateByType(unittest.TestCase):
    @patch("lazy_json_validator.validators.validate_object")
    def test_object(self, patch_validate_object):
        # set up
        schema_obj = {
            "type": "object"
        }
        payload_obj = Mock()

        # run
        TEST.validate_by_type(schema_obj, payload_obj)

        # asserts
        patch_validate_object.assert_called()

    @patch("lazy_json_validator.validators.validate_array")
    def test_array(self, patch_validate_array):
        # set up
        schema_obj = {
            "type": "array"
        }
        payload_obj = Mock()

        # run
        TEST.validate_by_type(schema_obj, payload_obj)

        # asserts
        patch_validate_array.assert_called()

    @patch("lazy_json_validator.validators.validate_string")
    def test_string(self, patch_validate_string):
        # set up
        schema_obj = {
            "type": "string"
        }
        payload_obj = Mock()

        # run
        TEST.validate_by_type(schema_obj, payload_obj)

        # asserts
        patch_validate_string.assert_called()

    @patch("lazy_json_validator.validators.validate_number")
    def test_number(self, patch_validate_number):
        # set up
        schema_obj = {
            "type": "number"
        }
        payload_obj = Mock()

        # run
        TEST.validate_by_type(schema_obj, payload_obj)

        # asserts
        patch_validate_number.assert_called()

    @patch("lazy_json_validator.validators.validate_boolean")
    def test_boolean(self, patch_validate_boolean):
        # set up
        schema_obj = {
            "type": "boolean"
        }
        payload_obj = Mock()

        # run
        TEST.validate_by_type(schema_obj, payload_obj)

        # asserts
        patch_validate_boolean.assert_called()

    @patch("lazy_json_validator.validators.validate_oneOf")
    def test_oneOf(self, patch_validate_oneOf):
        # set up
        schema_obj = {
            "oneOf": []
        }
        payload_obj = Mock()

        # run
        TEST.validate_by_type(schema_obj, payload_obj)

        # asserts
        patch_validate_oneOf.assert_called()

    @patch("lazy_json_validator.validators.validate_allOf")
    def test_allOf(self, patch_validate_allOf):
        # set up
        schema_obj = {
            "allOf": []
        }
        payload_obj = Mock()

        # run
        TEST.validate_by_type(schema_obj, payload_obj)

        # asserts
        patch_validate_allOf.assert_called()

    @patch("lazy_json_validator.validators.validate_anyOf")
    def test_anyOf(self, patch_validate_anyOf):
        # set up
        schema_obj = {
            "anyOf": []
        }
        payload_obj = Mock()

        # run
        TEST.validate_by_type(schema_obj, payload_obj)

        # asserts
        patch_validate_anyOf.assert_called()

    @patch("lazy_json_validator.parsers.schema_type")
    def test_type_not_handled(self, patch_schema_type):
        # set up
        patch_schema_type.return_value = "Other"
        schema_obj = Mock()
        payload_obj = Mock()

        # run
        with self.assertRaises(InternalError) as context:
            TEST.validate_by_type(schema_obj, payload_obj)

        # expectations
        expected_error = "Type Other not handled."

        # asserts
        self.assertEqual(context.exception.message, expected_error)


class TestSimpleValidateType(unittest.TestCase):
    def test_pass(self):
        # Set up
        schema_obj = {"type": "string"}
        payload_obj = "1"

        # run
        optional_error = TEST.simple_validate_type(schema_obj, payload_obj)

        # asserts
        self.assertIsNone(optional_error)

    def test_fail(self):
        # Set up
        schema_obj = {"type": "string"}
        payload_obj = 1

        # run
        optional_error = TEST.simple_validate_type(schema_obj, payload_obj)

        # asserts
        self.assertEqual(optional_error.schema_type, JTypes.STRING)
        self.assertEqual(optional_error.payload_type, JTypes.NUMBER)

    def test_non_json_type(self):
        # Set up
        schema_obj = {"type": "string"}
        payload_obj = (1,)

        # run
        optional_error = TEST.simple_validate_type(schema_obj, payload_obj)

        # asserts
        self.assertTrue(isinstance(optional_error, ValidationError))


class TestValidateRequiredField(unittest.TestCase):
    def test_contains_field(self):
        # Set up
        payload_dict = {
            "key": "value"
        }
        req_field = "key"

        # run
        optional_error = TEST.validate_required_field(payload_dict, req_field)

        # asserts
        self.assertIsNone(optional_error)

    def test_does_not_contain_field(self):
        # Set up
        payload_dict = {
            "key": "value"
        }
        req_field = "other"

        # run
        optional_error = TEST.validate_required_field(payload_dict, req_field)

        # expectations
        expected_message = "Required field 'other' not found."

        # asserts
        self.assertEqual(optional_error.message, expected_message)


class TestValidateRequiredFieldList(unittest.TestCase):
    def test_list(self):
        # Set up
        schema_obj = {
            "required": [
                "first",
                "second",
                "third"
            ]
        }
        payload_obj = {
            "first": "who"
        }

        # run
        errors = TEST.validate_required_field_list(schema_obj, payload_obj)

        # asserts
        self.assertEqual(len(errors), 2)


class TestValidateProperty(unittest.TestCase):
    def test_property_not_found(self):
        # Set up
        payload = {"other": "hi"}
        prop = ("field", {"type": "string"})

        # Run
        errors = TEST.validate_property(payload, prop)

        # Asserts
        self.assertEqual(len(errors), 0)

    def test_property_valid_type(self):
        # Set up
        payload = {"field": "hi"}
        prop = ("field", {"type": "string"})

        # Run
        errors = TEST.validate_property(payload, prop)

        # Asserts
        self.assertEqual(len(errors), 0)

    def test_property_invalid_type(self):
        # Set up
        payload = {"field": 7}
        prop = ("field", {"type": "string"})

        # Run
        errors = TEST.validate_property(payload, prop)

        # Asserts
        self.assertEqual(len(errors), 1)
        error = errors[0]
        self.assertEqual(error.path, "field")


class TestValidatePropertyList(unittest.TestCase):
    def test_validate(self):
        # Set Up
        schema_obj = {
            "type": "object",
            "properties": {
                "name": {
                    "type": "string"
                },
                "age": {
                    "type": "number"
                }
            }
        }
        payload = {
            "name": "Harold",
            "age": "18"
        }

        # Run
        errors = TEST.validate_property_list(schema_obj, payload)

        # Asserts
        self.assertEqual(len(errors), 1)


class TestValidateObject(unittest.TestCase):
    def setUp(self):
        self.schema_object = {
            "type": "object",
            "properties": {
                "name": {
                    "type": "string"
                },
                "age": {
                    "type": "string"
                }
            },
            "required": [
                "name",
                "age"
            ]
        }

    def test_type_error(self):
        # Set up
        json = "hi"

        # Run
        errors = TEST.validate_object(self.schema_object, json)

        # Asserts
        self.assertEqual(len(errors), 1)
        error = errors[0]
        self.assertTrue(isinstance(error, TypeValidationError))

    def test_required_fields(self):
        # Set up
        json = {
            "name": "Harold"
        }

        # Run
        errors = TEST.validate_object(self.schema_object, json)

        # Asserts
        self.assertEqual(len(errors), 1)
        error = errors[0]
        # assert path is at root level
        self.assertEqual(error.path, None)

    def test_property_errors(self):
        # Set up
        json = {
            "name": "Harold",
            "age": 18
        }

        # Run
        errors = TEST.validate_object(self.schema_object, json)

        # Asserts
        self.assertEqual(len(errors), 1)
        error = errors[0]
        self.assertEqual(error.path, "age")


class TestValidateArray(unittest.TestCase):
    def setUp(self):
        self.schema_object = {
            "type": "array",
            "items": {
                "type": "string"
            }
        }

    def test_type_error(self):
        # Set up
        json = "hi"

        # Run
        errors = TEST.validate_array(self.schema_object, json)

        # Asserts
        self.assertEqual(len(errors), 1)
        error = errors[0]
        self.assertTrue(isinstance(error, TypeValidationError))

    def test_validate_items(self):
        # Set up
        json = [
            "item",
            7
        ]

        # Run
        errors = TEST.validate_array(self.schema_object, json)

        # Asserts
        self.assertEqual(len(errors), 1)
        error = errors[0]
        self.assertEqual(error.path, "[1]")


class TestValidateString(unittest.TestCase):
    def setUp(self):
        pass

    def test_validate(self):
        pass

    def test_fail(self):
        pass


class TestValidateNumber(unittest.TestCase):
    pass


class TestValidateBoolean(unittest.TestCase):
    pass


if __name__ == "__main__":
    unittest.main()
