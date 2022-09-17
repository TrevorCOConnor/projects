""" Integration tests against whole schemas. """

# Standard Modules
import unittest
import json

# Local Modules
from lazy_json_validator import validate  # type: ignore
from lazy_json_validator.validation_errors import (
    ValidationError, TypeValidationError, SchemaError
)

# 3rd party modules
import jsonref  # type: ignore


class TestSimpleSchemas(unittest.TestCase):
    def setUp(self):
        """ Simple Schema """
        self.schema = {
            "type": "object",
            "properties": {
                "name": {
                    "type": "string"
                }
            },
            "required": [
                "name"
            ]
        }

    def test_simple(self):
        """ Pass Simple Schema """
        # set up
        json = {
            "name": "Doris"
        }

        # run
        errors = validate(self.schema, json)

        # asserts
        self.assertEqual(len(errors), 0)

    def test_simple_fail(self):
        """ Fail Valdation against simple schema. Name should be a string, not
            a number.
        """
        # set up
        json = {
            "name": 7
        }

        # run
        errors = validate(self.schema, json)

        # asserts
        self.assertEqual(len(errors), 1)
        self.assertTrue(isinstance(errors[0], TypeValidationError))

    def test_simple_error(self):
        """ Test error raised if schema has error. Schema is missing type field.
        """
        # set up
        schema = {
        }
        json = {
            "name": 7
        }

        # run
        with self.assertRaises(SchemaError):
            validate(schema, json)


class TestRecursiveSchemas(unittest.TestCase):
    def setUp(self):
        """ Schema Creation. Person with children that are also of the base type.
        """
        self.schema = {
            "type": "object",
            "properties": {
                "name": {
                    "type": "string"
                },
                "children": {
                    "type": "array",
                    "items": {
                        "$ref": "#"
                    }
                }
            },
            "required": [
                "name"
            ]
        }
        self.schema = jsonref.loads(json.dumps(self.schema))

    def test_recursive_pass(self):
        """ Validates that recursive schemas can validate via a json that is a
            person with children and grandchildren.
        """
        # set up
        json = {
            "name": "Elise",
            "children": [
                {
                    "name": "Joan"
                },
                {
                    "name": "Brian",
                    "children": [
                        {
                            "name": "Declan"
                        },
                        {
                            "name": "Sadie"
                        }
                    ]
                }
            ]
        }

        # Run
        errors = validate(self.schema, json)

        # Asserts
        self.assertEqual(len(errors), 0)

    def test_recursive_fail(self):
        """ Validates that recursive schemas can find errors in the recursive
            objects by validating against a person with errors in their
            grandchildren: Missing required field -- Name.
        """
        # set up
        json = {
            "name": "Elise",
            "children": [
                {
                    "name": "Joan"
                },
                {
                    "name": "Brian",
                    "children": [
                        {
                            "nickname": "Declan"
                        },
                        {
                            "name": "Sadie"
                        }
                    ]
                }
            ]
        }

        # Run
        errors = validate(self.schema, json)

        # Asserts
        self.assertEqual(len(errors), 1)
        self.assertTrue(isinstance(errors[0], ValidationError))

    def test_recursive_fail_raised(self):
        """ Validates that recursive schemas can find errors in the recursive
            objects by validating against a person with errors in their
            grandchildren: Missing required field -- Name.
        """
        # set up
        json = {
            "name": "Elise",
            "children": [
                {
                    "name": "Joan"
                },
                {
                    "name": "Brian",
                    "children": [
                        {
                            "nickname": "Declan"
                        },
                        {
                            "name": 4
                        }
                    ]
                }
            ]
        }

        # Run
        errors = validate(self.schema, json, True)

        # Asserts
        self.assertEqual(len(errors), 1)
        self.assertTrue(isinstance(errors[0], ValidationError))


class TestFeatures(unittest.TestCase):
    """ Testing various features of json schemas. """
    pass


if __name__ == "__main__":
    unittest.main()
