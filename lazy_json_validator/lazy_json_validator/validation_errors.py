# standard modules
from typing import Optional, Any
import operator as op
from functools import partial

# local modules
from .json_types import JTypes
from .aux import compose


class InternalError(Exception):
    def __init__(self, message=""):
        self.message = message

    def __repr__(self):
        return f"InternalError({self.message})"

    def __str__(self):
        return str(self.message)


class PathError(Exception):
    def __init__(self, path=None, message=""):
        Exception.__init__(self)
        self.message = message
        self.path = path

    def update_path(self, path: str) -> "PathError":
        if self.path:
            self.path = ".".join((path, self.path))
        else:
            self.path = path

        return self

    def __repr__(self):
        path = self.path or "root"
        name = self.__class__.__name__
        return f"{name}(path={path}, message={self.message})"

    def __str__(self):
        return ", ".join(("path: " + self.path, "message: " + self.message))

    def __eq__(self, other: Any):
        if isinstance(other, PathError):
            return (
                (self.path == other.path) and
                (self.message == other.message)
            )
        else:
            return False


class SchemaError(PathError):
    pass


class ValidationError(PathError):
    pass


class TypeValidationError(ValidationError):
    def __init__(self, schema_type: JTypes, payload_type: JTypes, path=None):
        self.schema_type = schema_type
        self.payload_type = payload_type
        message = (
            f"Field of type {payload_type.name} instead of {schema_type.name}"
        )
        self.message = message
        ValidationError.__init__(self, message=message, path=path)


class OneOfValidationError(ValidationError):
    def __init__(
        self, path: str = None,
        error_lists: Optional[list[list[ValidationError]]] = None,
        indices: Optional[list[int]] = None
    ):
        self.error_lists = error_lists
        self.indices = indices
        self.path = path

    @property
    def message(self):
        if self.error_lists is not None:
            message = (
                "Field does not match to any 'oneOf' options: "
                + str(self.error_lists)
            )
        elif self.indices is not None:
            message = (
                "field matches to multiple options for 'oneOf'. Matches to the following indices: "  # noqa
                + ", ".join((f"{i}" for i in self.indices))
            )
        else:
            message = "Failed to validate 'oneOf' field"

        return message

    def update_path(self, path: str):
        if self.error_lists:
            self.error_lists = list(map(
                compose(list, partial(map,
                        op.methodcaller("update_path", path))),
                self.error_lists
            ))

        return super().update_path(path)


class AnyOfValidationError(ValidationError):
    def __init__(
        self,
        error_lists: list[list[ValidationError]],
        path: str = None,
    ):
        self.error_lists = error_lists
        self.path = path

    @property
    def message(self):
        message = (
            "Field does not match to any 'anyOf' options: "
            + str(self.error_lists)
        )

        return message

    def update_path(self, path: str):
        self.error_lists = list(map(
            compose(list, partial(map,
                    op.methodcaller("update_path", path))),
            self.error_lists
        ))

        return super().update_path(path)


class AllOfValidationError(ValidationError):
    def __init__(
        self,
        error_lists: list[list[ValidationError]],
        path: str = None,
    ):
        self.error_lists = error_lists
        self.path = path

    @property
    def message(self):
        message = (
            "Field does not match to all requirements for 'allOf': "
            + str(self.error_lists)
        )

        return message

    def update_path(self, path: str):
        self.error_lists = list(map(
            compose(list, partial(map,
                    op.methodcaller("update_path", path))),
            self.error_lists
        ))

        return super().update_path(path)


class CollectiveError(Exception):
    def __init__(self, errors: list[ValidationError], max_errors=None):
        self.errors = errors
        self.max = max_errors

    @staticmethod
    def to_str(path_error: PathError) -> str:
        return "\n".join((
            (path_error.__class__.__name__ + ":"),
            ("\tpath: " + path_error.path),
            ("\tmessage: " + path_error.message)
        ))

    def __str__(self):
        num_errors = len(self.errors)
        num_listed = (
            str(min(self.max, num_errors)) if self.max
            else "all"
        )
        messages = [
            "",
            f"[{num_errors}] errors found.",
            f"Listing {num_listed} errors:"
        ]

        return "\n".join(
            messages + list(map(self.to_str, self.errors))
        )
