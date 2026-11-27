"""Python data classes generated from the INI JSON schema (ini.json.schema).

These classes mirror the structure defined in
``specifications/json-schemas/ini.json.schema`` and provide ``from_dict``
constructors to load a parsed JSON document into typed objects.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, TypeVar, Union

TEnum = TypeVar("TEnum", bound=Enum)


def parse_enum(enum_class: type[TEnum], value: Optional[str]) -> Optional[TEnum]:
    """Parse a string value into an enum member of the given enum class."""

    return enum_class(value) if value else None


class StatusValue(str, Enum):
    """Allowed values for a status entry."""

    GA = "GA"
    RESEARCH = "research"
    DEPRECATED = "deprecated"
    OBSOLETE = "obsolete"


class ValueType(str, Enum):
    """Allowed value types for an INI property."""

    STRING = "string"
    INT = "int"
    FLOAT = "float"
    INTBOOL = "intbool"
    PATH = "path"
    ENUM = "enum"
    INTENUM = "intenum"
    LIST_PATH = "list[path]"
    LIST_STRING = "list[string]"
    LIST_FLOAT = "list[float]"
    DATETIME = "datetime"


class FileFormatType(str, Enum):
    """Allowed file format types."""

    INI = "ini"


class FloatFormatType(str, Enum):
    """Allowed float format types."""

    GENERAL = "general"
    FIXED = "fixed"
    SCIENTIFIC = "scientific"


class DateTimeFormatType(str, Enum):
    """Allowed datetime format types."""

    DATE = "date"
    DATETIME = "datetime"


# Legacy datetime format values used before they were renamed to "date"/
# "datetime" in commit e936d85. Kept so historical JSON revisions can still
# be parsed when rebuilding the changelog from git history.
_LEGACY_DATETIME_FORMAT_ALIASES = {
    "yyyymmdd": DateTimeFormatType.DATE,
    "yyyymmddhhmmss": DateTimeFormatType.DATETIME,
}


def parse_datetime_format(value: Optional[str]) -> Optional[DateTimeFormatType]:
    """Parse a datetime format value, accepting legacy pre-rename aliases."""

    if not value:
        return None
    if value in _LEGACY_DATETIME_FORMAT_ALIASES:
        return _LEGACY_DATETIME_FORMAT_ALIASES[value]
    return DateTimeFormatType(value)


@dataclass
class Status:
    """Lifecycle status of a property or enum value."""

    value: StatusValue
    comment: Optional[str] = None
    since_release: Optional[float] = None
    date: Optional[datetime] = None

    @classmethod
    def from_dict(cls, data: Union[str, Dict[str, Any]]) -> "Status":

        if isinstance(data, str):
            return cls(value=parse_enum(StatusValue, data))
        return cls(
            value=parse_enum(StatusValue, data["value"]),
            comment=data.get("comment"),
            since_release=data.get("since_release"),
            date=(
                datetime.strptime(data["date"], "%Y-%m-%d")
                if data.get("date")
                else None
            ),
        )

    @classmethod
    def default(cls) -> "Status":
        return cls(value=StatusValue.GA)


@dataclass
class Range:
    """Numeric range constraints for a property value."""

    min_inclusive: Optional[float] = None
    max_inclusive: Optional[float] = None
    min_exclusive: Optional[float] = None
    max_exclusive: Optional[float] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "Range":
        return cls(
            min_inclusive=data.get("min_inclusive"),
            max_inclusive=data.get("max_inclusive"),
            min_exclusive=data.get("min_exclusive"),
            max_exclusive=data.get("max_exclusive"),
        )


@dataclass
class Validation:
    """Validation constraints for an INI property."""

    min_length: Optional[int] = None
    max_length: Optional[int] = None
    is_required: Optional[bool] = None
    range: Optional[Range] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "Validation":
        range_data = data.get("range")
        return cls(
            min_length=data.get("min_length"),
            max_length=data.get("max_length"),
            is_required=data.get("is_required"),
            range=Range.from_dict(range_data) if range_data is not None else None,
        )


@dataclass
class EnumValue:
    """A single enum value entry, optionally carrying a status.

    An enum value in the schema is either a plain description string or an
    object containing a ``status``. Both cases are captured here.
    """

    status: Status
    description: Optional[str] = None

    @classmethod
    def from_dict(cls, data: Union[str, Dict[str, Any]]) -> "EnumValue":
        if isinstance(data, str):
            return cls(description=data, status=Status.default())
        status = data.get("status")
        return cls(
            status=Status.from_dict(status) if status is not None else Status.default(),
            description=data.get("description"),
        )


@dataclass
class IniPropertySpec:
    """A single key/value property within an INI section."""

    key: str
    status: Status
    default_value: Any = None
    value_type: Optional[ValueType] = None
    description: Optional[str] = None
    validation: Optional[Validation] = None
    enum_values: Optional[Dict[str, EnumValue]] = None
    unit: Optional[str] = None
    format: Optional[Union[DateTimeFormatType, FloatFormatType]] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "IniPropertySpec":
        value_type = data.get("value_type")
        status = data.get("status")
        validation = data.get("validation")
        enum_values = data.get("enum_values")

        format = None
        if value_type == ValueType.DATETIME.value:
            format = parse_datetime_format(data.get("format"))
        elif (
            value_type == ValueType.FLOAT.value
            or value_type == ValueType.LIST_FLOAT.value
        ):
            format = parse_enum(FloatFormatType, data.get("format"))

        return cls(
            key=data["key"],
            default_value=data.get("default_value"),
            value_type=parse_enum(ValueType, value_type),
            description=data.get("description"),
            status=Status.from_dict(status) if status is not None else Status.default(),
            validation=(
                Validation.from_dict(validation) if validation is not None else None
            ),
            enum_values=(
                {
                    name: EnumValue.from_dict(value)
                    for name, value in enum_values.items()
                }
                if enum_values is not None
                else None
            ),
            unit=data.get("unit"),
            format=format,
        )


@dataclass
class IniSectionSpec:
    """A named section containing a list of INI properties."""

    name: str
    description: str
    is_unique: bool
    status: Status
    ini_properties: List[IniPropertySpec] = field(default_factory=list)

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "IniSectionSpec":
        status = data.get("status")
        return cls(
            name=data["name"],
            description=data["description"],
            is_unique=data["is_unique"],
            status=Status.from_dict(status) if status is not None else Status.default(),
            ini_properties=[
                IniPropertySpec.from_dict(prop)
                for prop in data.get("ini_properties", [])
            ],
        )


@dataclass
class IniFileSpec:
    """Top-level INI specification document."""

    file_version: str
    file_format_version: str
    file_format_type: FileFormatType
    description: str
    default_float_format: FloatFormatType
    ini_sections: List[IniSectionSpec] = field(default_factory=list)

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "IniFileSpec":
        return cls(
            file_version=data["file_version"],
            file_format_version=data["file_format_version"],
            file_format_type=parse_enum(FileFormatType, data.get("file_format_type")),
            description=data["description"],
            default_float_format=parse_enum(
                FloatFormatType, data.get("default_float_format")
            ),
            ini_sections=[
                IniSectionSpec.from_dict(section)
                for section in data.get("ini_sections", [])
            ],
        )
