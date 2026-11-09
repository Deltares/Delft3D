from __future__ import annotations

import builtins
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Dict, List, Optional


class ChangeType(str, Enum):
    ADDED = "added"
    REMOVED = "removed"
    CHANGED = "changed"


class TargetType(str, Enum):
    PROPERTY = "Property"
    SECTION = "Section"
    ENUMVALUE = "Option"


@dataclass
class ChangeTarget:
    """The section/property/enum_value a change applies to."""

    section: str
    property: Optional[str] = None
    enum_value: Optional[str] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ChangeTarget":
        return cls(
            section=data["section"],
            property=data.get("property"),
            enum_value=data.get("enum_value"),
        )

    def to_dict(self) -> Dict[str, Any]:
        return {
            "section": self.section,
            "property": self.property,
            "enum_value": self.enum_value,
        }

    @builtins.property
    def identifier(self) -> str:
        identifier = f"[{self.section}]"

        if self.property is not None:
            identifier += f" {self.property}"

            if self.enum_value is not None:
                identifier += f"={self.enum_value}"

        return identifier

    @builtins.property
    def target_type(self) -> TargetType:
        if self.enum_value is not None:
            return TargetType.ENUMVALUE
        if self.property is not None:
            return TargetType.PROPERTY
        return TargetType.SECTION


@dataclass
class Change:
    """A single change (added/changed/removed) applied to a target."""

    type: ChangeType
    target: ChangeTarget
    old_value: Any = None
    new_value: Any = None
    field: Optional[str] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "Change":
        return cls(
            type=ChangeType(data["type"]),
            target=ChangeTarget.from_dict(data["target"]),
            old_value=data.get("old_value"),
            new_value=data.get("new_value"),
            field=data.get("field"),
        )

    def to_dict(self) -> Dict[str, Any]:
        # old_value/new_value/field are only meaningful (and emitted) for field-level changes.
        data: Dict[str, Any] = {"type": self.type.value}
        data["target"] = self.target.to_dict()
        if self.field is not None:
            data["field"] = self.field
            if self.type is not ChangeType.ADDED:
                data["old_value"] = self.old_value
            if self.type is not ChangeType.REMOVED:
                data["new_value"] = self.new_value
        return data


@dataclass
class Commit:
    """The commit that introduced a set of changes."""

    sha: str
    date: str
    subject: str
    ref: str

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "Commit":
        return cls(
            sha=data["sha"],
            date=data["date"],
            subject=data["subject"],
            ref=data.get("ref"),
        )

    def to_dict(self) -> Dict[str, Any]:
        return {
            "sha": self.sha,
            "date": self.date,
            "subject": self.subject,
            "ref": self.ref,
        }


@dataclass
class ChangelogDocument:
    """Top-level changes document: the absolute diff of a spec file between two commits."""

    schema_version: str
    specification_file: str
    source_commit: Commit
    target_commit: Commit
    changes: List[Change] = field(default_factory=list)

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ChangelogDocument":
        return cls(
            schema_version=data["schema_version"],
            specification_file=data["specification_file"],
            source_commit=Commit.from_dict(data["source_commit"]),
            target_commit=Commit.from_dict(data["target_commit"]),
            changes=[Change.from_dict(change) for change in data.get("changes", [])],
        )

    def to_dict(self) -> Dict[str, Any]:
        return {
            "schema_version": self.schema_version,
            "specification_file": self.specification_file,
            "source_commit": self.source_commit.to_dict(),
            "target_commit": self.target_commit.to_dict(),
            "changes": [change.to_dict() for change in self.changes],
        }
