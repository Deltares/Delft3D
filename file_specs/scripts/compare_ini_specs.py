from __future__ import annotations

import dataclasses
import sys
from datetime import date, datetime
from enum import Enum
from pathlib import Path
from typing import Any, Callable, Dict, Iterable, Iterator, List, Set, Tuple, TypeVar

sys.path.insert(0, str(Path(__file__).parent))

from data.ini_spec import IniFileSpec, IniPropertySpec, IniSectionSpec
from data.ini_spec_changelog import Change, ChangeTarget, ChangeType

T = TypeVar("T")

# Fields that are identifiers or nested collections handled by dedicated
# comparison steps rather than the generic field-by-field diff.
SECTION_EXCLUDED_FIELDS = {"name", "ini_properties"}
PROPERTY_EXCLUDED_FIELDS = {"key", "enum_values"}


def create_change_list(old_spec: IniFileSpec, new_spec: IniFileSpec) -> List[Change]:
    """Compare two ``IniFileSpec`` documents and return the list of changes."""

    # MDU identifiers (section names, property keys, enum values) are case-insensitive.
    old_by_name, new_by_name, added, removed, common = _diff_by_key(
        old_spec.ini_sections, new_spec.ini_sections, key=lambda section: section.name.lower()
    )

    changes: List[Change] = []
    for name in sorted(added):
        section = new_by_name[name]
        changes.append(_added(ChangeTarget(section=section.name)))
        changes.extend(_compare_properties(section.name, [], section.ini_properties))
    for name in sorted(removed):
        section = old_by_name[name]
        changes.append(_removed(ChangeTarget(section=section.name)))
        changes.extend(_compare_properties(section.name, section.ini_properties, []))
    for name in sorted(common):
        changes.extend(_compare_section(old_by_name[name], new_by_name[name]))
    return changes



def _compare_section(old: IniSectionSpec, new: IniSectionSpec) -> List[Change]:
    target = ChangeTarget(section=new.name)
    changes = list(_compare_fields(old, new, target, exclude=SECTION_EXCLUDED_FIELDS))
    changes.extend(
        _compare_properties(new.name, old.ini_properties, new.ini_properties)
    )
    return changes


def _compare_properties(
    section_name: str,
    old_properties: List[IniPropertySpec],
    new_properties: List[IniPropertySpec],
) -> List[Change]:
    old_by_key, new_by_key, added, removed, common = _diff_by_key(
        old_properties, new_properties, key=lambda prop: prop.key.lower()
    )

    changes: List[Change] = []
    for key in sorted(added):
        changes.append(
            _added(ChangeTarget(section=section_name, property=new_by_key[key].key))
        )
    for key in sorted(removed):
        changes.append(
            _removed(ChangeTarget(section=section_name, property=old_by_key[key].key))
        )
    for key in sorted(common):
        changes.extend(
            _compare_property(section_name, old_by_key[key], new_by_key[key])
        )
    return changes


def _compare_property(
    section_name: str, old: IniPropertySpec, new: IniPropertySpec
) -> List[Change]:
    target = ChangeTarget(section=section_name, property=new.key)
    changes = list(_compare_fields(old, new, target, exclude=PROPERTY_EXCLUDED_FIELDS))
    changes.extend(
        _compare_enum_values(
            section_name, new.key, old.enum_values or {}, new.enum_values or {}
        )
    )
    return changes


def _compare_enum_values(
    section_name: str,
    property_key: str,
    old_values: Dict[str, Any],
    new_values: Dict[str, Any],
) -> List[Change]:
    old_by_lower = {name.lower(): (name, value) for name, value in old_values.items()}
    new_by_lower = {name.lower(): (name, value) for name, value in new_values.items()}

    added = new_by_lower.keys() - old_by_lower.keys()
    removed = old_by_lower.keys() - new_by_lower.keys()
    common = old_by_lower.keys() & new_by_lower.keys()

    changes: List[Change] = []
    for lower_name in sorted(added):
        original_name, _ = new_by_lower[lower_name]
        changes.append(
            _added(
                ChangeTarget(
                    section=section_name,
                    property=property_key,
                    enum_value=original_name,
                )
            )
        )
    for lower_name in sorted(removed):
        original_name, _ = old_by_lower[lower_name]
        changes.append(
            _removed(
                ChangeTarget(
                    section=section_name,
                    property=property_key,
                    enum_value=original_name,
                )
            )
        )
    for lower_name in sorted(common):
        original_name, new_value = new_by_lower[lower_name]
        _, old_value = old_by_lower[lower_name]
        target = ChangeTarget(
            section=section_name,
            property=property_key,
            enum_value=original_name,
        )
        changes.extend(_compare_fields(old_value, new_value, target, exclude=set()))
    return changes


def _compare_fields(
    old: Any, new: Any, target: ChangeTarget, exclude: Set[str]
) -> Iterator[Change]:
    """Yield a ``Change`` for every leaf value of ``old``/``new`` that differs.

    Nested structures (e.g. ``validation.range``) are flattened into dotted
    field paths so only the actual leaf value that changed is reported.
    """

    for spec_field in dataclasses.fields(old):
        if spec_field.name in exclude:
            continue

        old_value = _comparable(getattr(old, spec_field.name))
        new_value = _comparable(getattr(new, spec_field.name))
        for path, leaf_old, leaf_new in _flatten_diff(
            old_value, new_value, spec_field.name
        ):
            yield _field_change(target, path, leaf_old, leaf_new)


def _flatten_diff(
    old_value: Any, new_value: Any, path: str
) -> Iterator[Tuple[str, Any, Any]]:
    """Recursively diff nested dict values, yielding a ``(dotted_path, old, new)`` per leaf."""

    old_is_dict = isinstance(old_value, dict)
    new_is_dict = isinstance(new_value, dict)
    if old_is_dict or new_is_dict:
        old_dict = old_value if old_is_dict else {}
        new_dict = new_value if new_is_dict else {}
        for key in sorted(old_dict.keys() | new_dict.keys()):
            yield from _flatten_diff(
                old_dict.get(key), new_dict.get(key), f"{path}.{key}"
            )
    elif old_value != new_value:
        yield (path, old_value, new_value)


def _field_change(
    target: ChangeTarget, field_name: str, old_value: Any, new_value: Any
) -> Change:
    """Classify a field-level diff as added/removed when a value transitions to/from empty."""

    if old_value is None and new_value is not None:
        return Change(
            type=ChangeType.ADDED, target=target, field=field_name, new_value=new_value
        )
    if new_value is None and old_value is not None:
        return Change(
            type=ChangeType.REMOVED,
            target=target,
            field=field_name,
            old_value=old_value,
        )
    return Change(
        type=ChangeType.CHANGED,
        target=target,
        field=field_name,
        old_value=old_value,
        new_value=new_value,
    )


def _comparable(value: Any) -> Any:
    """Reduce a spec field's value to a plain, JSON-serializable form for diffing."""

    if isinstance(value, Enum):
        return value.value
    if isinstance(value, (datetime, date)):
        return value.strftime("%Y-%m-%d")
    if dataclasses.is_dataclass(value):
        return {
            field.name: _comparable(getattr(value, field.name))
            for field in dataclasses.fields(value)
        }
    return value


def _diff_by_key(
    old_items: Iterable[T], new_items: Iterable[T], key: Callable[[T], str]
) -> Tuple[Dict[str, T], Dict[str, T], Set[str], Set[str], Set[str]]:
    """Split two collections into added/removed/common keys, keyed by ``key``."""

    old_by_key = {key(item): item for item in old_items}
    new_by_key = {key(item): item for item in new_items}
    added = new_by_key.keys() - old_by_key.keys()
    removed = old_by_key.keys() - new_by_key.keys()
    common = old_by_key.keys() & new_by_key.keys()
    return old_by_key, new_by_key, added, removed, common


def _added(target: ChangeTarget) -> Change:
    return Change(type=ChangeType.ADDED, target=target)


def _removed(target: ChangeTarget) -> Change:
    return Change(type=ChangeType.REMOVED, target=target)
