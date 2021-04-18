#!/usr/bin/env python3

import os
import json
import collections
from dataclasses import dataclass, asdict, is_dataclass, fields as datafields
from typing import Optional

# https://karabiner-elements.pqrs.org/docs/json/complex-modifications-manipulator-definition/


@dataclass(frozen=True)
class Ident:
    vendor_id: int
    product_id: int


@dataclass(frozen=True)
class Condition:
    description: str
    _type: str
    identifiers: Optional[list[Ident]] = None


@dataclass(frozen=True)
class Modifiers:
    mandatory: list[str]
    optional: list[str]


@dataclass(frozen=True)
class FromKey:
    key_code: str
    modifiers: list[Modifiers]


@dataclass(frozen=True)
class SetVariable:
    name: str
    value: str


@dataclass(frozen=True)
class ToKey:
    key_code: Optional[str] = None
    shell_command: Optional[str] = None
    modifiers: Optional[list[str]] = None


@dataclass(frozen=True)
class Manipulator:
    _from: FromKey
    to: ToKey
    _type: str = "basic"
    conditions: Optional[list[Condition]] = None


@dataclass(frozen=True)
class Rule:
    manipulators: list[Manipulator]
    description: str


class Encoder(json.JSONEncoder):
    def default(self, obj):
        if is_dataclass(obj):
            d = dict()
            for f in datafields(obj):
                name = f.name
                value = getattr(obj, f.name)
                if value is None:
                    continue
                if name == "_from":
                    name = "from"
                elif name == "_type":
                    name = "type"
                d[name] = getattr(obj, f.name)
            return self.default(d)
        elif isinstance(obj, (bool, str, int, float, list, tuple, dict, type(None))):
            return obj
        else:
            return super().default(obj)


def generate_cmd_window_switch():
    cmd_shift_mods = Modifiers(mandatory=["command"], optional=["shift"])
    return [
        Rule(
            description="Rebind Cmd-Tab to Cmd-F19 for Alt-Tab",
            manipulators=[
                Manipulator(
                    _from=FromKey(key_code="tab", modifiers=cmd_shift_mods),
                    to=ToKey(key_code="f19", modifiers="left_command"),
                )
            ],
        ),
        Rule(
            description="Rebind Cmd-` to Cmd-F18 for Alt-Tab",
            manipulators=[
                Manipulator(
                    _from=FromKey(
                        key_code="grave_accent_and_tilde", modifiers=cmd_shift_mods
                    ),
                    to=ToKey(key_code="f18", modifiers="left_command"),
                )
            ],
        ),
    ]


def generate_internal_mods():
    internal_kb_cond = Condition(
        description="Internal Keyboard",
        _type="device_if",
        identifiers=[
            Ident(product_id=628, vendor_id=1452),
            Ident(product_id=832, vendor_id=1452),
            Ident(product_id=627, vendor_id=1452),
        ],
    )
    base_mods = Modifiers(mandatory=["fn"], optional=["caps_lock"])
    return [
        Rule(
            description="Change Fn + h/j/k/l to Arrows (Internal Keyboard)",
            manipulators=[
                Manipulator(
                    conditions=[internal_kb_cond],
                    _from=FromKey(key_code="h", modifiers=base_mods),
                    to=ToKey(key_code="left_arrow"),
                )
            ],
        ),
    ]


def generate_rules():
    return generate_internal_mods() + generate_cmd_window_switch()


def main():
    # read the existing config
    with open(os.path.expanduser("~/.config/karabiner/karabiner.json"), "r") as cfgfile:
        cfg = json.load(cfgfile)
    # generate the rules
    rules = generate_rules()
    # swap the rules into the config
    cfg["profiles"][0]["complex_modifications"]["rules"] = rules
    # write out the config
    # print("/* generated from generate_karabiner.py */")
    print(json.dumps(cfg, cls=Encoder, indent=4))


if __name__ == "__main__":
    main()
