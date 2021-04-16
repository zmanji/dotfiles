#!/usr/bin/env python3

import os
import json
import collections
from dataclasses import dataclass, asdict, is_dataclass, fields as datafields

# https://karabiner-elements.pqrs.org/docs/json/complex-modifications-manipulator-definition/

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
    key_code: str = None
    shell_command: str = None
    modifiers: list[str] = None


@dataclass(frozen=True)
class Manipulator:
    _from: FromKey
    to: ToKey
    _type: str = "basic"


@dataclass(frozen=True)
class Rule:
    manipulators: list[Manipulator]
    description: str

cmd_shift_mods = Modifiers(mandatory=["command"], optional="shift")


class Encoder(json.JSONEncoder):
    def default(self, obj):
        if is_dataclass(obj):
            d = dict()
            for f in datafields(obj):
                name = f.name
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
    return [
        Rule(
            description="Rebind Cmd-` to Cmd-F18 for Hammerspoon",
            manipulators=[
                Manipulator(
                    _from=FromKey(key_code="gave_accdent_and_tilde",modifiers=cmd_shift_mods),
                    to=ToKey())
                ]
        )
    ]


def generate_rules():
    return generate_cmd_window_switch()


def main():
    # read the existing config
    with open(os.path.expanduser("~/.config/karabiner/karabiner.json"), "r") as cfgfile:
        cfg = json.load(cfgfile)
    # generate the rules
    rules = generate_rules()
    # swap the rules into the config
    cfg["profiles"][0]["complex_modifications"]["rules"] = rules
    # write out the config
    print("// generated from generate_karabiner.py")
    print(json.dumps(cfg, cls=Encoder, indent=4))


if __name__ == "__main__":
    main()
