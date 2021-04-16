#!/usr/bin/env python3

import os
import json
import collections
from dataclasses import dataclass, asdict, is_dataclass

# https://karabiner-elements.pqrs.org/docs/json/complex-modifications-manipulator-definition/


@dataclass
class Modifiers:
    mandatory: list[str]
    optional: list[str]


@dataclass
class FromKey:
    key_code: str
    modifiers: list[Modifiers]


@dataclass
class SetVariable:
    name: str
    value: str


@dataclass
class ToKey:
    key_code: str = None
    shell_command: str = None
    modifiers: list[str] = None


@dataclass
class Manipulator:
    _from: FromKey
    to: ToKey
    _type: str = "basic"


@dataclass
class Rule:
    manipulators: list[Manipulator]
    description: str

cmd_shift_mods = Modifiers(mandatory=["command"], optional="shift")


class Encoder(json.JSONEncoder):
    def default(self, obj):

        print(repr(obj))
        if isinstance(obj, Manipulator):
            print("ZAMEER WAS HERE")
            d = asdict(obj, dict_factory=collections.OrderedDict)
            d = OrderedDict(("from" if k == "_from" else k, v) for k, v in d.items())
            return OrderedDict(("type" if k == "_type" else k, v) for k, v in d.items())
        # elif is_dataclass(obj):
        #     return asdict(obj, dict_factory=collections.OrderedDict)
        return json.JSONEncoder.default(self, obj)


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
        cfg = json.load(cfgfile, object_pairs_hook=collections.OrderedDict)
    # generate the rules
    rules = generate_rules()
    # swap the rules into the config
    cfg["profiles"][0]["complex_modifications"]["rules"] = rules
    # write out the config
    print("// generated from generate_karabiner.py")
    print(json.dumps(cfg, indent=4, cls=Encoder))


if __name__ == "__main__":
    main()
