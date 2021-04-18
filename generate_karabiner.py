#!/usr/bin/env python3

import os
import json
from dataclasses import dataclass, is_dataclass, replace, fields as datafields
from typing import Optional

# https://karabiner-elements.pqrs.org/docs/json/complex-modifications-manipulator-definition/


@dataclass(frozen=True)
class Ident:
    vendor_id: int
    product_id: int


@dataclass(frozen=True)
class Condition:
    _type: str
    description: Optional[str] = None
    identifiers: Optional[list[Ident]] = None
    bundle_identifiers: Optional[list[str]] = None


@dataclass(frozen=True)
class Modifiers:
    mandatory: list[str]
    optional: Optional[list[str]] = None


@dataclass(frozen=True)
class FromKey:
    key_code: str
    modifiers: Optional[Modifiers] = None


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
    to: Optional[list[ToKey]] = None
    to_if_alone: Optional[list[ToKey]] = None
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


def generate_cmd_window_switch() -> list[Rule]:
    cmd_shift_mods = Modifiers(mandatory=["command"], optional=["shift"])
    return [
        Rule(
            description="Rebind Cmd-Tab to Cmd-F19 for Alt-Tab",
            manipulators=[
                Manipulator(
                    _from=FromKey(key_code="tab", modifiers=cmd_shift_mods),
                    to=[ToKey(key_code="f19", modifiers=["left_command"])],
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
                    to=[ToKey(key_code="f18", modifiers=["left_command"])],
                )
            ],
        ),
    ]


def generate_internal_mods() -> list[Rule]:
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
            description="Change caps_lock to control when used as modifier, escape when used alone. (Internal Keyboard)",
            manipulators=[
                Manipulator(
                    conditions=[internal_kb_cond],
                    _from=FromKey(key_code="caps_lock"),
                    to=[ToKey(key_code="left_control")],
                    to_if_alone=[ToKey(key_code="escape")],
                ),
            ],
        ),
        Rule(
            description="Better Shifting: Parentheses on shift keys. (Internal Keyboard)",
            manipulators=[
                Manipulator(
                    conditions=[internal_kb_cond],
                    _from=FromKey(key_code="left_shift"),
                    to=[ToKey(key_code="left_shift")],
                    to_if_alone=[ToKey(key_code="9", modifiers=["left_shift"])],
                ),
                Manipulator(
                    conditions=[internal_kb_cond],
                    _from=FromKey(key_code="right_shift"),
                    to=[ToKey(key_code="right_shift")],
                    to_if_alone=[ToKey(key_code="0", modifiers=["right_shift"])],
                ),
            ],
        ),
        Rule(
            description="Change Fn + h/j/k/l to Arrows (Internal Keyboard)",
            manipulators=[
                Manipulator(
                    conditions=[internal_kb_cond],
                    _from=FromKey(key_code="h", modifiers=base_mods),
                    to=[ToKey(key_code="left_arrow")],
                ),
                Manipulator(
                    conditions=[internal_kb_cond],
                    _from=FromKey(key_code="j", modifiers=base_mods),
                    to=[ToKey(key_code="down_arrow")],
                ),
                Manipulator(
                    conditions=[internal_kb_cond],
                    _from=FromKey(key_code="k", modifiers=base_mods),
                    to=[ToKey(key_code="up_arrow")],
                ),
                Manipulator(
                    conditions=[internal_kb_cond],
                    _from=FromKey(key_code="l", modifiers=base_mods),
                    to=[ToKey(key_code="right_arrow")],
                ),
            ],
        ),
    ]


ctrl_k = FromKey(key_code="k", modifiers=Modifiers(mandatory=["control"]))
ctrl_j = FromKey(key_code="j", modifiers=Modifiers(mandatory=["control"]))

ctrl_k_opt = FromKey(
    key_code="k", modifiers=Modifiers(mandatory=["control"], optional=["shift"])
)
ctrl_j_opt = FromKey(
    key_code="j", modifiers=Modifiers(mandatory=["control"], optional=["shift"])
)


def generate_imessage() -> list[Rule]:
    imessage_cond = Condition(
        _type="frontmost_application_if",
        bundle_identifiers=["^com\.apple\.MobileSMS$"],
    )

    return [
        Rule(
            description="[iMessage] use C-j to navigate down conversation list",
            manipulators=[
                Manipulator(
                    conditions=[imessage_cond],
                    _from=ctrl_j,
                    to=[ToKey(key_code="tab", modifiers=["left_control"])],
                )
            ],
        ),
        Rule(
            description="[iMessage] use C-k to navigate up conversation list",
            manipulators=[
                Manipulator(
                    conditions=[imessage_cond],
                    _from=ctrl_k,
                    to=[ToKey(key_code="tab", modifiers=["left_control", "shift"])],
                )
            ],
        ),
    ]


def generate_spotify() -> list[Rule]:
    cond = Condition(
        _type="frontmost_application_if",
        bundle_identifiers=["^com\.spotify\.client$"],
    )
    return [
        Rule(
            description="[Spotify] use C-j for down",
            manipulators=[
                Manipulator(
                    conditions=[cond], _from=ctrl_j, to=[ToKey(key_code="down_arrow")]
                )
            ],
        ),
        Rule(
            description="[Spotify] use C-k for up",
            manipulators=[
                Manipulator(
                    conditions=[cond],
                    _from=ctrl_k,
                    to=[ToKey(key_code="up_arrow")],
                )
            ],
        ),
    ]


def generate_slack() -> list[Rule]:
    cond = Condition(
        _type="frontmost_application_if",
        bundle_identifiers=["^com\.tinyspeck\.slackmacgap$"],
    )

    return [
        Rule(
            description="[Slack] use C-j for prev convo",
            manipulators=[
                Manipulator(
                    conditions=[cond],
                    _from=ctrl_j_opt,
                    to=[ToKey(key_code="down_arrow", modifiers=["option"])],
                )
            ],
        ),
        Rule(
            description="[Slack] use C-k for next convo",
            manipulators=[
                Manipulator(
                    conditions=[cond],
                    _from=ctrl_k_opt,
                    to=[ToKey(key_code="up_arrow", modifiers=["option"])],
                )
            ],
        ),
    ]


def generate_discord() -> list[Rule]:
    cond = Condition(
        _type="frontmost_application_if",
        bundle_identifiers=["^com\.hnc\.Discord$"],
    )

    return [
        Rule(
            description="[Discord] use C-j for prev convo",
            manipulators=[
                Manipulator(
                    conditions=[cond],
                    _from=ctrl_j,
                    to=[ToKey(key_code="down_arrow", modifiers=["option"])],
                )
            ],
        ),
        Rule(
            description="[Discord] use C-k for next convo",
            manipulators=[
                Manipulator(
                    conditions=[cond],
                    _from=ctrl_k,
                    to=[ToKey(key_code="up_arrow", modifiers=["option"])],
                )
            ],
        ),
    ]


def generate_alfred() -> list[Rule]:
    cond = Condition(
        _type="frontmost_application_if",
        bundle_identifiers=["^com\.runningwithcrayons\.Alfred$"],
    )

    return [
        Rule(
            description="[Alfred] use C-j for down",
            manipulators=[
                Manipulator(
                    conditions=[cond], _from=ctrl_j, to=[ToKey(key_code="down_arrow")]
                )
            ],
        ),
        Rule(
            description="[Alfred] use C-k for up",
            manipulators=[
                Manipulator(
                    conditions=[cond],
                    _from=ctrl_k,
                    to=[ToKey(key_code="up_arrow")],
                )
            ],
        ),
    ]


def generate_rules() -> list[Rule]:
    return (
        generate_alfred()
        + generate_discord()
        + generate_slack()
        + generate_spotify()
        + generate_imessage()
        + generate_internal_mods()
        + generate_cmd_window_switch()
    )


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
