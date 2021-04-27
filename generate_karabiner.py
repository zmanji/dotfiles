#!/usr/bin/env python3

import os
import json
from dataclasses import dataclass, is_dataclass, replace, fields as datafields
from typing import Optional

# https://karabiner-elements.pqrs.org/docs/json/complex-modifications-manipulator-definition/


@dataclass(frozen=True)
class Ident:
    product_id: int
    vendor_id: int


@dataclass(frozen=True)
class Condition:
    _type: str
    description: Optional[str] = None
    identifiers: Optional[list[Ident]] = None
    bundle_identifiers: Optional[list[str]] = None
    name: Optional[str] = None
    value: Optional[int] = None


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
    value: int


@dataclass(frozen=True)
class ToKey:
    key_code: Optional[str] = None
    shell_command: Optional[str] = None
    modifiers: Optional[list[str]] = None
    set_variable: Optional[SetVariable] = None


@dataclass(frozen=True)
class Manipulator:
    conditions: Optional[list[Condition]] = None
    _from: Optional[FromKey] = None
    to: Optional[list[ToKey]] = None
    to_if_alone: Optional[list[ToKey]] = None
    _type: str = "basic"


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


internal_kb_cond = Condition(
    description="Internal Keyboard",
    _type="device_if",
    identifiers=[
        Ident(product_id=628, vendor_id=1452),
        Ident(product_id=832, vendor_id=1452),
        Ident(product_id=627, vendor_id=1452),
    ],
)


# Redox uses QMK so the vendor id and product id comes from the firmware
# 0xFEED = 65261 is the default vendor id
# 0 is the default product id for any keyboard
# Falbatech is the vendor which should be good enough to uniqely identify but
# karabiner doesn't filter on vendor name. May need to revisit this if there
# is another QMK kb.
redox_kb_cond = Condition(
    description="Redox",
    _type="device_if",
    identifiers=[
        Ident(product_id=0, vendor_id=65261),
    ],
)


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
    base_mods = Modifiers(mandatory=["fn"], optional=["caps_lock"])
    return [
        Rule(
            description="Change caps_lock to control when used as modifier, escape when used alone. (Internal Keyboard)",
            manipulators=[
                Manipulator(
                    conditions=[internal_kb_cond],
                    _from=FromKey(
                        key_code="caps_lock",
                        modifiers=Modifiers(mandatory=[], optional=["shift"]),
                    ),
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


def generate_kitty() -> list[Rule]:
    cond = Condition(
        _type="frontmost_application_if",
        bundle_identifiers=["^net\.kovidgoyal\.kitty$"],
    )

    nav_mode = Condition(
        _type="variable_if",
        name="kitty_nav_mode",
        value=1,
    )

    not_nav_mode = Condition(
        _type="variable_if",
        name="kitty_nav_mode",
        value=0,
    )

    def kitty_command(cmd):
        kitty_command_prefix = "/usr/local/bin/kitty @ --to=unix:/tmp/kitty.sock "
        return ToKey(shell_command=kitty_command_prefix + cmd)

    return [
        Rule(
            description="[kitty] use Cmd-w for nav mode",
            manipulators=[
                Manipulator(
                    conditions=[cond, not_nav_mode],
                    _from=FromKey(
                        key_code="w", modifiers=Modifiers(mandatory=["command"])
                    ),
                    to=[
                        ToKey(set_variable=SetVariable(name="kitty_nav_mode", value=1)),
                        kitty_command("set-colors -a 'background=#5b6268'"),
                    ],
                )
            ],
        ),
        Rule(
            description="[kitty] [Internal Keybord] use caps lock to escape nav mode",
            manipulators=[
                Manipulator(
                    conditions=[cond, nav_mode, internal_kb_cond],
                    _from=FromKey(key_code="caps_lock"),
                    to=[
                        ToKey(set_variable=SetVariable(name="kitty_nav_mode", value=0)),
                        kitty_command("set-colors -a --reset"),
                    ],
                )
            ],
        ),
        Rule(
            description="[kitty] esc to escape nav mode",
            manipulators=[
                Manipulator(
                    conditions=[cond, nav_mode],
                    _from=FromKey(key_code="escape"),
                    to=[
                        ToKey(set_variable=SetVariable(name="kitty_nav_mode", value=0)),
                        kitty_command("set-colors -a --reset"),
                    ],
                )
            ],
        ),
        Rule(
            description="[kitty] h is left in nav mode",
            manipulators=[
                Manipulator(
                    conditions=[cond, nav_mode],
                    _from=FromKey(key_code="h"),
                    to=[kitty_command("kitten neighbor.py left")],
                )
            ],
        ),
        Rule(
            description="[kitty] l is right in nav mode",
            manipulators=[
                Manipulator(
                    conditions=[cond, nav_mode],
                    _from=FromKey(key_code="l"),
                    to=[kitty_command("kitten neighbor.py right")],
                )
            ],
        ),
        Rule(
            description="[kitty] j is down in nav mode",
            manipulators=[
                Manipulator(
                    conditions=[cond, nav_mode],
                    _from=FromKey(key_code="j"),
                    to=[kitty_command("kitten neighbor.py bottom")],
                )
            ],
        ),
        Rule(
            description="[kitty] k is up in nav mode",
            manipulators=[
                Manipulator(
                    conditions=[cond, nav_mode],
                    _from=FromKey(key_code="k"),
                    to=[kitty_command("kitten neighbor.py top")],
                )
            ],
        ),
        Rule(
            description="[kitty] q is close window in nav mode",
            manipulators=[
                Manipulator(
                    conditions=[cond, nav_mode],
                    _from=FromKey(key_code="q"),
                    to=[kitty_command("close-window")],
                )
            ],
        ),
        Rule(
            description="[kitty] K is next tab in nav mode",
            manipulators=[
                Manipulator(
                    conditions=[cond, nav_mode],
                    _from=FromKey(
                        key_code="k", modifiers=Modifiers(mandatory=["shift"])
                    ),
                    to=[kitty_command("kitten tab.py next")],
                )
            ],
        ),
        Rule(
            description="[kitty] J is previous tab in nav mode",
            manipulators=[
                Manipulator(
                    conditions=[cond, nav_mode],
                    _from=FromKey(
                        key_code="j", modifiers=Modifiers(mandatory=["shift"])
                    ),
                    to=[kitty_command("kitten tab.py previous")],
                )
            ],
        ),
        Rule(
            description="[kitty] v is new split in nav mode",
            manipulators=[
                Manipulator(
                    conditions=[cond, nav_mode],
                    _from=FromKey(key_code="v"),
                    to=[kitty_command("new-window")],
                )
            ],
        ),
    ]


# This used to live in hammerspoon, but there was too much flakyness when
# trying to swtich between the internal vs external bindings
def generate_window() -> list[Rule]:
    def hs_command(f):
        cmd = f"/usr/local/bin/hs -c '{f}'"
        return ToKey(shell_command=cmd)

    internal_kb = Modifiers(mandatory=["command", "option"])
    redox_kb = Modifiers(mandatory=["command", "shift", "option", "control"])
    return [
        Rule(
            description="[window management] [Internal Keyboard] cmd+alt h is move window left",
            manipulators=[
                Manipulator(
                    conditions=[internal_kb_cond],
                    _from=FromKey(key_code="h", modifiers=internal_kb),
                    to=[hs_command("move_left()")],
                )
            ],
        ),
        Rule(
            description="[window management] [Internal Keyboard] cmd+alt l is move window right",
            manipulators=[
                Manipulator(
                    conditions=[internal_kb_cond],
                    _from=FromKey(key_code="l", modifiers=internal_kb),
                    to=[hs_command("move_right()")],
                )
            ],
        ),
        Rule(
            description="[window management] [Internal Keyboard] cmd+alt k is move window up",
            manipulators=[
                Manipulator(
                    conditions=[internal_kb_cond],
                    _from=FromKey(key_code="k", modifiers=internal_kb),
                    to=[hs_command("move_up()")],
                )
            ],
        ),
        Rule(
            description="[window management] [Internal Keyboard] cmd+alt j is move window down",
            manipulators=[
                Manipulator(
                    conditions=[internal_kb_cond],
                    _from=FromKey(key_code="j", modifiers=internal_kb),
                    to=[hs_command("move_down()")],
                )
            ],
        ),
        Rule(
            description="[window management] [Internal Keyboard] cmd+alt f is maximize window",
            manipulators=[
                Manipulator(
                    conditions=[internal_kb_cond],
                    _from=FromKey(key_code="f", modifiers=internal_kb),
                    to=[hs_command("full()")],
                )
            ],
        ),
        Rule(
            description="[window management] [Redox] hyper h is move window left",
            manipulators=[
                Manipulator(
                    conditions=[redox_kb_cond],
                    _from=FromKey(key_code="h", modifiers=redox_kb),
                    to=[hs_command("move_left()")],
                )
            ],
        ),
        Rule(
            description="[window management] [Redox] hyper l is move window right",
            manipulators=[
                Manipulator(
                    conditions=[redox_kb_cond],
                    _from=FromKey(key_code="l", modifiers=redox_kb),
                    to=[hs_command("move_right()")],
                )
            ],
        ),
        Rule(
            description="[window management] [Redox] hyper k is move window up",
            manipulators=[
                Manipulator(
                    conditions=[redox_kb_cond],
                    _from=FromKey(key_code="k", modifiers=redox_kb),
                    to=[hs_command("move_up()")],
                )
            ],
        ),
        Rule(
            description="[window management] [Redox] hyper j is move window down",
            manipulators=[
                Manipulator(
                    conditions=[redox_kb_cond],
                    _from=FromKey(key_code="j", modifiers=redox_kb),
                    to=[hs_command("move_down()")],
                )
            ],
        ),
        Rule(
            description="[window management] [Redox] hyper f is maximize window",
            manipulators=[
                Manipulator(
                    conditions=[redox_kb_cond],
                    _from=FromKey(key_code="f", modifiers=redox_kb),
                    to=[hs_command("full()")],
                )
            ],
        ),
    ]


def generate_rules() -> list[Rule]:
    return (
        generate_window()
        + generate_kitty()
        + generate_alfred()
        + generate_discord()
        + generate_slack()
        + generate_spotify()
        + generate_imessage()
        + generate_internal_mods()
        + generate_cmd_window_switch()
    )


def main():
    path = os.path.expanduser("~/.config/karabiner/karabiner.json")

    # generate the rules
    rules = generate_rules()

    # read the existing config
    with open(path, "r") as cfgfile:
        cfg = json.load(cfgfile)
        # swap the rules into the config
        cfg["profiles"][0]["complex_modifications"]["rules"] = rules

    # read the existing config
    with open(path, "w") as cfgfile:
        # write out the config
        json.dump(cfg, cfgfile, cls=Encoder, indent=4)


if __name__ == "__main__":
    main()
