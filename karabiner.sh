#!/bin/sh

cli=/Applications/Karabiner.app/Contents/Library/bin/karabiner

$cli set parameter.keyoverlaidmodifier_timeout 300
/bin/echo -n .
$cli set private.hyper_chrome 1
/bin/echo -n .
$cli set private.hyper_hints 1
/bin/echo -n .
$cli set private.hyper_iterm 1
/bin/echo -n .
$cli set private.hyper_key 1
/bin/echo -n .
$cli set remap.controlL2controlL_escape 1
/bin/echo -n .
$cli set repeat.initial_wait 200
/bin/echo -n .
$cli set repeat.keyoverlaidmodifier_initial_wait 300
/bin/echo -n .
$cli set repeat.wait 50
/bin/echo -n .
/bin/echo
