#!/bin/sh

cli=/Applications/Karabiner.app/Contents/Library/bin/karabiner

$cli set repeat.wait 50
/bin/echo -n .
$cli set option.vimode_fn_hjkl 1
/bin/echo -n .
$cli set parameter.keyoverlaidmodifier_timeout 500
/bin/echo -n .
$cli set repeat.initial_wait 200
/bin/echo -n .
$cli set remap.controlL2controlL_escape 1
/bin/echo -n .
$cli set remap.hjkl_arrow 1
/bin/echo -n .
/bin/echo