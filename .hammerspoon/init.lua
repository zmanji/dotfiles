local window_mash = {"cmd", "alt"}
local meh = {"ctrl", "shift", "alt", "cmd"}

local log = hs.logger.new('zmanji')

hs.window.filter.forceRefreshOnSpaceChange = true

function move_left()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f, 0)
end

-- laptop
hs.hotkey.bind(window_mash, "H", function()
  move_left()
end)

-- redox
hs.hotkey.bind(meh, "H", function()
  move_left()
end)

function move_right()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + (max.w / 2)
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f, 0)
end

-- laptop
hs.hotkey.bind(window_mash, "L", function()
  move_right()
end)

--redox
hs.hotkey.bind(meh, "L", function()
  move_right()
end)

function move_up()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w
  f.h = max.h / 2
  win:setFrame(f, 0)
end

-- laptop
hs.hotkey.bind(window_mash, "K", function()
  move_up()
end)

-- redox
hs.hotkey.bind(meh, "K", function()
  move_up()
end)

function move_down()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y + (max.h / 2)
  f.w = max.w
  f.h = max.h / 2
  win:setFrame(f, 0)
end

-- laptop
hs.hotkey.bind(window_mash, "J", function()
  move_down()
end)

-- redox
hs.hotkey.bind(meh, "J", function()
  move_down()
end)

function full()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w
  f.h = max.h
  win:setFrame(f, 0)
end

-- laptop
hs.hotkey.bind(window_mash, "F", function()
  full()
end)

-- redox
hs.hotkey.bind(meh, "F", function()
  full()
end)

-- TODO: find a way to visually indicate the active modal
local hyperModal = hs.hotkey.modal.new({}, "f17", nil)

hyperModal:bind({}, "escape", function()
  hyperModal:exit()
end)

hyperModal:bind({}, "k", function()
  hs.application.launchOrFocusByBundleID("com.googlecode.iterm2")
  hyperModal:exit()
end)

hyperModal:bind({}, "j", function()
  hs.application.launchOrFocusByBundleID("com.google.Chrome")
  hyperModal:exit()
end)

-- NOTE: This sometimes doesn't see windows in other spaces
local expose = hs.expose.new()
hs.hotkey.bind(window_mash, "space", function()
  expose:toggleShow()
  hyperModal:exit()
end)

local filter = hs.window.filter.new(function (w)
  return w:isStandard()
end)
local switcher = hs.window.switcher.new(filter)
hs.hotkey.bind({"cmd"}, "f19", function()
  switcher:next()
  hyperModal:exit()
end,
nil,
function ()
  switcher:next()
 end)

hs.hotkey.bind({"cmd", "shift"}, "f19", function()
  switcher:previous()
  hyperModal:exit()
end,
nil,
function ()
  switcher:previous()
end)

-- within app only window switching, activeApplication=true doesn't work for
-- some reason
local appFilter = hs.window.filter.new(function (w)
  return w:application():isFrontmost() and w:isStandard()
end)
local appSwitcher = hs.window.switcher.new(appFilter)
hs.hotkey.bind({"cmd"}, "f18", function()
  appSwitcher:next()
  hyperModal:exit()
end,
nil,
function ()
  appSwitcher:next()
 end)

hs.hotkey.bind({"cmd", "shift"}, "f18", function()
  appSwitcher:previous()
  hyperModal:exit()
end,
nil,
function ()
  appSwitcher:previous()
end)
