local window_mash = {"cmd", "alt"}

local log = hs.logger.new('zmanji')

hs.window.filter.forceRefreshOnSpaceChange = true

hs.hotkey.bind(window_mash, "H", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f, 0)
end)

hs.hotkey.bind(window_mash, "L", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + (max.w / 2)
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f, 0)
end)

hs.hotkey.bind(window_mash, "K", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w
  f.h = max.h / 2
  win:setFrame(f, 0)
end)

hs.hotkey.bind(window_mash, "J", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y + (max.h / 2)
  f.w = max.w
  f.h = max.h / 2
  win:setFrame(f, 0)
end)

hs.hotkey.bind(window_mash, "F", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w
  f.h = max.h
  win:setFrame(f, 0)
end)

-- NOTE: This sometimes doesn't see windows in other spaces
local expose = hs.expose.new()
hs.hotkey.bind(window_mash, "space", function()
  expose:show()
end)

local filter = hs.window.filter.new(function (w)
  return w:isStandard()
end)
local switcher = hs.window.switcher.new(filter)
hs.hotkey.bind({"cmd"}, "f19", function()
  switcher:next()
end,
nil,
function ()
  switcher:next()
 end)

hs.hotkey.bind({"cmd", "shift"}, "f19", function()
  switcher:previous()
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
end,
nil,
function ()
  appSwitcher:next()
 end)

hs.hotkey.bind({"cmd", "shift"}, "f18", function()
  appSwitcher:previous()
end,
nil,
function ()
  appSwitcher:previous()
end)