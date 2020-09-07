local window_mash = {"cmd", "alt"}
local meh = {"ctrl", "shift", "alt", "cmd"}

local log = hs.logger.new('zmanji')

hs.window.filter.forceRefreshOnSpaceChange = true
hs.window.animationDuration = 0
hs.window.setFrameCorrectness = true


-- Window Management Objects
-- NOTE: This sometimes doesn't see windows in other spaces
local expose = hs.expose.new()


local filter = hs.window.filter.new(function (w)
  return w:isStandard()
end)
local switcher = hs.window.switcher.new(filter)
-- within app only window switching, activeApplication=true doesn't work for
-- some reason
local appFilter = hs.window.filter.new(function (w)
  return w:application():isFrontmost() and w:isStandard()
end)
local appSwitcher = hs.window.switcher.new(appFilter)

-- Window Management Functions

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

-- Key bindings
-- Laptop
local laptopHotkeys = {}
table.insert(laptopHotkeys, hs.hotkey.new(window_mash, "H", function()
  move_left()
end)
)


table.insert(laptopHotkeys, hs.hotkey.new(window_mash, "L", function()
  move_right()
end)
)

table.insert(laptopHotkeys, hs.hotkey.new(window_mash, "K", function()
  move_up()
end)
)

table.insert(laptopHotkeys, hs.hotkey.new(window_mash, "J", function()
  move_down()
end)
)


table.insert(laptopHotkeys, hs.hotkey.new(window_mash, "F", function()
  full()
end)
)

table.insert(laptopHotkeys, hs.hotkey.new(window_mash, "space", function()
  expose:toggleShow()
end)
)


function bindLaptopHotkeys()
  for i, hk in ipairs(laptopHotkeys) do
    hk:enable()
  end
end

function unbindLaptopHotkeys()
  for i, hk in ipairs(laptopHotkeys) do
    hk:disable()
  end
end


-- Redox
local redoxHotkeys = {}
table.insert(redoxHotkeys, hs.hotkey.new(meh, "H", function()
  move_left()
end)
)

table.insert(redoxHotkeys, hs.hotkey.new(meh, "L", function()
  move_right()
end)
)

table.insert(redoxHotkeys, hs.hotkey.new(meh, "K", function()
  move_up()
end)
)

table.insert(redoxHotkeys, hs.hotkey.new(meh, "J", function()
  move_down()
end)
)

table.insert(redoxHotkeys, hs.hotkey.new(meh, "F", function()
  full()
end)
)

table.insert(redoxHotkeys, hs.hotkey.new(meh, "space", function()
  expose:toggleShow()
end)
)


function bindRedoxHotkeys()
  for i, hk in ipairs(redoxHotkeys) do
    hk:enable()
  end
end

function unbindRedoxHotkeys()
  for i, hk in ipairs(redoxHotkeys) do
    hk:disable()
  end
end

-- Hotkey setup
-- Always bind the laptop hotkeys
bindLaptopHotkeys()

--- http://www.hammerspoon.org/docs/hs.usb.watcher.html
rotateHotkeys = function(usb_table)
  -- Redox uses QMK so the vendor id and product id comes from the firmware
  -- 0xFEED = 65261 is the default vendor id
  -- 0 is the default product id for any keyboard
  -- Falbatech is the vendor which should be good enough to uniqely identify
  if usb_table.vendorID == 65261 and usb_table.productID == 0 and usb_table.vendorName == "Falbatech" then
    if usb_table.eventType == 'added' then
      bindRedoxHotkeys()
      unbindlaptophotkeys()
    elseif usb_table.eventType == 'removed' then
      unbindRedoxHotkeys()
      bindLaptopHotkeys()
    end
  end
end

watcher = hs.usb.watcher.new(rotateHotkeys)
watcher:start()


-- Global Shortcuts
-- F19 and F18 come from Karabiner highjacking cmd-tab and cmd-`

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
