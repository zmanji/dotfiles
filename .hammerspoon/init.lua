local window_mash = {"cmd", "alt"}
local reverse_window_mash = {"shift", "cmd", "alt"}


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

-- TODO(zmanji): Consider a window filter to allow for 'invisible'/'minimized'
-- windows like spotify or more sophisticated behaviour
local switcher = hs.window.switcher.new()
hs.hotkey.bind(window_mash, "tab", function()
  switcher:next()
end,
nil,
function ()
  switcher:next()
end)

hs.hotkey.bind(reverse_window_mash, "tab", function()
  switcher:previous()
end,
nil,
function ()
  switcher:previous()
end)

-- http://www.hammerspoon.org/docs/hs.usb.watcher.html
swich_karabiner_profile = function(usb_table)
  -- Sculpt Keyboard IDs
  if usb_table.vendorID == 1118 and usb_table.productID == 1957 then
    if usb_table.eventType == 'added' then
      output, status, ty, rc = hs.execute("/Library/Application\\ Support/org.pqrs/Karabiner-Elements/bin/karabiner_cli --select-profile 'MSFT Sculpt'")
    elseif usb_table.eventType == 'removed' then
      output, status, ty, rc = hs.execute("/Library/Application\\ Support/org.pqrs/Karabiner-Elements/bin/karabiner_cli --select-profile 'Default'")
    end
  end
end

watcher = hs.usb.watcher.new(swich_karabiner_profile)
watcher:start()
