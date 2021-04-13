local window_mash = {"cmd", "alt"}
local meh = {"ctrl", "shift", "alt", "cmd"}

local log = hs.logger.new('zmanji')

require("hs.ipc")
hs.ipc.cliInstall()

require("hs.json")

hs.window.filter.forceRefreshOnSpaceChange = true
hs.window.animationDuration = 0
-- hs.window.setFrameCorrectness = true

-- Disable default callback for hs.chooser
hs.chooser.globalCallback = function(a) end

function is_important_window(w)
    return w:isStandard() and w:isVisible()
end

local filter = hs.window.filter.new(function (w)
  return is_important_window(w)
end)

-- see https://github.com/rxhanson/Rectangle/blob/7832ace7bc89824c6321c0f8923c5841f07f249b/Rectangle/AccessibilityElement.swift#L80
function is_enhanced_ui(w)
  local app = w:application()
  local axapp = hs.axuielement.applicationElement(app)
  local value = axapp.AXEnhancedUserInterface
  return value
end

function set_enhanced_ui(w, v)
  local app = w:application()
  local axapp = hs.axuielement.applicationElement(app)
  axapp.AXEnhancedUserInterface = v
end


local axapps = {
  "org.epichrome.app.Todoist",
  "com.spotify.client",
  "org.mozilla.firefox",
}

-- Vimac will set this setting for everything but things can break, so just set
-- it for a few apps that work safely.
function appAXEnhance(appName, eventType, app)
    if (eventType == hs.application.watcher.activated) then
        if hs.fnutils.contains(axapps, app:bundleID()) == true then
            local axapp = hs.axuielement.applicationElement(app)
            axapp.AXEnhancedUserInterface = true
        end
    end
end
local appWatcher = hs.application.watcher.new(appAXEnhance)
appWatcher:start()

-- Window Management Functions

function move_left()
  local win = hs.window.focusedWindow()
  local f = win:frame():toUnitRect(win:screen():frame())
  local is_enhanced = false

  if is_enhanced_ui(win) then
    is_enhanced = true
    set_enhanced_ui(win, false)
  end

  -- if moving to position for the first time then always half width
  -- is the screen not on the left half of the screen?
  if (f.x >= 0.5) then
    win:move({0, 0, 0.5, 1}, nil, true)
  elseif math.abs(f.w - 0.5) <= 0.1 then
    win:move({0, 0, 1/3, 1}, nil, true)
  elseif math.abs(f.w - 1/3) <= 0.1 then
    win:move({0, 0, 2/3, 1}, nil, true)
  else
    win:move({0, 0, 0.5, 1}, nil, true)
  end

  if is_enhaned then
    set_enhanced_ui(win, true)
  end
end

function move_right()
  local win = hs.window.focusedWindow()
  local f = win:frame():toUnitRect(win:screen():frame())
  local is_enhanced = false

  if is_enhanced_ui(win) then
    is_enhanced = true
    set_enhanced_ui(win, false)
  end

  -- if moving to position for the first time then always half width
  -- is the screen not on the right half of the screen?
  if (f.x < 0.5) then
    win:move({0.5, 0, 0.5, 1}, nil, true)
  -- otherwise regular logic to size
  elseif math.abs(f.w - 0.5) <= 0.1 then
    win:move({2/3, 0, 1/3, 1}, nil, true)
  elseif math.abs(f.w - 1/3) <= 0.1 then
    win:move({1/3, 0, 2/3, 1}, nil, true)
  else
    win:move({0.5, 0, 0.5, 1}, nil, true)
  end

  if is_enhaned then
    set_enhanced_ui(win, true)
  end

end

function move_up()
  local win = hs.window.focusedWindow()
  local is_enhanced = false

  if is_enhanced_ui(win) then
    is_enhanced = true
    set_enhanced_ui(win, false)
  end

  win:move({0, 0, 1, 0.5}, nil, true)

  if is_enhaned then
    set_enhanced_ui(win, true)
  end
end

function move_down()
  local win = hs.window.focusedWindow()
  local is_enhanced = false

  if is_enhanced_ui(win) then
    is_enhanced = true
    set_enhanced_ui(win, false)
  end

  win:move({0, 0.5, 1, 0.5}, nil, true)
  if is_enhaned then
    set_enhanced_ui(win, true)
  end
end

function full()
  local win = hs.window.focusedWindow()

  if win:application():bundleID() == "com.todoist.mac.Todoist" then
    -- do native fullscreen
    win:setFullscreen(true)
    return
  end

  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w
  f.h = max.h
  win:setFrame(f, 0)
end

function get_all_windows_json()
    local items = {}
    local current_focus = hs.window.focusedWindow()

    hs.fnutils.ieach(filter:getWindows(), function(w)
      -- Don't add the currently focused window here
      if current_focus:id() == w:id() then
        return
      end

      local app = w:application()
      local title = app:name() .. " " .. w:title()
      local bundlePath = hs.application.pathForBundleID(app:bundleID())
      local uid = app:bundleID() .. " " .. w:title()

      table.insert(items, {
          uid = uid,
          title = title,
          subtitle = "Switch to this window...",
          arg = w:id(),
          icon = {type = "fileicon", path = bundlePath},
          type = "file:skipcheck"
        }
      )
    end)

    return hs.json.encode({items = items}, true)
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
-- If the keyboard is connected, bind them otherwise bind the laptop
-- This is key when hammerspoon is reset when the keyboard is already
-- connected

local usbDevices = hs.usb.attachedDevices()
local hasKeyboard = false
for i in pairs(usbDevices) do
  -- See below for info about these values
  if usbDevices[i].vendorID == 65261 and usbDevices[i].productID == 0 and usbDevices[i].vendorName == "Falbatech" then
    hasKeyboard = true
  end
end

if hasKeyboard then
  bindRedoxHotkeys()
else
  bindLaptopHotkeys()
end

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


-- kitty specific modal mode

function kitty_go_left()
  local output, status, type, rc = hs.execute('/usr/local/bin/kitty @ --to=unix:/tmp/kitty.sock kitten neighbor.py left')
end

function kitty_go_right()
  local output, status, type, rc = hs.execute('/usr/local/bin/kitty @ --to=unix:/tmp/kitty.sock kitten neighbor.py right')
end

function kitty_go_up()
  local output, status, type, rc = hs.execute('/usr/local/bin/kitty @ --to=unix:/tmp/kitty.sock kitten neighbor.py top')
end

function kitty_go_down()
  local output, status, type, rc = hs.execute('/usr/local/bin/kitty @ --to=unix:/tmp/kitty.sock kitten neighbor.py bottom')
end

function kitty_next_tab()
  local output, status, type, rc = hs.execute('/usr/local/bin/kitty @ --to=unix:/tmp/kitty.sock kitten tab.py next')
end

function kitty_previous_tab()
  local output, status, type, rc = hs.execute('/usr/local/bin/kitty @ --to=unix:/tmp/kitty.sock kitten tab.py previous')
end

function kitty_close_window()
  local output, status, type, rc = hs.execute('/usr/local/bin/kitty @ --to=unix:/tmp/kitty.sock close-window')
end

function kitty_lighten_background()
  local output, status, type, rc = hs.execute('/usr/local/bin/kitty @ --to=unix:/tmp/kitty.sock set-colors -a "background=#5b6268"')
end


function kitty_reset_background()
  local output, status, type, rc = hs.execute('/usr/local/bin/kitty @ --to=unix:/tmp/kitty.sock set-colors -a --reset')
end

local kittyNavModal = hs.hotkey.modal.new()
kittyNavModal:bind({}, 'escape', function() kittyNavModal:exit() end)
kittyNavModal:bind({}, 'h', kitty_go_left, nil, kitty_go_left)
kittyNavModal:bind({}, 'j', kitty_go_down, nil, kitty_go_down)
kittyNavModal:bind({}, 'k', kitty_go_up, nil, kitty_go_up)
kittyNavModal:bind({}, 'l', kitty_go_right, nil, kitty_go_right)

kittyNavModal:bind({'shift'}, 'k', kitty_next_tab, nil, kitty_next_tab)
kittyNavModal:bind({'shift'}, 'j', kitty_previous_tab, nil, kitty_previous_tab)

kittyNavModal:bind({}, 'q', kitty_close_window, nil, close_window)

local kittyNavHotKey = hs.hotkey.new({'cmd'}, 'w', function()
  kittyNavModal:enter()
end)

function kittyNavModal:entered()
  kitty_lighten_background()
end
function kittyNavModal:exited()
  kitty_reset_background()
end


-- local wf_kitty = hs.window.filter.new{'kitty'}
-- wf_kitty:subscribe(hs.window.filter.windowFocused, function()
--           kittyNavHotKey:enable()
-- end )
-- wf_kitty:subscribe(hs.window.filter.windowUnfocused, function()
--           kittyNavHotKey:disable()
--           kittyNavModal:exit()
-- end)

-- hs.window.filter.new('kitty')
--   :subscribe({hs.window.filter.windowUnfcused, hs.window.filter.windowUnfocused}, function()
--     log:e(event)
--     if event == hs.window.filter.windowUnfocused then
--         elseif event == hs.window.filter.windowFocused then
--         end
--               end):start()

hs.application.watcher.new(function (appName, eventType, app)
    if app:bundleID() == 'net.kovidgoyal.kitty' then
        if (eventType == hs.application.watcher.activated) then
          kittyNavHotKey:enable()
        elseif (eventType == hs.application.watcher.deactivated) then
          kittyNavHotKey:disable()
          kittyNavModal:exit()
        end
    end
  end):start()
