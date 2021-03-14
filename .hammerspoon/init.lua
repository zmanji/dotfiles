local window_mash = {"cmd", "alt"}
local meh = {"ctrl", "shift", "alt", "cmd"}

local log = hs.logger.new('zmanji')

hs.window.filter.forceRefreshOnSpaceChange = true
hs.window.animationDuration = 0
-- hs.window.setFrameCorrectness = true

-- Disable default callback for hs.chooser
hs.chooser.globalCallback = function(a) end

-- Window Management Objects
-- NOTE: This sometimes doesn't see windows in other spaces

hs.expose.ui.fontName = ".AppleSystemUIFont"

local expose = hs.expose.new()

function is_important_window(w)
    return w:isStandard() and w:isVisible()
end

-- hs.window.switcher.ui.fontName = ".AppleSystemUIFont"
local filter = hs.window.filter.new(function (w)
  return is_important_window(w)
end)

-- Window Management Functions

function move_left()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  local half_width = math.floor(max.w / 2)
  local third_width = math.floor(max.w / 3)
  local two_thirds_width = math.floor(max.w * 2 / 3)

  -- if moving to position for the first time then always half width
  if math.abs(f.x - max.x) > 10 then
    f.w = half_width
  elseif math.abs(f.w - half_width) <= 10 then
    f.w = third_width
  elseif math.abs(f.w - third_width) <= 10 then
    f.w = two_thirds_width
  else
    f.w = half_width
  end

  f.x = max.x
  f.y = max.y
  f.h = max.h
  win:setFrame(f, 0)
end

function move_right()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  local half_width = math.floor(max.w / 2)
  local third_width = math.floor(max.w / 3)
  local two_thirds_width = math.floor(max.w * 2 / 3)

  -- if moving to position for the first time then always half width
  -- is the screen not on the right half of the screen?
  if (f.x < (max.w / 2)) then
    f.w = half_width
    f.x = max.x + (max.w / 2)
  -- otherwise regular logic to size
  elseif math.abs(f.w - half_width) <= 10 then
    f.w = third_width
    f.x = max.x + (max.w * 2 / 3)
  elseif math.abs(f.w - third_width) <= 10 then
    f.w = two_thirds_width
    f.x = max.x + (max.w * 1 / 3)
  else
    f.w = half_width
    f.x = max.x + (max.w / 2)
  end

  f.y = max.y
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

function window_chooser()

  local chooser = hs.chooser.new(function(choice)
    if choice then
      -- For reasons unknown looking up the window id if the window id
      -- is not in this space fails
      -- recall the filter again here
      local window_id = choice["window_id"]
      local w = hs.fnutils.find(filter:getWindows(), function(w)
        return w:id() == window_id
      end)
      if w then
        w:focus()
      end
    end
   end)


  chooser:choices(function()
    local options = {}
    local current_focus = hs.window.focusedWindow()

    hs.fnutils.ieach(filter:getWindows(), function(w)
      -- Don't add the currently focused window here
      if current_focus:id() == w:id() then
        return
      end

      local app = w:application()
      local title = w:title()
      local fText = app:name() .. " " .. title
      local styledText = hs.styledtext.new(
        fText,
        {font = {name = ".AppleSystemUIFont", size = 16.0},
        color = hs.drawing.color.definedCollections["hammerspoon"]["white"] }
        )

      local icon = hs.image.imageFromAppBundle(app:bundleID())

      table.insert(options, {
          text = styledText,
          rawText = fText,
          image = icon,
          window_id = w:id()}
      )
    end)

    -- filter the table using fzf if there is a query
    local q = chooser:query()

    if q == nil then
      return options
    end

    if q == "" then
      return options
    end

    -- Write data to temp file for fzf since piping to a process in lua is not
    -- easy
    local temp_file = os.tmpname()
    local f = io.open(temp_file, "w")
    hs.fnutils.ieach(options, function(w)
      f:write(w["rawText"])
      f:write("\t")
      f:write(w["window_id"])
      f:write("\t")
      f:write("\n")
    end)
    f:close()

    -- call fzf with file
    local command = "cat " .. temp_file .. " | /usr/local/bin/fzf --delimiter '\\t' --nth=1 --filter '" .. q .. "' | cut -f 2"
    -- log:e(command)

    local output, status, type, rc = hs.execute(command)

    -- log:e(hs.inspect(output))
    -- log:e(hs.inspect(status))
    -- log:e(type)
    -- log:e(hs.inspect(rc))
    -- cleanup file
    os.remove(temp_file)

    local matching_window_ids = {}
    for s in output:gmatch("[^\n]+") do
      matching_window_ids[tonumber(s)] = true
    end

    local filtered_options = {}

    hs.fnutils.ieach(options, function(w)
      id = w["window_id"]
      if matching_window_ids[id] then
        table.insert(filtered_options, w)
      end
    end)

    -- auto select if there is only one entry
    if #filtered_options == 1 then
      hs.timer.doAfter(0.1, function()
        chooser:select()
      end)
    end

    return filtered_options
  end)

  chooser:queryChangedCallback(function(q)
    chooser:refreshChoicesCallback()
  end)

  chooser:show()
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
  window_chooser()
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
  window_chooser()
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


