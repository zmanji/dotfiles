local window_mash = {"cmd", "alt"}
local meh = {"ctrl", "shift", "alt", "cmd"}

local log = hs.logger.new('zmanji')

require("hs.ipc")
-- used to use hs.ipc.cliInstall here but instead just add hs binary
-- straight to path

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
  "com.spotify.client",
  "org.mozilla.firefox",
  "com.electron.nativefier.todoist-nativefier-6c11dd",
}

-- Vimac will set this setting for everything but things can break, so just set
-- it for a few apps that work safely.
function appAXEnhance(appName, eventType, app)
    if (eventType == hs.application.watcher.activated) then
        if hs.fnutils.contains(axapps, app:bundleID()) == true then
            local axapp = hs.axuielement.applicationElement(app)
            axapp.AXEnhancedUserInterface = true
            axapp.AXManualAccessibility = true
        end
    end
end
local appWatcher = hs.application.watcher.new(appAXEnhance)
appWatcher:start()

local defocusapps = {
  'com.spotify.client',
  'com.grailr.CARROTweather',
  'com.apple.MobileSMS'
}

function is_defocous_window(w)
    local app = w:application()
    if (app == nil) then
      return false
    end
    return hs.fnutils.contains(defocusapps, app:bundleID())
end

function try_to_defocus(w, name, event) 
  -- is this the last window of the app? if so try to defocus
  local app = w:application()
  if (app == nil) then
    return
  end

  local tofocus = filter:getWindows()[1]
  tofocus:focus()
end

local defocus = hs.window.filter.new(is_defocous_window)
defocus:subscribe(hs.window.filter.hasNoWindows, try_to_defocus)

-- Window Management Functions
-- these are called by karabbiner

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

  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w
  f.h = max.h
  win:setFrame(f, 0)
end

function move_screen_north()
  local win = hs.window.focusedWindow()
  win:moveOneScreenNorth(true, true, 0)
end

function move_screen_south()
  local win = hs.window.focusedWindow()
  win:moveOneScreenSouth(true, true, 0)
end

function move_screen_east()
  local win = hs.window.focusedWindow()
  win:moveOneScreenEast(true, true, 0)
end

function move_screen_west()
  local win = hs.window.focusedWindow()
  win:moveOneScreenWest(true, true, 0)
end

-- this is called by alfred workflow
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

      if app:bundleID() == nil then
        return
      end

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


