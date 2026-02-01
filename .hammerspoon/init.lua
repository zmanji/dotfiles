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
  local value = axapp.AXEnhancedUserInterface or axapp.AXManualAccessibility
  return value
end

function set_enhanced_ui(w, v)
  local app = w:application()
  local axapp = hs.axuielement.applicationElement(app)
  axapp.AXEnhancedUserInterface = v
  axapp.AXManualAccessibility = v
end


local axapps = {
  "com.spotify.client",
  "org.mozilla.firefox",
  "org.mozilla.thunderbird",
  "org.whispersystems.signal-desktop",
}

local electronapps = {
  "com.spotify.client",
  "org.whispersystems.signal-desktop",
  'com.hnc.Discord',
}

-- Vimac will set this setting for everything but things can break, so just set
-- it for a few apps that work safely.
function appAXEnhance(appName, eventType, app)
    if (eventType == hs.application.watcher.activated) then
        if hs.fnutils.contains(axapps, app:bundleID()) == true then
            local axapp = hs.axuielement.applicationElement(app)
            if (axapp == nil) then
              return
            end
            axapp.AXEnhancedUserInterface = true
            if electronapps[app:bundleID()] ~= nil then
              axapp.AXManualAccessibility = true
            end
        end
    end
end
local appWatcher = hs.application.watcher.new(appAXEnhance)
appWatcher:start()

local defocusapps = {
  'com.spotify.client',
  'com.grailr.CARROTweather',
  'com.apple.MobileSMS',
  'com.hnc.Discord',
  "org.whispersystems.signal-desktop",
  "com.yahoo.finance",
  "com.markmcguill.strongbox.pro",
  "com.apple.finder",
  "org.mozilla.thunderbird",
}

function is_defocous_window(w)
    if (w == nil) then
      return false
    end
    local app = w:application()
    if (app == nil) then
      return false
    end
    return hs.fnutils.contains(defocusapps, app:bundleID())
end

function try_to_defocus(w, name, event) 
    if (w == nil) then
      return
    end
  -- is this the last window of the app? if so try to defocus
  local app = w:application()
  if (app == nil) then
    return
  end

  log.w("Trying to defocus " .. app:bundleID())

  local tofocus = filter:getWindows()[1]
  tofocus:focus()
  log.w("Focusing to " .. tofocus:application():bundleID())
end

local defocus = hs.window.filter.new(is_defocous_window)
defocus:subscribe(hs.window.filter.hasNoWindows, try_to_defocus)

-- Window Management Functions
-- these are called by karabbiner
--
local horizonal_offsets = {1/2, 1/3, 2/3}

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
  if (f.x > 0 or f.h < 0.98 or (f.h >= 0.97 and f.w > 0.97)) then
    win:move({0, 0, 1/2, 1}, nil, true)
  else 
    -- find the closest offset and then shift one over
    local idx = 1
    for k,v in pairs(horizonal_offsets) do
       local diff = math.abs(f.w - v)
       if diff <= 0.01 then
         idx = k
         break
       end
    end

    if idx == #horizonal_offsets then
      idx = 1
    else 
      idx = idx + 1
    end

    win:move({0, 0, horizonal_offsets[idx], 1}, nil, true)
  end

  if is_enhanced then
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
  if (f.x < 0.5 or f.h < 0.98 or (f.h >= 0.97 and f.w > 0.97)) then
    win:move({1/2, 0, 1/2, 1}, nil, true)

  else 
    -- find the closest offset and then shift one over
    local idx = 1
    for k,v in pairs(horizonal_offsets) do
       local diff = math.abs(f.w - v)
       if diff <= 0.01 then
         idx = k
         break
       end
    end

    if idx == #horizonal_offsets then
      idx = 1
    else 
      idx = idx + 1
    end

    win:move({1 - horizonal_offsets[idx], 0, horizonal_offsets[idx], 1}, nil, true)

  end

  if is_enhanced then
    set_enhanced_ui(win, true)
  end

end

function move_up()
  local win = hs.window.focusedWindow()
  local f = win:frame():toUnitRect(win:screen():frame())
  local is_enhanced = false

  if is_enhanced_ui(win) then
    is_enhanced = true
    set_enhanced_ui(win, false)
  end

  win:move({f.x, 0, f.w, 0.5}, nil, true)

  if is_enhanced then
    set_enhanced_ui(win, true)
  end
end

function move_down()
  local win = hs.window.focusedWindow()
  local f = win:frame():toUnitRect(win:screen():frame())
  local is_enhanced = false

  if is_enhanced_ui(win) then
    is_enhanced = true
    set_enhanced_ui(win, false)
  end

  win:move({f.x, 0.5, f.w, 0.5}, nil, true)
  if is_enhanced then
    set_enhanced_ui(win, true)
  end
end

function full()
  local win = hs.window.focusedWindow()

  win:move({0, 0, 1, 1}, nil, true)
end

function naturalTrackpadSwipe(direction, strength)
    strength = strength or 1.0
    
    local baseDistance = 100 * strength
    
    local directionMap = {
        left = {baseDistance, 0},
        right = {-baseDistance, 0},
        up = {0, baseDistance},
        down = {0, -baseDistance}
    }
    
    local delta = directionMap[direction]
    if not delta then return end
    
    -- Gesture phases
    local function sendPhase(dx, dy, scrollPhase, momentumPhase)
        local event = hs.eventtap.event.newScrollEvent({dx, dy}, {}, "pixel")
        event:setProperty(hs.eventtap.event.properties.scrollWheelEventScrollPhase, scrollPhase)
        event:setProperty(hs.eventtap.event.properties.scrollWheelEventMomentumPhase, momentumPhase)
        event:post()
    end
    
    -- Begin
    sendPhase(delta[1], delta[2], 1, 0)
    hs.timer.usleep(10000)
    
    -- Changed (accelerating)
    local steps = {0.7, 1.0, 1.2, 1.0}
    for _, scale in ipairs(steps) do
        sendPhase(delta[1] * scale, delta[2] * scale, 2, 0)
        hs.timer.usleep(10000)
    end
    
    -- Ended
    sendPhase(0, 0, 4, 0)
    hs.timer.usleep(10000)
    
    -- Momentum (decelerating)
    local momentumSteps = {0.8, 0.6, 0.4, 0.25, 0.15, 0.08, 0.03}
    
    -- Momentum began
    sendPhase(delta[1] * momentumSteps[1], delta[2] * momentumSteps[1], 0, 1)
    hs.timer.usleep(10000)
    
    -- Momentum changed
    for i = 2, #momentumSteps do
        sendPhase(delta[1] * momentumSteps[i], delta[2] * momentumSteps[i], 0, 2)
        hs.timer.usleep(10000)
    end
    
    -- Momentum ended
    sendPhase(0, 0, 0, 4)
end


function swipe_left()
  naturalTrackpadSwipe("left", 1.5)
end

function swipe_right()
  naturalTrackpadSwipe("right", 1.5)
end
