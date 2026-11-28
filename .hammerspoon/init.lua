local log = hs.logger.new('zmanji')

require("hs.ipc")
-- used to use hs.ipc.cliInstall here but instead just add hs binary
-- straight to path

require("hs.json")

-- Disable default callback for hs.chooser
hs.chooser.globalCallback = function(a) end

local axapps = {
  "com.spotify.client",
  "org.mozilla.firefox",
  "org.whispersystems.signal-desktop",
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
        end
    end
end
local appWatcher = hs.application.watcher.new(appAXEnhance)
appWatcher:start()
