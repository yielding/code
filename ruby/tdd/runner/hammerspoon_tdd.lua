local canvas = require("hs.canvas")
local base64 = require("hs.base64")
local image = require("hs.image")
local screen = require("hs.screen")
local timer = require("hs.timer")

local status = "info"
local appName = "Ruby TDD"
local title = "Specs Green"
local message = "Tests passed"
local bundleID = "com.apple.Terminal"

if _cli and _cli.args and #_cli.args >= 5 then
  status = _cli.args[#_cli.args - 4]
  appName = base64.decode(_cli.args[#_cli.args - 3])
  title = base64.decode(_cli.args[#_cli.args - 2])
  message = base64.decode(_cli.args[#_cli.args - 1])
  bundleID = base64.decode(_cli.args[#_cli.args])
elseif _cli and _cli.args and #_cli.args > 0 then
  status = _cli.args[#_cli.args]
end

local palette = {
  pass = {
    iconFill = { red = 0.19, green = 0.70, blue = 0.34, alpha = 1.00 },
    iconText = { white = 1, alpha = 1.00 },
    progress = { red = 0.19, green = 0.70, blue = 0.34, alpha = 1.00 },
  },
  fail = {
    iconFill = { red = 0.88, green = 0.25, blue = 0.25, alpha = 1.00 },
    iconText = { white = 1, alpha = 1.00 },
    progress = { red = 0.88, green = 0.25, blue = 0.25, alpha = 1.00 },
  },
  info = {
    iconFill = { red = 0.48, green = 0.50, blue = 0.56, alpha = 1.00 },
    iconText = { white = 1, alpha = 1.00 },
    progress = { red = 0.48, green = 0.50, blue = 0.56, alpha = 1.00 },
  },
}

local style = palette[status] or palette.info
local textFont = "Agave Nerd Font Mono"

local targetScreen = screen.mainScreen()
local frame = targetScreen:frame()
local overlayWidth = 348
local overlayHeight = 78
local marginTop = 20
local marginRight = 20
local stackGap = 10
local displaySeconds = 2.2
local maxVisible = 3
local appIcon = image.imageFromAppBundle(bundleID)

local overlayFrame = {
  x = frame.x + frame.w - overlayWidth - marginRight,
  y = frame.y + marginTop,
  w = overlayWidth,
  h = overlayHeight,
}

local function trimOverlays()
  _G.__ruby_tdd_overlays = _G.__ruby_tdd_overlays or {}
  local active = {}
  for _, entry in ipairs(_G.__ruby_tdd_overlays) do
    if entry and entry.canvas then
      table.insert(active, entry)
    end
  end
  _G.__ruby_tdd_overlays = active
end

local function layoutOverlays()
  trimOverlays()
  for index, entry in ipairs(_G.__ruby_tdd_overlays) do
    local newFrame = {
      x = overlayFrame.x,
      y = overlayFrame.y + (index - 1) * (overlayHeight + stackGap),
      w = overlayWidth,
      h = overlayHeight,
    }
    entry.frame = newFrame
    entry.canvas:frame(newFrame)
  end
end

local function removeOverlay(entry)
  if not entry then
    return
  end

  if entry.progressTimer then
    entry.progressTimer:stop()
    entry.progressTimer = nil
  end

  if entry.dismissTimer then
    entry.dismissTimer:stop()
    entry.dismissTimer = nil
  end

  if entry.canvas then
    entry.canvas:delete()
    entry.canvas = nil
  end

  trimOverlays()
  for index, current in ipairs(_G.__ruby_tdd_overlays) do
    if current == entry then
      table.remove(_G.__ruby_tdd_overlays, index)
      break
    end
  end
  layoutOverlays()
end

local overlay = canvas.new(overlayFrame)
overlay:level("status")
overlay:behavior({
  "canJoinAllSpaces",
  "stationary",
})
overlay[1] = {
  type = "rectangle",
  action = "fill",
  antialias = false,
  roundedRectRadii = { xRadius = 0, yRadius = 0 },
  fillColor = { red = 0.16, green = 0.16, blue = 0.17, alpha = 0.90 },
  strokeColor = { white = 1, alpha = 0.00 },
  strokeWidth = 0,
  withShadow = false,
}
overlay[2] = {
  type = "rectangle",
  action = "fill",
  fillColor = { white = 1, alpha = 0.00 },
  frame = { x = 0, y = 0, w = 0, h = 0 },
  roundedRectRadii = { xRadius = 0, yRadius = 0 },
}
overlay[3] = {
  type = "rectangle",
  action = "fill",
  fillColor = style.iconFill,
  antialias = false,
  frame = { x = 16, y = 24, w = 24, h = 24 },
}
overlay[4] = {
  type = "segments",
  action = "stroke",
  strokeColor = style.iconText,
  strokeWidth = 2.2,
  strokeCapStyle = "round",
  strokeJoinStyle = "round",
  coordinates = status == "pass" and {
    { x = 21, y = 37 },
    { x = 25, y = 41 },
    { x = 35, y = 30 },
  } or {
    { x = 21, y = 30 },
    { x = 35, y = 44 },
    { x = 28, y = 37 },
    { x = 35, y = 30 },
    { x = 21, y = 44 },
  },
}
overlay[5] = {
  type = "text",
  text = title,
  textColor = { white = 1, alpha = 0.98 },
  textFont = textFont,
  textSize = 15,
  frame = { x = 52, y = 16, w = 276, h = 20 },
  textLineBreak = "truncateTail",
}
overlay[6] = {
  type = "text",
  text = message,
  textColor = { white = 1, alpha = 0.62 },
  textFont = textFont,
  textSize = 15,
  frame = { x = 52, y = 36, w = 276, h = 24 },
  textLineBreak = "wordWrap",
}
overlay:show()

local entry = {
  canvas = overlay,
  frame = overlayFrame,
  createdAt = os.time(),
}

trimOverlays()
table.insert(_G.__ruby_tdd_overlays, 1, entry)
while #_G.__ruby_tdd_overlays > maxVisible do
  removeOverlay(_G.__ruby_tdd_overlays[#_G.__ruby_tdd_overlays])
end
layoutOverlays()

entry.dismissTimer = timer.doAfter(displaySeconds, function()
  removeOverlay(entry)
end)

local file = io.open("/tmp/ruby_tdd_hammerspoon_last.txt", "w")
if file then
  file:write(status .. "\n")
  file:write(appName .. "\n")
  file:write(title .. "\n")
  file:write(message .. "\n")
  file:close()
end
