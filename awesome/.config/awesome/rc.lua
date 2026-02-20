local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
local wibox = require("wibox")
local beautiful = require("beautiful")
local naughty = require("naughty")

-- Load theme
beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")
-- theme.wallpaper = themes_path .. "default/background.png"
-- theme.wallpaper = "/usr/share/backgrounds/my-awesome-wallpaper.png"

-- Define default applications
terminal = "ghostty"
editor = os.getenv("EDITOR") or "nvim"
editor_cmd = terminal .. " -e " .. editor

-- Define modkey
modkey = "Mod4"

awful.layout.layouts = {
	-- awful.layout.suit.floating,
	awful.layout.suit.tile,
	-- awful.layout.suit.tile.left,
	-- awful.layout.suit.tile.bottom,
	-- awful.layout.suit.tile.top,
	-- awful.layout.suit.fair,
	-- awful.layout.suit.fair.horizontal,
	-- awful.layout.suit.spiral,
	-- awful.layout.suit.spiral.dwindle,
	-- awful.layout.suit.max,
	-- awful.layout.suit.max.fullscreen,
	-- awful.layout.suit.magnifier,
	-- awful.layout.suit.corner.nw,
	-- awful.layout.suit.corner.ne,
	-- awful.layout.suit.corner.sw,
	-- awful.layout.suit.corner.se,
}

-- Keybindings
-- globalkeys = gears.table.join(
-- 	awful.key({ modkey }, "s", hotkeys_popup.show_help, { description = "show help", group = "awesome" }),
-- 	awful.key({ modkey }, "Left", awful.tag.viewprev, { description = "view previous", group = "tag" }),
-- 	awful.key({ modkey }, "Right", awful.tag.viewnext, { description = "view next", group = "tag" })
-- )
--
-- root.keys(globalkeys)
