-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices

-- local colors = wezterm.color.load_scheme(wezterm.home_dir .. "/.dotfiles/wezterm")
-- config.color_scheme = colors

config.font = wezterm.font 'Fira Code Regular'

-- and finally, return the configuration to wezterm
return config
