-- Pull in the wezterm API
local wezterm = require("wezterm")

-- Plugins
local resurrect = wezterm.plugin.require("https://github.com/MLFlexer/resurrect.wezterm")

-- This will hold the configuration.
local config = wezterm.config_builder()

config.color_scheme = "Catppuccin Mocha" -- or Macchiato, Frappe, Latte
config.window_decorations = "INTEGRATED_BUTTONS|RESIZE"
config.enable_tab_bar = true
config.integrated_title_button_style = "MacOsNative"
config.integrated_title_button_color = "Auto"
config.enable_scroll_bar = true
config.dpi = 96

config.leader = { key = "s", mods = "CTRL", timeout_milliseconds = 1000 } -- Set CTRL-a as the leader key.

local act = wezterm.action
config.keys = {
	-- Backscroll Copy Mode
	{
		key = "[",
		mods = "LEADER",
		action = wezterm.action.ActivateCopyMode,
	},
	-- Rename Tab
	{
		key = ",",
		mods = "LEADER",
		action = act.PromptInputLine({
			description = "Enter new name for tab",
			action = wezterm.action_callback(function(window, pane, line)
				if line then
					window:active_tab():set_title(line)
				end
			end),
		}),
	},
	-- Pane Selection
	{
		mods = "LEADER",
		key = "i",
		action = wezterm.action.PaneSelect({
			show_pane_ids = true,
		}),
	},
	-- Splitting Panes
	{ mods = "LEADER", key = "-", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
	{ mods = "LEADER", key = "\\", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
	-- Navigation
	{ mods = "LEADER", key = "h", action = act.ActivatePaneDirection("Left") },
	{ mods = "LEADER", key = "j", action = act.ActivatePaneDirection("Down") },
	{ mods = "LEADER", key = "k", action = act.ActivatePaneDirection("Up") },
	{ mods = "LEADER", key = "l", action = act.ActivatePaneDirection("Right") },
}

-- SSH Domains
config.ssh_domains = {
	{
		name = "wrds",
		remote_address = "wrds-cloud-sshkey.wharton.upenn.edu",
		username = "eddyhu",
		remote_wezterm_path = "~/.local/bin/wezterm",
		ssh_option = {
			identityfile = "~/.ssh/wrds_rsa",
		},
	},
}

return config
