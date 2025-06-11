-- Pull in the wezterm API
local wezterm = require("wezterm")

-- Plugins
local resurrect = wezterm.plugin.require("https://github.com/MLFlexer/resurrect.wezterm")

-- This will hold the configuration.
local config = wezterm.config_builder()

config.color_scheme = "Catppuccin Mocha" -- or Macchiato, Frappe, Latte
config.window_decorations = "INTEGRATED_BUTTONS|RESIZE"
config.enable_tab_bar = false
config.integrated_title_button_style = "MacOsNative"
config.integrated_title_button_color = "Auto"
config.enable_scroll_bar = true
config.dpi = 96

config.leader = { key = "s", mods = "CTRL", timeout_milliseconds = 1000 } -- Set CTRL-a as the leader key.

local act = wezterm.action
config.keys = {
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
	-- Resurrect
	{
		mods = "LEADER",
		key = "w",
		action = wezterm.action_callback(function(win, pane)
			resurrect.state_manager.save_state(resurrect.workspace_state.get_workspace_state())
		end),
	},
	{ mods = "LEADER", key = "W", action = resurrect.window_state.save_window_action() },
	{ mods = "LEADER", key = "T", action = resurrect.tab_state.save_tab_action() },
	{
		mods = "LEADER",
		key = "s",
		action = wezterm.action_callback(function(win, pane)
			resurrect.state_manager.save_state(resurrect.workspace_state.get_workspace_state())
			resurrect.window_state.save_window_action()
		end),
	},
	{
		mods = "LEADER",
		key = "r",
		action = wezterm.action_callback(function(win, pane)
			resurrect.fuzzy_loader.fuzzy_load(win, pane, function(id, label)
				local type = string.match(id, "^([^/]+)") -- match before '/'
				id = string.match(id, "([^/]+)$") -- match after '/'
				id = string.match(id, "(.+)%..+$") -- remove file extention
				local opts = {
					relative = true,
					restore_text = true,
					on_pane_restore = resurrect.tab_state.default_on_pane_restore,
				}
				if type == "workspace" then
					local state = resurrect.state_manager.load_state(id, "workspace")
					resurrect.workspace_state.restore_workspace(state, opts)
				elseif type == "window" then
					local state = resurrect.state_manager.load_state(id, "window")
					resurrect.window_state.restore_window(pane:window(), state, opts)
				elseif type == "tab" then
					local state = resurrect.state_manager.load_state(id, "tab")
					resurrect.tab_state.restore_tab(pane:tab(), state, opts)
				end
			end)
		end),
	},
	{
		mods = "LEADER",
		key = "d",
		action = wezterm.action_callback(function(win, pane)
			resurrect.fuzzy_loader.fuzzy_load(win, pane, function(id)
				resurrect.state_manager.delete_state(id)
			end, {
				title = "Delete State",
				description = "Select State to Delete and press Enter = accept, Esc = cancel, / = filter",
				fuzzy_description = "Search State to Delete: ",
				is_fuzzy = true,
			})
		end),
	},
}

return config
