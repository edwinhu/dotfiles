-- Pull in the wezterm API
local wezterm = require("wezterm")

-- Plugins
local resurrect = wezterm.plugin.require("https://github.com/MLFlexer/resurrect.wezterm")

-- Workspace utilities
local workspace = require("workspace_utils")

-- This will hold the configuration.
local config = wezterm.config_builder()

config.color_scheme = "Catppuccin Mocha" -- or Macchiato, Frappe, Latte

-- Font Configuration
config.font = wezterm.font("JetBrainsMono Nerd Font", { weight = "Regular" })
config.font_size = 13

config.window_decorations = "INTEGRATED_BUTTONS|RESIZE"
config.enable_tab_bar = true
config.integrated_title_button_style = "MacOsNative"
config.integrated_title_button_color = "Auto"
config.enable_scroll_bar = true
config.dpi_by_screen = {
	["Built-in Display"] = 96, -- Example value for a retina display
	["DELL U3419W"] = 96,
	["Studio Display"] = 144,
}

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
	-- Newline in Claude Code
	{ mods = "SHIFT", key = "Enter", action = act.SendString("\n") },

	-- Custom workspace/project switcher
	{
		mods = "LEADER",
		key = "w",
		action = wezterm.action_callback(function(window, pane)
			workspace.workspace_switcher(window, pane)
		end),
	},
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

-- Event handlers for workspace management
wezterm.on("gui-startup", function()
	local tab, pane, window = wezterm.mux.spawn_window({})
	-- Set initial workspace name based on current directory
	workspace.rename_workspace_to_project(window, pane)
end)

-- Custom launcher entries
wezterm.on("update-launcher-menu", function(window, pane)
	local home = wezterm.home_dir
	local workspaces = wezterm.mux.get_workspace_names()
	local items = {}
	local seen = {}

	-- Add existing workspaces
	for _, ws in ipairs(workspaces) do
		table.insert(items, {
			label = "📂 " .. ws,
			args = { "wezterm", "cli", "activate", "--workspace", ws },
		})
		seen[ws] = true
	end

	-- Function to add projects from a directory
	local function add_projects(dir_path, prefix)
		local success, stdout = wezterm.run_child_process({
			"find",
			dir_path,
			"-maxdepth",
			"1",
			"-type",
			"d",
			"-not",
			"-path",
			dir_path,
		})
		if success then
			for project_path in stdout:gmatch("[^\n]+") do
				local project_name = project_path:match("([^/]+)$")
				if project_name and not seen[project_name] then
					table.insert(items, {
						label = "📁 " .. prefix .. "/" .. project_name,
						cwd = project_path,
						args = { wezterm.home_dir .. "/.nix-profile/bin/zsh" },
					})
				end
			end
		end
	end

	-- Add projects and areas
	add_projects(home .. "/projects", "projects")
	add_projects(home .. "/areas", "areas")

	-- Add specific directories if not seen
	if not seen["dotfiles"] then
		table.insert(items, {
			label = "📁 ~/dotfiles",
			cwd = home .. "/dotfiles",
			args = { wezterm.home_dir .. "/.nix-profile/bin/zsh" },
		})
	end

	if not seen["nix"] then
		table.insert(items, {
			label = "📁 ~/nix",
			cwd = home .. "/nix",
			args = { wezterm.home_dir .. "/.nix-profile/bin/zsh" },
		})
	end

	if not seen["Notes"] then
		table.insert(items, {
			label = "📁 ~/Documents/Notes",
			cwd = home .. "/Documents/Notes",
			args = { wezterm.home_dir .. "/.nix-profile/bin/zsh" },
		})
	end

	return items
end)

-- Custom launcher entries for projects
wezterm.on("augment-command-palette", function(window, pane)
	wezterm.log_info("augment-command-palette called")
	local home = wezterm.home_dir
	local workspaces = wezterm.mux.get_workspace_names()
	local commands = {}
	local seen = {}

	-- Add existing workspaces
	for _, ws in ipairs(workspaces) do
		table.insert(commands, {
			brief = "Workspace: " .. ws .. " [active]",
			icon = "md_folder_open",
			action = wezterm.action.SwitchToWorkspace({ name = ws }),
		})
		seen[ws] = true
	end

	-- Function to add projects from a directory
	local function add_projects(dir_path, prefix)
		local success, stdout = wezterm.run_child_process({
			"find",
			dir_path,
			"-maxdepth",
			"1",
			"-type",
			"d",
			"-not",
			"-path",
			dir_path,
		})
		if success then
			for project_path in stdout:gmatch("[^\n]+") do
				local project_name = project_path:match("([^/]+)$")
				if project_name and not seen[project_name] then
					table.insert(commands, {
						brief = "Workspace: " .. project_name .. " (" .. prefix .. ")",
						icon = "md_folder",
						action = wezterm.action.SwitchToWorkspace({
							name = project_name,
							spawn = { cwd = project_path },
						}),
					})
				end
			end
		end
	end

	-- Add projects and areas
	add_projects(home .. "/projects", "project")
	add_projects(home .. "/areas", "area")

	-- Add specific directories if not seen
	if not seen["dotfiles"] then
		table.insert(commands, {
			brief = "Workspace: dotfiles",
			icon = "md_folder",
			action = wezterm.action.SwitchToWorkspace({
				name = "dotfiles",
				spawn = { cwd = home .. "/dotfiles" },
			}),
		})
	end

	if not seen["nix"] then
		table.insert(commands, {
			brief = "Workspace: nix",
			icon = "md_folder",
			action = wezterm.action.SwitchToWorkspace({
				name = "nix",
				spawn = { cwd = home .. "/nix" },
			}),
		})
	end

	if not seen["Notes"] then
		table.insert(commands, {
			brief = "Workspace: Notes",
			icon = "md_folder",
			action = wezterm.action.SwitchToWorkspace({
				name = "Notes",
				spawn = { cwd = home .. "/Documents/Notes" },
			}),
		})
	end

	wezterm.log_info("Returning " .. #commands .. " workspace commands")
	return commands
end)

-- Helper function for right status segments
local function segments_for_right_status(window)
	local workspace = window:active_workspace()
	-- Add folder icon based on workspace status
	local workspace_icon = "📁"
	local active_workspaces = wezterm.mux.get_workspace_names()
	for _, ws in ipairs(active_workspaces) do
		if ws == workspace then
			workspace_icon = "📂"
			break
		end
	end

	local hostname = wezterm.hostname():gsub("%.local$", "")

	return {
		workspace_icon .. " " .. workspace,
		wezterm.strftime("%a %b %-d %H:%M"),
		hostname:upper(),
	}
end

-- Powerline status bar (based on gist approach)
wezterm.on("update-status", function(window, pane)
	local SOLID_LEFT_ARROW = wezterm.nerdfonts.pl_right_hard_divider
	local segments = segments_for_right_status(window)

	-- Use Catppuccin Mocha colors with better contrast
	local bg = "#1e1e2e"
	local fg = "#cdd6f4" -- Brighter text

	-- Create gradient with more contrast between segments
	local gradient = {
		"#45475a", -- Workspace (surface1)
		"#313244", -- Time (surface0)
		"#585b70", -- Hostname (surface2)
	}

	local elements = {}
	for i, seg in ipairs(segments) do
		local is_first = i == 1

		if is_first then
			table.insert(elements, { Background = { Color = "none" } })
		end

		table.insert(elements, { Foreground = { Color = gradient[i] } })
		table.insert(elements, { Text = SOLID_LEFT_ARROW })

		table.insert(elements, { Foreground = { Color = fg } })
		table.insert(elements, { Background = { Color = gradient[i] } })
		table.insert(elements, { Text = " " .. seg .. " " })
	end

	window:set_right_status(wezterm.format(elements))

	-- Clear left status (we have workspace info on the right)
	window:set_left_status("")
end)

-- Automatically name new workspaces based on their initial directory
wezterm.on("new-tab-button-click", function(window, pane, button, default_action)
	if button == "Left" then
		-- Create new tab in current workspace
		window:perform_action(default_action, pane)
	elseif button == "Right" then
		-- Create new workspace
		workspace.workspace_switcher(window, pane)
	end
	return false
end)

return config
