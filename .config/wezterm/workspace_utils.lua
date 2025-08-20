local wezterm = require("wezterm")

local M = {}

-- Get project name from a path
function M.get_project_name(path)
    -- Common project directories to check
    local project_dirs = {
        "~/projects/",
        "~/Documents/",
        "~/dotfiles",
        "~/nix",
        "~/work/",
        "~/code/",
        "~/dev/",
    }
    
    -- Expand tilde to home directory
    local home = wezterm.home_dir
    path = path:gsub("^~", home)
    
    -- Check if path is in any of the project directories
    for _, dir in ipairs(project_dirs) do
        local expanded_dir = dir:gsub("^~", home)
        if path:find("^" .. expanded_dir) then
            -- Extract the project folder name
            local relative = path:sub(#expanded_dir + 1)
            local project = relative:match("^([^/]+)")
            if project and project ~= "" then
                return project
            end
        end
    end
    
    -- Check if we're in a git repo
    local success, stdout = wezterm.run_child_process({"git", "-C", path, "rev-parse", "--show-toplevel"})
    if success then
        local git_root = stdout:gsub("\n", "")
        return git_root:match("([^/]+)$")
    end
    
    -- Fallback to current directory name
    return path:match("([^/]+)$") or "default"
end

-- Get or create workspace for a project
function M.get_or_create_workspace(window, project_path)
    local project_name = M.get_project_name(project_path)
    
    -- Check if workspace already exists
    local workspaces = wezterm.mux.get_workspace_names()
    for _, ws in ipairs(workspaces) do
        if ws == project_name then
            return project_name
        end
    end
    
    -- Workspace doesn't exist, it will be created when we switch to it
    return project_name
end

-- Simple workspace switcher 
function M.workspace_switcher(window, pane)
    local home = wezterm.home_dir
    local workspaces = wezterm.mux.get_workspace_names()
    
    -- Build list of choices
    local choices = {}
    local seen = {}
    
    -- Add existing workspaces
    for _, ws in ipairs(workspaces) do
        table.insert(choices, {
            label = "📂 " .. ws,
            id = ws,
        })
        seen[ws] = true
    end
    
    -- Add separator
    if #choices > 0 then
        table.insert(choices, { label = "──────────────────", id = "" })
    end
    
    -- Function to scan a directory and add projects
    local function add_projects_from_dir(dir_path, prefix)
        -- Use find instead of ls to avoid color codes and get clean paths
        local success, stdout = wezterm.run_child_process({
            "find", dir_path, "-maxdepth", "1", "-type", "d", "-not", "-path", dir_path
        })
        if success then
            for project_path in stdout:gmatch("[^\n]+") do
                local project_name = project_path:match("([^/]+)$")
                if project_name and not seen[project_name] then
                    table.insert(choices, {
                        label = "📁 " .. prefix .. "/" .. project_name,
                        id = "NEW:" .. project_name .. ":" .. project_path
                    })
                end
            end
        end
    end
    
    -- Add projects from ~/projects
    add_projects_from_dir(home .. "/projects", "projects")
    
    -- Add projects from ~/areas
    add_projects_from_dir(home .. "/areas", "areas")
    
    -- Add specific directories
    if not seen["dotfiles"] then
        table.insert(choices, { 
            label = "📁 ~/dotfiles", 
            id = "NEW:dotfiles:" .. home .. "/dotfiles" 
        })
    end
    
    if not seen["nix"] then
        table.insert(choices, { 
            label = "📁 ~/nix", 
            id = "NEW:nix:" .. home .. "/nix" 
        })
    end
    
    if not seen["Notes"] then
        table.insert(choices, { 
            label = "📁 ~/Documents/Notes", 
            id = "NEW:Notes:" .. home .. "/Documents/Notes" 
        })
    end
    
    -- Show the selector with padding to appear more centered
    window:perform_action(
        wezterm.action.InputSelector({
            title = "                    🗂️  Workspace Switcher                    ",
            choices = choices,
            fuzzy = true,
            fuzzy_description = "Type to filter workspaces...",
            action = wezterm.action_callback(function(inner_window, inner_pane, id, label)
                if id and id ~= "" then
                    if id:find("^NEW:") then
                        -- New workspace with directory
                        local parts = {}
                        for part in id:gmatch("[^:]+") do
                            table.insert(parts, part)
                        end
                        local name = parts[2]
                        local path = parts[3]
                        
                        inner_window:perform_action(
                            wezterm.action.SwitchToWorkspace({
                                name = name,
                                spawn = {
                                    cwd = path,
                                },
                            }),
                            inner_pane
                        )
                    else
                        -- Existing workspace
                        inner_window:perform_action(
                            wezterm.action.SwitchToWorkspace({
                                name = id,
                            }),
                            inner_pane
                        )
                    end
                end
            end),
        }),
        pane
    )
end

-- Rename current workspace based on current directory
function M.rename_workspace_to_project(window, pane)
    local cwd = pane:get_current_working_dir()
    if cwd then
        local project_name = M.get_project_name(cwd.file_path)
        wezterm.mux.rename_workspace(
            wezterm.mux.get_active_workspace(),
            project_name
        )
        wezterm.log_info("Renamed workspace to: " .. project_name)
    end
end

return M