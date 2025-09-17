return {
	"coder/claudecode.nvim",
	dependencies = { "folke/snacks.nvim" }, -- Optional for enhanced terminal support
	event = "VeryLazy",
	config = function()
		require("claudecode").setup({
			-- Server configuration
			port_range = { min = 10000, max = 65535 },
			auto_start = true,
			log_level = "info",

			-- Terminal configuration
			terminal = {
				split_side = "right",
				split_width_percentage = 0.5,
				provider = "auto",
				auto_close = true,
			},

			-- Diff integration
			diff_opts = {
				auto_close_on_accept = true,
				vertical_split = true,
				open_in_current_tab = true,
			},
		})

		-- YOLO mode command with bypass permissions
		vim.api.nvim_create_user_command("Yolo", function(opts)
			-- Launch Claude Code CLI with bypass permissions in terminal
			local cmd = "claude --dangerously-skip-permissions"
			if opts.args and opts.args ~= "" then
				cmd = cmd .. " " .. opts.args
			end
			vim.cmd("terminal " .. cmd)
		end, {
			nargs = "*",
			desc = "Launch Claude Code with --dangerously-skip-permissions",
		})

		-- Window navigation setup that leverages existing smart-splits configuration
		-- Uses C-h/j/k/l which are already configured in smart-splits.lua and don't conflict with insert mode
		local ok, smart_splits = pcall(require, "smart-splits")
		
		-- Terminal mode navigation (for when inside the Claude Code terminal)
		-- These use the same C-h/j/k/l pattern as normal mode navigation
		vim.keymap.set("t", "<C-h>", "<C-\\><C-n><C-h>", { desc = "Move to left window from terminal" })
		vim.keymap.set("t", "<C-j>", "<C-\\><C-n><C-j>", { desc = "Move to down window from terminal" })
		vim.keymap.set("t", "<C-k>", "<C-\\><C-n><C-k>", { desc = "Move to up window from terminal" })
		vim.keymap.set("t", "<C-l>", "<C-\\><C-n><C-l>", { desc = "Move to right window from terminal" })
		
		-- Additional leader-based navigation for convenience (works in all modes)
		-- These provide alternative navigation that doesn't conflict with any existing keybindings
		vim.keymap.set("n", "<leader>wh", function() 
			if ok then smart_splits.move_cursor_left() else vim.cmd("wincmd h") end 
		end, { desc = "Move to left window" })
		vim.keymap.set("n", "<leader>wj", function() 
			if ok then smart_splits.move_cursor_down() else vim.cmd("wincmd j") end 
		end, { desc = "Move to down window" })
		vim.keymap.set("n", "<leader>wk", function() 
			if ok then smart_splits.move_cursor_up() else vim.cmd("wincmd k") end 
		end, { desc = "Move to up window" })
		vim.keymap.set("n", "<leader>wl", function() 
			if ok then smart_splits.move_cursor_right() else vim.cmd("wincmd l") end 
		end, { desc = "Move to right window" })
		
		-- Window management shortcuts for Claude Code workflow (these are safe C-w combinations)
		vim.keymap.set("n", "<leader>w=", "<C-w>=", { desc = "Equalize window sizes" })
		vim.keymap.set("n", "<leader>w|", "<C-w>|", { desc = "Maximize window width" })
		vim.keymap.set("n", "<leader>w_", "<C-w>_", { desc = "Maximize window height" })
		vim.keymap.set("n", "<leader>wq", "<C-w>q", { desc = "Close current window" })
		vim.keymap.set("n", "<leader>ws", "<C-w>s", { desc = "Split window horizontally" })
		vim.keymap.set("n", "<leader>wv", "<C-w>v", { desc = "Split window vertically" })
	end,
}

