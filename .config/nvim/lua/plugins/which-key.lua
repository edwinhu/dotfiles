return {
	"folke/which-key.nvim",
	event = "VeryLazy",
	init = function()
		vim.o.timeout = true
		vim.o.timeoutlen = 300
	end,
	config = function()
		local wk = require("which-key")

		wk.setup({
			preset = "helix", -- Use the helix preset layout
		})

		-- Register Claude Code keymappings under leader+a with orange asterisk icon
		wk.add({
			{
				"<leader>a",
				group = " Claude Code",
				icon = { icon = "\u{e370}", color = "orange" },
			},
			{
				"<leader>ac",
				":ClaudeCode<CR>",
				desc = "Toggle Claude Code",
				icon = { icon = "💬", color = "orange" },
			},
			{
				"<leader>aC",
				":ClaudeCodeContinue<CR>",
				desc = "Continue Claude Code",
				icon = { icon = "▶️", color = "orange" },
			},
			{
				"<leader>ar",
				":ClaudeCodeResume<CR>",
				desc = "Resume Claude Code",
				icon = { icon = "🔄", color = "orange" },
			},
			{
				"<leader>ay",
				":Yolo<CR>",
				desc = "Claude Code (YOLO Mode)",
				icon = { icon = "🚀", color = "orange" },
			},
		})
	end,
}

