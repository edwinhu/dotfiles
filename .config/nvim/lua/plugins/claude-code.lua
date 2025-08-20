return {
	"greggh/claude-code.nvim",
	dependencies = { "nvim-lua/plenary.nvim" },
	event = "VeryLazy",
	config = function()
		require("claude-code").setup({
			window = {
				split_ratio = 0.4,      -- 40% of screen width for the terminal window
				position = "vertical",  -- Position on the right side
			},
		})

		-- Create custom command aliases
		vim.api.nvim_create_user_command("Yolo", function(opts)
			-- Launch claude-code with --dangerously-skip-permissions flag
			local claude_code = require("claude-code")
			local args = "--dangerously-skip-permissions"
			if opts.args and opts.args ~= "" then
				args = opts.args .. " " .. args
			end
			claude_code.toggle({ args = args })
		end, {
			nargs = "*",
			desc = "Launch Claude Code with --dangerously-skip-permissions",
		})
	end,
}