return {
	-- slime (REPL integration)
	{
		"jpalardy/vim-slime",
		init = function()
			vim.g.slime_target = "wezterm"
			vim.g.slime_bracketed_paste = 1
			vim.g.slime_default_config = {
				-- tmux
				socket_name = "default",
				target_pane = "{top-right}",
				-- wezterm
				pane_direction = "Right",
				default_send_cmd = "<C-c><C-c>",
			}
		end,
	},
}
