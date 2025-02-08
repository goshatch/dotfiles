-- init.lua
-- Load plugin manager (using lazy.nvim, a modern alternative)
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable",
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

-- Plugin specifications
require("lazy").setup({
	-- Modern alternatives to your plugins
	{
		"nvim-treesitter/nvim-treesitter",  -- Better syntax highlighting than vim-polyglot
		build = ":TSUpdate",
		config = function()
			require("nvim-treesitter.configs").setup({
				ensure_installed = { "lua", "vim", "vimdoc" },
				highlight = { enable = true },
			})
		end
	},
	{
		"ibhagwan/fzf-lua",
		dependencies = { "nvim-tree/nvim-web-devicons" },
	},
	{ 
		"stevearc/conform.nvim",
		opts = {},
		config = function()
			require("conform").setup({
				formatters_by_ft = {
					clojure = { "standard-clj" },
					javascript = { "prettierd", "prettier", stop_after_first = true },
				},
			})
		end
	},
	{
		"Olical/conjure"
	}
})

-- Basic settings (converting your vim settings to lua)
local opt = vim.opt

-- Editor behavior
opt.compatible = false
opt.syntax = "on"
opt.termguicolors = true
opt.laststatus = 2
opt.encoding = "utf-8"
opt.autoindent = true
opt.magic = true
opt.number = true
opt.scrolloff = 3
opt.sidescrolloff = 3
opt.ruler = true
opt.textwidth = 80
opt.wrap = false
opt.ignorecase = true
opt.smartcase = true
opt.splitbelow = true
opt.hidden = true
opt.incsearch = true
opt.showmatch = true
opt.hlsearch = true
opt.mouse = "a"
opt.swapfile = false
opt.foldenable = false
opt.lazyredraw = true

-- Modern Neovim-specific settings
opt.clipboard = "unnamedplus"  -- Better clipboard integration
opt.updatetime = 300          -- Faster completion
opt.timeoutlen = 500         -- Faster key sequence completion
opt.completeopt = "menuone,noselect"  -- Better completion experience

vim.cmd("colorscheme lunaperche")

-- Key mappings (converting your maps to lua)
local keymap = vim.keymap.set
vim.g.mapleader = " "
keymap("i", "jj", "<Esc>", { silent = true })
keymap("i", "jk", "<Esc>", { silent = true })

keymap("n", "<leader><leader>", function()
	require('fzf-lua').files({ 
		cwd_only = true,  -- Search from git root
		hidden = true     -- Include hidden files
	})
end, { silent = true, desc = "Find files in git project" })

-- File type settings
vim.cmd([[
filetype plugin indent on
]])
