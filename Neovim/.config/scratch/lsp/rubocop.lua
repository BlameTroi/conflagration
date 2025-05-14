---@brief
---
--- https://github.com/rubocop/rubocop
return {
   cmd = { "rubocop", "--lsp" },
   filetypes = { "ruby" },
   root_markers = { "Gemfile", ".git" },
   init_options = {
      provideFormatter = false,
   },
}
