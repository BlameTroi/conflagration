return {
   cmd = { "lua-language-server" },
   filetypes = { "lua" },
   root_markers = {
      ".luarc.json",
      ".luarc.jsonc",
      ".stylua.toml",
      "stylua.toml",
      ".git",
   },
   init_options = {
      provideFormatter = false,
   },
   settings = {
      runtime = { version = "LuaJIT" },
      signatureHelp = { enabled = true },
   },
}
