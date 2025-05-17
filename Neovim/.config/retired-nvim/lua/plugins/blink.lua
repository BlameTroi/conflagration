local M = { -- optional blink completion source for require statements and module annotations
   "saghen/blink.cmp",
   -- the below is from lazydev for lua lsp support, but lazydev isn't
   -- working so I'll just leave blink in place.
   -- opts = {
   --    sources = {
   --       -- add lazydev to your completion providers
   --       default = { "lazydev", "lsp", "path", "snippets", "buffer" },
   --       providers = {
   --          lazydev = {
   --             name = "LazyDev",
   --             module = "lazydev.integrations.blink",
   --             -- make lazydev completions top priority (see `:h blink.cmp`)
   --             score_offset = 100,
   --          },
   --       },
   --    },
   -- },
}
return M
