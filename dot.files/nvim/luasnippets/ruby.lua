local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

return {
  -- Shebang
  s("#!", { t("#!/usr/bin/env ruby"), i(0) }),

  -- Main
  s("ifmain", {
    t({"if __FILE__ == $PROGRAM_NAME", "\t"}),
    i(0, "main()"),
    t({"", "end"}),
  }),
}
