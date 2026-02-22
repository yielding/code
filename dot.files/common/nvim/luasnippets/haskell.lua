local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

return {
  s("main", {
    t({"main :: IO()", "main = "}),
    i(0, 'print $ "hi"'),
  }),
  s("pr", {
    t("print $ "),
    i(0),
  }),
}
