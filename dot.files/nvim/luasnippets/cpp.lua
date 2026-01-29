local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

return {
  -- STL Containers
  s("array", { t("array<"), i(1, "T"), t(", "), i(2, "N"), t("> "), i(3), t(";") }),
  s("deque", { t("deque<"), i(1, "T"), t("> "), i(2), t(";") }),
  s("flist", { t("forward_list<"), i(1, "T"), t("> "), i(2), t(";") }),
  s("list", { t("list<"), i(1, "T"), t("> "), i(2), t(";") }),
  s("set", { t("set<"), i(1, "T"), t("> "), i(2), t(";") }),
  s("map", { t("map<"), i(1, "key"), t(", "), i(2, "value"), t("> "), i(3), t(";") }),
  s("vector", { t("vector<"), i(1, "char"), t("> "), i(2), t(";") }),
  s("mset", { t("multiset<"), i(1, "T"), t("> "), i(2), t(";") }),
  s("mmap", { t("multimap<"), i(1, "Key"), t(", "), i(2, "T"), t("> "), i(3), t(";") }),
  s("uset", { t("unordered_set<"), i(1, "T"), t("> "), i(2), t(";") }),
  s("umap", { t("unordered_map<"), i(1, "Key"), t(", "), i(2, "T"), t("> "), i(3), t(";") }),
  s("umset", { t("unordered_multiset<"), i(1, "T"), t("> "), i(2), t(";") }),
  s("ummap", { t("unordered_multimap<"), i(1, "Key"), t(", "), i(2, "T"), t("> "), i(3), t(";") }),
  s("stack", { t("stack<"), i(1, "T"), t("> "), i(2), t(";") }),
  s("queue", { t("queue<"), i(1, "T"), t("> "), i(2), t(";") }),
  s("pqueue", { t("priority_queue<"), i(1, "T"), t("> "), i(2), t(";") }),

  -- Smart Pointers
  s("msp", { t("shared_ptr<"), i(1, "T"), t("> "), i(2), t(" = make_shared<"), i(3, "T"), t(">("), i(4), t(");") }),
  s("amsp", { t("auto "), i(1), t(" = make_shared<"), i(2, "T"), t(">("), i(3), t(");") }),
  s("mup", { t("unique_ptr<"), i(1, "T"), t("> "), i(2), t(" = make_unique<"), i(3, "T"), t(">("), i(4), t(");") }),
  s("amup", { t("auto "), i(1), t(" = make_unique<"), i(2, "T"), t(">("), i(3), t(");") }),

  -- I/O
  s("cout", { t("cout << "), i(1), t(" << endl;") }),
  s("cin", { t("cin >> "), i(1), t(";") }),

  -- Control Flow (Allman style)
  s("if", {
    t("if ("), i(1, "condition"), t({")", "{", "\t"}),
    i(0),
    t({"", "}"}),
  }),
  s("eli", {
    t("else if ("), i(1, "condition"), t({")", "{", "\t"}),
    i(0),
    t({"", "}"}),
  }),
  s("el", {
    t({"else", "{", "\t"}),
    i(0),
    t({"", "}"}),
  }),
  s("fori", {
    t("for ("), i(1, "int"), t(" "), i(2, "i"), t("=0; "), i(3, "i"), t("<"), i(4, "count"), t("; ++"), i(5, "i"), t({")", "{", "\t"}),
    i(0),
    t({"", "}"}),
  }),
  s("fora", {
    t("for (auto "), i(1, "var"), t(": "), i(2, "container"), t({")", "{", "\t"}),
    i(0),
    t({"", "}"}),
  }),
  s("wh", {
    t("while ("), i(1, "condition"), t({")", "{", "\t"}),
    i(0),
    t({"", "}"}),
  }),
  s("try", {
    t({"try", "{", "\t"}),
    i(0),
    t({"", "}", "catch ("}), i(1), t({")", "{", "}"}),
  }),

  -- Namespace
  s("ns", {
    t("namespace "), i(1, "name"), t({"", "{", "\t"}),
    i(0),
    t({"", "}"}),
  }),

  -- Lambda
  s("ld", { t("["), i(1), t("]("), i(2), t(") {"), i(3), t("};") }),
  s("lld", {
    t("["), i(1), t("]("), i(2), t({")", "{", "\t"}),
    i(3),
    t({"", "};"}),
  }),

  -- Struct
  s("st", {
    t("struct "), i(1, "name"), t({"", "{", "\t"}),
    i(0, "// data"),
    t({"", "};"}),
  }),

  -- Class
  s("class", {
    t("class "), i(1, "ClassName"), t({"", "{", "public:", "\t"}),
    i(0),
    t({"", "};"}),
  }),
  s("classi", {
    t("class "), i(1, "ClassName"), t(" : public "), i(2, "Base"), t({"", "{", "public:", "\t"}),
    i(0),
    t({"", "};"}),
  }),

  -- Main
  s("main", {
    t({"auto main(int argc, char* argv[]) -> int", "{", "\t"}),
    i(0),
    t({"", "", "\treturn 0;", "}"}),
  }),
  s("mainn", {
    t({"#include <print>", "", "using namespace std;", "", "auto main(int argc, char* argv[]) -> int", "{", "\t"}),
    i(0),
    t({"", "", "\treturn 0;", "}"}),
  }),

  -- Fixed-width integers
  s("i8", { t("int8_t"), i(0) }),
  s("u8", { t("uint8_t"), i(0) }),
  s("i16", { t("int16_t"), i(0) }),
  s("u16", { t("uint16_t"), i(0) }),
  s("i32", { t("int32_t"), i(0) }),
  s("u32", { t("uint32_t"), i(0) }),
  s("i64", { t("int64_t"), i(0) }),
  s("u64", { t("uint64_t"), i(0) }),

  -- Namespace shortcuts
  s("nsrg", { t("namespace g = ranges;"), i(0) }),
  s("nsrv", { t("namespace v = ranges::views;"), i(0) }),
  s("nsra", { t("namespace a = ranges::actions;"), i(0) }),
  s("nsstd", { t("using namespace std;"), i(0) }),
  s("nschrono", { t("using namespace std::chrono_literals;"), i(0) }),
  s("nsboost", { t("using namespace boost;"), i(0) }),
  s("nscv", { t("using namespace cv;"), i(0) }),
  s("nszmq", { t("using namespace zmq;"), i(0) }),

  -- Pragma
  s("once", { t("#pragma once"), i(0) }),

  -- Functions (Allman style)
  s("fun", {
    t("auto "), i(1, "function_name"), t("("), i(2), t(") -> "), i(3, "void"), t({"", "{", "\t"}),
    i(0),
    t({"", "}"}),
  }),
  s("mfun", {
    i(1, "void"), t(" "), i(2, "ClassName"), t("::"), i(3, "function_name"), t("("), i(4), t({")", "{", "\t"}),
    i(0),
    t({"", "}"}),
  }),
  s("cfun", {
    t("const "), i(1, "int"), t("& "), i(2, "function_name"), t("("), i(3), t({") const", "{", "\t"}),
    i(0),
    t({"", "}"}),
  }),
}
