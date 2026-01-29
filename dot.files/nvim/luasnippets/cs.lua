local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

return {
  -- Language basics
  s("nimp", { t('throw new System.NotImplementedException("Not implemented.");') }),
  s("obsolete", { t('[System.Obsolete("Please use '), i(1), t(' instead.")]') }),
  s("pr", { t("private readonly ") }),
  s("priv", { t("private ") }),
  s("pub", { t("public ") }),
  s("pubv", { t("public void ") }),
  s("ret", { t("return ") }),
  s("rett", { t("return "), i(1), t(";") }),
  s("ro", { t("readonly ") }),
  s("vd", { t("void ") }),
  s("wr", { t('System.Console.WriteLine("'), i(1), t('");') }),
  s("sys", { t("System.") }),
  s("sysd", { t("using static System.Diagnostics.Debug;") }),

  -- Asserts
  s("as", { t("Assert("), i(1, "false"), t(");") }),
  s("asm", { t("Assert("), i(1, "condition"), t(', "'), i(2, "error message"), t('");') }),
  s("asn", { t("Assert("), i(1), t(" != null);") }),

  -- String
  s("snoe", { t("string.IsNullOrEmpty("), i(1, "string"), t(")") }),
  s("strf", { t("string.Format("), i(1, "string"), t(")") }),
  s("stre", { t("string.Empty") }),
  s("ts", { t("ToString()") }),

  -- Branching
  s("case", {
    t("case "), i(1, "value"), t({":", "\t"}),
    i(2),
    t({"", "\tbreak;"}),
  }),
  s("el", {
    t({"else", "{", "\t"}),
    i(1, "code"),
    t({"", "}"}),
  }),
  s("elif", {
    t("else if ("), i(1), t({")", "{", "\t"}),
    i(0),
    t({"", "}"}),
  }),
  s("if", {
    t("if ("), i(1, "condition"), t({")", "{", "\t"}),
    i(2, "code"),
    t({"", "}"}),
  }),
  s("ife", {
    t("if ("), i(1), t({")", "{", "\t"}),
    i(2),
    t({"", "}", "else", "{", "\t"}),
    i(3),
    t({"", "}"}),
  }),
  s("ifnn", {
    t("if ("), i(1), t({" != null)", "{", "\t"}),
    i(0),
    t({"", "}"}),
  }),
  s("sw", {
    t("switch ("), i(1, "statement"), t({")", "{", "\tcase "}),
    i(2, "value"), t({":", "\t\tbreak;", "", "\tdefault:", "\t\t"}),
    i(0), t({"break;", "}"}),
  }),

  -- Loops
  s("do", {
    t({"do", "{", "\t"}),
    i(0),
    t({"", "}", "while ("}), i(1, "true"), t(");"),
  }),
  s("for", {
    t("for ("), i(1, "int"), t(" "), i(2, "i"), t(" = 0; "), i(3, "i"), t(" < "), i(4, "count"), t("; ++"), i(5, "i"), t({")", "{", "\t"}),
    i(0, "code"),
    t({"", "}"}),
  }),
  s("fore", {
    t("foreach ("), i(1, "var"), t(" "), i(2, "item"), t(" in "), i(3, "items"), t({")", "{", "\t"}),
    i(0),
    t({"", "}"}),
  }),
  s("fori", {
    t({"for (int i = 0; i < "}), i(1, "count"), t({"; ++i)", "{", "\t"}),
    i(0, "code"),
    t({"", "}"}),
  }),
  s("wh", {
    t("while ("), i(1, "true"), t({")", "{", "\t"}),
    i(0),
    t({"", "}"}),
  }),

  -- Type definitions
  s("class", {
    t("public class "), i(1, "MyClass"), t({"", "{", "\t"}),
    i(0),
    t({"", "}"}),
  }),
  s("enum", {
    t("enum "), i(1, "MyEnum"), t({"", "{", "\t"}),
    i(2, "Item"), t({",", "};"}),
  }),
  s("interface", {
    t("public interface I"), i(1, "Interface"), t({"", "{", "\t"}),
    i(0),
    t({"", "}"}),
  }),
  s("struct", {
    t("public struct "), i(1, "MyStruct"), t({"", "{", "\t"}),
    i(0),
    t({"", "}"}),
  }),

  -- Functions
  s("fun", {
    i(1, "void"), t(" "), i(2, "FunctionName"), t("("), i(3, "args"), t({")", "{", "\t"}),
    i(4, "code"),
    t({"", "}"}),
  }),
  s("func", {
    t("void "), i(1, "FunctionName"), t("("), i(2, "args"), t({")", "{", "\t"}),
    i(3, "code"),
    t({"", "}"}),
  }),
  s("funp", {
    t("public "), i(1, "void"), t(" "), i(2, "FunctionName"), t("("), i(3, "args"), t({")", "{", "\t"}),
    i(4, "code"),
    t({"", "}"}),
  }),
  s("get", { t("public "), i(1, "int"), t(" "), i(2, "MyProperty"), t(" { get; private set; }") }),
  s("getset", { t("public "), i(1, "int"), t(" "), i(2, "MyProperty"), t(" { get; set; }") }),
  s("main", {
    t({"public static void Main(string[] args)", "{", "\t"}),
    i(0, "code"),
    t({"", "}"}),
  }),
  s("maini", {
    t({"public static int Main(string[] args)", "{", "\t"}),
    i(0, "code"),
    t({"", "}"}),
  }),

  -- Usings
  s("uscg", { t("using System.Collections.Generic;") }),
  s("usi", { t("using System.IO;") }),
  s("utt", { t("using System.Threading.Tasks;") }),
  s("ulinq", { t("using System.Linq;") }),
  s("ununit", { t("using NUnit.Framework;") }),

  -- Exception handling
  s("try", {
    t({"try", "{", "\t"}),
    i(1, "code"),
    t({"", "}", "catch ("}), i(2, "System"), t({".Exception ex)", "{", "\t"}),
    i(3, "handle"),
    t({"", "}"}),
  }),
  s("tryf", {
    t({"try", "{", "\t"}),
    i(1, "code"),
    t({"", "}", "catch ("}), i(2), t({"Exception e)", "{", "\t"}),
    i(3, "handle"),
    t({"", "}", "finally", "{", "\t"}),
    i(4, "cleanup"),
    t({"", "}"}),
  }),
  s("catch", {
    t("catch ("), i(1), t({"Exception e)", "{", "\t"}),
    i(2, "handle"),
    t({"", "}"}),
  }),
  s("thr", { t("throw new "), i(1), t('Exception("'), i(2), t('");') }),

  -- Comments
  s("fix", { t("// O-FIXME: "), i(0) }),
  s("todo", { t("// TODO: "), i(1) }),

  -- NUnit
  s("test", {
    t({"[Test]public void "}), i(1, "Name"), t({"() {", "\t"}),
    i(2),
    t({"", "}"}),
  }),
  s("testcase", { t("[TestCase("), i(1, "args"), t(")]") }),
  s("testfix", { t("[TestFixture]") }),
  s("aseq", { t("Assert.That("), i(1, "actual"), t(", Is.EqualTo("), i(2, "expected"), t("));") }),
  s("asneq", { t("Assert.That("), i(1, "actual"), t(", Is.Not.EqualTo("), i(2, "expected"), t("));") }),
  s("ast", { t("Assert.That("), i(1), t(", Is.True);") }),
  s("asf", { t("Assert.That("), i(1), t(", Is.False);") }),
  s("asnull", { t("Assert.That("), i(1, "obj"), t(", Is.Null);") }),
  s("asnnull", { t("Assert.That("), i(1, "obj"), t(", Is.Not.Null);") }),
}
