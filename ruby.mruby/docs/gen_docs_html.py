#!/usr/bin/env python3
"""
Generate HTML viewers for every Markdown file in docs/.

Adapted from forensic.filesystem/docs/gen_docs_html.py (same layout/CSS).

Plain, wide-column layout — system fonts, light background, no
decorative margin markers, no warm-paper texture.  The previous
"engineering-journal" style was too dressed up for engineering
reference docs.

Run from the repo root or from docs/:

  python3 docs/gen_docs_html.py
"""

from __future__ import annotations

import datetime as _dt
import html
import pathlib
import re
import subprocess
import sys


# -----------------------------------------------------------------------------
# Catalogue
# -----------------------------------------------------------------------------

HERE = pathlib.Path(__file__).parent

# (markdown stem, display title, one-line summary for the index)
DOCS = [
    ("mruby_embedding",       "mruby 임베딩 — C++ 호스트 매뉴얼",
     "mruby VM을 C++ 호스트에 임베드하는 방법 전반 — 기본 골격, DATA "
     "타입, mrb_get_args 지정자, GC 아레나, 소유권(borrowed/owned), "
     "그리고 이를 캡슐화한 mrubybind 헬퍼(vm/klass/holder/to_ruby) 상세. "
     "MD:: 모듈 네임스페이스로 내장 File과 충돌을 피하는 규칙 포함."),
]


# -----------------------------------------------------------------------------
# Stylesheet — plain, wide-column, system fonts
# -----------------------------------------------------------------------------

CSS = r"""
/* ===========================================================================
   Reference-doc style — gmdsoft slide-deck palette adapted for flowing prose.
   Warm-gray (stone) neutrals + teal/amber/rose accents + Pretendard body /
   JetBrains Mono code.  Slide-scale typography (96 px h1) is scaled DOWN to
   reading-friendly sizes (40 px h1), but the box/eyebrow/dark-table
   treatments come straight across.
   =========================================================================== */

:root {
  --bg:         #ffffff;
  --bg-soft:    #fafaf9;   /* warm gray 50 — info-box / table-stripe ground */
  --bg-cool:    #f8fafc;
  --bg-code:    #fafaf9;
  --ink:        #1c1917;   /* stone 900 */
  --ink-soft:   #57534e;   /* stone 600 */
  --ink-faint:  #78716c;   /* stone 500 */
  --rule:       #d6d3d1;   /* stone 300 */
  --rule-soft:  #e7e5e4;   /* stone 200 */
  --code-ink:   #1c1917;

  /* gmdsoft accents */
  --teal:        #0d9488;
  --teal-dark:   #115e59;
  --teal-bg:     #f0fdfa;
  --teal-border: #99f6e4;
  --amber:       #d97706;
  --amber-dark:  #92400e;
  --amber-bg:    #fffbeb;
  --amber-border:#fde68a;
  --rose:        #e11d48;
  --rose-dark:   #9f1239;
  --rose-bg:     #fff1f2;
  --rose-border: #fecdd3;
  --emerald:     #059669;
  --emerald-bg:  #ecfdf5;

  /* Aliases used elsewhere (pills, SVG, links). */
  --accent:    var(--teal);
  --ok:        var(--emerald);
  --warn:      var(--amber);
  --gap:       var(--rose);
}

@media (prefers-color-scheme: dark) {
  :root {
    --bg:        #18181b;     /* zinc 900 */
    --bg-soft:   #27272a;     /* zinc 800 */
    --bg-cool:   #1f2937;
    --bg-code:   #1c1917;
    --ink:       #f5f5f4;     /* stone 100 */
    --ink-soft:  #d6d3d1;     /* stone 300 */
    --ink-faint: #a8a29e;     /* stone 400 */
    --rule:      #44403c;     /* stone 700 */
    --rule-soft: #292524;     /* stone 800 */
    --code-ink:  #f5f5f4;

    --teal:        #2dd4bf;
    --teal-dark:   #5eead4;
    --teal-bg:     #042f2e;
    --teal-border: #115e59;
    --amber:       #fbbf24;
    --amber-dark:  #fcd34d;
    --amber-bg:    #451a03;
    --amber-border:#92400e;
    --rose:        #fb7185;
    --rose-dark:   #fda4af;
    --rose-bg:     #4c0519;
    --rose-border: #9f1239;
    --emerald:     #34d399;
    --emerald-bg:  #022c22;
  }
}

*, *::before, *::after { box-sizing: border-box; }

body {
  margin: 0;
  background: var(--bg);
  color: var(--ink);
  font-family: 'Pretendard', 'Inter', -apple-system, BlinkMacSystemFont,
               "Segoe UI", "Noto Sans KR", "Helvetica Neue", Arial,
               sans-serif;
  font-size: 16px;
  line-height: 1.7;
  letter-spacing: -0.003em;
  -webkit-font-smoothing: antialiased;
  text-rendering: optimizeLegibility;
}

/* Single column convention — every prose element shares the same
   ~72ch reading width.  Wide blocks (tables, code, figures) opt
   out via `.full-bleed` below.  This removes the visual mismatch
   between narrow paragraphs and full-width headings/tables. */
:root { --reading-col: 72ch; }

/* ---------- Top navigation -------------------------------------------------- */

.topnav {
  border-bottom: 1px solid var(--rule);
  background: var(--bg);
  padding: 14px 28px;
  display: flex;
  align-items: center;
  gap: 18px;
  font-size: 14px;
  position: relative;
}
/* Gmdsoft cover-side gradient stripe — runs the full top-nav height as a
   subtle brand signal.  Three accents in sequence echo the slide deck. */
.topnav::before {
  content: '';
  position: absolute;
  left: 0;
  top: 0;
  bottom: 0;
  width: 4px;
  background: linear-gradient(180deg,
              var(--teal) 0%, var(--amber) 50%, var(--rose) 100%);
}
.topnav .brand {
  font-weight: 700;
  color: var(--ink);
  text-decoration: none;
  letter-spacing: 0.4px;
}
.topnav .spacer { flex: 1; }
.topnav a {
  color: var(--ink-soft);
  text-decoration: none;
  padding: 4px 10px;
  border-radius: 4px;
}
.topnav a:hover {
  color: var(--teal-dark);
  background: var(--teal-bg);
}

/* ---------- Page container -------------------------------------------------- */

/* 920 px is wide enough for a tight reading column + ~3 cm of side
   gutter on a 1080 p screen, narrow enough that tables and code
   blocks don't sprawl horizontally on big monitors.  Past this
   width readability degrades sharply for prose. */
.page {
  max-width: 920px;
  margin: 0 auto;
  padding: 48px 32px 96px;
}

/* ---------- Document head --------------------------------------------------- */

/* Mirror of gmdsoft cover/closing slide: a slim teal→amber→rose stripe down
   the left, eyebrow chip up top, oversized title. */
.doc-head {
  position: relative;
  padding: 4px 0 24px 24px;
  margin-bottom: 36px;
  border-bottom: 1px solid var(--rule);
}
.doc-head::before {
  content: '';
  position: absolute;
  left: 0;
  top: 0;
  bottom: 24px;
  width: 6px;
  background: linear-gradient(180deg,
              var(--teal) 0%, var(--amber) 55%, var(--rose) 100%);
}
.doc-head .breadcrumb {
  color: var(--teal);
  font-size: 12px;
  font-weight: 700;
  letter-spacing: 1.5px;
  text-transform: uppercase;
  margin-bottom: 10px;
}
.doc-head .breadcrumb a {
  color: var(--teal);
  text-decoration: none;
}
.doc-head .breadcrumb a:hover {
  color: var(--teal-dark);
  text-decoration: underline;
}
.doc-head h1 {
  font-size: 32px;
  line-height: 1.2;
  letter-spacing: -0.7px;
  margin: 0 0 12px 0;
  font-weight: 800;
  color: var(--ink);
}
.doc-head .lede {
  color: var(--ink-soft);
  font-size: 16.5px;
  line-height: 1.6;
  margin: 0;
  max-width: var(--reading-col);
}

/* ---------- TOC (collapsible, top of doc) ---------------------------------- */

details.toc {
  margin: 20px 0 32px;
  border: 1px solid var(--rule);
  border-left: 4px solid var(--teal);
  border-radius: 2px;
  background: var(--bg-soft);
  font-size: 14px;
}
details.toc summary {
  cursor: pointer;
  padding: 12px 16px;
  font-weight: 700;
  color: var(--ink);
  text-transform: uppercase;
  letter-spacing: 1px;
  font-size: 12px;
  user-select: none;
}
details.toc summary:hover { color: var(--teal-dark); }
details.toc ul {
  list-style: none;
  margin: 0;
  padding: 4px 16px 16px;
  columns: 2;
  column-gap: 32px;
  font-size: 14px;
}
@media (max-width: 720px) {
  details.toc ul { columns: 1; }
}
details.toc li {
  margin: 5px 0;
  break-inside: avoid;
}
details.toc li.lvl-3 { padding-left: 14px; }
details.toc li.lvl-4 { padding-left: 28px; font-size: 13px; }
details.toc a {
  color: var(--ink-soft);
  text-decoration: none;
}
details.toc a:hover {
  color: var(--teal-dark);
  text-decoration: underline;
}

/* ---------- Headings -------------------------------------------------------- */

main h2, main h3, main h4, main h5, main h6 {
  color: var(--ink);
  font-weight: 700;
  letter-spacing: -0.3px;
  scroll-margin-top: 24px;
}
main h2 {
  font-size: 23px;
  line-height: 1.25;
  margin: 2.4em 0 0.7em;
  padding-bottom: 0.35em;
  border-bottom: 1px solid var(--rule);
  font-weight: 800;
}
main h3 {
  font-size: 18px;
  line-height: 1.3;
  margin: 1.8em 0 0.5em;
  font-weight: 700;
}
main h4 {
  font-size: 15.5px;
  margin: 1.5em 0 0.4em;
  font-weight: 700;
  letter-spacing: -0.1px;
}
main h5, main h6 {
  font-size: 12.5px;
  margin: 1.4em 0 0.3em;
  color: var(--teal-dark);
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 1.3px;
}

/* ---------- Paragraphs & inline -------------------------------------------- */

main p {
  margin: 0 0 1.05em 0;
  max-width: var(--reading-col);
  font-size: 15.5px;
  line-height: 1.75;
}

main a {
  color: var(--accent);
  text-decoration: none;
  border-bottom: 1px solid transparent;
  transition: border-color 0.1s;
}
main a:hover {
  text-decoration: none;
  border-bottom-color: var(--accent);
}

main strong { font-weight: 700; color: var(--ink); }
main em     { font-style: italic; color: var(--ink); }

/* Inline `code` — small, tinted, no border (gmdsoft slide-deck
   convention).  ~91% of paragraph font size so it sits flush
   with surrounding text. */
main code {
  font-family: 'JetBrains Mono', ui-monospace, "SF Mono", "Cascadia Code",
               "Roboto Mono", Menlo, Consolas, monospace;
  font-size: 0.91em;
  background: var(--bg-code);
  padding: 0.1em 0.4em;
  border-radius: 3px;
  color: var(--code-ink);
}

/* ---------- Code blocks ----------------------------------------------------- */

main pre {
  font-family: 'JetBrains Mono', ui-monospace, "SF Mono", "Cascadia Code",
               "Roboto Mono", Menlo, Consolas, monospace;
  font-size: 13px;
  line-height: 1.55;
  color: var(--code-ink);
  background: var(--bg-code);
  border: 1px solid var(--rule);
  border-left: 3px solid var(--teal);
  border-radius: 2px;
  padding: 14px 18px;
  margin: 1.3em 0;
  overflow-x: auto;
  /* Code can be wider than prose — fill the page container. */
  max-width: 100%;
}
main pre code {
  background: transparent;
  border: none;
  padding: 0;
  border-radius: 0;
  font-size: inherit;
  color: inherit;
}

/* ---------- Tables --------------------------------------------------------- */
/* Gmdsoft track-table: black header row, white uppercase text, warm-gray
   row stripes, hover tint.  Tables can be wider than the prose reading
   column — they fill the page container. */

.table-wrap {
  margin: 1.5em 0;
  overflow-x: auto;
  border: 1px solid var(--rule);
  border-radius: 2px;
}
main table {
  width: 100%;
  border-collapse: collapse;
  font-size: 14px;
  line-height: 1.55;
  /* Cell text wraps inside the cell instead of forcing horizontal
     scroll on the whole table.  Long URLs / paths still break. */
  table-layout: auto;
  word-wrap: break-word;
  overflow-wrap: anywhere;
}
main table th,
main table td {
  padding: 11px 14px;
  border-bottom: 1px solid var(--rule);
  text-align: left;
  vertical-align: top;
}
main table th {
  background: var(--ink);
  color: #fafaf9;
  font-weight: 700;
  font-size: 11.5px;
  text-transform: uppercase;
  letter-spacing: 0.8px;
  border-bottom: none;
}
main table tbody tr:nth-child(even) td {
  background: var(--bg-soft);
}
main table tbody tr:hover td {
  background: var(--teal-bg);
}
main table tbody tr:last-child td { border-bottom: none; }
main table code { font-size: 0.9em; }

/* Status pills (auto-applied to common tokens in cells) */
.pill {
  display: inline-block;
  font-size: 11px;
  font-weight: 700;
  letter-spacing: 0.6px;
  text-transform: uppercase;
  padding: 3px 10px;
  border-radius: 2px;
  white-space: nowrap;
}
.pill.ok   { color: var(--emerald);   background: var(--emerald-bg);
             border: 1px solid var(--emerald); }
.pill.gap  { color: var(--rose);      background: var(--rose-bg);
             border: 1px solid var(--rose-border); }
.pill.warn { color: var(--amber-dark); background: var(--amber-bg);
             border: 1px solid var(--amber-border); }

/* ---------- Lists ---------------------------------------------------------- */

main ul, main ol {
  padding-left: 1.6em;
  margin: 0.6em 0 1.1em;
  max-width: var(--reading-col);
  font-size: 15.5px;
}
main li { margin: 0.3em 0; line-height: 1.65; }
main li ul, main li ol { margin: 0.3em 0; }
main li code { font-size: 0.93em; }

/* ---------- Blockquote — gmdsoft info-box treatment ----------------------- */

main blockquote {
  margin: 1.4em 0;
  padding: 16px 22px;
  background: var(--bg-soft);
  border: 1px solid var(--rule-soft);
  border-left: 4px solid var(--teal);
  color: var(--ink);
  font-size: 15.5px;
  line-height: 1.65;
  max-width: var(--reading-col);
}
main blockquote p { font-size: inherit; line-height: inherit; }
main blockquote p:last-child { margin-bottom: 0; }
main blockquote strong { color: var(--teal-dark); }

/* ---------- HR — subtle teal→amber→rose gradient -------------------------- */

main hr {
  border: none;
  height: 2px;
  margin: 2.4em 0;
  background: linear-gradient(90deg,
              var(--teal) 0%, var(--amber) 50%, var(--rose) 100%);
  opacity: 0.5;
}

/* ---------- Diagrams (SVG figures) ----------------------------------------- */

.diagram {
  margin: 1.8em 0;
  padding: 20px 24px;
  background: var(--bg);
  border: 1px solid var(--rule);
  border-top: 4px solid var(--teal);
  border-radius: 2px;
}
.diagram::before {
  content: attr(data-label);
  display: block;
  font-size: 11px;
  font-weight: 700;
  letter-spacing: 1.5px;
  text-transform: uppercase;
  color: var(--teal-dark);
  margin-bottom: 14px;
}
.diagram svg {
  display: block;
  width: 100%;
  height: auto;
  max-width: 1000px;
  margin: 0 auto;
}
.diagram figcaption {
  font-size: 13px;
  color: var(--ink-soft);
  margin-top: 12px;
  padding-top: 10px;
  border-top: 1px solid var(--rule-soft);
}

/* ---------- Footer --------------------------------------------------------- */

footer.colophon {
  margin-top: 4em;
  padding-top: 1.5em;
  border-top: 1px solid var(--rule);
  font-size: 13px;
  color: var(--ink-faint);
}
footer.colophon p { margin: 0.3em 0; }
footer.colophon code { font-size: 0.9em; }

/* ---------- Index page ----------------------------------------------------- */

.index-head {
  position: relative;
  padding: 4px 0 28px 24px;
  margin-bottom: 36px;
  border-bottom: 1px solid var(--rule);
}
.index-head::before {
  content: '';
  position: absolute;
  left: 0;
  top: 0;
  bottom: 28px;
  width: 6px;
  background: linear-gradient(180deg,
              var(--teal) 0%, var(--amber) 55%, var(--rose) 100%);
}
.index-head .eyebrow {
  display: block;
  font-size: 12px;
  font-weight: 700;
  letter-spacing: 2px;
  text-transform: uppercase;
  color: var(--teal);
  margin-bottom: 10px;
}
.index-head h1 {
  font-size: 44px;
  line-height: 1.05;
  letter-spacing: -1.2px;
  margin: 0 0 12px;
  font-weight: 800;
}
.index-head p {
  margin: 0;
  color: var(--ink-soft);
  font-size: 17px;
  max-width: 80ch;
}

.index-list {
  display: grid;
  grid-template-columns: 1fr;
  gap: 0;
  margin-top: 12px;
  border-top: 1px solid var(--rule);
}
.index-row {
  display: grid;
  grid-template-columns: minmax(240px, 1.1fr) 2fr;
  gap: 28px;
  align-items: baseline;
  padding: 18px 16px 18px 12px;
  border-bottom: 1px solid var(--rule-soft);
  border-left: 4px solid transparent;
  text-decoration: none;
  color: var(--ink);
  transition: background 0.1s, border-left-color 0.1s;
}
.index-row:hover {
  background: var(--teal-bg);
  border-left-color: var(--teal);
}
.index-row .index-title {
  font-weight: 700;
  font-size: 17px;
  color: var(--ink);
  letter-spacing: -0.2px;
}
.index-row:hover .index-title { color: var(--teal-dark); }
.index-row .index-summary {
  color: var(--ink-soft);
  font-size: 14.5px;
  line-height: 1.5;
}
@media (max-width: 640px) {
  .index-row { grid-template-columns: 1fr; gap: 6px; }
}

/* ---------- Print ---------------------------------------------------------- */

@media print {
  .topnav, details.toc { display: none; }
  body { color: black; background: white; }
  main pre { border: 1px solid #ccc; break-inside: avoid; }
  .diagram { break-inside: avoid; }
  table { break-inside: avoid; }
}
"""


SVG_INJECTIONS: list[tuple[str, str, str]] = []


# -----------------------------------------------------------------------------
# Status pill detection (auto-applied in tables)
# -----------------------------------------------------------------------------

_PILL_PATTERNS = [
    (re.compile(r"^✓( shipping)?$"),                  "ok"),
    (re.compile(r"^shipping$", re.IGNORECASE),        "ok"),
    (re.compile(r"^✗$"),                              "gap"),
    (re.compile(r"^pending$", re.IGNORECASE),         "gap"),
    (re.compile(r"^planned$", re.IGNORECASE),         "gap"),
    (re.compile(r"^—$"),                              "gap"),
    (re.compile(r"^partial$", re.IGNORECASE),         "warn"),
    (re.compile(r"^opt-in$", re.IGNORECASE),          "warn"),
]

def maybe_pill(cell: str) -> str | None:
    stripped = cell.strip()
    for rgx, cls in _PILL_PATTERNS:
        if rgx.match(stripped):
            return f'<span class="pill {cls}">{stripped}</span>'
    return None


# -----------------------------------------------------------------------------
# Minimal Markdown converter
# -----------------------------------------------------------------------------

INLINE_CODE_RE = re.compile(r"`([^`]+?)`")
BOLD_RE = re.compile(r"\*\*([^*]+?)\*\*")
EMPH_RE = re.compile(r"(?<!\*)\*([^*\n]+?)\*(?!\*)")
LINK_RE = re.compile(r"\[([^\]]+?)\]\(([^)]+?)\)")


def render_inline(text: str) -> str:
    out = html.escape(text, quote=False)
    out = INLINE_CODE_RE.sub(r"<code>\1</code>", out)
    out = BOLD_RE.sub(r"<strong>\1</strong>", out)
    out = EMPH_RE.sub(r"<em>\1</em>", out)
    out = LINK_RE.sub(r'<a href="\2">\1</a>', out)
    return out


def slugify(text: str) -> str:
    s = text.lower()
    s = re.sub(r"`", "", s)
    s = re.sub(r"[^a-z0-9]+", "-", s)
    return s.strip("-")


def convert(md_text: str) -> tuple[str, list[tuple[int, str, str]]]:
    """Convert markdown to HTML body + TOC list."""
    lines = md_text.splitlines()
    out: list[str] = []
    toc: list[tuple[int, str, str]] = []
    i, n = 0, len(lines)

    while i < n:
        line = lines[i]

        # Fenced code block
        if line.startswith("```"):
            lang = line[3:].strip()
            out.append(
                f'<pre><code class="lang-{html.escape(lang)}">'
            )
            i += 1
            while i < n and not lines[i].startswith("```"):
                out.append(html.escape(lines[i]))
                i += 1
            out.append("</code></pre>")
            i += 1
            continue

        # Headings
        m = re.match(r"^(#{1,6})\s+(.*)$", line)
        if m:
            level = len(m.group(1))
            text = m.group(2).rstrip()
            sid = slugify(text)
            toc.append((level, text, sid))
            inner = render_inline(text)
            if level == 1:
                # H1 collapses into doc-head (set from extract_h1_and_lede)
                i += 1
                continue

            out.append(f'<h{level} id="{sid}">{inner}</h{level}>')

            # Inject SVG figure after specific headings
            for needle, label, svg in SVG_INJECTIONS:
                if needle in text:
                    out.append(
                        f'<figure class="diagram" data-label="{label}">'
                    )
                    out.append(svg.strip())
                    out.append("</figure>")
                    break

            i += 1
            continue

        # Tables
        if (line.lstrip().startswith("|")
                and i + 1 < n
                and re.match(r"^\s*\|?\s*:?-+", lines[i + 1].lstrip())):
            headers = [c.strip() for c in line.strip().strip("|").split("|")]
            i += 2
            rows: list[list[str]] = []
            while i < n and lines[i].lstrip().startswith("|"):
                cells = [c.strip() for c in lines[i].strip().strip("|").split("|")]
                rows.append(cells)
                i += 1

            out.append('<div class="table-wrap"><table>')
            out.append("<thead><tr>")
            for h_ in headers:
                out.append(f"<th>{render_inline(h_)}</th>")
            out.append("</tr></thead><tbody>")
            for r in rows:
                out.append("<tr>")
                for c in r:
                    pill = maybe_pill(c)
                    inner = pill if pill else render_inline(c)
                    out.append(f"<td>{inner}</td>")
                out.append("</tr>")
            out.append("</tbody></table></div>")
            continue

        # Blockquote
        if line.startswith("> "):
            chunk: list[str] = []
            while i < n and lines[i].startswith(">"):
                chunk.append(lines[i].lstrip("> "))
                i += 1
            inner = " ".join(render_inline(c) for c in chunk)
            out.append(f"<blockquote><p>{inner}</p></blockquote>")
            continue

        # Unordered list
        if re.match(r"^\s*[-*]\s+", line):
            out.append("<ul>")
            while i < n and re.match(r"^\s*[-*]\s+", lines[i]):
                item = re.sub(r"^\s*[-*]\s+", "", lines[i])
                i += 1
                while (i < n and lines[i].strip()
                       and not re.match(r"^\s*[-*]\s+", lines[i])
                       and not re.match(r"^\s*\d+\.\s+", lines[i])
                       and lines[i].startswith("  ")):
                    item += " " + lines[i].strip()
                    i += 1
                out.append(f"<li>{render_inline(item)}</li>")
            out.append("</ul>")
            continue

        # Ordered list
        if re.match(r"^\s*\d+\.\s+", line):
            out.append("<ol>")
            while i < n and re.match(r"^\s*\d+\.\s+", lines[i]):
                item = re.sub(r"^\s*\d+\.\s+", "", lines[i])
                i += 1
                while (i < n and lines[i].strip()
                       and not re.match(r"^\s*\d+\.\s+", lines[i])
                       and lines[i].startswith("  ")):
                    item += " " + lines[i].strip()
                    i += 1
                out.append(f"<li>{render_inline(item)}</li>")
            out.append("</ol>")
            continue

        # Horizontal rule
        if re.match(r"^-{3,}\s*$", line) or re.match(r"^_{3,}\s*$", line):
            out.append("<hr>")
            i += 1
            continue

        # Blank line
        if not line.strip():
            i += 1
            continue

        # Paragraph
        para: list[str] = [line]
        i += 1
        while i < n and lines[i].strip() and not (
            lines[i].startswith("#")
            or lines[i].startswith("```")
            or lines[i].startswith("> ")
            or lines[i].lstrip().startswith("|")
            or re.match(r"^\s*[-*]\s+", lines[i])
            or re.match(r"^\s*\d+\.\s+", lines[i])
        ):
            para.append(lines[i])
            i += 1
        rendered = " ".join(render_inline(p) for p in para)
        out.append(f"<p>{rendered}</p>")

    return "\n".join(out), toc


# -----------------------------------------------------------------------------
# Page assembly
# -----------------------------------------------------------------------------

def extract_h1_and_lede(md_text: str) -> tuple[str | None, str | None]:
    """Pull the first H1 + the first paragraph following it."""
    lines = md_text.splitlines()
    h1, lede = None, None
    i = 0
    while i < len(lines):
        if lines[i].startswith("# ") and h1 is None:
            h1 = lines[i][2:].strip()
            j = i + 1
            while j < len(lines) and not lines[j].strip():
                j += 1
            buf: list[str] = []
            while (j < len(lines) and lines[j].strip()
                   and not lines[j].startswith("#")
                   and not lines[j].startswith("```")
                   and not lines[j].startswith("|")
                   and not lines[j].startswith(">")):
                buf.append(lines[j].strip())
                j += 1
            lede = " ".join(buf) if buf else None
            break
        i += 1
    return h1, lede


def render_toc(toc: list[tuple[int, str, str]]) -> str:
    """Inline collapsible TOC (no sidebar)."""
    items = [(lvl, text, sid) for lvl, text, sid in toc if 2 <= lvl <= 4]
    if not items:
        return ""

    out = ['<details class="toc" open><summary>Contents</summary><ul>']
    for level, text, sid in items:
        out.append(
            f'<li class="lvl-{level}"><a href="#{sid}">{render_inline(text)}</a></li>'
        )
    out.append('</ul></details>')
    return "\n".join(out)


def page_template(*, title: str, lede: str | None,
                  toc_html: str, body_html: str,
                  build_meta: dict) -> str:
    lede_html = f'<p class="lede">{render_inline(lede)}</p>' if lede else ""

    return f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="color-scheme" content="light dark">
<title>{html.escape(title)} · ruby.mruby docs</title>
<style>{CSS}</style>
</head>
<body>
<nav class="topnav">
  <a href="index.html" class="brand">ruby.mruby docs</a>
  <span class="spacer"></span>
  <a href="index.html">Index</a>
</nav>

<div class="page">
  <header class="doc-head">
    <div class="breadcrumb"><a href="index.html">docs</a> / {html.escape(build_meta['stem'])}.md</div>
    <h1>{render_inline(title)}</h1>
    {lede_html}
  </header>

  {toc_html}

  <main>
  {body_html}
  </main>

  <footer class="colophon">
    <p>Generated from <code>docs/{build_meta['stem']}.md</code> on
       {build_meta['date']}{', commit ' + build_meta['commit'] if build_meta['commit'] else ''}.</p>
  </footer>
</div>
</body>
</html>
"""


# -----------------------------------------------------------------------------
# Index page
# -----------------------------------------------------------------------------

def render_index(build_meta: dict) -> str:
    rows = []
    for stem, title, summary in DOCS:
        rows.append(
            f'<a class="index-row" href="{stem}.html">'
            f'  <span class="index-title">{html.escape(title)}</span>'
            f'  <span class="index-summary">{html.escape(summary)}</span>'
            f'</a>'
        )

    return f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="color-scheme" content="light dark">
<title>ruby.mruby docs</title>
<style>{CSS}</style>
</head>
<body>
<nav class="topnav">
  <a href="index.html" class="brand">ruby.mruby docs</a>
  <span class="spacer"></span>
</nav>

<div class="page">
  <header class="index-head">
    <span class="eyebrow">ruby.mruby · docs</span>
    <h1>Reference documentation</h1>
    <p>mruby를 C++ 호스트에 임베드하는 실험 시리즈의 참조 문서 —
       임베딩 패턴, 소유권 규칙, <code>mrubybind</code> 헬퍼.</p>
  </header>

  <div class="index-list">
    {''.join(rows)}
  </div>

  <footer class="colophon">
    <p>Generated {build_meta['date']}
       {('· commit ' + build_meta['commit']) if build_meta['commit'] else ''}
       · source: <code>docs/*.md</code></p>
  </footer>
</div>
</body>
</html>
"""


# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------

def git_short_sha() -> str:
    try:
        return subprocess.check_output(
            ["git", "rev-parse", "--short", "HEAD"],
            cwd=HERE.parent, stderr=subprocess.DEVNULL,
        ).decode().strip()
    except Exception:
        return ""


def main(argv: list[str]) -> int:
    build_meta = {
        "date":   _dt.datetime.now().strftime("%Y-%m-%d"),
        "commit": git_short_sha(),
    }

    missing = [stem for (stem, *_) in DOCS if not (HERE / f"{stem}.md").exists()]
    if missing:
        sys.stderr.write(f"missing markdown sources: {missing}\n")
        return 1

    for stem, title, summary in DOCS:
        md_path = HERE / f"{stem}.md"
        md_text = md_path.read_text(encoding="utf-8")

        h1, lede = extract_h1_and_lede(md_text)
        body, toc = convert(md_text)
        toc_html = render_toc(toc)

        page = page_template(
            title=h1 or title,
            lede=lede,
            toc_html=toc_html,
            body_html=body,
            build_meta={**build_meta, "stem": stem},
        )

        out_path = HERE / f"{stem}.html"
        out_path.write_text(page, encoding="utf-8")
        print(f"wrote {out_path.relative_to(HERE.parent)} ({len(page):,} B)")

    index_html = render_index(build_meta)
    (HERE / "index.html").write_text(index_html, encoding="utf-8")
    print(f"wrote docs/index.html ({len(index_html):,} B)")

    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
