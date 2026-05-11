#!/usr/bin/python3

# dotfiles - Personal configuration files and scripts
# Copyright (C) 2026  Zach Podbielniak
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

"""Bootstrap the Art of War study tree from Project Gutenberg's Giles 1910.

Emits, under <notes-dir>/02_areas/study/art-of-war/:
  - translations/giles.org   (canonical text, verse-addressable via :AOW_REF:)
  - chapters/NN-<slug>.org   (13 org-roam nodes that transclude their chapter)
  - index.org                (top-level roam TOC node)

Idempotent: re-running overwrites translations/giles.org and index.org but
preserves chapter node :ID: values when --keep-ids is passed and the chapter
file already exists. This keeps roam backlinks stable across re-runs.
"""

import argparse
import os
import re
import sys
import uuid
import urllib.request
from pathlib import Path

GUTENBERG_URL = "https://www.gutenberg.org/cache/epub/132/pg132.txt"

CHAPTER_TITLES = [
    "Laying Plans",
    "Waging War",
    "Attack by Stratagem",
    "Tactical Dispositions",
    "Energy",
    "Weak Points and Strong",
    "Maneuvering",
    "Variation of Tactics",
    "The Army on the March",
    "Terrain",
    "The Nine Situations",
    "The Attack by Fire",
    "The Use of Spies",
]

ROMAN = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII",
        "IX", "X", "XI", "XII", "XIII"]


def slugify(s: str) -> str:
    return re.sub(r"[^a-z0-9]+", "-", s.lower()).strip("-")


def roman_to_int(r: str) -> int:
    values = {"I": 1, "V": 5, "X": 10}
    total, prev = 0, 0
    for ch in reversed(r):
        v = values[ch]
        total += -v if v < prev else v
        prev = v
    return total


VERSE_RE   = re.compile(r"^(\d+(?:,\s*\d+)*)\.\s+(.*)$")
CHAPTER_RE = re.compile(r"^Chapter ([IVX]+)\.\s+(.+)$")
START_RE   = re.compile(r"^\*\*\*\s*START OF THE PROJECT GUTENBERG")
END_RE     = re.compile(r"^\*\*\*\s*END OF THE PROJECT GUTENBERG")
# Strips trailing footnote definitions like "[1] \"Words on Wellington,\"..."
# from the last verse of each chapter. Embedded [N] refs inside bracketed
# commentary are preserved by virtue of not matching at the END of the text.
TRAILING_FOOTNOTE_RE = re.compile(r"(?:\n\s*\n\[\d+\][^\[]*?)+\s*$", re.DOTALL)


def fetch_source(cache: Path, force_fetch: bool) -> str:
    if cache.exists() and not force_fetch:
        return cache.read_text(encoding="utf-8")
    print(f"Fetching {GUTENBERG_URL}", file=sys.stderr)
    with urllib.request.urlopen(GUTENBERG_URL, timeout=30) as r:
        text = r.read().decode("utf-8")
    cache.write_text(text, encoding="utf-8")
    return text


def parse(text: str) -> list[dict]:
    """Return [{num, title, intro, verses: [{refs, text}]}] across all 13 chapters."""
    lines = text.splitlines()

    # Locate the body (between START / END markers, then after first chapter line)
    body_start, body_end = 0, len(lines)
    for i, l in enumerate(lines):
        if START_RE.match(l):
            body_start = i + 1
            break
    for i in range(body_start, len(lines)):
        if END_RE.match(lines[i]):
            body_end = i
            break
    lines = lines[body_start:body_end]

    chapters: list[dict] = []
    cur: dict | None = None
    verse_refs: list[str] | None = None
    verse_buf: list[str] = []
    in_intro = False
    intro_buf: list[str] = []

    def flush_verse():
        nonlocal verse_refs, verse_buf
        if verse_refs is None:
            return
        text = "\n".join(verse_buf).strip()
        cur["verses"].append({"refs": verse_refs, "text": text})
        verse_refs, verse_buf = None, []

    def flush_intro():
        nonlocal in_intro, intro_buf
        if intro_buf:
            cur["intro"] = "\n".join(intro_buf).strip()
        in_intro = False
        intro_buf = []

    for raw in lines:
        line = raw.rstrip()

        m = CHAPTER_RE.match(line)
        if m:
            flush_verse()
            if cur is not None:
                flush_intro()
            cur = {
                "num": roman_to_int(m.group(1)),
                "title": CHAPTER_TITLES[roman_to_int(m.group(1)) - 1],
                "intro": "",
                "verses": [],
            }
            chapters.append(cur)
            in_intro = True
            intro_buf = []
            continue

        if cur is None:
            continue

        m = VERSE_RE.match(line)
        if m:
            flush_verse()
            flush_intro()
            refs = [r.strip() for r in m.group(1).split(",")]
            verse_refs = [f"{cur['num']}.{r}" for r in refs]
            verse_buf = [m.group(2)] if m.group(2) else []
            continue

        if in_intro:
            intro_buf.append(line)
        elif verse_refs is not None:
            verse_buf.append(line)

    flush_verse()
    if cur is not None:
        flush_intro()

    return chapters


def normalize_block(s: str) -> str:
    """Collapse runs of blank lines, strip leading/trailing blanks."""
    s = re.sub(r"\n{3,}", "\n\n", s)
    return s.strip()


def strip_trailing_footnotes(s: str) -> str:
    """Remove footnote definitions ([1] ..., [2] ...) that appear at the end."""
    return TRAILING_FOOTNOTE_RE.sub("", s).rstrip()


def emit_giles_org(chapters: list[dict]) -> str:
    out = [
        "#+title: The Art of War — Giles (1910)",
        "#+author: Sun Tzŭ (translated by Lionel Giles)",
        "#+filetags: art-of-war translation giles",
        "",
        "Public-domain translation, sourced from Project Gutenberg eBook #132.",
        "Each verse carries an :AOW_REF: property of the form CHAPTER.VERSE so",
        "elisp consumers can resolve by reference in O(1).",
        "",
    ]
    for ch in chapters:
        out += [
            f"* {ROMAN[ch['num']-1]}. {ch['title']}",
            ":PROPERTIES:",
            f":AOW_CHAPTER: {ch['num']}",
            ":END:",
            "",
        ]
        if ch["intro"]:
            out += ["** Intro", normalize_block(ch["intro"]), ""]
        last_idx = len(ch["verses"]) - 1
        for i, v in enumerate(ch["verses"]):
            label = ", ".join(r.split(".")[1] for r in v["refs"])
            body = v["text"]
            if i == last_idx:
                body = strip_trailing_footnotes(body)
            out += [
                f"** {label}",
                ":PROPERTIES:",
                f":AOW_REF: {','.join(v['refs'])}",
                ":END:",
                normalize_block(body),
                "",
            ]
    return "\n".join(out) + "\n"


def existing_id(path: Path) -> str | None:
    """Read :ID: from an existing org file to keep roam IDs stable."""
    if not path.exists():
        return None
    for line in path.read_text(encoding="utf-8").splitlines()[:10]:
        m = re.match(r"^:ID:\s+(\S+)", line)
        if m:
            return m.group(1)
    return None


def emit_chapter_node(ch: dict, giles_path: Path, keep_ids: bool,
                     chapter_path: Path) -> tuple[str, str]:
    node_id = (existing_id(chapter_path) if keep_ids else None) or str(uuid.uuid4())
    rel = os.path.relpath(giles_path, chapter_path.parent)
    body = "\n".join([
        ":PROPERTIES:",
        f":ID:       {node_id}",
        f":AOW_CHAPTER: {ch['num']}",
        ":END:",
        f"#+title: Art of War — {ROMAN[ch['num']-1]}. {ch['title']}",
        f"#+filetags: art-of-war chapter-{ch['num']:02d}",
        "",
        "* Canonical text (Giles)",
        f"#+transclude: [[file:{rel}::*{ROMAN[ch['num']-1]}. {ch['title']}]] :level 2",
        "",
        "* Notes",
        "",
        "* Backlinks",
        "",
    ])
    return node_id, body


def emit_index(chapter_ids: list[str], keep_ids: bool, index_path: Path) -> str:
    node_id = (existing_id(index_path) if keep_ids else None) or str(uuid.uuid4())
    lines = [
        ":PROPERTIES:",
        f":ID:       {node_id}",
        ":END:",
        "#+title: Art of War — Index",
        "#+filetags: art-of-war index",
        "",
        "Top-level entry for the Art of War study tree.",
        "",
        "* Translations",
        "- [[file:translations/giles.org][Giles (1910)]] — public domain, canonical reference",
        "",
        "* Chapters",
    ]
    for i, ch_id in enumerate(chapter_ids):
        lines.append(f"- [[id:{ch_id}][{ROMAN[i]}. {CHAPTER_TITLES[i]}]]")
    lines.append("")
    return "\n".join(lines) + "\n"


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--notes-dir", default=os.path.expanduser("~/Documents/notes"))
    ap.add_argument("--cache", default="/tmp/aow-giles-source.txt")
    ap.add_argument("--force-fetch", action="store_true",
                    help="Re-download even if cache exists")
    ap.add_argument("--keep-ids", action="store_true", default=True,
                    help="Preserve existing :ID: values in chapter/index files (default)")
    ap.add_argument("--regenerate-ids", action="store_true",
                    help="Allocate fresh :ID: values, breaking existing backlinks")
    args = ap.parse_args()

    keep_ids = args.keep_ids and not args.regenerate_ids

    aow = Path(args.notes_dir) / "02_areas" / "study" / "art-of-war"
    (aow / "translations").mkdir(parents=True, exist_ok=True)
    (aow / "chapters").mkdir(parents=True, exist_ok=True)

    text = fetch_source(Path(args.cache), args.force_fetch)
    chapters = parse(text)

    if len(chapters) != 13:
        print(f"ERROR: parsed {len(chapters)} chapters, expected 13", file=sys.stderr)
        sys.exit(1)

    giles_path = aow / "translations" / "giles.org"
    giles_path.write_text(emit_giles_org(chapters), encoding="utf-8")

    chapter_ids: list[str] = []
    for ch in chapters:
        fname = f"{ch['num']:02d}-{slugify(ch['title'])}.org"
        path = aow / "chapters" / fname
        cid, body = emit_chapter_node(ch, giles_path, keep_ids, path)
        path.write_text(body, encoding="utf-8")
        chapter_ids.append(cid)

    index_path = aow / "index.org"
    index_path.write_text(emit_index(chapter_ids, keep_ids, index_path),
                          encoding="utf-8")

    # Stamp the canonical text with the index ID so the elisp module can find it
    stamp = aow / ".aow-meta.el"
    elisp_list = "(" + " ".join(f'"{cid}"' for cid in chapter_ids) + ")"
    stamp.write_text(
        ";;; .aow-meta.el --- art-of-war.el data stamp -*- lexical-binding: t; -*-\n"
        ";;; Auto-generated by aow-bootstrap.py — do not edit by hand.\n"
        f'(setq aow--index-id "{existing_id(index_path)}")\n'
        f"(setq aow--chapter-ids '{elisp_list})\n",
        encoding="utf-8")

    total_verses = sum(len(c["verses"]) for c in chapters)
    print(f"Generated {aow}")
    print(f"  - translations/giles.org  ({total_verses} verses across 13 chapters)")
    print(f"  - chapters/*.org          (13 roam nodes)")
    print(f"  - index.org               (roam TOC)")
    print(f"  - .aow-meta.el            (elisp stamp consumed by art-of-war.el)")


if __name__ == "__main__":
    main()
