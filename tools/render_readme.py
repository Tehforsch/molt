#!/usr/bin/env python3
# /// script
# requires-python = ">=3.11"
# dependencies = []
# ///
"""Render README.md from README.template.md.

The template supports one include directive:

    <!-- molt-readme: include-markdown-test example.md -->

The included file is resolved relative to tests/markdown/ unless a path with a
directory component is provided. Test-only code fence labels are rewritten into
labels that render well in the README.
"""

from __future__ import annotations

import argparse
import difflib
import re
import shlex
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
DEFAULT_TEMPLATE = ROOT / "README.template.md"
DEFAULT_OUTPUT = ROOT / "README.md"
MARKDOWN_TEST_DIR = ROOT / "tests" / "markdown"

DIRECTIVE_RE = re.compile(
    r"^[ \t]*<!--\s*molt-readme:\s*(?P<body>.*?)\s*-->[ \t]*\n?",
    re.MULTILINE,
)

FENCE_LANGS = {
    "molt": "rust",
    "molt error": "rust",
    "molt check_only": "rust",
    "rust": "rust",
    "rust reference": "rust",
    "output": "text",
}


class RenderError(Exception):
    pass


def resolve_test_path(raw_path: str) -> Path:
    path = Path(raw_path)
    if path.is_absolute():
        raise RenderError(f"absolute include paths are not allowed: {raw_path}")
    if path.parent == Path("."):
        path = MARKDOWN_TEST_DIR / path
    else:
        path = ROOT / path

    resolved = path.resolve()
    root = ROOT.resolve()
    try:
        resolved.relative_to(root)
    except ValueError as exc:
        raise RenderError(f"include escapes repository root: {raw_path}") from exc

    if not resolved.is_file():
        raise RenderError(f"included markdown test does not exist: {raw_path}")
    return resolved


def rewrite_fence_languages(markdown: str) -> str:
    output = []
    in_fence = False
    fence_marker = ""

    for line in markdown.splitlines(keepends=True):
        stripped = line.lstrip()
        indent = line[: len(line) - len(stripped)]

        if not in_fence:
            match = re.match(r"(```+|~~~+)([^\n`]*)?(\n?)$", stripped)
            if match:
                marker = match.group(1)
                info = (match.group(2) or "").strip()
                newline = match.group(3)
                rewritten = FENCE_LANGS.get(info, info)
                output.append(f"{indent}{marker}{rewritten}{newline}")
                in_fence = True
                fence_marker = marker[0]
            else:
                output.append(line)
        else:
            output.append(line)
            if stripped.startswith(fence_marker * 3):
                in_fence = False
                fence_marker = ""

    return "".join(output)


def render_include(args: list[str]) -> str:
    if len(args) != 1:
        raise RenderError(
            "include-markdown-test expects exactly one path, for example "
            "<!-- molt-readme: include-markdown-test basic_mod.md -->"
        )

    path = resolve_test_path(args[0])
    included = path.read_text(encoding="utf-8")
    rendered = rewrite_fence_languages(included).strip()
    return f"{rendered}\n"


def render_directive(match: re.Match[str]) -> str:
    body = match.group("body")
    try:
        parts = shlex.split(body)
    except ValueError as exc:
        raise RenderError(f"invalid directive syntax: {body}") from exc

    if not parts:
        raise RenderError("empty molt-readme directive")

    command, *args = parts
    if command == "include-markdown-test":
        return render_include(args)

    raise RenderError(f"unknown molt-readme directive: {command}")


def render_template(template: Path) -> str:
    content = template.read_text(encoding="utf-8")
    return DIRECTIVE_RE.sub(render_directive, content)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--template", type=Path, default=DEFAULT_TEMPLATE)
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument(
        "--check",
        action="store_true",
        help="fail if the output file is not up to date",
    )
    args = parser.parse_args()

    try:
        rendered = render_template(args.template)
    except OSError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1
    except RenderError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1

    if args.check:
        current = args.output.read_text(encoding="utf-8")
        if current != rendered:
            diff = difflib.unified_diff(
                current.splitlines(keepends=True),
                rendered.splitlines(keepends=True),
                fromfile=str(args.output),
                tofile=f"{args.template} (rendered)",
            )
            sys.stderr.writelines(diff)
            return 1
        return 0

    args.output.write_text(rendered, encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
