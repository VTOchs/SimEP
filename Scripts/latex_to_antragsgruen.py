#!/usr/bin/env python3
"""
Script to convert LaTeX motion documents to plain text format suitable for Antragsgrün.
Removes LaTeX formatting while preserving document structure.
"""

import re
import sys
from pathlib import Path


def to_roman(num, upper=False):
    """Convert integer to Roman numeral."""
    vals = [
        (1000, "M"),
        (900, "CM"),
        (500, "D"),
        (400, "CD"),
        (100, "C"),
        (90, "XC"),
        (50, "L"),
        (40, "XL"),
        (10, "X"),
        (9, "IX"),
        (5, "V"),
        (4, "IV"),
        (1, "I"),
    ]
    out = []
    n = max(1, int(num))
    for value, symbol in vals:
        while n >= value:
            out.append(symbol)
            n -= value
    result = "".join(out)
    return result if upper else result.lower()


def to_alpha(num, upper=False):
    """Convert integer to alphabetic sequence: 1->a, 27->aa."""
    n = max(1, int(num))
    chars = []
    while n > 0:
        n -= 1
        chars.append(chr(ord("A") + (n % 26)))
        n //= 26
    result = "".join(reversed(chars))
    return result if upper else result.lower()


def parse_enumerate_style(options):
    """Determine enumerate style and start value from optional arguments."""
    style = "decimal"
    start = 1
    if not options:
        return style, start

    m_start = re.search(r"start\s*=\s*(\d+)", options)
    if m_start:
        start = int(m_start.group(1))

    m_label = re.search(r"label\s*=\s*([^,\]]+)", options)
    if m_label:
        label = m_label.group(1)
        if "\\Alph" in label:
            style = "Alph"
        elif "\\alph" in label:
            style = "alph"
        elif "\\Roman" in label:
            style = "Roman"
        elif "\\roman" in label:
            style = "roman"
        else:
            style = "decimal"

    return style, start


def enumerate_marker(counter, style):
    """Create numbering marker for a given style."""
    if style == "Alph":
        return f"{to_alpha(counter, upper=True)}."
    if style == "alph":
        return f"{to_alpha(counter)})"
    if style == "Roman":
        return f"{to_roman(counter, upper=True)}."
    if style == "roman":
        return f"{to_roman(counter)})"
    return f"{counter}."


def strip_inline_latex(text):
    """Strip common inline LaTeX formatting commands while preserving content."""
    text = re.sub(r"\\ldots", "...", text)
    text = re.sub(r"\\%", "%", text)
    text = re.sub(r"\\noindent\b", "", text)
    text = re.sub(r"\\quad\b", " ", text)
    text = re.sub(r"\\qquad\b", "  ", text)
    text = text.replace("~", " ")
    text = text.replace("\\\\", "")

    prev = None
    while prev != text:
        prev = text
        text = re.sub(r"\\(?:textbf|textit|texttt|emph)\{([^{}]*)\}", r"\1", text)

    text = re.sub(r"\\[A-Za-z]+\*?(?:\[[^\]]*\])?(?:\{[^{}]*\})?", "", text)
    text = text.replace("{", "").replace("}", "")
    text = re.sub(r"\s+", " ", text).strip()
    return text


def normalize_for_plain_antragsgruen(text):
    """Prepare plain text for paste and force indentation with non-breaking spaces."""
    lines = []
    for raw in text.splitlines():
        # Convert only leading spaces to NBSP so HTML rendering can't collapse indentation.
        leading = len(raw) - len(raw.lstrip(" "))
        rest = raw[leading:]
        lines.append(("\u00A0" * leading) + rest)

    def item_kind(line):
        stripped = line.replace("\u00A0", " ").lstrip()
        if re.match(r"^\d+\.\s+", stripped):
            return "number"
        if re.match(r"^(?:-|[A-Z]\.|[a-z]\)|[ivxlcdm]+\))\s+", stripped):
            return "sub"
        return None

    # Spacing by level: 2 blank lines before numbers, 1 before letter/roman/bullet items.
    normalized_lines = []
    for line in lines:
        if not line.strip():
            if normalized_lines and normalized_lines[-1] != "":
                normalized_lines.append("")
            continue

        kind = item_kind(line)
        if kind:
            required_blanks = 2 if kind == "number" else 1
            trailing_blanks = 0
            i = len(normalized_lines) - 1
            while i >= 0 and normalized_lines[i] == "":
                trailing_blanks += 1
                i -= 1

            has_previous_content = i >= 0
            if has_previous_content and trailing_blanks < required_blanks:
                normalized_lines.extend([""] * (required_blanks - trailing_blanks))

        normalized_lines.append(line)

    normalized = "\n".join(normalized_lines)
    normalized = re.sub(r"\n{5,}", "\n\n\n\n", normalized).strip()
    return normalized


def clean_latex_document(latex_content):
    """
    Convert LaTeX document to plain text suitable for Antragsgrün.
    
    Args:
        latex_content (str): The content of the LaTeX file
        
    Returns:
        str: Cleaned plain text content
    """
    
    # Skip title page and start at the first legislative content block.
    start_index = -1
    start_marker = "DAS EUROPÄISCHE PARLAMENT,"
    if start_marker in latex_content:
        start_index = latex_content.find(start_marker)
    else:
        fallback_markers = [r"\subsection*{", r"\subsubsection*{", r"\section*{"]
        positions = [latex_content.find(m) for m in fallback_markers if latex_content.find(m) != -1]
        if positions:
            start_index = min(positions)

    if start_index != -1:
        latex_content = latex_content[start_index:]

    out_lines = []
    env_stack = []
    indent_unit = "  "

    just_closed_env = False
    # Helper: render a LaTeX tabular block into centered plain-text rows with dotted separators
    def render_tabular_text(tab_block):
        # Normalize line endings and remove surrounding begin/end
        content = re.sub(r"\\begin\{tabular\}.*?\n", "", tab_block, flags=re.S)
        content = re.sub(r"\\end\{tabular\}.*", "", content, flags=re.S)
        # Split rows on LaTeX row terminator \\\\ (allow trailing spaces)
        rows = [r.strip() for r in content.split('\\\\')]
        parsed_rows = []
        for row in rows:
            row = row.strip()
            if not row:
                continue
            # split columns by & but ignore escaped \& (simple approach)
            cols = [c.replace('\\&', '&').strip() for c in re.split(r'(?<!\\)&', row)]
            row_cells = []
            for c in cols:
                m_inc = re.search(r"\\\includegraphics(?:\[[^\]]*\])?\{([^}]+)\}", c)
                if m_inc:
                    # use filename as placeholder for logo, keep basename
                    cell = Path(m_inc.group(1)).name
                else:
                    cell = strip_inline_latex(c)
                row_cells.append(cell)
            parsed_rows.append(row_cells)

        if not parsed_rows:
            return ""

        # compute column widths
        ncols = max(len(r) for r in parsed_rows)
        widths = [0] * ncols
        for r in parsed_rows:
            for i in range(ncols):
                cell = r[i] if i < len(r) else ""
                widths[i] = max(widths[i], len(cell))

        lines_out = []
        total_width = sum(widths) + 3 * (ncols - 1)
        sep_line = "." * total_width

        for ridx, r in enumerate(parsed_rows):
            cells = []
            for i in range(ncols):
                cell = r[i] if i < len(r) else ""
                # center cell content
                pad = widths[i] - len(cell)
                left = pad // 2
                right = pad - left
                cells.append(" " * left + cell + " " * right)
            lines_out.append("   ".join(cells))
            # add dotted separator between rows (but not after last)
            if ridx < len(parsed_rows) - 1:
                lines_out.append(sep_line)

        return "\n".join(lines_out)

    lines = latex_content.splitlines()
    i = 0
    while i < len(lines):
        raw_line = lines[i]
        line = re.sub(r"(?<!\\)%.*$", "", raw_line).strip()
        if not line:
            if out_lines and out_lines[-1] != "":
                out_lines.append("")
            just_closed_env = False
            i += 1
            continue

        # detect tabular start and consume the whole block
        if re.match(r"\\begin\{tabular\}", line):
            # collect until \end{tabular}
            j = i
            block_lines = []
            while j < len(lines):
                block_lines.append(lines[j])
                if re.search(r"\\end\{tabular\}", lines[j]):
                    break
                j += 1
            tab_block = "\n".join(block_lines)
            rendered = render_tabular_text(tab_block)
            if rendered:
                for rl in rendered.splitlines():
                    out_lines.append(rl)
            i = j + 1
            just_closed_env = False
            continue

        m_begin = re.match(r"\\begin\{(enumerate|itemize)\}(?:\[([^\]]*)\])?", line)
        if m_begin:
            env = m_begin.group(1)
            options = m_begin.group(2) or ""
            if env == "enumerate":
                style, start = parse_enumerate_style(options)
                env_stack.append(
                    {
                        "type": "enumerate",
                        "style": style,
                        "counter": start,
                        "last_item_index": None,
                    }
                )
            else:
                env_stack.append({"type": "itemize", "last_item_index": None})
            just_closed_env = False
            i += 1
            continue

        if re.match(r"\\end\{(enumerate|itemize)\}", line):
            if env_stack:
                env_stack.pop()
            just_closed_env = True
            i += 1
            continue

        m_section = re.match(r"\\(?:subsection|subsubsection|section)\*?\{(.*)\}", line)
        if m_section:
            title = strip_inline_latex(m_section.group(1))
            if title:
                if out_lines and out_lines[-1] != "":
                    out_lines.append("")
                out_lines.append(f"<<H>>{title}")
                out_lines.append("")
            just_closed_env = False
            i += 1
            continue

        m_item = re.match(r"\\item\s*(.*)", line)
        if m_item:
            content = strip_inline_latex(m_item.group(1))
            depth = len(env_stack)
            indent = indent_unit * max(0, depth - 1)

            if env_stack and env_stack[-1]["type"] == "enumerate":
                marker = enumerate_marker(env_stack[-1]["counter"], env_stack[-1]["style"])
                is_first_in_this_list = env_stack[-1]["counter"] == 1
                env_stack[-1]["counter"] += 1
                env_stack[-1]["last_marker"] = marker
            else:
                marker = "-"
                is_first_in_this_list = False

            # Add visual separation between a parent point and the first nested subpoint.
            if depth > 1 and is_first_in_this_list:
                out_lines.append("")

            out_lines.append(f"{indent}{marker} {content}".rstrip())
            env_stack[-1]["last_item_index"] = len(out_lines) - 1
            just_closed_env = False
            i += 1
            continue

        cleaned = strip_inline_latex(line)
        if not cleaned:
            i += 1
            continue

        if env_stack:
            target_index = None
            for env in reversed(env_stack):
                if env.get("last_item_index") is not None:
                    target_index = env["last_item_index"]
                    break

            if just_closed_env:
                indent = indent_unit * len(env_stack)
                out_lines.append(f"{indent}{cleaned}".rstrip())
            elif target_index is not None:
                out_lines[target_index] = f"{out_lines[target_index]} {cleaned}".strip()
            else:
                out_lines.append(cleaned)
        else:
            out_lines.append(cleaned)

        just_closed_env = False
        i += 1

    for i, value in enumerate(out_lines):
        if value.strip():
            if value.strip() == "DAS EUROPÄISCHE PARLAMENT,":
                out_lines[i] = "<<H>>DAS EUROPÄISCHE PARLAMENT,"
            break

    text = "\n".join(out_lines)
    text = re.sub(r"\n{3,}", "\n\n", text)
    text = text.strip()
    return text


def convert_file(input_path):
    """Convert one LaTeX file to Antragsgrün plain-text output."""
    with open(input_path, 'r', encoding='utf-8') as f:
        latex_content = f.read()

    cleaned_content = clean_latex_document(latex_content)
    txt_content = re.sub(r"^<<H>>(.*)$", r"\1", cleaned_content, flags=re.MULTILINE)
    txt_content = normalize_for_plain_antragsgruen(txt_content)

    txt_output_file = input_path.stem + "_antragsgruen.txt"
    txt_output_path = input_path.parent / txt_output_file
    with open(txt_output_path, 'w', encoding='utf-8') as f:
        f.write(txt_content)

    return txt_output_path


def main():
    """Main function to process one file or batch-convert all drafts."""
    default_dir = Path(r"c:\Users\Admin\Desktop\JEF\SimEP\LaTeX\Gesetzesentwürfe")

    if len(sys.argv) > 1:
        arg_path = Path(sys.argv[1])
        if arg_path.is_dir():
            input_paths = sorted(arg_path.glob("Entwurf_*.tex"))
        else:
            input_paths = [arg_path]
    else:
        input_paths = sorted(default_dir.glob("Entwurf_*.tex"))

    input_paths = [p for p in input_paths if p.name.endswith(".tex")]
    if not input_paths:
        print("Error: No input .tex files found.")
        sys.exit(1)

    print("✓ Successfully converted LaTeX to plain text")
    for input_path in input_paths:
        if not input_path.exists():
            print(f"  Skipped missing file: {input_path}")
            continue
        output_path = convert_file(input_path)
        print(f"  Input:  {input_path}")
        print(f"  Output: {output_path}")

    print("\n✓ Ausschussübersicht Druck")
    print("\nOutput is strict plain text for direct paste into Antragsgrün text fields.")


if __name__ == "__main__":
    main()
