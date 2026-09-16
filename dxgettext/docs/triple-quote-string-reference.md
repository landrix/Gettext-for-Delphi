# Triple-Quoted String Extraction Reference

## Overview

dxgettext supports extraction of Delphi 12.3+ triple-quoted strings (`'''...'''`) from source files. This feature extracts string literals that span multiple lines with intelligent indentation handling.

## Syntax and Semantics

### String Format

Triple-quoted strings follow Delphi's native rules:

```
'''[optional whitespace]
content line 1
content line 2
...
'''[optional trailing text]
```

- **Opener**: Line must start with `'''` followed only by whitespace
- **Content**: All lines between opener and closer are included verbatim
- **Closer**: Line starting with `'''` (after optional leading whitespace) terminates the string

### Indentation Handling

The extraction strips common indentation from all content lines:

1. The closing delimiter's leading whitespace determines `baseIndent`
2. Up to `baseIndent` leading spaces/tabs are removed from each content line
3. Tabs count as one column (no expansion)
4. Blank lines are preserved as empty entries
5. Lines joined with `#10` (LF)

**Example:**
```pascal
resourcestring
  Triple1 = '''
    Line 1
    Line 2
  ''';
```

Extracted:
```
  Line 1
  Line 2
```

## Supported Contexts

Triple-quoted strings are extracted in these contexts:

| Context | Extracted? | Notes |
|---------|-----------|-------|
| `resourcestring` declarations | ✓ Always | Added to default domain |
| `const` declarations with `$gnugettext:` domain | ✓ When domain active | Gated by `definedDomain <> ''` |
| `gettext()` context parameter | ✓ | Appends `GETTEXT_CONTEXT_GLUE` |
| `gettext()` msgid parameter | ✓ | |
| `ngettext()` plural second string | ✓ | Prefixed with `PluralSplitter` |
| Domain parameter | ✗ | Not supported |

## Implementation Details

### Key Methods

#### `IsTripleQuoteOpener(line: string): boolean`

**Location**: `xgettext.pas:377-388`

Detects if a line is a valid triple-quote opener:

```pascal
if (Length(line) >= 3) and (Copy(line, 1, 3) = '''''''') then begin
  s := Trim(Copy(line, 4, MaxInt));
  Result := s = '';
end else
  Result := False;
```

**Requirements:**
- Line must start at first quote (no leading spaces)
- Only whitespace may follow `'''`
- Lines like `'''text''');` are **not** recognized

#### `ReadTripleQuotedString(var line, firstline, isutf8): string`

**Location**: `xgettext.pas:390-455`

Extracts multi-line triple-quoted content:

- Reads directly from `FLines[FLineNr]` (bypasses `dxreadln`)
- Preserves comments and quotes verbatim
- Advances `FLineNr` during read
- Returns empty string + warning if unclosed

### Call Sites

| Line Range | Context | Behavior |
|------------|---------|----------|
| 597-601 | const/resourcestring | Falls back to `readstring` if not opener |
| 602-608 | resourcestring (mode=2) | Always extracts to default domain |
| 609-626 | plain const | Extracts only when `definedDomain <> ''` |
| 730-734 | gettext context param | Appends `GETTEXT_CONTEXT_GLUE` |
| 753-757 | gettext msgid param | Direct extraction |
| 768-772 | ngettext plural string | Prefixed with `PluralSplitter` |

## Test Coverage

**Test file**: `tests/ExtractionValidationTestsets/TestTripleQuotes.pas` (relative to the project root, i.e. one level above the dxgettext subproject)

**Verified cases:**

1. **Multi-line resourcestring** - indentation preserved relative to closer
2. **Single-line triple-quote** - quotes inside content preserved
3. **Blank lines** - preserved in output
4. **Regular single-quoted strings** - parsed with `''` escape (not triple-quote)
5. **Plain const without domain** - not extracted
6. **gettext() calls** - all parameters support triple-quotes
7. **Mixed extraction** - only gettext args extracted, others ignored

## Limitations

1. **Only whitespace after `'''`** - nothing else may follow the opening quotes on the opener line (leading indentation is fine; in const/resourcestring contexts an identifier/equals prefix like `Ident = '''` is stripped before the check). Lines like `'''text''');` are not recognized as openers
2. **Content starting with `'''`** terminates the string (Delphi semantics)
3. **No domain support** - domains cannot be triple-quoted strings
4. **Tabs not expanded** - count as single column for indentation
5. **Unclosed strings** → warning + empty extraction

## Example Usage

### Resource String
```pascal
resourcestring
  WelcomeMessage = '''
    Welcome to the application.
    
    Please read the terms below.
  ''';
```

### Gettext Context
```pascal
gettext('context', '''
  Multi-line
  message text
''');
```

### Plural Form
```pascal
ngettext('''
  One item
''', '''
  Multiple items
''', count);
```

## Line Number Reporting

The `#:` comment in output reports the **closer delimiter's line number**, not the opener.

**Example:**
```
#: TestTripleQuotes.pas:51
msgid ""
"Line 1\n"
"Line 2"
msgstr ""
```

(opener at line 47, closer at line 51)
