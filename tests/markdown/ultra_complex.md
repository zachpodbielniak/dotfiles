# Ultra Complex Markdown Test

This document tests all the complex Markdown features that might challenge a converter.

## 1. Nested Formatting

Here's some **bold text with *italic* nested inside it**. 

And here's ***bold and italic together***.

And _italics with **bold** inside them_.

Even more complex: **_*crazy* formatting** with `code` inside_ it all!

## 2. Code Spans and Escaped Characters

Inline code: `const x = function() { return 10 * 2; }` should not be formatted.

Code with backticks inside: `` `backtick` character ``

Escaped characters: \*not italic\*, \**not bold**\, \`not code\`, \[not a link\]

Code with formatting inside: `**this should not be bold**`

## 3. Complex Nested Blocks

> This is a blockquote
> With multiple lines
> 
> And a paragraph break
> 
> - And a list
> - Inside the blockquote
>   - With nested items
>   - That have *formatting*
>
> > And a nested blockquote
> > With its own content

Complex lists:

1. First ordered item
   - Unordered sub-item
   - Another one with **bold**
   
2. Second ordered item
   
   With a second paragraph inside the list item.
   
   ```javascript
   // And a code block
   function test() {
     return "inside a list item";
   }
   ```
   
   - And then another list
     1. With its own numbering
     2. And more items

- [ ] Task list item that's not checked
- [x] Task list item that's checked
  - [ ] Nested task item
  - [x] Completed nested task item

## 4. Reference-Style Links

This is a [reference link][ref1] and this is [another one][ref2].

This is a [link with the reference text itself as the reference][].

And this is an [implicit reference link].

Here's an ![image reference][logo].

[ref1]: https://example.com "Optional title"
[ref2]: https://example.org/path/to/page
[link with the reference text itself as the reference]: https://example.net
[implicit reference link]: https://implicit.example.com
[logo]: https://example.com/logo.png "Logo Alt Text"

## 5. URLs With Parentheses

Regular link: [Regular link](https://example.com)

Link with parentheses: [Parentheses link](https://example.com/path(with)parentheses)

Link with parentheses and title: [Parentheses link with title](https://example.com/path(complex)path "Title with (parentheses)")

## 6. Markdown Extensions

### Footnotes

Here's a text with a footnote[^1] and another footnote[^longnote].

[^1]: This is the first footnote.
[^longnote]: This is a longer footnote with multiple paragraphs.

    Indented paragraphs are part of the footnote.
    
    ```
    Code blocks too.
    ```

### Definition Lists

Apple
: A fruit that grows on trees.
: Can be red, green, or yellow.

Programming
: The act of writing code for computers to execute.

### Complex Tables

| Function | Description | Example |
|:---------|:------------|:--------|
| `map()`  | Transforms elements in an array | `[1, 2].map(x => x * 2)` |
| `filter()` | Filters elements in an array | `[1, 2, 3].filter(x => x > 1)` |
| `reduce()` | Accumulates values | `[1, 2, 3].reduce((a, b) => a + b)` |

Table with alignment:

| Left | Center | Right |
|:-----|:------:|------:|
| 1    |   2    |     3 |
| Long | Centered | Value |

## 7. Additional Features

### HTML Embedded in Markdown

<div class="custom-class">
  <p>This is a <em>HTML block</em> that should be handled appropriately.</p>
  <ul>
    <li>Item 1</li>
    <li>Item 2</li>
  </ul>
</div>

### Strikethrough

This is ~~struck through~~ text.

### Horizontal Rules

---

***

___

### Heading with ID {#custom-id}

### Math Expressions

Inline math: $E = mc^2$

Block math:

$$
\frac{d}{dx}(x^n) = nx^{n-1}
$$

### Alternative Heading Styles

Alt Heading 1
============

Alt Heading 2
------------

### Autolinks and URLs

Visit <https://example.com> or send email to <user@example.com>.

Plain URLs: https://auto.example.com should be auto-linked.