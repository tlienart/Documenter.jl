"""
...

Provides the [`walk`](@ref) function for walking over a [`Markdown2.MD`](@ref) tree.
"""
module Markdown2
using Compat
using DocStringExtensions

using Compat
import Compat.Markdown

# Contains all the types for the Markdown AST tree
abstract type MarkdownNode end
abstract type MarkdownBlockNode <: MarkdownNode end
abstract type MarkdownInlineNode <: MarkdownNode end

"""
    MD

A type to represent Markdown documents.
"""
struct MD
    nodes :: Vector{MarkdownBlockNode}

    MD(content::AbstractVector) = new(content)
end
MD() = MD([])

# Forward some array methods
Base.push!(md::MD, x) = push!(md.nodes, x)
Base.getindex(md::MD, args...) = md.nodes[args...]
Base.setindex!(md::MD, args...) = setindex!(md.nodes, args...)
Base.endof(md::MD) = endof(md.nodes)
Base.length(md::MD) = length(md.nodes)
Base.isempty(md::MD) = isempty(md.nodes)


# Block nodes
# ===========

struct ThematicBreak <: MarkdownBlockNode end

struct Heading <: MarkdownBlockNode
    level :: Int
    nodes :: Vector{MarkdownInlineNode}

    function Heading(level::Integer, nodes::Vector{MarkdownInlineNode})
        @assert 1 <= level <= 6 # TODO: error message
        new(level, nodes)
    end
end

struct CodeBlock <: MarkdownBlockNode
    language :: String
    code :: String
end

#struct HTMLBlock <: MarkdownBlockNode end # the parser in Base does not support this currently
#struct LinkDefinition <: MarkdownBlockNode end # the parser in Base does not support this currently

"""
"""
struct Paragraph <: MarkdownBlockNode
    nodes :: Vector{MarkdownInlineNode}
end

## Container blocks
struct BlockQuote <: MarkdownBlockNode
    nodes :: Vector{MarkdownBlockNode}
end

"""
If `.orderedstart` is `nothing` then the list is unordered. Otherwise is specifies the first
number in the list.
"""
struct List <: MarkdownBlockNode
    tight :: Bool
    orderedstart :: Union{Int, Nothing}
    items :: Vector{Vector{MarkdownBlockNode}} # TODO: Better types?
end

# Non-Commonmark extensions
struct DisplayMath <: MarkdownBlockNode
    formula :: String
end

struct Footnote <: MarkdownBlockNode
    id :: String
    nodes :: Vector{MarkdownBlockNode} # Footnote is a container block
end

struct Table <: MarkdownBlockNode
    align :: Vector{Symbol}
    cells :: Array{Vector{MarkdownInlineNode}, 2} # TODO: better type?
    # Note: Table is _not_ a container type -- the cells can only contan inlines.
end

struct Admonition <: MarkdownBlockNode
    category :: String
    title :: String
    nodes :: Vector{MarkdownBlockNode} # Admonition is a container block
end

# Inline nodes
# ============

struct Text <: MarkdownInlineNode
    text :: String
end

struct CodeSpan <: MarkdownInlineNode
    code :: String
end

struct Emphasis <: MarkdownInlineNode
    nodes :: Vector{MarkdownInlineNode}
end

struct Strong <: MarkdownInlineNode
    nodes :: Vector{MarkdownInlineNode}
end

struct Link <: MarkdownInlineNode
    destination :: String
    #title :: String # the parser in Base does not support this currently
    nodes :: Vector{MarkdownInlineNode}
end

struct Image <: MarkdownInlineNode
    destination :: String
    description :: String
    #title :: String # the parser in Base does not support this currently
    #nodes :: Vector{MarkdownInlineNode} # the parser in Base does not parse the description currently
end
#struct InlineHTML <: MarkdownInlineNode end # the parser in Base does not support this currently
struct LineBreak <: MarkdownInlineNode end

# Non-Commonmark extensions
struct InlineMath <: MarkdownInlineNode
    formula :: String
end

struct FootnoteReference <: MarkdownInlineNode
    id :: String
end


# Conversion methods
# ==================

function Base.convert(::Type{MD}, md::Markdown.MD)
    nodes = map(_convert_block, md.content)
    MD(nodes)
end

_convert_block(xs::Vector) = MarkdownBlockNode[_convert_block(x) for x in xs]
_convert_block(b::Markdown.HorizontalRule) = ThematicBreak()
_convert_block(b::Markdown.Header{N}) where N = Heading(N, _convert_inline(b.text))
_convert_block(b::Markdown.Code) = CodeBlock(b.language, b.code)
_convert_block(b::Markdown.Paragraph) = Paragraph(_convert_inline(b.content))
_convert_block(b::Markdown.BlockQuote) = BlockQuote(_convert_block(b.content))
function _convert_block(b::Markdown.List)
    tight = all(isequal(1), length.(b.items))
    orderedstart = (b.ordered == -1) ? nothing : b.ordered
    List(tight, orderedstart, _convert_block.(b.items))
end

# Non-Commonmark extensions
_convert_block(b::Markdown.LaTeX) = DisplayMath(b.formula)
_convert_block(b::Markdown.Footnote) = Footnote(b.id, _convert_block(b.text))
function _convert_block(b::Markdown.Table)
    @assert all(isequal(length(b.align)), length.(b.rows)) # TODO: error
    cells = [_convert_inline(b.rows[i][j]) for i = 1:length(b.rows), j = 1:length(b.align)]
    Table(
        b.align,
        [_convert_inline(b.rows[i][j]) for i = 1:length(b.rows), j = 1:length(b.align)]
    )
end
_convert_block(b::Markdown.Admonition) = Admonition(b.category, b.title, _convert_block(b.content))


_convert_inline(xs::Vector) = MarkdownInlineNode[_convert_inline(x) for x in xs]
_convert_inline(s::String) = Text(s)
function _convert_inline(s::Markdown.Code)
    @assert isempty(s.language) # TODO: error
    CodeSpan(s.code)
end
_convert_inline(s::Markdown.Bold) = Strong(_convert_inline(s.text))
_convert_inline(s::Markdown.Italic) = Emphasis(_convert_inline(s.text))
_convert_inline(s::Markdown.Link) = Link(s.url, _convert_inline(s.text))
_convert_inline(s::Markdown.Image) = Image(s.url, s.alt)
# struct InlineHTML <: MarkdownInlineNode end # the parser in Base does not support this currently
_convert_inline(::Markdown.LineBreak) = LineBreak()

# Non-Commonmark extensions
_convert_inline(s::Markdown.LaTeX) = InlineMath(s.formula)
function _convert_inline(s::Markdown.Footnote)
    @assert s.text === nothing # footnote references should not have any content, TODO: error
    FootnoteReference(s.id)
end


# walk()
# ======

"""
    walk(f, element)

Calls `f(element)` on `element` and any of its child elements. The elements are
assumed to be [`Markdown2`](@ref) elements.
"""
function walk end

function walk(f, node::T) where {T <: Union{MarkdownNode, MD}}
    f(node) || return
    if :nodes in fieldnames(T)
        walk(f, node.nodes)
    end
    return
end

function walk(f, nodes::Vector)
    for node in nodes
        walk(f, node)
    end
end

function walk(f, list::List)
    f(list) || return
    for item in list.items
        walk(f, item)
    end
end

function walk(f, table::Table)
    f(table) || return
    for cell in table.cells
        walk(f, cell)
    end
end
end
