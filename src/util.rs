use ropey::Rope;
use streaming_iterator::StreamingIterator;
use tower_lsp::lsp_types::*;
use tree_sitter::{Node, Point, Query, QueryCursor};

pub fn position_to_index(position: Position, rope: &Rope) -> Result<usize, ropey::Error> {
    let line = position.line as usize;
    let line = rope.try_line_to_char(line)?;
    Ok(line + position.character as usize)
}

pub fn position_to_byte_offset(position: Position, rope: &Rope) -> Result<usize, ropey::Error> {
    rope.try_char_to_byte(position_to_index(position, rope)?)
}

pub fn byte_offset_to_position(index: usize, rope: &Rope) -> Result<Position, ropey::Error> {
    let line = rope.try_byte_to_line(index)?;
    let char = index - rope.line_to_char(line);
    Ok(Position {
        line: line as u32,
        character: char as u32,
    })
}

pub fn lsp_range_to_rope_range(
    range: Range,
    rope: &Rope,
) -> Result<std::ops::Range<usize>, ropey::Error> {
    let start = position_to_index(range.start, rope)?;
    let end = position_to_index(range.end, rope)?;
    Ok(start..end)
}

pub fn lsp_position_to_ts_point(position: Position) -> Point {
    Point {
        row: position.line as usize,
        column: position.character as usize,
    }
}

pub fn ts_point_to_lsp_position(point: Point) -> Position {
    Position {
        line: point.row as u32,
        character: point.column as u32,
    }
}

pub fn ts_node_to_lsp_range(node: Node) -> Range {
    Range {
        start: ts_point_to_lsp_position(node.start_position()),
        end: ts_point_to_lsp_position(node.end_position()),
    }
}

pub fn get_current_capture_node(root: Node, point: Point) -> Option<Node> {
    root.named_descendant_for_point_range(point, point)
        .and_then(|node| {
            if node.grammar_name() == "capture" {
                Some(node)
            } else {
                node.parent()
                    .filter(|parent| parent.grammar_name() == "capture")
            }
        })
}

pub fn get_references<'a>(
    uri: &'a Url,
    root: &'a Node,
    node: &'a Node,
    query: &'a Query,
    cursor: &'a mut QueryCursor,
    contents: &'a [u8],
) -> impl Iterator<Item = Location> + 'a {
    return cursor
        .matches(query, root.child_with_descendant(*node).unwrap(), contents)
        .map_deref(|match_| {
            match_.captures.iter().filter_map(|cap| {
                if cap.node.grammar_name() == node.grammar_name()
                    && cap.node.utf8_text(contents) == node.utf8_text(contents)
                {
                    Some(Location {
                        uri: uri.clone(),
                        range: ts_node_to_lsp_range(cap.node),
                    })
                } else {
                    None
                }
            })
        })
        .flatten();
}
