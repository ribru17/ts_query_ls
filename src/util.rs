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

pub fn byte_offset_to_ts_point(index: usize, rope: &Rope) -> Result<Point, ropey::Error> {
    let line = rope.try_byte_to_line(index)?;
    let char = index - rope.line_to_char(line);
    Ok(Point {
        row: line,
        column: char,
    })
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

pub fn node_is_or_has_ancestor(root: Node, node: Node, kind: &str) -> bool {
    let mut cur_node = root.child_with_descendant(node);
    while cur_node.is_some() {
        if cur_node.unwrap().grammar_name() == kind {
            return true;
        }
        cur_node = cur_node.unwrap().child_with_descendant(node);
    }
    false
}

pub fn lsp_textdocchange_to_ts_inputedit(
    source: &ropey::Rope,
    change: &TextDocumentContentChangeEvent,
) -> Result<tree_sitter::InputEdit, Box<dyn std::error::Error>> {
    let text = change.text.as_str();
    let text_bytes = text.as_bytes();
    let text_end_byte_idx = text_bytes.len();

    let range = if let Some(range) = change.range {
        range
    } else {
        let start = byte_offset_to_position(0, source)?;
        let end = byte_offset_to_position(text_end_byte_idx, source)?;
        Range { start, end }
    };

    let start_point = lsp_position_to_ts_point(range.start);
    let start_byte = position_to_byte_offset(range.start, source)?;
    let old_end_point = lsp_position_to_ts_point(range.end);
    let old_end_byte = position_to_byte_offset(range.end, source)?;

    let new_end_byte = start_byte as usize + text_end_byte_idx;

    let new_end_position = {
        if new_end_byte >= source.len_bytes() {
            let line_idx = text.lines().count();
            let line_byte_idx = ropey::str_utils::line_to_byte_idx(text, line_idx);
            let row = u32::try_from(source.len_lines() + line_idx)? as usize;
            let column = u32::try_from(text_end_byte_idx - line_byte_idx)? as usize;
            Ok(tree_sitter::Point::new(row, column))
        } else {
            byte_offset_to_ts_point(new_end_byte, source)
        }
    }?;

    Ok(tree_sitter::InputEdit {
        start_byte: start_byte as usize,
        old_end_byte: old_end_byte as usize,
        new_end_byte: u32::try_from(new_end_byte)? as usize,
        start_position: start_point,
        old_end_position: old_end_point,
        new_end_position,
    })
}
