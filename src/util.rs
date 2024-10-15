use ropey::Rope;
use tower_lsp::lsp_types::*;
use tree_sitter::Point;

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
