use std::iter::{Enumerate, Peekable};
use std::str::{CharIndices, Chars};
use crate::lexer::lexer::LexerContext;

pub fn unescape(base: &str, context: &mut LexerContext) -> Result<String, (String, usize, usize)> {
    let mut output = String::with_capacity(base.len());

    let mut iter = base.char_indices().peekable();

    while let Some((i, c)) = iter.next() {
        if c == '\\' {
            if let Some((i, c2)) = iter.next() {
                match c2 {
                    '"' => output += "\"",
                    '\'' => output += "'",
                    '\\' => output += "\\",
                    't' => output += "\t",
                    'n' => output += "\n",
                    'r' => output += "\r",
                    'b' => output += "\u{0008}",
                    'f' => output += "\u{000C}",
                    'a' => output += "\u{0007}",
                    'v' => output += "\u{000B}",
                    '\n' => { /* ignore backslash and literal newline */ },
                    'x' => process_hex(&mut iter, &mut output),
                    'u' => process_unicode(&mut iter, &mut output, false, i)?,
                    'U' => process_unicode(&mut iter, &mut output, true, i)?,
                    '0'..='7' => process_octal(&mut iter, &mut output, c2, context),
                    u => output += &format!("\\{u}"),
                }

            } else {
                return Err(("Unclosed \\ escape".to_string(), i, 1))
            }
        } else {
            output += &format!("{c}");
        }
    }


    Ok(output)
}

enum ConversionResult {
    Ok(char),
    Exit(Option<char>),
}

fn get_hex_char(iter: &mut Peekable<CharIndices>) -> ConversionResult {
    if let Some((i, d0)) = iter.next() {
        match d0 {
            '0'..='9' | 'a'..='f' | 'A'..='F' => ConversionResult::Ok(d0),
            _ => ConversionResult::Exit(Some(d0))
        }
    } else {
        ConversionResult::Exit(None)
    }
}

fn get_octal_char(iter: &mut Peekable<CharIndices>) -> ConversionResult {
    if let Some((i, d0)) = iter.next() {
        match d0 {
            '0'..='7' => ConversionResult::Ok(d0),
            _ => ConversionResult::Exit(Some(d0))
        }
    } else {
        ConversionResult::Exit(None)
    }
}

macro_rules! get_hex {
    ( $output:expr, $iter:expr, $st:expr, $ret:expr ) => {
        match get_hex_char($iter) {
            ConversionResult::Ok(c) => c,
            ConversionResult::Exit(Some(c)) => {
                *$output += &format!($st, c);
                return $ret
            }
            ConversionResult::Exit(None) => return $ret
        }
    };
}
macro_rules! get_octal {
    ( $output:expr, $iter:expr, $st:expr, $ret:expr ) => {
        match get_octal_char($iter) {
            ConversionResult::Ok(c) => c,
            ConversionResult::Exit(Some(c)) => {
                *$output += &format!($st, c);
                return $ret
            }
            ConversionResult::Exit(None) => return $ret
        }
    };
}

macro_rules! as_digit {
    ( $x:expr, $base:expr ) => {
        $x.to_digit($base).unwrap()
    };
}

fn process_hex(iter: &mut Peekable<CharIndices>, output: &mut String) {
    let mut code = 0;
    let base = 0x10;
    let c1 = get_hex! { output, iter, "\\x{}", () };
    code += as_digit!(c1, base);
    code *= base;
    let c2 = get_hex! { output, iter, "\\x{c1}{}", () };
    code += as_digit!(c2, base);

    *output += &char::from_u32(code).unwrap().to_string();
}

fn process_unicode(iter: &mut Peekable<CharIndices>, output: &mut String, size8: bool, position: usize) -> Result<(), (String, usize, usize)> {
    let pf = if size8 { 'U' } else { 'u' };

    let base = 0x10;
    let mut code = 0;
    let c1 = get_hex! { output, iter, "\\{pf}{}", Ok(()) };
    code += as_digit!(c1, base);

    code *= base;
    let c2 = get_hex! { output, iter, "\\{pf}{c1}{}", Ok(()) };
    code += as_digit!(c2, base);

    code *= base;
    let c3 = get_hex! { output, iter, "\\{pf}{c1}{c2}{}", Ok(()) };
    code += as_digit!(c3, base);

    code *= base;
    let c4 = get_hex! { output, iter, "\\{pf}{c1}{c2}{c3}{}", Ok(()) };
    code += as_digit!(c4, base);

    if size8 {
        code *= base;
        let c5 = get_hex!{ output, iter, "\\U{c1}{c2}{c3}{c4}{}", Ok(()) };
        code += as_digit!(c5, base);

        code *= base;
        let c6 = get_hex!{ output, iter, "\\U{c1}{c2}{c3}{c4}{c5}{}", Ok(()) };
        code += as_digit!(c6, base);

        code *= base;
        let c7 = get_hex!{ output, iter, "\\U{c1}{c2}{c3}{c4}{c5}{c6}{}", Ok(()) };
        code += as_digit!(c7, base);

        code *= base;
        let c8 = get_hex!{ output, iter, "\\U{c1}{c2}{c3}{c4}{c5}{c6}{c7}{}", Ok(()) };
        code += as_digit!(c8, base);
    }

    if let Some(valid) = char::from_u32(code) {
        *output += &valid.to_string();
        Ok(())
    } else {
        Err(("Syntax error: (unicode error) 'unicodeescape' codec can't decode bytes in position 0-9: illegal Unicode character".to_string(), position, if size8 { 10 } else { 6 }))
    }

}

fn process_octal(iter: &mut Peekable<CharIndices>, output: &mut String, c1: char, context: &mut LexerContext) {

    let base = 0o10;
    let mut code = 0;
    code += as_digit!(c1, base);

    match iter.peek() {
        Some((_, '0'..='7')) => {
            code *= base;
            let c2 = get_octal! { output, iter, "\\{c1}{}", () };
            code += as_digit!(c2, base);

            match iter.peek() {
                Some((_, '0'..='7')) => {
                    code *= base;
                    let c3 = get_octal! { output, iter, "\\{c1}{c2}{}", () };
                    code += as_digit!(c3, base);
                }
                Some(_) | None => {}
            }

        }
        Some(_) | None => {}
    }

    if code > 255 {
        context.add_warning(format!("<python-input>:{}: SyntaxWarning: invalid octal escape sequence '\\{:o}'", context.position.line, code))
    }

    *output += &char::from_u32(code).unwrap().to_string();

}

