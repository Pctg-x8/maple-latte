// Lexicalizer

#![cfg_attr(not(test), allow(dead_code))]

use std;
use rustc_serialize::*;

// line, left
#[derive(Clone, Debug, PartialEq, RustcEncodable)]
pub struct Location(usize, usize);
impl Default for Location { fn default() -> Self { Location(1, 1) } }
impl Location
{
    pub fn advance(&mut self, count: usize) { self.1 += count; }
    pub fn advance_line(&mut self, count: usize) { self.0 += count; self.1 = 1; }
}
pub struct SourceSlice<'a>(Location, &'a [char]);
impl<'s> SourceSlice<'s>
{
    pub fn new(source: &'s [char]) -> Self { SourceSlice(Location::default(), source) }
    pub fn current(&self) -> &Location { &self.0 }
    pub fn front(&self) -> Option<char> { if self.1.is_empty() { None } else { Some(self.1[0]) } }
    pub fn peek(&self, count: usize) -> Option<char> { if self.1.len() > count { Some(self.1[count]) } else { None } }
    pub fn drop_opt(&mut self, count: usize) -> &mut Self { self.0.advance(count); self.1 = &self.1[count..]; self }
    pub fn drop_line(&mut self) -> &mut Self { self.0.advance_line(1); self.1 = &self.1[1..]; self }
    pub fn drop_one(&mut self) -> &mut Self { if self.front() == Some('\n') { self.drop_line() } else { self.drop_opt(1) } }
    pub fn drop_and<F>(&mut self, count: usize, act: F) -> Result<Token, TokenizeError> where F: FnOnce(Location) -> Token
    {
        let l = self.0.clone();
        self.drop_opt(count);
        Ok(act(l))
    }

    pub fn drop_while<F>(&mut self, pred: F) -> &mut Self where F: Fn(char) -> bool
    {
        if pred('\n')
        {
            // unoptimized mode
            while self.front().map(&pred).unwrap_or(false)
            {
                if self.front() == Some('\n') { self.drop_line(); } else { self.drop_opt(1); }
            }
            self
        }
        else
        {
            // optimized mode
            let mut count = 0;
            while self.peek(count).map(&pred).unwrap_or(false) { count += 1; }
            self.drop_opt(count)
        }
    }
    pub fn take_while<F>(&mut self, pred: F) -> Self where F: Fn(char) -> bool
    {
        if pred('\n')
        {
            //unoptimized mode for Location
            let mut count = 0;
            let (start_loc, start_slice) = (self.0.clone(), self.1);
            while self.front().map(&pred).unwrap_or(false)
            {
                if self.front() == Some('\n') { self.drop_line(); } else { self.drop_opt(1); }
                count += 1;
            }
            SourceSlice(start_loc, &start_slice[..count])
        }
        else
        {
            // optimized mode
            let mut count = 0;
            while self.peek(count).map(&pred).unwrap_or(false) { count += 1; }
            let retval = SourceSlice(self.0.clone(), &self.1[..count]);
            self.drop_opt(count);
            retval
        }
    }
}
#[derive(Debug, PartialEq)]
pub struct Token { pub pos: Location, pub subtype: TokenSubtype }
#[derive(Debug, PartialEq, RustcEncodable, Clone, Copy)] pub enum PairDirection { Open, Close }
#[derive(Debug, PartialEq, RustcEncodable, Clone, Copy)] pub enum OperatorOptions { None, WithEqual, Twice }
#[derive(Debug, PartialEq)]
pub enum TokenSubtype
{
    Term, Identifier(String), StringLiteral(String), CharacterLiteral(char),
    Parenthese(PairDirection), Brace(PairDirection), Bracket(PairDirection), AngleBracket { twice: bool, equal: bool, dir: PairDirection }, Period(usize),
    Colon, Comma, Semicolon, Atmark, Sharp,
    Plus(OperatorOptions), Minus(OperatorOptions), Asterisk(OperatorOptions), Slash { equal: bool }, Percent { equal: bool },
    Ampasand { equal: bool, twice: bool }, VerticalLine { equal: bool, twice: bool }, Accent { equal: bool },
    Equal { twice: bool }, Exclamation { equal: bool }, Tilde { equal: bool }, Question
}
impl<'s> Encodable for Token
{
    fn encode<S: Encoder>(&self, s: &mut S) -> Result<(), S::Error>
    {
        use self::TokenSubtype::*;

        s.emit_struct("Token", 3, |s|
        {
            try!(s.emit_struct_field("line", 0, |s| s.emit_usize(self.pos.0)));
            try!(s.emit_struct_field("column", 1, |s| s.emit_usize(self.pos.1)));
            s.emit_struct_field("subtype", 2, |s| match &self.subtype
            {
                &Term => s.emit_str("Term"),
                &Identifier(ref v) => s.emit_struct("Identifier", 2, |s|
                {
                    try!(s.emit_struct_field("name", 0, |s| s.emit_str("Identifier")));
                    s.emit_struct_field("value", 1, |s| s.emit_str(v))
                }),
                &StringLiteral(ref v) => s.emit_struct("StringLiteral", 2, |s|
                {
                    try!(s.emit_struct_field("name", 0, |s| s.emit_str("StringLiteral")));
                    s.emit_struct_field("value", 1, |s| s.emit_str(v))
                }),
                &CharacterLiteral(c) => s.emit_struct("CharacterLiteral", 2, |s|
                {
                    try!(s.emit_struct_field("name", 0, |s| s.emit_str("CharacterLiteral")));
                    s.emit_struct_field("value", 1, |s| s.emit_char(c))
                }),
                &Parenthese(p) => s.emit_struct("Parenthese", 2, |s|
                {
                    try!(s.emit_struct_field("name", 0, |s| s.emit_str("Parenthese")));
                    s.emit_struct_field("dir", 1, |s| p.encode(s))
                }),
                &Bracket(p) => s.emit_struct("Bracket", 2, |s|
                {
                    try!(s.emit_struct_field("name", 0, |s| s.emit_str("Bracket")));
                    s.emit_struct_field("dir", 1, |s| p.encode(s))
                }),
                &Brace(p) => s.emit_struct("Brace", 2, |s|
                {
                    try!(s.emit_struct_field("name", 0, |s| s.emit_str("Brace")));
                    s.emit_struct_field("dir", 1, |s| p.encode(s))
                }),
                &AngleBracket { twice, equal, dir } => s.emit_struct("AngleBracket", 4, |s|
                {
                    try!(s.emit_struct_field("name", 0, |s| s.emit_str("AngleBracket")));
                    try!(s.emit_struct_field("dir", 1, |s| dir.encode(s)));
                    try!(s.emit_struct_field("twice", 2, |s| s.emit_bool(twice)));
                    s.emit_struct_field("equal", 3, |s| s.emit_bool(equal))
                }),
                &Period(c) => s.emit_struct("Period", 2, |s|
                {
                    try!(s.emit_struct_field("name", 0, |s| s.emit_str("Period")));
                    s.emit_struct_field("count", 1, |s| s.emit_usize(c))  
                }),
                &Colon => s.emit_str("Colon"),
                &Semicolon => s.emit_str("Semicolon"),
                &Comma => s.emit_str("Comma"),
                &Atmark => s.emit_str("Atmark"),
                &Sharp => s.emit_str("Sharp"),
                &Question => s.emit_str("Question"),
                &Plus(opts) => s.emit_struct("Plus", 2, |s|
                {
                    try!(s.emit_struct_field("name", 0, |s| s.emit_str("Plus")));
                    s.emit_struct_field("options", 1, |s| opts.encode(s))
                }),
                &Minus(opts) => s.emit_struct("Minus", 2, |s|
                {
                    try!(s.emit_struct_field("name", 0, |s| s.emit_str("Minus")));
                    s.emit_struct_field("options", 1, |s| opts.encode(s))
                }),
                &Asterisk(opts) => s.emit_struct("Asterisk", 2, |s|
                {
                    try!(s.emit_struct_field("name", 0, |s| s.emit_str("Asterisk")));
                    s.emit_struct_field("options", 1, |s| opts.encode(s))
                }),
                &Slash { equal } => s.emit_struct("Slash", 2, |s|
                {
                    try!(s.emit_struct_field("name", 0, |s| s.emit_str("Slash")));
                    s.emit_struct_field("equal", 1, |s| s.emit_bool(equal))
                }),
                &Percent { equal } => s.emit_struct("Percent", 2, |s|
                {
                    try!(s.emit_struct_field("name", 0, |s| s.emit_str("Percent")));
                    s.emit_struct_field("equal", 1, |s| s.emit_bool(equal))
                }),
                &Accent { equal } => s.emit_struct("Accent", 2, |s|
                {
                    try!(s.emit_struct_field("name", 0, |s| s.emit_str("Accent")));
                    s.emit_struct_field("equal", 1, |s| s.emit_bool(equal))
                }),
                &Exclamation { equal } => s.emit_struct("Exclamation", 2, |s|
                {
                    try!(s.emit_struct_field("name", 0, |s| s.emit_str("Exclamation")));
                    s.emit_struct_field("equal", 1, |s| s.emit_bool(equal))
                }),
                &Tilde { equal } => s.emit_struct("Tilde", 2, |s|
                {
                    try!(s.emit_struct_field("name", 0, |s| s.emit_str("Tilde")));
                    s.emit_struct_field("equal", 1, |s| s.emit_bool(equal))
                }),
                &Equal { twice } => s.emit_struct("Equal", 2, |s|
                {
                    try!(s.emit_struct_field("name", 0, |s| s.emit_str("Equal")));
                    s.emit_struct_field("twice", 1, |s| s.emit_bool(twice))
                }),
                &Ampasand { twice, equal } => s.emit_struct("Ampasand", 3, |s|
                {
                    try!(s.emit_struct_field("name", 0, |s| s.emit_str("Ampasand")));
                    try!(s.emit_struct_field("twice", 1, |s| s.emit_bool(twice)));
                    s.emit_struct_field("equal", 2, |s| s.emit_bool(equal))
                }),
                &VerticalLine { twice, equal } => s.emit_struct("VerticalLine", 3, |s|
                {
                    try!(s.emit_struct_field("name", 0, |s| s.emit_str("VerticalLine")));
                    try!(s.emit_struct_field("twice", 1, |s| s.emit_bool(twice)));
                    s.emit_struct_field("equal", 2, |s| s.emit_bool(equal))
                })
            })
        })
    }
}
#[derive(Debug, PartialEq)]
pub enum TokenizeError
{
    UnclosedComment(Location), UnclosedStringLiteral(Location), UnclosedCharacterLiteral(Location), EmptyCharacterLiteral(Location)
}

pub fn tokenize<'s>(input: &mut SourceSlice<'s>) -> Result<Token, TokenizeError>
{
    use self::TokenizeError::*;
    use self::TokenSubtype::*;
    use self::PairDirection::*;

    fn split_ident(c: char) -> bool
    {
        c == '\n' || c == '\t' ||
        c == ' ' || c == '/' || c == '(' || c == ')' || c == '{' || c == '}' || c == '<' || c == '>' || c == '[' || c == ']' || c == '.' ||
        c == ':' || c == ';' || c == ','
    }
    fn drop_block_comment<'s, 'd>(input: &'d mut SourceSlice<'s>) -> Result<&'d mut SourceSlice<'s>, TokenizeError>
    {
        fn recursive<'s, 'd>(input: &'d mut SourceSlice<'s>) -> Result<&'d mut SourceSlice<'s>, TokenizeError>
        {
            match input.front()
            {
                None => Err(TokenizeError::UnclosedComment(input.current().clone())),
                Some('*') if input.peek(1) == Some('/') => Ok(input.drop_opt(2)),
                Some('/') if input.peek(1) == Some('*') => drop_block_comment(input).and_then(recursive),
                _ => recursive(input.drop_one())
            }
        }
        recursive(input.drop_opt(2))
    }
    fn parse_char_fragment<'s, 'i, 'd>(input: &'i mut SourceSlice<'s>, sink: &'d mut String) -> Result<(), TokenizeError>
    {
        match input.front()
        {
            Some('\\') => parse_escape(input.drop_opt(1), sink, parse_char_fragment),
            Some('"') => { input.drop_opt(1); Ok(()) },
            Some(c) => { sink.push(c); parse_char_fragment(input.drop_one(), sink) },
            None => Err(UnclosedStringLiteral(input.current().clone()))
        }
    }
    fn parse_escape<'s, 'd, F>(input: &mut SourceSlice<'s>, sink: &'d mut String, returning: F) -> Result<(), TokenizeError>
        where F: FnOnce(&mut SourceSlice<'s>, &'d mut String) -> Result<(), TokenizeError>
    {
        match input.front()
        {
            Some('n') => { sink.push('\n'); returning(input.drop_opt(1), sink) },
            Some('t') => { sink.push('\t'); returning(input.drop_opt(1), sink) },
            Some(c@'x') | Some(c@'X') => if let (Some(b1), Some(b2)) = (input.peek(1).and_then(|c| c.to_digit(16)), input.peek(2).and_then(|c| c.to_digit(16)))
            {
                sink.push(char::from(((b1 << 4) | b2) as u8)); returning(input.drop_opt(3), sink)
            }
            else { sink.push(c); returning(input.drop_opt(1), sink) },
            Some(c@'u') | Some(c@'U') => if let (Some(b1), Some(b2), Some(b3), Some(b4)) = (
                input.peek(1).and_then(|c| c.to_digit(16)), input.peek(2).and_then(|c| c.to_digit(16)),
                input.peek(3).and_then(|c| c.to_digit(16)), input.peek(4).and_then(|c| c.to_digit(16))
            ) { sink.push_str(&std::char::decode_utf16([((b1 << 12) | (b2 << 8) | (b3 << 4) | b4) as u16].into_iter().cloned()).collect::<Result<String, _>>().unwrap()); returning(input.drop_opt(5), sink) }
            else { sink.push(c); returning(input.drop_opt(1), sink) },
            Some(c) => { sink.push(c); returning(input.drop_one(), sink) },
            None => Err(UnclosedStringLiteral(input.current().clone()))
        }
    }
    fn parse_string<'s>(input: &mut SourceSlice<'s>) -> Result<Token, TokenizeError>
    {
        let mut sink = String::new();
        let start_loc = input.current().clone();
        parse_char_fragment(input.drop_opt(1), &mut sink).map(|_| Token { pos: start_loc, subtype: StringLiteral(sink) })
    }
    fn parse_char<'s>(input: &mut SourceSlice<'s>) -> Result<Token, TokenizeError>
    {
        let mut sink = String::new();
        let start_loc = input.current().clone();
        input.drop_opt(1);
        match input.front()
        {
            Some('\\') => parse_escape(input, &mut sink, |_, _| Ok(())).and_then(|_|
            {
                if input.front() == Some('\'')
                {
                    input.drop_opt(1);
                    Ok(Token { pos: start_loc, subtype: CharacterLiteral(sink.chars().next().unwrap()) })
                }
                else { Err(UnclosedCharacterLiteral(input.current().clone())) }
            }),
            Some('\'') => Err(EmptyCharacterLiteral(input.current().clone())),
            Some(c) => if input.drop_opt(1).front() == Some('\'')
            {
                input.drop_one(); input.drop_opt(1);
                Ok(Token { pos: start_loc, subtype: CharacterLiteral(c) })
            }
            else { Err(UnclosedCharacterLiteral(input.current().clone())) },
            None => Err(UnclosedCharacterLiteral(input.current().clone()))
        }
    }
    match input.front()
    {
        None => Ok(Token { pos: input.current().clone(), subtype: TokenSubtype::Term }),
        Some(' ') | Some('\t') | Some('\n') => tokenize(input.drop_while(|c| c == ' ' || c == '\t' || c == '\n')),
        Some('/') if input.peek(1) == Some('/') => tokenize(input.drop_while(|c| c != '\n').drop_line()),
        Some('/') if input.peek(1) == Some('*') => drop_block_comment(input).and_then(tokenize),
        Some('(') => input.drop_and(1, |p| Token { pos: p, subtype: Parenthese(PairDirection::Open) }),
        Some(')') => input.drop_and(1, |p| Token { pos: p, subtype: Parenthese(PairDirection::Close) }),
        Some('[') => input.drop_and(1, |p| Token { pos: p, subtype: Bracket(PairDirection::Open) }),
        Some(']') => input.drop_and(1, |p| Token { pos: p, subtype: Bracket(PairDirection::Close) }),
        Some('{') => input.drop_and(1, |p| Token { pos: p, subtype: Brace(PairDirection::Open) }),
        Some('}') => input.drop_and(1, |p| Token { pos: p, subtype: Brace(PairDirection::Close) }),
        Some('<') => match input.peek(1)
        {
            Some('<') => match input.peek(2)
            {
                Some('=') => input.drop_and(3, |p| Token { pos: p, subtype: AngleBracket { twice: true, equal: true, dir: Open } }),
                _ => input.drop_and(2, |p| Token { pos: p, subtype: AngleBracket { twice: true, equal: false, dir: Open } })
            },
            Some('=') => input.drop_and(2, |p| Token { pos: p, subtype: AngleBracket { twice: false, equal: true, dir: Open } }),
            _ => input.drop_and(1, |p| Token { pos: p, subtype: AngleBracket { twice: false, equal: false, dir: Open } })
        },
        Some('>') => match input.peek(1)
        {
            Some('>') => match input.peek(2)
            {
                Some('=') => input.drop_and(3, |p| Token { pos: p, subtype: AngleBracket { twice: true, equal: true, dir: Close } }),
                _ => input.drop_and(2, |p| Token { pos: p, subtype: AngleBracket { twice: true, equal: false, dir: Close } })
            },
            Some('=') => input.drop_and(2, |p| Token { pos: p, subtype: AngleBracket { twice: false, equal: true, dir: Close } }),
            _ => input.drop_and(1, |p| Token { pos: p, subtype: AngleBracket { twice: false, equal: false, dir: Close } })
        },
        Some('.') => if input.peek(1) == Some('.')
        {
            if input.peek(2) == Some('.') { input.drop_and(3, |p| Token { pos: p, subtype: Period(3) }) }
            else { input.drop_and(2, |p| Token { pos: p, subtype: Period(2) }) }
        }
        else { input.drop_and(1, |p| Token { pos: p, subtype: Period(1) }) },
        Some(':') => input.drop_and(1, |p| Token { pos: p, subtype: Colon }),
        Some(';') => input.drop_and(1, |p| Token { pos: p, subtype: Semicolon }),
        Some(',') => input.drop_and(1, |p| Token { pos: p, subtype: Comma }),
        Some('@') => input.drop_and(1, |p| Token { pos: p, subtype: Atmark }),
        Some('#') => input.drop_and(1, |p| Token { pos: p, subtype: Sharp }),
        Some('?') => input.drop_and(1, |p| Token { pos: p, subtype: Question }),
        Some('"') => parse_string(input),
        Some('\'') => parse_char(input),
        Some('+') => match input.peek(1)
        {
            Some('=') => input.drop_and(2, |p| Token { pos: p, subtype: Plus(OperatorOptions::WithEqual) }),
            Some('+') => input.drop_and(2, |p| Token { pos: p, subtype: Plus(OperatorOptions::Twice) }),
            _ => input.drop_and(1, |p| Token { pos: p, subtype: Plus(OperatorOptions::None) })
        },
        Some('-') => match input.peek(1)
        {
            Some('=') => input.drop_and(2, |p| Token { pos: p, subtype: Minus(OperatorOptions::WithEqual) }),
            Some('-') => input.drop_and(2, |p| Token { pos: p, subtype: Minus(OperatorOptions::Twice) }),
            _ => input.drop_and(1, |p| Token { pos: p, subtype: Minus(OperatorOptions::None) })
        },
        Some('*') => match input.peek(1)
        {
            Some('=') => input.drop_and(2, |p| Token { pos: p, subtype: Asterisk(OperatorOptions::WithEqual) }),
            Some('*') => input.drop_and(2, |p| Token { pos: p, subtype: Asterisk(OperatorOptions::Twice) }),
            _ => input.drop_and(1, |p| Token { pos: p, subtype: Asterisk(OperatorOptions::None) })
        },
        Some('/') => match input.peek(1)
        {
            Some('=') => input.drop_and(2, |p| Token { pos: p, subtype: Slash { equal: true } }),
            _ => input.drop_and(1, |p| Token { pos: p, subtype: Slash { equal: false } })
        },
        Some('%') => match input.peek(1)
        {
            Some('=') => input.drop_and(2, |p| Token { pos: p, subtype: Percent { equal: true } }),
            _ => input.drop_and(1, |p| Token { pos: p, subtype: Percent { equal: false } })
        },
        Some('&') => match input.peek(1)
        {
            Some('&') => if input.peek(2) == Some('=') { input.drop_and(3, |p| Token { pos: p, subtype: Ampasand { twice: true, equal: true } }) }
            else { input.drop_and(2, |p| Token { pos: p, subtype: Ampasand { twice: true, equal: false } }) },
            Some('=') => input.drop_and(2, |p| Token { pos: p, subtype: Ampasand { twice: false, equal: true } }),
            _ => input.drop_and(1, |p| Token { pos: p, subtype: Ampasand { twice: false, equal: false } })
        },
        Some('|') => match input.peek(1)
        {
            Some('|') => if input.peek(2) == Some('=') { input.drop_and(3, |p| Token { pos: p, subtype: VerticalLine { twice: true, equal: true } }) }
            else { input.drop_and(2, |p| Token { pos: p, subtype: VerticalLine { twice: true, equal: false } }) },
            Some('=') => input.drop_and(2, |p| Token { pos: p, subtype: VerticalLine { twice: false, equal: true } }),
            _ => input.drop_and(1, |p| Token { pos: p, subtype: VerticalLine { twice: false, equal: false } })
        },
        Some('^') => match input.peek(1)
        {
            Some('=') => input.drop_and(2, |p| Token { pos: p, subtype: Accent { equal: true } }),
            _ => input.drop_and(1, |p| Token { pos: p, subtype: Accent { equal: false } })
        },
        Some('=') => match input.peek(1)
        {
            Some('=') => input.drop_and(2, |p| Token { pos: p, subtype: Equal { twice: true } }),
            _ => input.drop_and(1, |p| Token { pos: p, subtype: Equal { twice: false } })
        },
        Some('!') => match input.peek(1)
        {
            Some('=') => input.drop_and(2, |p| Token { pos: p, subtype: Exclamation { equal: true } }),
            _ => input.drop_and(1, |p| Token { pos: p, subtype: Exclamation { equal: false } })
        },
        Some('~') => match input.peek(1)
        {
            Some('=') => input.drop_and(2, |p| Token { pos: p, subtype: Tilde { equal: true } }),
            _ => input.drop_and(1, |p| Token { pos: p, subtype: Tilde { equal: false } })
        },
        Some(_) =>
        {
            let SourceSlice(idloc, id) = input.take_while(|c| !split_ident(c));
            assert!(!id.is_empty());
            Ok(Token { pos: idloc, subtype: Identifier(id.iter().cloned().collect()) })
        }
    }
}

#[test] fn tokenize_idents()
{
    assert_eq!(tokenize(&mut SourceSlice::new(&['a', 'b', 'c', '\n'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Identifier("abc".into()) }));
}
#[test] fn tokenize_comments()
{
    assert_eq!(tokenize(&mut SourceSlice::new(&['/', '*', 'a', '*', '/'])), Ok(Token { pos: Location(1, 6), subtype: TokenSubtype::Term }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['/', '/', 'a', ' ', '\n', 't'])), Ok(Token { pos: Location(2, 1), subtype: TokenSubtype::Identifier("t".into()) }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['/', '*', 'a', '/', '*', 'b', '*', '*', '/', '*', '/', '\n'])), Ok(Token { pos: Location(2, 1), subtype: TokenSubtype::Term }));
}
#[test] fn tokenize_pairs()
{
    assert_eq!(tokenize(&mut SourceSlice::new(&['<'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::AngleBracket { twice: false, equal: false, dir: PairDirection::Open } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['>'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::AngleBracket { twice: false, equal: false, dir: PairDirection::Close } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['['])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Bracket(PairDirection::Open) }));
    assert_eq!(tokenize(&mut SourceSlice::new(&[']'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Bracket(PairDirection::Close) }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['{'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Brace(PairDirection::Open) }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['}'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Brace(PairDirection::Close) }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['('])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Parenthese(PairDirection::Open) }));
    assert_eq!(tokenize(&mut SourceSlice::new(&[')'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Parenthese(PairDirection::Close) }));
}
#[test] fn tokenize_periods()
{
    assert_eq!(tokenize(&mut SourceSlice::new(&['.'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Period(1) }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['.', '.'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Period(2) }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['.', '.', '.'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Period(3) }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['.', ' ', '.', '.'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Period(1) }));
}
#[test] fn tokenize_delimiters()
{
    assert_eq!(tokenize(&mut SourceSlice::new(&[':'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Colon }));
    assert_eq!(tokenize(&mut SourceSlice::new(&[';'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Semicolon }));
    assert_eq!(tokenize(&mut SourceSlice::new(&[','])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Comma }));
}
#[test] fn tokenize_special_chars()
{
    assert_eq!(tokenize(&mut SourceSlice::new(&['@'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Atmark }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['#'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Sharp }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['?'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Question }));
}
#[test] fn tokenize_operators()
{
    assert_eq!(tokenize(&mut SourceSlice::new(&['+'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Plus(OperatorOptions::None) }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['+', '+'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Plus(OperatorOptions::Twice) }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['+', '='])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Plus(OperatorOptions::WithEqual) }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['-'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Minus(OperatorOptions::None) }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['-', '-'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Minus(OperatorOptions::Twice) }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['-', '='])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Minus(OperatorOptions::WithEqual) }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['*'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Asterisk(OperatorOptions::None) }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['*', '*'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Asterisk(OperatorOptions::Twice) }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['*', '='])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Asterisk(OperatorOptions::WithEqual) }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['/'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Slash { equal: false } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['/', '='])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Slash { equal: true } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['%'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Percent { equal: false } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['%', '='])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Percent { equal: true } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['&'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Ampasand { twice: false, equal: false } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['&', '='])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Ampasand { twice: false, equal: true } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['&', '&'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Ampasand { twice: true, equal: false } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['&', '&', '='])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Ampasand { twice: true, equal: true } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['|'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::VerticalLine { twice: false, equal: false } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['|', '='])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::VerticalLine { twice: false, equal: true } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['|', '|'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::VerticalLine { twice: true, equal: false } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['|', '|', '='])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::VerticalLine { twice: true, equal: true } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['^'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Accent { equal: false } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['^', '='])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Accent { equal: true } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['='])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Equal { twice: false } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['=', '='])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Equal { twice: true } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['!'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Exclamation { equal: false } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['!', '='])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Exclamation { equal: true } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['~'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Tilde { equal: false } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['~', '='])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::Tilde { equal: true } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['<', '='])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::AngleBracket { twice: false, equal: true, dir: PairDirection::Open } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['>', '='])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::AngleBracket { twice: false, equal: true, dir: PairDirection::Close } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['<', '<'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::AngleBracket { twice: true, equal: false, dir: PairDirection::Open } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['>', '>'])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::AngleBracket { twice: true, equal: false, dir: PairDirection::Close } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['<', '<', '='])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::AngleBracket { twice: true, equal: true, dir: PairDirection::Open } }));
    assert_eq!(tokenize(&mut SourceSlice::new(&['>', '>', '='])), Ok(Token { pos: Location::default(), subtype: TokenSubtype::AngleBracket { twice: true, equal: true, dir: PairDirection::Close } }));
}
