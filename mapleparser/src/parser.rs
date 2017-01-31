// Parser

use {Token, TokenSubtype};

#[derive(Clone)]
pub struct TokenRef<'s>
{
	container: &'s [Token], ptr: usize
}
impl<'s> TokenRef<'s>
{
	pub fn new(c: &'s [Token]) -> Self { TokenRef { container: c, ptr: 0 } }
	pub fn current(&self) -> &'s Token { self.peek(0) }
	pub fn peek(&self, offset: usize) -> &'s Token { if self.ptr + offset >= self.container.len() { self.container.last().unwrap() } else { &self.container[self.ptr + offset] } }
}
