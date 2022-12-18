pub struct PeekPeekIterator<I: Iterator> {
    iter: I,
    peek_one: Option<Option<I::Item>>,
    peek_two: Option<Option<I::Item>>,
}

impl<I: Iterator> PeekPeekIterator<I> {
    pub fn new(iter: I) -> Self {
        PeekPeekIterator {
            iter,
            peek_one: None,
            peek_two: None,
        }
    }

    pub fn peek(&mut self) -> Option<&I::Item> {
        let iter = &mut self.iter;
        self.peek_one.get_or_insert_with(|| iter.next()).as_ref()
    }

    pub fn peek_peek(&mut self) -> Option<&I::Item> {
        self.peek();
        let iter = &mut self.iter;
        self.peek_two.get_or_insert_with(|| iter.next()).as_ref()
    }
}

impl<I: Iterator> Iterator for PeekPeekIterator<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peek_one.take() {
            Some(v) => {
                self.peek_one = self.peek_two.take();
                v
            }
            None => match self.peek_two.take() {
                Some(v) => v,
                None => self.iter.next(),
            },
        }
    }
}
