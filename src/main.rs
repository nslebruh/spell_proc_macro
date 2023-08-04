use mylib::{wizlang, wizlang2};
fn main() {
  wizlang!(
    conjure test, which requires an integer x, an integer y and a boolean z, thusly containing,
      proclaimeth, "x: {}, y: {}, z: {}", concatenated with x, y, z.
    conclude.
    conjure test2, which requires nothing, that bestows an integer, thusly containing,
      declare y as 4 .
      bestow y
    conclude.
    declare z as invoke test2.
    invoke the magic of test, which takes z, 1 and true.
  );
  wizlang2!(
    conjure test3, which requires nothing and that bestows an integer, thusly containing, 
      declare y as 4 .
      bestow y times y .
    conclude.
    proclaimeth "{}, {}", invoke test3; 7 times 3 .
  );
}