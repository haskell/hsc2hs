## 0.68.1

 - Fix type signature of generated `main` test function
   to avoid C compiler warnings about unused `argc`/`argv`
   function parameters during feature testing.

 - Double-escape paths used to build call to `hsc_line`
   ([#12504](http://ghc.haskell.org/ticket/12504))

