# TODO:

 - [ ] Try using memoization.  (The few libraries I tried actually
       slowed down things, likely because of fine granularity.  But
       this is a case flush with memoization opportunities!  I believe
       some hand-tuned memoization will help.)
 - [ ] Try alternate implementation with Repa.
 - [ ] Try DPH/Stream fusion.
 - [ ] Measure space usage; use pipes/conduit, if necessary.
 - [ ] Handle scanline padding, if present.
 - [ ] Add support for more complex (8/16/32-bit colors, more than one
       plane, newer/older BMP versions) input.
 - [ ] Use a histogram-based palette, rather than a static fixed
       palette.
 - [ ] Use exceptions instead of 'error'.
 - [ ] Use Strategies.
 - [ ] Use Critereon for benchmarking.

