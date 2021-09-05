# Since the CWC algo is domain specific heres a brainstorm on how to fix this mess
+ Simulate a stack on the heap by generating the abstract spill instructions and then incrementing the heap pointer by the maximum number of spill locations we will need.
**This might actually work**
This is basically the inverse of what I was thinking before where we would pre allocate all heap data at the beginning of the block but much better since heap data could be arbitrarily large (arrays and such) but spill locations will almost always be zero or a small number.
+ Abstract spill instruction will take the offset
+ We would need to assign a block-local "stack pointer" at the beginning of the block as the heap ptr will change when allocating to the heap (duh).
+ **How would this integrate with graph colouring???**
+ Is graph colouring incompatible with this kind of "temporary spill" strategy?
+ Maybe we should consider just going full CWC and doing all register allocation their way? This would make instructions with specific register requirements (division) annoying to generate however.
+ **Consider just reading a lot of register allocation papers so you can actually learn this stuff?**
