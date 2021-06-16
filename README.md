# THICCRV32

Risc-V core goes here, hopefully in a couple of years.

### TODO

1. ~Implement ALU and CLU~
1. Implement decoding
1. Implement RVFI
1. Simulate with Verilator or some custom Rust simulator yeeehaw!
1. Use Shake to build, synthesize and verilate
1. Pipeline!
1. Build cache hierarchy
1. Predict some branches
1. Synthesize! With Shake of course
1. `cabal run clash --write-ghc-environment-files=always -- Core --verilog`



### Resources

#### Building a RISC-V SoC with Haskell:
* https://www.youtube.com/watch?v=NFguFKbuB_c
* https://github.com/christiaanb/contranomy/blob/tutorial/Tutorial.md

#### Gergő Érdi blog:
* https://unsafeperform.io/blog/2018-09-23-cpu_modeling_in_c_ash/

#### Gergő Érdi Intel8080:
* https://github.com/gergoerdi/clash-intel8080

#### RISC-V spec:
* https://riscv.org/technical/specifications/

#### RISC-V assembly:
* https://github.com/riscv/riscv-asm-manual/blob/master/riscv-asm.md

#### Clash docs:
* http://hackage.haskell.org/package/clash-prelude-1.2.4/docs/Clash-Prelude.html

#### RISC-V formal:
* https://github.com/SymbioticEDA/riscv-formal

#### Adam Walker implementation
* https://github.com/adamwalker/clash-riscv

#### RAM Tutorial
* http://hackage.haskell.org/package/clash-prelude-1.2.4/docs/Clash-Prelude-BlockRam.html#usingrams

#### Verilator integration
* https://gergo.erdi.hu/blog/2020-05-07-integrating_verilator_and_clash_via_cabal/
