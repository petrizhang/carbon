<div align=center>
<img src="https://raw.githubusercontent.com/pzque/carbon/master/doc/coco-logo-v3.png?token=GHSAT0AAAAAABSTI2RA7RM6OOC6WNI3KZZ4YSNCKRQ" width="300" alt="[coco logo]"/>
</div>

# Coco
A purely functional and statically typed programming language with HM type inference running on JVM.

## Development Status

This project aims to implement a language that covers Haskell's core features (in particular, the type system) on JVM with Scala.

Up to now, I have implemented a type inferencer for the simplest version of [Hindley–Milner (HM)](https://en.m.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) type system (located at [typer](https://github.com/pzque/carbon/tree/master/src/main/scala/com/pzque/coco/typer)), which is the basis of Haskell's typing.

The full-fledged type system outlined in [Typing in Haskell](https://web.cecs.pdx.edu/~mpj/thih/thih.pdf) is still under development in the mid-stage (located at [typer2](https://github.com/pzque/carbon/tree/master/src/main/scala/com/pzque/coco/typer2)).

I will resume development when I have enough time.
