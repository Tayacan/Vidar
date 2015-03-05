# Vidar

Vidar is a system for testing structural optimizations in compilers. It is being designed primarily for use with (Futhark)[https://github.com/HIPERFIT/futhark], but it should be possible to adapt it to practically any optimizing compiler.

## The Vidar language

Vidar contains a small language for specifying program structures. How these structures correspond to structures in the language being tested is entirely up to the user. For example, in Futhark, a function:

    fun int main(int a) =
      let b = a + 1 in
      b

Would be represented in Vidar as:

    `main` ((`a`),
    (
      `b` = `plus` (`a`, `1`),
      (`b`)
    )

This may seem a bit cumbersome, but it is rare that one would want to write a direct translation into Vidar by hand - that sort of thing should be handled automatically. Instead, one might write a test that checks if the function main has exactly one argument, and if that argument is used in a plus-expression inside the body.

    `main` (("x"),
    (
      _ = `plus` {"x"}
    )

This checks that:

1. There is exactly one parameter, which will be refered to as "x"
2. There is a plus expression, and the result of that expression is bound to some name which we don't care about
3. A variable with the same name as the parameter is an argument to that expression. It doesn't matter _which_ argument.


## Real-world Example

An example of a futhark program with a Vidar test:

```
// BEGIN_VIDAR
// `main` ({},
//   {
//     ~ _ = `assert` {}
//   }
// )
// END_VIDAR

fun int main(bool b) =
     let a = [1,2,3] in
     let i = if b then 0 else 1 in
     a[i]
```

The Vidar code says that there must be a block with the name "main", consisting of two sub-blocks (the parameters and the body). We do not care about the parameters, so that block is empty. In the body, we have a single statement saying that there must not be any assert-statements.
