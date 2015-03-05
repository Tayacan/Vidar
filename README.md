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
