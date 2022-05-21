module Tests

open Xunit
open Tree
open FsCheck.Xunit
open FsCheck

module ConstructorTests =
    [<Fact>]
    let ``Trees can consist of just a leaf node`` () =
        let tree = Leaf 1
        Assert.NotNull tree

    [<Fact>]
    let ``Trees can consist of branches and leaves`` () =
        let tree = Branch((Leaf 1), (Leaf 2))
        Assert.NotNull tree

module FunctorTests =
    open FsCheck

    [<Property>]
    let ``Trees follow the identity law of Functors`` (tree: Tree<int>) = tree |> map id = tree

    [<Property>]
    let ``Trees follow the composition law of Functors``
        (Fun f: Function<int, int>)
        (Fun g: Function<int, string>)
        (tree: Tree<int>)
        =
        (map f >> map g) tree = map (f >> g) tree

module ApplicativeTests =
    [<Property>]
    let ``Trees follow the identity law`` (tree: Tree<int>) = pureT id <*> tree = tree

    [<Property>]
    let ``Trees are homomorphic`` (value: int) =
        pureT string <*> pureT value = pureT (string value)

    [<Property>]
    let ``Trees follow the interchange law`` (value: int) =
        Leaf(string) <*> pureT value = (pureT (fun f -> f value) <*> Leaf(string))

    [<Property>]
    let ``Trees follow the composition law`` (u: Tree<float -> string>) (v: Tree<int -> float>) (w: Tree<int>) =
        (u <*> (v <*> w)) = (pureT (<<) <*> u <*> v <*> w)

module MonadTests =
    open FsCheck

    [<Property>]
    let ``Tree >>= pureT gives the same tree back`` (tree: Tree<int>) = tree = (tree >>= pureT)

    [<Property>]
    let ``Trees follow the right-identity law`` (value: int) (Fun func: Function<int, Tree<string>>) =
        (pureT value) >>= func = (func value)

    [<Property>]
    let ``Trees follow the associativity law``
        (tree: Tree<int>)
        (Fun f: Function<int, Tree<float>>)
        (Fun g: Function<float, Tree<string>>)
        =
        ((tree >>= f) >>= g) = (tree >>= (fun x -> f x >>= g))

module ComputationExpressionTests =
    let ``Trees work with and!`` (u: Tree<int>) (v: Tree<string>) (f: int -> string -> string) =
        let comp =
            tree {
                let! uItem = u
                and! vItem = v
                return f uItem vItem
            }

        let standard = f <!> u <*> v

        comp = standard

    let ``Trees work with let! and return!``
        (u: Tree<int>)
        (Fun f: Function<int, Tree<float>>)
        (Fun g: Function<float, Tree<string>>)
        =
        let comp =
            tree {
                let! x = u
                let! y = f x
                let! z = g y
                return z
            }

        let standard = u >>= f >>= g

        comp = standard
