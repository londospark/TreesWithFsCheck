module Tree

type Tree<'a> =
    | Leaf of 'a
    | Branch of Tree<'a> * Tree<'a>

let rec map func =
    function
    | Leaf (node) -> Leaf(func node)
    | Branch (left, right) -> Branch((map func left), (map func right))

let (<!>) = map

let pureT x = Leaf x

let rec (<*>) fTree xTree =
    match (fTree, xTree) with
    | (Leaf f, Leaf x) -> pureT (f x)
    | (Leaf f, Branch (left, right)) -> Branch((pureT f <*> left), (pureT f <*> right))
    | (Branch (fLeft, fRight), Leaf x) -> Branch((fLeft <*> pureT x), (fRight <*> pureT x))
    | (Branch (fLeft, fRight), Branch (xLeft, xRight)) -> Branch((fLeft <*> xLeft), (fRight <*> xRight))

let rec (>>=) tree func =
    match tree with
    | Leaf node -> func node
    | Branch (left, right) -> Branch((left >>= func), (right >>= func))

let mergeSources x y = (fun a b -> (a, b)) <!> x <*> y

let (>=>) f g x = f x >>= g

type TreeBulider() =
    member _.BindReturn(x, f) = f <!> x
    member _.MergeSources(x, y) = mergeSources x y
    member _.Bind(x, f) = x >>= f

let tree = TreeBulider()
