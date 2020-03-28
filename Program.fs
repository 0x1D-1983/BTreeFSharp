open System

type Tree<'a> = 
    | Empty
    | Node of leaf:'a * left:Tree<'a> * right:Tree<'a> 

let tree = 
    Node (
        20, 
        Node (
            9,
            Node (
                4,
                Node (2, Empty, Empty),
                Empty),
            Node (
                10,
                Empty,
                Empty)), 
        Empty)


let rec contains item tree = 
    match tree with
    | Empty -> false
    | Node (leaf, left, right) ->
        if item = leaf then true
        elif item < leaf then contains item left
        else contains item right

let rec insert item tree = 
    match tree with
    | Empty -> Node (item, Empty, Empty)
    | Node (leaf, left, right) as node ->
        if item = leaf then node
        elif item < leaf then Node (leaf, insert item left, right)
        else Node (leaf, left, insert item right)

let rec inorder action tree = 
    seq {
        match tree with
        | Empty -> ()
        | Node (leaf, left, right) ->
            yield! inorder action left
            yield action leaf
            yield! inorder action right
    }


[<EntryPoint>]
tree |> inorder (printfn "%d") |> printfn "%A"
