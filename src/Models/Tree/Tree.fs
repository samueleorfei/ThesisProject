namespace Models

open Models.Types

module Tree =
    let rec inorder (tree: Tree<'T>) : 'T list =
        match tree with
        | Null -> []
        | Node(e, l, r) -> inorder l @ [ e ] @ inorder r

    let rec preorder (tree: Tree<'T>) : 'T list =
        match tree with
        | Null -> []
        | Node(e, l, r) -> [ e ] @ inorder l @ inorder r

    let rec postorder (tree: Tree<'T>) : 'T list =
        match tree with
        | Null -> []
        | Node(e, l, r) -> inorder l @ inorder r @ [ e ]
