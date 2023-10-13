namespace Models

module Types =
    type Formula =
        | True
        | False
        | Atom of string
        | Not of Formula
        | And of Formula * Formula
        | Or of Formula * Formula
        | Imp of Formula * Formula
        | Iff of Formula * Formula

    type Tree<'T> =
        | Null
        | Node of 'T * Tree<'T> * Tree<'T>
