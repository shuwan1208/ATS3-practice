datatype 'a tree=
    Empty
    | Node of 'a tree * 'a * 'a tree;

fun preorder tree=
    case tree of
        Empty => []
        | Node(left, v, right) => [v] @ preorder left @ preorder right;
fun inorder tree =
    case tree of
        Empty => []
      | Node(left, v, right) => inorder left @ [v] @ inorder right;
fun postorder tree =
    case tree of
        Empty => []
      | Node(left, v, right) => postorder left @ postorder right @ [v];