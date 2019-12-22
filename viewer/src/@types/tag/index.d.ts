declare namespace Tag {
    interface Node {
        tag: string;
        items: Gallery.Item[];
        children: Index;
    }
    interface NodeWithParent extends Node {
        parent: Node;
    }
    type Search = Node | NodeWithParent;
    type Index = { [index: string]: Node };
}