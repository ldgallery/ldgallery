declare namespace Tag {
    interface Node {
        tag: string;
        items: Gallery.Item[];
        children: Index;
    }
    interface Search extends Node {
        parent?: Node;
        operation: string; // Enum Operation
        display: string;
    }
    type SearchByOperation = { [index: string]: Tag.Search[] };
    type Index = { [index: string]: Node };
}