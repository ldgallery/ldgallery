declare namespace Tag {
    interface Node {
        tag: string;
        items: Gallery.Item[];
        children: Index;
    }
    type Index = { [index: string]: Node };
}