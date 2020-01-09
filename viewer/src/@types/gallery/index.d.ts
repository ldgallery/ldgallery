declare namespace Gallery {
    interface Picture extends Item {
        properties: PictureProperties,
    }
    interface Directory extends Item {
        properties: DirectoryProperties,
    }
    interface Item {
        title: string,
        datetime: string,
        description: string,
        tags: RawTag[],
        path: string,
        thumbnail?: string,
        properties: PictureProperties | DirectoryProperties,
    }
    interface PictureProperties {
        type: "picture",
        resource: string,
    }
    interface DirectoryProperties {
        type: "directory",
        items: Item[]
    }
    type RawTag = string;
}