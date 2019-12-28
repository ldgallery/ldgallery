declare namespace Gallery {
    interface Image extends Item {
        properties: ImageProperties,
    }
    interface Directory extends Item {
        properties: DirectoryProperties,
    }
    interface Item {
        title: string,
        date: string,
        description: string,
        tags: RawTag[],
        path: string,
        thumbnail: {
            path: string,
        },
        properties: ImageProperties | DirectoryProperties,
    }
    interface ImageProperties {
        type: "image",
        filesize: number,
        resolution: {
            width: number,
            height: number,
        }
    }
    interface DirectoryProperties {
        type: "directory",
        items: Item[]
    }
    type RawTag = string;
}