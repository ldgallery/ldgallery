declare namespace Gallery {
    interface Item {
        title: string,
        date: string,
        description: string,
        tags: string[],
        path: string,
        thumbnail: {
            path: string,
        },
        properties: Image | Directory,
    }
    interface Image {
        type: "image",
        filesize: number,
        resolution: {
            width: number,
            height: number,
        }
    }
    interface Directory {
        type: "directory",
        items: Item[]
    }
}